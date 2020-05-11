{-# LANGUAGE BinaryLiterals #-}
module Codegen where
import           AST
import           Control.Monad (when)
import           Control.Monad (unless)
import           Data.Bits     hiding (Bits)
import           Data.Char     (intToDigit)
import           Data.IORef
import           Data.List     (intercalate)
import           Data.Maybe    (catMaybes)
import           Debug.Trace
import           Numeric       (showIntAtBase)

---
--- High-level matching on instruction encoding
---

-- | An instruction encoding and its constraints
data InstrMatch = InstrMatch { iname       :: String
                             , bitstring   :: BitStr
                             , constraints :: [InstrConstraint]
                             }
              deriving (Eq, Ord)

instance Show InstrMatch where
  show (InstrMatch name str constrs) =
    let tests = intercalate "\n" $ map show constrs
    in (unlines [name ++ ":", str]) ++ tests

type BitStr = String

genConstantMatchInstr :: (Instruction, String) -> IO InstrMatch
genConstantMatchInstr (inst, name) = do
  bstr <- mapM genInstrMatch $ reverse $ allBits inst
  let bitstring = concat $ catMaybes bstr
  ref <- newIORef 0
  tests <- mapM (genComplexConstraint ref) $ complexConstraints inst
  return $ InstrMatch name bitstring (map mkConstraint tests)

genInstrMatch :: Bits -> IO (Maybe BitStr)
genInstrMatch bits =
  case bits of
    Global {} -> return Nothing
    Bits bs c -> do
      let len  = high bs - low bs + 1
      case c of
        Constant v -> do
          let str       = showIntAtBase 2 intToDigit v ""
          when (length str > len) $ error $ unwords [ "Overly long string:"
                                                    , str
                                                    , ". Expected"
                                                    , show bs
                                                    ]
          return $ Just $ replicate (len - length str) '0' ++ str
        _          -> return $ Just $ replicate len 'x'

---
--- Generating invariant tests for a given instruction
---

-- | Constraints on an instruction
data InstrConstraint = BitConstraint String [BitTest]
                   deriving (Eq, Ord)


instance Show InstrConstraint where
    show (BitConstraint str tests) =
      unlines $ ["Tests for" ++ str ++ ":"] ++ map show (reverse tests)

mkConstraint :: [BitTest] -> InstrConstraint
mkConstraint = BitConstraint "Debug"

-- | A test can either be a top-level comparison against 1,
-- or an assingment to a temporary variable
data BitTest = BinOp { left  :: BitTest
                     , op    :: Op
                     , right :: BitTest
                     }
             | UnaryOp { op   :: Op
                       , node :: BitTest
                       }
             | NoOp { var :: Var }
             deriving (Eq, Ord)

instance Show BitTest where
    show (BinOp l o r) = unwords ["(", show l, ")", show o, "(", show r, ")"]
    show (UnaryOp o n) = unwords [show o, show n]
    show (NoOp v)      = show v

-- | Vars are either temporaries, constants, or the encoding itself
data Var = Val Int
         | Encoding
         deriving (Eq, Ord)

instance Show Var where
    show (Val i)  = show i
    show Encoding = "enc"

-- | The operation: And, or, etc.
data Op = AndBits
        | OrBits
        | XorBits
        | ShiftBits
        | AddBits
        | NotBits
        | EqBits
        | NeqBits
        deriving (Eq, Ord)

instance Show Op where
    show AndBits   = "&"
    show OrBits    = "|"
    show XorBits   = "^"
    show ShiftBits = "<<"
    show AddBits   = "+"
    show NotBits   = "~"
    show EqBits    = "=="
    show NeqBits   = "!="

---
--- Generating bittests
---

noShift = -1
shiftOf n = n

genBitTest :: GlobalConstraint -> IO (BitTest, Int)
genBitTest gc =
  case gc of
    -- Number is unshifted: caller deals with this
    Num n -> return (NoOp $ Val n, noShift)
    -- Shift the variable all the way left so that it is isolated and ready to
    -- be used. Also zero all the other bits so they don't get in the way
    Var v -> do
      print v
      let sv = 32 - (high v - low v + 1)
      return (BinOp (NoOp Encoding) ShiftBits (NoOp $ Val sv), shiftOf sv)
    LogicalNot gc -> do
      (bt, amt) <- genBitTest gc
      return (UnaryOp NotBits bt, amt)
    And gc1 gc2 -> simpleBitTest AndBits gc1 gc2
    Or gc1 gc2  -> simpleBitTest OrBits gc1 gc2
    Neq gc1 gc2 -> simpleBitTest XorBits gc1 gc2
    Eq gc1 gc2 -> do
      (bt, shift) <- simpleBitTest XorBits gc1 gc2
      return (UnaryOp NotBits bt, shift)
    _     -> error "Nope"
    where simpleBitTest op gc1 gc2 = do
            (bt1, shift1) <- genBitTest gc1
            (bt2, shift2) <- genBitTest gc2
            return (BinOp bt1 op bt2, shift1)


genComplexConstraint :: IORef Int -> GlobalConstraint -> IO [BitTest]
genComplexConstraint ref gc = do
  (bt, _) <- genBitTest gc
  return [bt]
