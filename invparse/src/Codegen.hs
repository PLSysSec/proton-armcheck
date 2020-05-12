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
    unlines [name ++ ":", str] ++ unlines (map show constrs)

type BitStr = String

genConstantMatchInstr :: (Instruction, String) -> IO InstrMatch
genConstantMatchInstr (inst, name) = do
  bstr <- mapM genInstrMatch $ reverse $ allBits inst
  let bitstring = concat $ catMaybes bstr
  ref <- newIORef 0
  test <- mapM genBitTest $ complexConstraints inst
  return $ InstrMatch name bitstring $ map mkConstraint test

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
data InstrConstraint = BitConstraint { constraintName :: String
                                     , constraintTest :: BitTest
                                     }
                   deriving (Eq, Ord)


instance Show InstrConstraint where
    show (BitConstraint str test) = unlines ["Test for" ++ str ++ ":", show test]

mkConstraint :: BitTest -> InstrConstraint
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
    show (UnaryOp o n) = unwords ["(", show o, show n, ")"]
    show (NoOp v)      = show v

-- | Vars are either temporaries, constants, or the encoding itself
data Var = Val Int
         | Encoding
         deriving (Eq, Ord)

instance Show Var where
    show (Val i)  = show i
    show Encoding = "instr"

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
    show ShiftBits = ">>"
    show AddBits   = "+"
    show NotBits   = "~"
    show EqBits    = "=="
    show NeqBits   = "!="

---
--- Generating bittests
---

genBitTest :: GlobalConstraint -> IO BitTest
genBitTest gc =
  case gc of
    Num n -> return $ NoOp $ Val n
    Var v -> do
      -- Shift the variable all the way right so that it is isolated and ready to use
      -- Then mask it to eliminate any other irrelevant bits
      let shifted = BinOp (NoOp Encoding) ShiftBits (NoOp $ Val $ low v)
          masked  = 2 ^ (high v - low v)
      return $ BinOp shifted AndBits (NoOp $ Val masked)
    LogicalNot gc -> do
      bt <- genBitTest gc
      return $ UnaryOp NotBits bt
    And gc1 gc2 -> simpleBitTest AndBits gc1 gc2
    Or gc1 gc2  -> simpleBitTest OrBits gc1 gc2
    Add gc1 gc2 -> simpleBitTest AddBits gc1 gc2
    Neq gc1 gc2 -> simpleBitTest XorBits gc1 gc2 -- returns 1 if gc1 != gc2
    Eq gc1 gc2  -> do                            -- returns 1 if gc1 == gc2
      bt <- simpleBitTest XorBits gc1 gc2
      return $ UnaryOp NotBits bt
    where simpleBitTest op gc1 gc2 = do
            bt1 <- genBitTest gc1
            bt2 <- genBitTest gc2
            return $ BinOp bt1 op bt2

