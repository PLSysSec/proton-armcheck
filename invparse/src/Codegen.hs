{-# LANGUAGE BinaryLiterals #-}
module Codegen where
import           AST
import           Control.Monad (forM, when)
import           Control.Monad (unless)
import           Data.Bits     hiding (Bits)
import           Data.Char     (intToDigit)
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
    unlines $ (name ++ " " ++ str) : map show constrs

type BitStr = String

genConstantMatchInstr :: (Instruction, String) -> IO InstrMatch
genConstantMatchInstr (inst, name) = do
  bstr <- mapM genInstrMatch $ reverse $ allBits inst
  let bitstring = concat $ catMaybes bstr
  test <- forM (complexConstraints inst) $ \(gc, str) -> do
    genned <- genBitTest gc
    return (str, genned)
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
    show (BitConstraint str test) = str ++ " " ++ show test

showCompilable :: InstrConstraint -> String
showCompilable (BitConstraint _ eq) = show eq

mkConstraint :: (String, BitTest) -> InstrConstraint
mkConstraint (s, b) = BitConstraint s b

-- | A test can either be a top-level comparison against 1,
-- or an assingment to a temporary variable
data BitTest = BinOp { left  :: BitTest
                     , op    :: Op
                     , right :: BitTest
                     }
             | NotOp { node :: BitTest } -- Logical, not bitwise not
             | TernOp { cond    :: BitTest
                      , trueBr  :: BitTest
                      , falseBr :: BitTest
                      }
             | NoOp { var :: Var }
             deriving (Eq, Ord)

instance Show BitTest where
    show (BinOp l o r)  = unwords ["((", show l, ")", show o, "(", show r, "))"]
    show (NotOp n)      = unwords ["((", show n, "== 0) ? 1 : 0 )"]
    show (TernOp c t f) = unwords ["(",  show c, "?", show t, ":", show f, ")"]
    show (NoOp v)       = show v

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
        | EqBits
        | NeqBits
        deriving (Eq, Ord)

instance Show Op where
    show AndBits   = "&"
    show OrBits    = "|"
    show XorBits   = "^"
    show ShiftBits = ">>"
    show AddBits   = "+"
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
          masked  = (2 ^ (1 + high v - low v)) - 1
      return $ BinOp shifted AndBits (NoOp $ Val masked)
    LogicalNot gc -> do
      bt <- genBitTest gc
      return $ NotOp bt
    And gc1 gc2 -> simpleBitTest AndBits gc1 gc2
    Or gc1 gc2  -> simpleBitTest OrBits gc1 gc2
    Add gc1 gc2 -> simpleBitTest AddBits gc1 gc2
    Neq gc1 gc2 -> simpleBitTest NeqBits gc1 gc2
    Eq gc1 gc2  -> simpleBitTest EqBits gc1 gc2
    where simpleBitTest op gc1 gc2 = do
            bt1 <- genBitTest gc1
            bt2 <- genBitTest gc2
            return $ BinOp bt1 op bt2

