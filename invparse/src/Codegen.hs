{-# LANGUAGE BinaryLiterals #-}
module Codegen where
import           AST
import           Control.Monad (when)
import           Data.Bits     hiding (Bits)
import           Data.Char     (intToDigit)
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
              deriving (Eq, Ord, Show)

type BitStr = String

genConstantMatchInstr :: (Instruction, String) -> IO InstrMatch
genConstantMatchInstr (inst, name) = do
  bstr <- mapM genInstrMatch $ reverse $ allBits inst
  let bitstring = concat $ catMaybes bstr
      tests     = map genComplexConstraint $ complexConstraints inst
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
data BitTest = Test { test :: TestContents }
             | Assign { lhs :: Var
                      , rhs :: TestContents
                      }
             deriving (Eq, Ord)

instance Show BitTest where
    show (Test t)     = unwords ["TEST:", show t]
    show (Assign l r) = unwords [show l, "=", show r]

mkAssign :: Var -> TestContents -> BitTest
mkAssign = Assign

mkTest :: TestContents -> BitTest
mkTest = Test

-- | The contents of a test can either be a binary operation (e.g., and)
-- or a unary operation (e.g., not)
data TestContents = BinOp { left  :: Var
                          , op    :: Op
                          , right :: Var
                          }
                  | UnOp { op   :: Op
                         , oper :: Var
                         }
                  deriving (Eq, Ord)

instance Show TestContents where
    show (BinOp l op r) = unwords [show l, show op, show r]
    show (UnOp op oper) = show op ++ show oper

mkBinOp :: Var -> Op -> Var -> TestContents
mkBinOp = BinOp

mkUnOp :: Op -> Var -> TestContents
mkUnOp = UnOp

-- | Vars are either temporaries, constants, or the encoding itself
data Var = Temp String
         | Val Int
         | Encoding
         deriving (Eq, Ord)

instance Show Var where
    show (Temp s) = s
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
    show AndBits   = "+"
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

genComplexConstraint :: GlobalConstraint -> [BitTest]
genComplexConstraint gc =
  case gc of
    Num{}        -> error "Num is not a complex constraint"
    Var{}        -> error "Var is not a complex constraint"
    Eq c1 c2     -> genOp EqBits c1 c2
    Neq c1 c2    -> genOp NeqBits c1 c2
    And c1 c2    -> genOp AndBits c1 c2
    Or c1 c2     -> genOp OrBits c1 c2
    Add c1 c2    -> genOp AddBits c1 c2
    LogicalNot c -> genUnary NotBits c

genUnary :: Op -> GlobalConstraint -> [BitTest]
genUnary op c
  | isNum c = error "You shouldn't be not-ing a constant idiot"
  | isVar c = error "You shouldn't really be notting a var either? I mean I guess but"
  | otherwise =
      let test'    = genComplexConstraint c
          (Test t) = head test'
          tvar     = Temp "t"
          temp     = mkAssign tvar t
          newTest  = mkTest $ mkUnOp op tvar
      in [newTest, temp] ++ tail test'

genOp :: Op -> GlobalConstraint -> GlobalConstraint -> [BitTest]
genOp op c1 c2
    | isNum c1 && isNum c2 = error "You shouldn't be making all-const constraints idiot"
    | isVar c1 && isNum c2 = genOpWithConst op c1 c2
    | isNum c1 && isVar c2 = genOpWithConst op c2 c1
    | isVar c1 && isVar c2 = genOpWithVars op c1 c2
    | otherwise =
        let test1     = genComplexConstraint c1
            (Test t1) = head test1
            tvar1     = Temp "t1"
            temp1     = mkAssign tvar1 t1
            test2     = genComplexConstraint c2
            (Test t2) = head test2
            tvar2     = Temp "t2"
            temp2     = mkAssign tvar2 t2
            newTest   = mkTest $ mkBinOp tvar1 op tvar2
        in [newTest, temp1, temp2] ++ tail test1 ++ tail test2

genOpWithConst :: Op
               -> GlobalConstraint -- ^ The variable
               -> GlobalConstraint -- ^ The number
               -> [BitTest]
genOpWithConst op var num
  | isVar var && isNum num =
      -- Shift the constant to be even with the value, then do the test
      let shiftedVal = Val $ val num `shiftL` low (slice var)
      in [mkTest $ mkBinOp Encoding op shiftedVal]
  | otherwise = error "Malformed inputs to genOpWithConst"

genOpWithVars :: Op
              -> GlobalConstraint -- ^ Variable
              -> GlobalConstraint -- ^ Other variable
              -> [BitTest]
genOpWithVars op var1 var2
    | isVar var1 && isVar var2 =
        let (v1, name1) = genVar $ slice var1
            (v2, name2) = genVar $ slice var2
        in [mkTest $ mkBinOp name1 op name2] ++ v1 ++ v2
    | otherwise = error "Malformed inputs to genOpWithVars"

-- | Use the slice numbers to zero out only the bits in the slice.
-- | Return an and of the sliced out shit, shifted all the way left.
genVar :: Slice -> ([BitTest], Var)
genVar slice =
  let andMaskVar   = Temp "andVar"
      andMaskVal   = Val 0
      andMask      = mkAssign andMaskVar $ mkBinOp Encoding AndBits andMaskVal
      shiftMaskVar = Temp "ShiftVar"
      shiftMaskVal = Val 0
      shiftMask    = mkAssign shiftMaskVar $ mkBinOp andMaskVar ShiftBits shiftMaskVal
  in ([andMask, shiftMask], shiftMaskVar)


