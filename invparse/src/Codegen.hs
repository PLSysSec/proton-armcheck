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
  return $ InstrMatch name bitstring []

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
                   deriving (Eq, Ord, Show)


-- | A test can either be a top-level comparison against 1,
-- or an assingment to a temporary variable
data BitTest = Test { test :: TestContents }
             | Assign { lhs :: Var
                      , rhs :: TestContents
                      }
             deriving (Eq, Ord, Show)

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
                  deriving (Eq, Ord, Show)

mkBinOp :: Var -> Op -> Var -> TestContents
mkBinOp = BinOp

mkUnOp :: Op -> Var -> TestContents
mkUnOp = UnOp

-- | Vars are either temporaries, constants, or the encoding itself
data Var = Temp String
         | Val Int
         | Encoding
         deriving (Eq, Ord, Show)

-- | The operation: And, or, etc.
data Op = AndBits
        | OrBits
        | XorBits
        | ShiftBits
        | PlusBits
        | NotBits
        deriving (Eq, Ord, Show)

---
--- Generating bittests
---

genComplexConstraint :: GlobalConstraint -> [BitTest]
genComplexConstraint gc =
  case gc of
    Num{}     -> error "Num is not a complex constraint"
    Var{}     -> error "Var is not a complex constraint"
    Eq c1 c2  -> genOp AndBits c1 c2
    Neq c1 c2 -> genOp OrBits c1 c2
    And c1 c2 -> genOp AndBits c1 c2
--    Not c     -> error ""

genUnary :: Op -> GlobalConstraint -> [BitTest]
genUnary op c = error ""
  -- | isNum c = error "You shouldn't be not-ing a constant idiot"
  -- | isVar c = error "You shouldn't really be notting a var either? I mean I guess but"
  -- | otherwise =
  --     let test = genComplexConstraint c
  --         (Test )


genOp :: Op -> GlobalConstraint -> GlobalConstraint -> [BitTest]
genOp op c1 c2
    | isNum c1 && isNum c2 = error "You shouldn't be making all-const constraints idiot"
    | isVar c1 && isNum c2 = genOpWithConst op c1 c2
    | isNum c1 && isVar c2 = genOpWithConst op c2 c1
    | isVar c1 && isVar c2 = genOpWithVars op c1 c2
    | otherwise = error ""
        -- let test1              = genComplexConstraint c1
        --     (Test v1 op v2)    = head test1
        --     test2              = genComplexConstraint c2
        --     (Test v1' op' v2') = head test2
        --     temp1              = Assign (Temp "t1") v1 op v2
        --     temp2              = Assign (Temp "t2") v1' op' v2'
        -- in (Test (Temp "t1") op (Temp "t2")):([temp1] ++ [temp2] ++ tail test1 ++ tail test2)

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
        in v1 ++ v2 ++ [mkTest $ mkBinOp name1 op name2]
    | otherwise = error "Malformed inputs to genOpWithVars"

-- | Use the slice numbers to zero out only the bits in the slice.
-- | Return an and of the sliced out shit, shifted all the way left.
genVar :: Slice -> ([BitTest], Var)
genVar slice =
  let andMaskVar   = Temp "andVar"
      andMask      = mkAssign andMaskVar $ mkBinOp Encoding AndBits undefined
      shiftMaskVar = Temp "ShiftVar"
      shiftMask    = mkAssign shiftMaskVar $ mkBinOp andMaskVar ShiftBits undefined
  in ([andMask, shiftMask], shiftMaskVar)


