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


data BitTest = Test { left  :: Var
                    , op    :: Op
                    , right :: Var
                    }
             | Assign { lhs   :: Var
                      , left  :: Var
                      , op    :: Op
                      , right:: Var
                      }
             | Wrapped Var
             deriving (Eq, Ord, Show)

data Var = Temp String
         | Val Int
         | Encoding
         deriving (Eq, Ord, Show)

data Op = AndBits
        | OrBits
        | XorBits
        | ShiftBits
        | PlusBits
        deriving (Eq, Ord, Show)

genConstraint :: GlobalConstraint -> BitTest
genConstraint gc =
  case gc of
    Num n     -> Wrapped $ Val n
    Var slice -> error $ show $ genVar slice
    -- Eq c1 c2  -> head $ genEq True c1 c2
    -- Neq c1 c2 -> head $ genEq False c1 c2
    And c1 c2 -> head $ genBinOp AndBits c1 c2
    _         -> error ""

genOp :: Op -> GlobalConstraint -> GlobalConstraint -> [BitTest]
genOp op c1 c2
    | isNum c1 && isNum c2 = error "You shouldn't be making all-const constraints idiot"
    | isVar c1 && isNum c2 = genOpWithConst op c1 c2
    | isNum c1 && isVar c2 = genOpWithConst op c2 c1
    | otherwise = error "not done"

genOpWithConst :: Op
               -> GlobalConstraint -- ^ The variable
               -> GlobalConstraint -- ^ The number
               -> [BitTest]
genOpWithConst op var num
  | isVar var && isNum num =
      -- Shift the constant to be even with the value, then do the test
      [Test Encoding op $ Val $ (val num) `shiftL` low (slice var)]
  | otherwise = error "Malformed inputs to genOpWithConst"

genOpWithVars :: Op
              -> GlobalConstraint -- ^ Variable
              -> GlobalConstraint -- ^ Other variable
              -> [BitTest]
genOpWithVars op var1 var2
    | isVar var1 && isVar var2 =
        let (v1, name1) = genVar $ slice var1
            (v2, name2) = genVar $ slice var2
        in v1 ++ v2 ++ [Test name1 op name2]
    | otherwise = error "Malformed inputs to genOpWithVars"

genBinOp :: Op -> GlobalConstraint -> GlobalConstraint -> [BitTest]
genBinOp = undefined

genNot :: GlobalConstraint -> [BitTest]
genNot = undefined


--- Variables

-- | Use the slice numbers to zero out only the bits in the slice.
-- | Return an and of the sliced out shit, shifted all the way left.
genVar :: Slice -> ([BitTest], Var)
genVar slice =
  let andMaskVar   = Temp "andVar"
      andMask      = Assign andMaskVar Encoding AndBits undefined
      shiftMaskVar = Temp "ShiftVar"
      shiftMask    = Assign shiftMaskVar andMaskVar ShiftBits undefined
  in ([andMask, shiftMask], shiftMaskVar)


