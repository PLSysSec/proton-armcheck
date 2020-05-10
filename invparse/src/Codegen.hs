{-# LANGUAGE BinaryLiterals #-}
module Codegen where
import           AST
import           Data.Bits  hiding (Bits)
import           Data.Char  (intToDigit)
import           Data.Maybe (catMaybes)
import           Numeric    (showIntAtBase)

---
--- High-level matching on instruction encoding
---

-- | An instruction encoding and its constraints
data InstrMatch = InstrMatch String BitStr [InstrConstraint]
              deriving (Eq, Ord, Show)

type BitStr = String

genConstantMatchInstr :: (Instruction, String) -> InstrMatch
genConstantMatchInstr (inst, name) =
  let bitstring = concat $ catMaybes $ map genInstrMatch $ allBits inst
  in InstrMatch name bitstring []

genInstrMatch :: Bits -> Maybe BitStr
genInstrMatch bits =
  case bits of
    Global {} -> Nothing
    Bits bs c ->
      let len  = high bs - low bs + 1
      in case c of
        Constant v ->
          let str       = showIntAtBase 2 intToDigit v ""
          in if length str > len
             then error $ unwords [ "Overly long string:"
                                  , str
                                  , ". Expected"
                                  , show bs
                                  ]
             else Just $ replicate (len - length str) '0' ++ str
        _          -> Just $ replicate len 'x'



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
             deriving (Eq, Ord, Show)

data Var = Temp String
         | Val Int
         | Encoding
         deriving (Eq, Ord, Show)

data Op = Matc
        | AndBits
        | ShiftBits
        | OrBits
        | XorBits
        deriving (Eq, Ord, Show)

genConstraint :: GlobalConstraint -> BitTest
genConstraint gc =
  case gc of
    Num{}     -> error "Did not expect num as top level constraint"
    Var{}     -> error "Did not expect var as top level constraint"
    Eq c1 c2  -> head $ genEq True c1 c2
    Neq c1 c2 -> head $ genEq False c1 c2
    And c1 c2 -> head $ genAnd c1 c2
    _         -> error ""


genAnd :: GlobalConstraint -> GlobalConstraint -> [BitTest]
genAnd c1 c2 = error ""

genEq :: Bool -> GlobalConstraint -> GlobalConstraint -> [BitTest]
genEq eq c1 c2 =
  case c2 of
    Num val    -> [Test Encoding AndBits $ Val $ val `shiftL` low (slice c1)]
    Var slice2 ->
      -- Make temporaries for the variables
      let (var1, name1) = genVar $ slice c1
          (var2, name2) = genVar slice2
          op            = if eq then AndBits else XorBits
      -- Actually do the equality test
      in var1 ++ var2 ++ [Test name1 op name2]
    _          -> error ""

-- | Use the slice numbers to zero out only the bits in the slice.
-- | Return an and of the sliced out shit, shifted all the way left.
genVar :: Slice -> ([BitTest], Var)
genVar slice =
  let andMaskVar   = Temp "andVar"
      andMask      = Assign andMaskVar Encoding AndBits undefined
      shiftMaskVar = Temp "ShiftVar"
      shiftMask    = Assign shiftMaskVar andMaskVar ShiftBits undefined
  in ([andMask, shiftMask], shiftMaskVar)

-- may not need these guys

make32BitBinaryString :: Slice -> Int -> String
make32BitBinaryString s val =
  let leftZeros  = replicate (32 - high s) '0'
      middleVal  = makeBinaryString s val
      rightZeros = replicate (low s) '0'
  in leftZeros ++ middleVal ++ rightZeros

makeBinaryString :: Slice -> Int -> String
makeBinaryString s val =
  let len = high s - low s + 1
      str = showIntAtBase 2 intToDigit val ""
  in if length str > len
     then error $ unwords [ "Overly long string:"
                          , str
                          , ". Expected"
                          , show s
                          ]
     else replicate (len - length str) '0' ++ str

