{-# LANGUAGE BinaryLiterals #-}
module Codegen where
import           AST
import           Data.Bits
import           Data.Char  (intToDigit)
import           Data.Maybe (catMaybes)
import           Numeric    (showIntAtBase)

-- | An instruction encoding and its constraints
data InstrMatch = InstrMatch BitStr [InstrConstraint]
              deriving (Eq, Ord, Show)

type BitStr = String


-- | Constraints on an instruction
data InstrConstraint = BitConstraint String [BitTest]
                   deriving (Eq, Ord, Show)

-- pull out the left op right part

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
        | OrBits Int
        | XorBits Int
        deriving (Eq, Ord, Show)



genConstraint :: GlobalConstraint -> BitTest
genConstraint gc =
  case gc of
    Num{}    -> error "Did not expect num as top level constraint"
    Var{}    -> error "Did not expect var as top level constraint"
    Eq c1 c2 -> head $ genEq c1 c2
    _        -> error ""

genEq :: GlobalConstraint -> GlobalConstraint -> [BitTest]
genEq c1 c2 =
  case c2 of
    Num val    -> [Test Encoding AndBits $ Val $ val `shiftL` low (slice c1)]
    Var slice2 ->
      -- Make temporaries for the variables
      let (var1, name1) = genVar $ slice c1
          (var2, name2) = genVar slice2
      -- Actually do the equality test
      in var1 ++ var2 ++ [Test name1 AndBits name2]
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














-- -- | Make sure bit strings are well formed
-- mkStr :: String -> BitStr
-- mkStr str = str

-- data Output = Output { bitstring :: BitStr
--                      , operator  :: Op
--                      }
--             deriving (Eq, Ord, Show)


-- genConstantMatchInstr :: Instruction -> Output
-- genConstantMatchInstr inst =
--   let bitstring = concat $ catMaybes $ map genConstantMatchBits $ allBits inst
--   in Output bitstring Matc

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


-- genConstantMatchBits :: Bits -> Maybe BitStr
-- genConstantMatchBits bits =
--   case bits of
--     Global {} -> Nothing
--     Bits bs c ->
--       let len  = high bs - low bs + 1
--       in case c of
--         Constant v ->
--           let str       = showIntAtBase 2 intToDigit v ""
--           in if length str > len
--              then error $ unwords [ "Overly long string:"
--                                   , str
--                                   , ". Expected"
--                                   , show bs
--                                   ]
--              else Just $ replicate (len - length str) '0' ++ str
--         _          -> Just $ replicate len 'x'


-- ---
-- --- Generating more complicated constraints
-- ---

-- -- genVar :: Slice -> Bitstring
-- -- genVar slice = error ""

