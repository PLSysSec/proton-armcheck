{-# LANGUAGE BinaryLiterals #-}
module AST ( Bits(..)
           , complexConstraints
           , dname
           , Slice(..)
           , Instruction(..)
           , instr
           -- * Helpers that constrain bits within an instruction
           , anyReg        -- Bits can represent any register
           , anyRegOrSp    -- Bits can represent any register or the stack pointer
           , constant
           , zeroed
           , undef
           , any
           , not
           , choice
           , range
           -- * Helpers for building for complex constraints on instructions,
           -- * Over all bits in the instruction
           , c -- constant
           , v -- variable
           , neqc'
           , eqc'
           , and'
           , or'
           , eq'
           , neq'
           , not'
           , add'
           , Constraint(..)
           , GlobalConstraint(..)
           , isVar
           , isNum
           )
    where
import           Prelude hiding (any, not)

-- | An ARM instruction encoding is just some constraints over bits
data Instruction = Instruction { allBits :: [Bits] }
                 deriving (Eq, Ord, Show)

complexConstraints :: Instruction -> [(GlobalConstraint, String)]
complexConstraints inst =
  map (\i -> (globConstraint i, debugName i)) $ filter isComplex $ allBits inst

-- | Make a new instruction
instr :: [Bits] -> Instruction
instr bits = Instruction $ checkBits 32 bits []
  where
    checkBits :: Int -> [Bits] -> [Bits] -> [Bits]
    checkBits _ [] instrs                  = instrs
    checkBits lastSlice (bits:rest) instrs = case bits of
      Global {} -> checkBits lastSlice rest (bits:instrs)
      Bits bs _ | lastSlice <= high bs -> error "Disallowed instr construction"
      Bits bs _ -> checkBits (low bs) rest (bits:instrs)

---
--- Constraints on individual slices of bits
---

-- | This is how you constrain individual slices of bits
-- You build instruction encodings by constraining individual
-- slices of bits, or sets of slices of bits
data Bits = Bits { bits       :: Slice          -- constrain contiguous bits
                 , constraint :: Constraint
                 }
          | Global { debugName      :: String
                   , globConstraint :: GlobalConstraint
                   } -- constrain global properties
            deriving (Eq, Ord, Show)

isComplex :: Bits -> Bool
isComplex Global{} = True
isComplex _        = False

dname :: String -> Bits -> Bits
dname n (Global _ gc) = Global n gc
dname _ b             = b

-- | A slice of bits from high to low, INCLUSIVE
data Slice = Slice { high :: Int
                   , low  :: Int
                   }
           deriving (Eq, Ord, Show)

mkBits :: Int -> Int -> Constraint -> Bits
mkBits h l c
    | l > h = error $ unwords ["Low greater than high:", show l, show h]
    | h > 31 || h < 0 = error $ unwords ["High out of range:", show h]
    | l > 31 || l < 0 = error $ unwords ["End out of range:", show l]
    | otherwise = Bits (Slice h l) c

-- | Any register or the stack pointer
anyRegOrSp :: Int -> Int -> Bits
anyRegOrSp high low
    | high - low == 4 = range high low 0 31
    | otherwise = error "Expected four bit field for register or stack pointer"

-- | Any register, NOT including the stack pointer
anyReg :: Int -> Int -> Bits
anyReg high low
    | high - low == 4 = range high low 0 30
    | otherwise = error "Expected four bit field for register"

constant :: Int -> Int -> Int -> Bits
constant high low constant = mkBits high low $ Constant constant

zeroed :: Int -> Int -> Bits
zeroed high low = mkBits high low $ Constant 0

any :: Int -> Int -> Bits
any high low = mkBits high low Any

not :: Int -> Int -> Int -> Bits
not high low val = mkBits high low $ Not val

-- | No constraints on these bits
undef :: Int -> Int -> Bits
undef high low = mkBits high low Undefined

-- | Choice between possible constants
choice :: Int -> Int -> [Int] -> Bits
choice high low cs = mkBits high low $ ConstantChoice cs

-- | Between the values start and end, inclusive
range :: Int -> Int -> Int -> Int -> Bits
range high low start end = mkBits high low $ Range start end

-- | TODO: what is the bitmask encoding exactly
immedite :: Int -> Int -> Bits
immedite high low = any high low

data Constraint = Constant Int
                | ConstantChoice [Int]
                | Not Int
                | Range { start :: Int
                        , end   :: Int
                        }
                | Undefined
                | Any
                deriving (Eq, Ord, Show)

c :: Int -> Bits
c v = Global "debug" $ Num v

v :: Int -> Int -> Bits
v h l = Global "debug" $ Var $ Slice h l

-- | Not equal a constant
neqc' :: Int -> Int -> Int -> Bits
neqc' hi lo val = (v hi lo) `neq'` (c val)

-- | Equal a constant
eqc' :: Int -> Int -> Int -> Bits
eqc' hi lo val = (v hi lo) `eq'` (c val)

and' :: Bits -> Bits -> Bits
and' (Global n c1) (Global _ c2) = Global n $ And c1 c2
and' _ _                     = error "Expected global constraint argument to and"

or' :: Bits -> Bits -> Bits
or' (Global n c1) (Global _ c2) = Global n $ Or c1 c2
or' _ _                     = error "Expected global constraint argument to or"

-- | enforce well-formed-ness
neq' :: Bits -> Bits -> Bits
neq' (Global n c1) (Global _ c2) = Global n $ Neq c1 c2
neq' _ _                     = error "Expected global constraint argument to eq"

eq' :: Bits -> Bits -> Bits
eq' (Global n c1) (Global _ c2) = Global n $ Eq c1 c2
eq' _ _                         = error "Expected global constraint argument to eq"

-- | Logical not (not bitwise not)
not' :: Bits -> Bits
not' (Global n c) = Global n $ LogicalNot c

add' :: Bits -> Bits -> Bits
add' (Global n c1) (Global _ c2) = Global n $ Add c1 c2
add' _ _ = error "Expected global constraint argument to add"

data GlobalConstraint = Num { val :: Int }
                      | Var { slice :: Slice }
                      | Eq GlobalConstraint GlobalConstraint
                      | Neq GlobalConstraint GlobalConstraint
                      | And GlobalConstraint GlobalConstraint
                      | Or GlobalConstraint GlobalConstraint
                      | Add GlobalConstraint GlobalConstraint
                      | LogicalNot GlobalConstraint
                      deriving (Eq, Ord, Show)

isVar :: GlobalConstraint -> Bool
isVar Var{} = True
isVar _     = False

isNum :: GlobalConstraint -> Bool
isNum Num{} = True
isNum _     = False




