{-# LANGUAGE BinaryLiterals #-}
module AST ( Bits
           , instr
           -- * Instruction building helpers
           , threeArgInstr -- Three argument instruction
           , threeArgSr    -- Three argument shifted register instruction
           , twoArgEx      -- Two argument extended instruction
           , twoArgImm     -- Two argument immediate instruction
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
           , neq
           , Constraint(..)
           )
    where
import           Prelude hiding (any, not)

-- | An ARM instruction encoding is just some constraints over bits
data Instruction = Instruction [Bits]
                 deriving (Eq, Ord, Show)

-- | Make a new instruction. This will have some safeguards
instr :: [Bits] -> Instruction
instr = Instruction

---
--- Some helpers for standard kinds of instructions
---

threeArgInstr :: Int -> Instruction
threeArgInstr op = instr [ constant 31 21 op
                         , anyReg 20 16
                         , zeroed 15 10
                         , anyReg 9 5
                         , anyReg 4 0
                         ]

twoArgImm :: Int
          -> Bits -- ^ Register 1
          -> Bits -- ^ Register 2
          -> Instruction
twoArgImm op r1 r2 = instr [ constant 31 23 op
                           , any 22 22
                           , range 21 10 0 4095
                           , r1
                           , r2
                           ]

twoArgEx :: Int
         -> Bits -- ^ Register 1
         -> Bits -- ^ Register 2
         -> Instruction
twoArgEx op r1 r2 = instr [ constant 31 21 op
                          , undef 20 16
                          , any 15 13
                          , range 12 10 0 4
                          , r1
                          , r2
                          ]

threeArgSr :: Int
           -> Bits -- ^ Register 1
           -> Bits -- ^ Register 2
           -> Bits -- ^ Register 3
           -> Instruction
threeArgSr op r1 r2 r3 = instr [ constant 31 24 op
                               , choice 23 22 [ 00
                                              , 01
                                              , 10
                                              ]
                               , constant 21 21 0
                               , r1
                               , range 15 10 0 63
                               , r2
                               , r3
                               ]

---
--- Constraints on individual slices of bits
---

-- | This is how you constrain individual slices of bits
-- You build instruction encodings by constraining individual
-- slices of bits, or sets of slices of bits
data Bits = Bits { bits       :: Slice          -- constrain contiguous bits
                 , constraint :: Constraint
                 }
          | Slices { slices     :: [Slice]      -- constrain non-contiguous bits
                   , constraint :: Constraint
                   }
          | Global { constraint :: Constraint } -- constrain global properties
       deriving (Eq, Ord, Show)

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

neq :: Int -> Int -> Int -> Int -> Bits
neq high1 low1 high2 low2 = Global $ Neq [Slice high1 low1, Slice high2 low2]

data Constraint = Constant Int
                | ConstantChoice [Int]
                | Not Int
                | Range { start :: Int
                        , end   :: Int
                        }
                | Neq [Slice]
                | Eq [Slice]
                | Undefined
                | Any
                deriving (Eq, Ord, Show)







