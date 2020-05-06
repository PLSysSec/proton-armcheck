{-# LANGUAGE BinaryLiterals #-}
module AST ( Bits
           , instr
           -- * Instruction building helpers
           , threeArgInstr
           -- * Helpers that constrain bits within an instruction
           , constant
           , zeroed
           , undef
           , anyReg
           , anyRegOrSp
           , Constraint(..)
           )
    where


data Instruction = Instruction [Bits]
                 deriving (Eq, Ord, Show)

instr :: [Bits] -> Instruction
instr = Instruction

threeArgInstr :: Int -> Instruction
threeArgInstr op = instr [ constant 31 21 op
                         , anyReg 20 16
                         , zeroed 15 10
                         , anyReg 9 5
                         , anyReg 4 0
                         ]

mkInstruction = error



data Bits = Bits { high       :: Int
                 , low        :: Int
                 , constraint :: Constraint
                 }
       deriving (Eq, Ord, Show)

mkBits :: Int -> Int -> Constraint -> Bits
mkBits h l c
    | l >= h = error $ unwords ["Low greater than or equal to high:", show l, show h]
    | h > 31 || h < 0 = error $ unwords ["High out of range:", show h]
    | l > 31 || l < 0 = error $ unwords ["End out of range:", show l]
    | otherwise = Bits h l c

anyRegOrSp :: Int -> Int -> Bits
anyRegOrSp high low
    | high - low == 4 = Bits high low $ Range 0 31
    | otherwise = error "Expected four bit field for register or stack pointer"

anyReg :: Int -> Int -> Bits
anyReg high low
    | high - low == 4 = Bits high low $ Range 0 30
    | otherwise = error "Expected four bit field for register"

constant :: Int -> Int -> Int -> Bits
constant high low constant = mkBits high low $ Constant constant

zeroed :: Int -> Int -> Bits
zeroed high low = mkBits high low $ Constant 0

undef :: Int -> Int -> Bits
undef high low = mkBits high low Undefined

data Constraint = Constant Int
                | Range { start :: Int
                        , end   :: Int
                        }
                | Undefined
                | Other
                deriving (Eq, Ord, Show)



-- RegisterType = R64 | SP64 | Z64
--                | SF8 | SF16 | SF32 | SF64 | SF128
--                | S8B | S16B | S4H | S8H | S2S
--                | S4S | S1D | S2D
--                  deriving (Eq, Ord, Show)


-- Range = Range { start :: Int
--               , end :: Int
--               }




-- data Instr = Instr String

-- data Register = Register String RegisterType

-- data RegisterType = R8
--                   | R16
--                   | R32
--                   | R64
--                   | R128
--                   | SIMDFP

-- data Value = Value { start :: Maybe Int
--                    , end   :: Maybe Int
--                    }

-- -- | Some more complicated restriction on the input
-- data Restriction = Distinct [Register]




