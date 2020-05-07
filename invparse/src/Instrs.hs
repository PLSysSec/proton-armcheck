{-# LANGUAGE BinaryLiterals #-}
module Instrs where
import           AST
import           Prelude hiding (any, not)

-- Basic instructions

adc = threeArgInstr 0b10011010000

adcs = threeArgInstr 0b10111010000

addex = twoArgEx 0b10001011001 (anyRegOrSp 9 5) (anyRegOrSp 4 0)

addim = twoArgImm 0b100100010 (anyRegOrSp 9 5) (anyRegOrSp 4 0)

addsr = threeArgSr 0b10001011 (anyReg 20 16) (anyReg 9 5) (anyReg 4 0)

addsex = twoArgEx 0b10101011001 (anyRegOrSp 9 5) (anyReg 4 0)

addsim = twoArgImm 0b101100010 (anyRegOrSp 9 5) (anyReg 4 0)

addssr = threeArgSr 0b10101011 (anyReg 20 16) (anyReg 9 5) (anyReg 4 0)

andim = instr [ constant 31 23 0b100100100
              , any 21 10
              , anyReg 9 5
              , anyRegOrSp 4 0
              ]

andsr = instr [ constant 31 24 0b10001010
              , any 23 22
              , constant 21 21 0
              , anyReg 20 16
              , range 15 10 0 63
              , anyReg 9 5
              , anyReg 4 0
              ]


-- Loads: ldr, ldrh, ldrsh, ldrb, ldrsb ldrd

-- ldrreg = instr [ not 21 28 0b1111
--                , constant 27 25 0b011
--                , any -- p
--                , -- u
--                , constant 22 22 0
--                , -- w
--                , constant 20 20 1
--                , anyReg 19 16
--                , anyReg 15 12
--                , -- imm5
--                , -- stype
--                , constant 4 4 0
--                , anyReg 3 0
--                ]

-- Stores: str, strh, strb, strd



-- Atomics: ldrexd, ldrex, ldrexh, ldrexb, strexd, strex, strexh, strexb

ldrexd = instr [ not 31 28 0b1111
               , constant 27 20 0b00011011
               ]

