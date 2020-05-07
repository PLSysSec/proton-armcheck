{-# LANGUAGE BinaryLiterals #-}
module Instrs where
import           AST
import           Prelude hiding (any)

adc = threeArgInstr 0b10011010000

adcs = threeArgInstr 0b10111010000

addex = twoArgEx 0b10001011001 (anyRegOrSp 9 5) (anyRegOrSp 4 0)

addim = twoArgImm 0b100100010 (anyRegOrSp 9 5) (anyRegOrSp 4 0)

addsr = threeArgSr 0b10001011 (anyReg 20 16) (anyReg 9 5) (anyReg 4 0)

addsex = twoArgEx 0b10101011001 (anyRegOrSp 9 5) (anyReg 4 0)

addsim = twoArgImm 0b101100010 (anyRegOrSp 9 5) (anyReg 4 0)

addssr = threeArgSr 0b10101011 (anyReg 20 16) (anyReg 9 5) (anyReg 4 0)

-- adr, adrp

-- andim = instr [ constant 31 23 0b100100100
--               ,
--               ]

