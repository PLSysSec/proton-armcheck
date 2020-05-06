{-# LANGUAGE BinaryLiterals #-}
module Instrs where
import           AST
import           Prelude hiding (any)

adc = threeArgInstr 0b10011010000

adcs = threeArgInstr 0b10111010000

addex = twoArgEx 0b10001011001 (anyRegOrSp 9 5) (anyRegOrSp 4 0)

addim = instr [ constant 31 23 0b100100010
              , choice 22 22 [0,1]
              , range 21 10 0 4095
              , anyRegOrSp 9 5
              , anyRegOrSp 4 0
              ]

addsr = instr [ constant 31 24 0b10001011
              , choice 23 22 [ 00
                             , 01
                             , 10
                             ]
              , constant 21 21 0
              , anyReg 20 16
              , range 15 10 0 63
              , anyReg 9 5
              , anyReg 4 0
              ]

addsex = twoArgEx 0b10101011001 (anyRegOrSp 9 5) (anyReg 4 0)

