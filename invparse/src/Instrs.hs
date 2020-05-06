{-# LANGUAGE BinaryLiterals #-}
module Instrs where
import           AST

adc = threeArgInstr 0b10011010000

adcs = threeArgInstr 0b10111010000

addex = instr [ constant 31 21 0b10001011001
              , undef 20 16
              , choice 15 13 [ 000
                             , 001
                             , 010
                             , 011
                             , 100
                             , 101
                             , 110
                             , 111
                             ]
              , range 12 10 0 4
              , anyRegOrSp 9 5
              , anyRegOrSp 4 0
              ]
