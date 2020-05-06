{-# LANGUAGE BinaryLiterals #-}
module Instrs where
import           AST

adc = threeArgInstr 0b10011010000

adcs = threeArgInstr 0b10111010000

addex = instr [ constant 31 21 0b10001011001
              ]
