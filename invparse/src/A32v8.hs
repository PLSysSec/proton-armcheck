{-# LANGUAGE BinaryLiterals #-}
module A32v8 where
import           AST
import           Prelude hiding (any, not)

-- F5 in the manual

-- Common instructions:

-- Floating point and SIMD

-- Memory:

-- Loads: ldr, ldrh, ldrsh, ldrb, ldrsb, ldrd
-- Stores: str, strh, strb, strd

p    = v 24 24
w    = v 21 21
zero = c 0
one  = c 1
n    = v 19 16
t    = v 15 12

ldr = [ not 31 28 0b1111
      , constant 27 25 0b010
      , any 24 23
      , zeroed 22 22
      , any 21 21
      , constant 20 20 1
      , reg 19 16
      , not 19 16 0b1111
      , reg 15 12
      , any 11 0 -- imm12. what do we do here
      , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
      ]

-- Stores


-- Are we worried about: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.kui0100a/armasm_cihbghef.htm
-- Atomic loads
atomicLoad c = [ not 31 28 0b1111
               , constant 27 20 c
               , not15 19 16
               , not15 15 12
               , any 11 10
               , constant 9 4 0b111001
               , any 3 0
               ]

ldrex = instr $ atomicLoad 0b00011001
ldrexb = instr $ atomicLoad 0b00011101
ldrexd = instr $ atomicLoad 0b00011011 -- and Rt<0> == 1, Rt in range 15-12
ldrexh = instr $ atomicLoad 0b00011111

-- Atomic stores
atomicStore c = [ not 31 28 0b1111
                , constant 27 20 c
                , not15 19 16
                , not15 15 12
                , any 11 10
                , constant 9 4 0b111001
                , not15 3 0
                -- , neq 19 16 15 12
                -- , neq 19 16 3 0
                ]

strex = instr $ atomicStore 0b00011000
strexb = instr $ atomicStore 0b00011100
strexd = instr $ atomicStore 0b00011010 -- and Rt<0> == 1, Rt in in range 3-0
strexh = instr $ atomicStore 0b00011110

-- Helpers:

reg hi lo = range hi lo 0 9

not15 hi lo = range hi lo 0 9
