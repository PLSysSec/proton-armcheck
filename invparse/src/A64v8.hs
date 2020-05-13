{-# LANGUAGE BinaryLiterals #-}
module A64v8 where
import           AST

-- Section C6.2

-- Loads: ldr X , ldrb X , ldrsb, ldrh X , ldrsh X , ldrsw X

o1 = v 14 14
zero = c 0

load co = [ constant 31 21 co
          , constant 11 10 0b10
          , o1 `eq'` zero
          ]

ldrreg     = load 0b11111000011
ldrbreg    = load 0b00111000011
ldrhreg    = load 0b01111000011
ldrsbreg32 = load 0b00111000111
ldrsbreg64 = load 0b001111000101
ldrshreg32 = load 0b01111000101
ldrshreg64 = load 0b01101000101
ldrswreg   = load 0b10111000101

-- Stores: str, strb, strh

store co = [ constant 31 21 co
           , constant 11 10 0b10
           , o1 `eq'` zero
           ]

strreg32 = store 0b10111000001
strreg64 = store 0b11111000001
strbreg  = store 0b00111000001
strhreg  = store 0b11111000001
