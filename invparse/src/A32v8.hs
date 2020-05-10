{-# LANGUAGE BinaryLiterals #-}
module A32v8 (a32v8instrs) where
import           AST
import           Prelude hiding (any, not)

a32v8instrs :: [(Instruction, String)]
a32v8instrs = map (\i -> (instr $ fst i, snd i)) [ (ldrimm, "ldrimm")
                                                 -- , (ldrlit, "ldrlit")
                                                 -- , (ldrreg, "ldrreg")
                                                 -- , (ldrbimm, "ldrbimm")
                                                 -- , (ldrblit, "ldrblit")
                                                 -- , (ldrbreg, "ldrbreg")
                                                 -- , (ldrdimm, "ldrdimm")
                                                 -- , (ldrdlit, "ldrdlit")
                                                 -- , (ldrdreg, "ldrdreg")
                                                 -- , (ldrhimm, "ldrhimm")
                                                 -- , (ldrhlit, "ldrhlit")
                                                 -- , (ldrhreg, "ldrhreg")
                                                 -- , (ldrsbimm, "ldrsbimm")
                                                 -- , (ldrsblit, "ldrsblit")
                                                 -- , (ldrsbreg, "ldrsbreg")
                                                 -- , (ldrshimm, "ldrshimm")
                                                 -- , (ldrshlit, "ldrshlit")
                                                 -- , (ldrshreg, "ldrshreg")
                                                 -- -- stores
                                                 -- , (strimm, "strimm")
                                                 -- , (strreg, "strreg")
                                                 -- , (strhimm, "strhimm")
                                                 -- , (strhreg, "strhreg")
                                                 -- , (strbimm, "strbimm")
                                                 -- , (strbreg, "strbreg")
                                                 -- , (strdimm, "strdimm")
                                                 -- , (strdreg, "strdreg")
                                                 -- -- atomic loads
                                                 -- , (ldrex, "ldrex")
                                                 -- , (ldrexb, "ldrexb")
                                                 -- , (ldrexd, "ldrexd")
                                                 -- , (ldrexh, "ldrexh")
                                                 -- -- atomic stores
                                                 -- , (strex, "strex")
                                                 -- , (strexb, "strexb")
                                                 -- , (strexd, "strexd")
                                                 -- , (strexh, "strexh")
                                                 ]


-- F5 in the manual

-- Common instructions:

-- Floating point and SIMD

-- Memory:

-- Loads: ldr, ldrb, ldrd, ldrh, ldrsh, ldrsb

p    = v 24 24
w    = v 21 21
zero = c 0
one  = c 1
fifteen = c 15
n    = v 19 16
t    = v 15 12
m    = v 3 0

ldrimm = [ not 31 28 0b1111
         , constant 27 25 0b010
         , any 24 23
         , zeroed 22 22
         , any 21 21
         , constant 20 20 1
         , reg 19 16
         , reg 15 12
         , any 11 0 -- imm12.
         -- restrictions
         -- , neqc' 19 16 0b1111
         -- , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
         ]

ldrlit = [ not 31 28 0b1111
         , constant 27 25 0b010
         , any 24 23
         , zeroed 22 22
         , any 21 21
         , constant 20 16 0b11111
         , reg 15 12
         , any 11 0 -- imm12. what do we do here
         , not' $ (p `eq'` zero) `or'` (w `eq'` one)
         ]

ldrreg = [ not 31 28 0b1111
         , constant 27 25 0b011
         , any 24 23
         , zeroed 22 22
         , any 21 21
         , constant 20 20 1
         , reg 19 16
         , reg 15 12
         , any 11 7 -- imm5
         , any 6 5 -- stype
         , constant 4 4 0
         , reg 3 0
         , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` c 15)
         , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
         ]

ldrbimm = [ not 31 28 0b1111
          , constant 27 25 0b010
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 1
          , reg 19 16
--          , not 19 16 0b1111
          , reg 15 12
--          , not 15 12 15
          , any 11 0 -- imm12. what do we do here
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          ]

ldrblit = [ not 31 28 0b1111
          , constant 27 25 0b010
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 1
          , constant 19 16 0b1111
          , reg 15 12
--          , not 15 12 15
          , any 11 0 -- imm12. what do we do here
          , not' $ (p `eq'` zero) `or'` (w `eq'` one)
          ]

ldrbreg = [ not 31 28 0b1111
          , constant 27 25 0b011
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 1
          , reg 19 16
          , reg 15 12
          , any 11 7 -- imm5
          , any 6 5 -- stype
          , constant 4 4 0
          , reg 3 0
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` c 15)
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          ]

ldrdimm = [ not 31 28 0b1111
          , constant 27 25 0b000
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 0
          , reg 19 16
--          , not 19 16 0b1111
          , reg 15 12
--          , not 15 12 0
          , any 11 8 -- imm4H
          , constant 7 4 0b1101
          , any 3 0 -- imm4L
            -- restrictions
--          , not 15 12 0
          , not' $ (p `eq'` zero) `and'` (w `eq'` one)
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` ((n `eq'` t) `or'` (n `eq'` (t `add'` one)))
          , not' $ (t `add'` one) `eq'` fifteen
          ]

ldrdlit = [ not 31 28 0b1111
          , constant 27 25 0b000
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 0
          , constant 19 16 0b1111
          , reg 15 12
--          , not 15 12 14
          , any 11 8 -- imm4H
          , constant 7 4 0b1101
          , any 3 0 -- imm4L
            -- restrictions
--          , not 12 12 1 -- not Rt<0> == 1
          ]

ldrdreg = [ not 31 28 0b1111
          , constant 27 25 0b000
          , any 24 23
          , constant 22 22 0
          , any 21 21
          , constant 20 20 0
          , reg 19 16
          , reg 15 12
          , any 11 8 -- unconstrained
          , constant 7 4 0b1101
          , reg 3 0
          -- restrictions
--          , not 12 12 1 -- not Rt<0> == 1
          , not' $ (p `eq'` zero) `and'` (w `eq'` one)
          , not' $ m `eq'` fifteen
          , not' $ t `eq'` (c 14)
          , not' $ m `eq'` t
          , not' $ m `eq'` (t `add'` one)
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` c 15)
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` (t `add'` one))
          ]

ldrhimm = [ not 31 28 0b1111
          , constant 27 25 0b000
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 1
          , reg 19 16
--          , not 19 16 0b1111
          , reg 15 12
--          , not 15 12 0
          , any 11 8 -- imm4H
          , constant 7 4 0b1011
          , any 3 0 -- imm4L
            -- restrictions
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          , not' $ (t `add'` one) `eq'` fifteen
          ]

ldrhlit = [ not 31 28 0b1111
          , constant 27 25 0b000
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 1
          , constant 19 16 0b1111
          , reg 15 12
          , any 11 8 -- imm4H
          , constant 7 4 0b1011
          , any 3 0 -- imm4L
            -- restrictions
          , not' $ (p `eq'` zero) `or'` (w `eq'` one)
          , not' $ t `eq'` fifteen
          ]

ldrhreg = [ not 31 28 0b1111
          , constant 27 25 0b000
          , any 24 23
          , constant 22 22 0
          , any 21 21
          , constant 20 20 1
          , reg 19 16
          , reg 15 12
          , any 11 8 -- unconstrained
          , constant 7 4 0b1011
          , reg 3 0
          -- restrictions
--          , not 12 12 1 -- not Rt<0> == 1
          , not' $ (t `eq'` fifteen) `or'` (m `eq'` fifteen)
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          ]

ldrsbimm = [ not 31 28 0b1111
           , constant 27 25 0b000
           , any 24 23
           , constant 22 22 1
           , any 21 21
           , constant 20 20 1
           , reg 19 16
--           , not 19 16 0b1111
           , reg 15 12
           , any 11 8 -- imm4H
           , constant 7 4 0b1101
           , any 3 0 -- imm4L
           -- restrictions
           , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
           , not' $ (t `add'` one) `eq'` fifteen
           ]

ldrsblit = [ not 31 28 0b1111
           , constant 27 25 0b000
           , any 24 23
           , constant 22 22 1
           , any 21 21
           , constant 20 20 1
           , constant 19 16 0b1111
           , reg 15 12
           , any 11 8 -- imm4H
           , constant 7 4 0b1101
           , any 3 0 -- imm4L
                     -- restrictions
           , not' $ (p `eq'` zero) `or'` (w `eq'` one)
           , not' $ t `eq'` fifteen
           ]

ldrsbreg = [ not 31 28 0b1111
           , constant 27 25 0b000
           , any 24 23
           , constant 22 22 0
           , any 21 21
           , constant 20 20 1
           , reg 19 16
           , reg 15 12
           , any 11 8 -- unconstrained
           , constant 7 4 0b1101
           , reg 3 0
           -- restrictions
           , not' $ (t `eq'` fifteen) `or'` (m `eq'` fifteen)
           , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
           , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
           ]

ldrshimm = [ not 31 28 0b1111
           , constant 27 25 0b000
           , any 24 23
           , constant 22 22 1
           , any 21 21
           , constant 20 20 1
           , reg 19 16
--           , not 19 16 0b1111
           , reg 15 12
           , any 11 8 -- imm4H
           , constant 7 4 0b1111
           , any 3 0 -- imm4L
           -- restrictions
           , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
           , not' $ t `eq'` fifteen
           ]

ldrshlit = [ not 31 28 0b1111
           , constant 27 25 0b000
           , any 24 23
           , constant 22 22 1
           , any 21 21
           , constant 20 20 1
           , constant 19 16 0b1111
           , reg 15 12
           , any 11 8 -- imm4H
           , constant 7 4 0b1111
           , any 3 0 -- imm4L
                     -- restrictions
           , not' $ (p `eq'` zero) `or'` (w `eq'` one)
           , not' $ t `eq'` fifteen
           ]

ldrshreg = [ not 31 28 0b1111
           , constant 27 25 0b000
           , any 24 23
           , constant 22 22 0
           , any 21 21
           , constant 20 20 1
           , reg 19 16
           , reg 15 12
           , any 11 8 -- unconstrained
           , constant 7 4 0b1111
           , reg 3 0
           -- restrictions
           , not' $ (t `eq'` fifteen) `or'` (m `eq'` fifteen)
           , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
           , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
           ]

-- Stores
-- Stores: str, strh, strb, strd

strimm = [ not 31 28 0b1111
         , constant 27 25 0b010
         , any 24 23
         , zeroed 22 22
         , any 21 21
         , constant 20 20 0
         , reg 19 16
         , reg 15 12
         , any 11 0 -- imm12. what do we do here
         -- restrictions
         , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
         , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
         ]

strreg = [ not 31 28 0b1111
         , constant 27 25 0b011
         , any 24 23
         , zeroed 22 22
         , any 21 21
         , constant 20 20 0
         , reg 19 16
         , reg 15 12
         , any 11 7 -- imm5
         , any 6 5 -- stype
         , constant 4 4 0
         , reg 3 0
         -- restrictions
         , not' $ m `eq'` fifteen
         , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
         , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
         ]

strbimm = [ not 31 28 0b1111
          , constant 27 25 0b010
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 0
          , reg 19 16
          , reg 15 12
          , any 11 0 -- imm12. what do we do here
          -- restrictions
          , not' $ t `eq'` fifteen
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
          ]

strbreg = [ not 31 28 0b1111
          , constant 27 25 0b011
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 0
          , reg 19 16
          , reg 15 12
          , any 11 7 -- imm5
          , any 6 5 -- stype
          , constant 4 4 0
          , reg 3 0
          -- restrictions
          , not' $ t `eq'` fifteen
          , not' $ m `eq'` fifteen
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` c 15)
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          ]

strdimm = [ not 31 28 0b1111
          , constant 27 25 0b000
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 0
          , reg 19 16
          , reg 15 12
--          , not 15 12 0
          , any 11 8 -- imm4H
          , constant 7 4 0b1111
          , any 3 0 -- imm4L
            -- restrictions
          , not' $ (p `eq'` zero) `and'` (w `eq'` one)
          , not' $ n `eq'` fifteen
          ]

strdreg = [ not 31 28 0b1111
          , constant 27 25 0b000
          , any 24 23
          , zeroed 22 22
          , any 21 21
          , constant 20 20 0
          , reg 19 16
          , reg 15 12
          , any 11 8 -- unconstrained
          , constant 7 4 0b1111
          , reg 3 0
          -- restrictions
          , not' $ (p `eq'` zero) `or'` (w `eq'` one)
          , not' $ m `eq'` fifteen
          , not' $ (t `add'` one) `eq'` fifteen
          ]

strhimm = [ not 31 28 0b1111
          , constant 27 25 0b000
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 0
          , reg 19 16
          , reg 15 12
          , any 11 8 -- imm4H
          , constant 7 4 0b1011
          , any 3 0 -- imm4L
            -- restrictions
          , not' $ t `eq'` fifteen
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          ]

strhreg = [ not 31 28 0b1111
          , constant 27 25 0b000
          , any 24 23
          , constant 22 22 0
          , any 21 21
          , constant 20 20 0
          , reg 19 16
          , reg 15 12
          , any 11 8 -- unconstrained
          , constant 7 4 0b1011
          , reg 3 0
          -- restrictions
          , not' $ (t `eq'` fifteen) `or'` (m `eq'` fifteen)
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
          , not' $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          ]

-- Atomic loads
atomicLoad c = [ not 31 28 0b1111
               , constant 27 20 c
               , not15 19 16
               , not15 15 12
               , any 11 10
               , constant 9 4 0b111001
               , any 3 0
               ]

ldrex = atomicLoad 0b00011001
ldrexb = atomicLoad 0b00011101
ldrexd = atomicLoad 0b00011011 -- and Rt<0> == 1, Rt in range 15-12
ldrexh = atomicLoad 0b00011111

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

strex  = atomicStore 0b00011000
strexb = atomicStore 0b00011100
strexd = atomicStore 0b00011010 -- and Rt<0> == 1, Rt in in range 3-0
strexh = atomicStore 0b00011110

-- Helpers:

reg hi lo = range hi lo 0 9

not15 hi lo = range hi lo 0 9
