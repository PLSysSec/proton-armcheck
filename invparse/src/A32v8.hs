{-# LANGUAGE BinaryLiterals #-}
module A32v8 (a32v8instrs) where
import           AST
import           Prelude hiding (any, not)

a32v8instrs :: [(Instruction, String)]
a32v8instrs = map (\i -> (instr $ fst i, snd i)) [ (dummy, "dummy")
                                                 , (ldrimm, "ldrimm")
                                                 , (ldrlit, "ldrlit")
                                                 , (ldrreg, "ldrreg")
                                                 , (ldrbimm, "ldrbimm")
                                                 , (ldrblit, "ldrblit")
                                                 , (ldrbreg, "ldrbreg")
                                                 , (ldrdimm, "ldrdimm")
                                                 , (ldrdlit, "ldrdlit")
                                                 , (ldrdreg, "ldrdreg")
                                                 , (ldrhimm, "ldrhimm")
                                                 , (ldrhlit, "ldrhlit")
                                                 , (ldrhreg, "ldrhreg")
                                                 , (ldrsbimm, "ldrsbimm")
                                                 , (ldrsblit, "ldrsblit")
                                                 , (ldrsbreg, "ldrsbreg")
                                                 , (ldrshimm, "ldrshimm")
                                                 , (ldrshlit, "ldrshlit")
                                                 , (ldrshreg, "ldrshreg")
                                                 -- stores
                                                 , (strimm, "strimm")
                                                 , (strreg, "strreg")
                                                 , (strhimm, "strhimm")
                                                 , (strhreg, "strhreg")
                                                 , (strbimm, "strbimm")
                                                 , (strbreg, "strbreg")
                                                 , (strdimm, "strdimm")
                                                 , (strdreg, "strdreg")
                                                 -- atomic loads
                                                 , (ldrex, "ldrex")
                                                 , (ldrexb, "ldrexb")
                                                 , (ldrexd, "ldrexd")
                                                 , (ldrexh, "ldrexh")
                                                 -- atomic stores
                                                 , (strex, "strex")
                                                 , (strexb, "strexb")
                                                 , (strexd, "strexd")
                                                 , (strexh, "strexh")
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
rt0 = v 12 12


dummy = [ constant 31 28 0b1111
        , any 27 0
        , (p `eq'` zero) `and'` (w `eq'` one)
        ]

ldrimm = [ not 31 28 0b1111
         , constant 27 25 0b010
         , any 24 23
         , zeroed 22 22
         , any 21 21
         , constant 20 20 1
         , reg 19 16
         , reg 15 12
         , any 11 0
         -- restrictions
         , ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
         ]

ldrlit = [ not 31 28 0b1111
         , constant 27 25 0b010
         , any 24 23
         , zeroed 22 22
         , any 21 21
         , constant 20 16 0b11111
         , reg 15 12
         , any 11 0
         -- restrictions
         , (p `eq'` zero) `or'` (w `eq'` one)
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
         -- restrictions
         , dname "m" $ m `eq'` fifteen
         , dname "fifteen" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
         , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
         ]

ldrbimm = [ not 31 28 0b1111
          , constant 27 25 0b010
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 1
          , reg 19 16
          , reg 15 12
          , any 11 0
          -- restrictions
          , dname "t" $ t `eq'` fifteen
          , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          ]

ldrblit = [ not 31 28 0b1111
          , constant 27 25 0b010
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 1
          , constant 19 16 0b1111
          , reg 15 12
          , any 11 0
          -- restrictions
          , dname "t" $ t `eq'` fifteen
          , dname "wback" $ (p `eq'` zero) `or'` (w `eq'` one)
          ]

ldrbreg = [ not 31 28 0b1111
          , constant 27 25 0b011
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 1
          , reg 19 16
          , reg 15 12
          , any 11 7
          , any 6 5
          , constant 4 4 0
          , reg 3 0
          -- restrictions
          , dname "t" $ t `eq'` fifteen
          , dname "m" $ m `eq'` fifteen
          , dname "fifteen" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` c 15)
          , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          ]

ldrdimm = [ not 31 28 0b1111
          , constant 27 25 0b000
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 0
          , reg 19 16
          , reg 15 12
          , any 11 8 -- imm4H
          , constant 7 4 0b1101
          , any 3 0 -- imm4L
            -- restrictions
          , dname "pw" $ (p `eq'` zero) `and'` (w `eq'` one)
          , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          , dname "nt1" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` (t `add'` one))
          , dname "fifteen" $ (t `add'` one) `eq'` fifteen
          , dname "r0" $ rt0 `eq'` one
          ]

ldrdlit = [ not 31 28 0b1111
          , constant 27 25 0b000
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 0
          , constant 19 16 0b1111
          , reg 15 12
          , any 11 8 -- imm4H
          , constant 7 4 0b1101
          , any 3 0 -- imm4L
            -- restrictions
          , dname "r0" $ rt0 `eq'` one
          , dname "t2" $ (t `add'` one) `eq'` fifteen
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
          , dname "r0" $ rt0 `eq'` one
          , dname "pw" $ (p `eq'` zero) `and'` (w `eq'` one)
          , dname "fifteen" $ m `eq'` fifteen
          , dname "fourteen" $ (t `add'` one) `eq'` fifteen
          , dname "mt" $ m `eq'` t
          , dname "mt1" $ m `eq'` (t `add'` one)
          , dname "n15" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
          , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          , dname "nt1" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` (t `add'` one))
          ]

ldrhimm = [ not 31 28 0b1111
          , constant 27 25 0b000
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 1
          , reg 19 16
          , reg 15 12
          , any 11 8 -- imm4H
          , constant 7 4 0b1011
          , any 3 0 -- imm4L
            -- restrictions
          , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          , dname "fifteen" $ t `eq'` fifteen
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
          , dname "pw" $ (p `eq'` zero) `or'` (w `eq'` one)
          , dname "fifteen" $ t `eq'` fifteen
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
          , dname "t" $ t `eq'` fifteen
          , dname "m" $ m `eq'` fifteen
          , dname "fifteen" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
          , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          ]

ldrsbimm = [ not 31 28 0b1111
           , constant 27 25 0b000
           , any 24 23
           , constant 22 22 1
           , any 21 21
           , constant 20 20 1
           , reg 19 16
           , reg 15 12
           , any 11 8 -- imm4H
           , constant 7 4 0b1101
           , any 3 0 -- imm4L
           -- restrictions
           , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
           , dname "t" $ t `eq'` fifteen
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
           , dname "pw" $ (p `eq'` zero) `or'` (w `eq'` one)
           , dname "fifteen" $ t `eq'` fifteen
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
           , dname "t" $ t `eq'` fifteen
           , dname "m" $ m `eq'` fifteen
           , dname "n" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
           , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
           ]

ldrshimm = [ not 31 28 0b1111
           , constant 27 25 0b000
           , any 24 23
           , constant 22 22 1
           , any 21 21
           , constant 20 20 1
           , reg 19 16
           , reg 15 12
           , any 11 8 -- imm4H
           , constant 7 4 0b1111
           , any 3 0 -- imm4L
           -- restrictions
           , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
           , dname "fifteen" $ t `eq'` fifteen
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
           , dname "pw" $ (p `eq'` zero) `or'` (w `eq'` one)
           , dname "fifteen" $ t `eq'` fifteen
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
           , dname "t" $ t `eq'` fifteen
           , dname "m" $ m `eq'` fifteen
           , dname "fifteen" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
           , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
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
         , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
         , dname "fifteen" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
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
         , dname "fifteen" $ m `eq'` fifteen
         , dname "n" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
         , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
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
          , dname "fifteen" $ t `eq'` fifteen
          , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          , dname "nf" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
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
          , dname "t" $ t `eq'` fifteen
          , dname "m" $ m `eq'` fifteen
          , dname "n15" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` c 15)
          , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          ]

strdimm = [ not 31 28 0b1111
          , constant 27 25 0b000
          , any 24 23
          , constant 22 22 1
          , any 21 21
          , constant 20 20 0
          , reg 19 16
          , reg 15 12
          , any 11 8 -- imm4H
          , constant 7 4 0b1111
          , any 3 0 -- imm4L
            -- restrictions
          , dname "pw" $ (p `eq'` zero) `and'` (w `eq'` one)
          , dname "n15"$ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
          , dname "nt"$ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          , dname "nt2"$ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` (t `add'` one))
          , dname "t2" $ (t `add'` one) `eq'` fifteen
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
          , dname "pw" $ (p `eq'` zero) `and'` (w `eq'` one)
          , dname "t2" $ (t `add'` one) `eq'` fifteen
          , dname "m" $ m `eq'` fifteen
          , dname "n15"$ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
          , dname "nt"$ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
          , dname "nt2"$ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` (t `add'` one))
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
          , dname "t15" $ t `eq'` fifteen
          , dname "n15" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
          , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
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
          , dname "t" $ t `eq'` fifteen
          , dname "m" $ m `eq'` fifteen
          , dname "n15" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` fifteen)
          , dname "nt" $ ((p `eq'` zero) `or'` (w `eq'` one)) `and'` (n `eq'` t)
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

ldrex = atomicLoad 0b00011001 ++ [ dname "t" $ t `eq'` fifteen
                                 , dname "n" $ n `eq'` fifteen
                                 ]
ldrexb = atomicLoad 0b00011101 ++ [ dname "t" $ t `eq'` fifteen
                                  , dname "n" $ n `eq'` fifteen
                                  ]
ldrexd = atomicLoad 0b00011011 ++ [ dname "rt0" $ rt0 `eq'` one
                                  , dname "t2" $ (t `add'` one) `eq'` fifteen
                                  , dname "n" $ n `eq'` fifteen
                                  ]
ldrexh = atomicLoad 0b00011111 ++ [ dname "t" $ t `eq'` fifteen
                                  , dname "n" $ n `eq'` fifteen
                                  ]

-- Atomic stores
atomicStore c = [ not 31 28 0b1111
                , constant 27 20 c
                , not15 19 16
                , not15 15 12
                , any 11 10
                , constant 9 4 0b111001
                , not15 3 0
                ]

rn = v 19 16
rd = v 15 12
rt = v 3 0
rt0' = v 0 0

strex  = atomicStore 0b00011000 ++ [ dname "d" $ rd `eq'` fifteen
                                   , dname "t" $ rt `eq'` fifteen
                                   , dname "n" $ rn `eq'` fifteen
                                   , dname "dn" $ rd `eq'` rn
                                   , dname "dt" $ rd `eq'` rt
                                   ]
strexb = atomicStore 0b00011100 ++ [ dname "d" $ rd `eq'` fifteen
                                   , dname "t" $ rt `eq'` fifteen
                                   , dname "n" $ rn `eq'` fifteen
                                   , dname "dn" $ rd `eq'` rn
                                   , dname "dt" $ rd `eq'` rt
                                   ]
strexd = atomicStore 0b00011010 ++ [ dname "d" $ rd `eq'` fifteen
                                   , dname "rt0" $ rt0' `eq'` one
                                   , dname "t2" $ (rt `add'` one) `eq'` fifteen
                                   , dname "n" $ rn `eq'` fifteen
                                   , dname "dn" $ rd `eq'` rn
                                   , dname "dt" $ rd `eq'` rt
                                   , dname "dt2" $ rd `eq'` (rt `add'` one)
                                   ]
strexh = atomicStore 0b00011110 ++ [ dname "d" $ rd `eq'` fifteen
                                   , dname "t" $ rt `eq'` fifteen
                                   , dname "n" $ rn `eq'` fifteen
                                   , dname "dn" $ rd `eq'` rn
                                   , dname "dt" $ rd `eq'` rt
                                   ]

-- Helpers:

reg hi lo = range hi lo 0 9

not15 hi lo = range hi lo 0 9
