{-# LANGUAGE BinaryLiterals #-}
module A64v8 where
import           AST

-- Section C6.2

-- Instructions with notable unpredictable behavior:

t = v 4 0
n = v 9 5
t2 = v 14 10
_31 = c 31

-- ldp

ldppre c = [ constant c 31 30
           , constant 0b10100011 29 22
           , t `eq'` n
           , t2 `eq'` n
           ]

ldppost c = [ constant c 31 30
            , constant 0b10100111 29 22
            , t `eq'` n
            , t2 `eq'` n
            ]

ldppre32  = ldppre 00
ldppost32 = ldppost 00
ldppre64  = ldppre 10
ldppost64 = ldppost 10

-- loads

loadpre c1 c2 c3 = [ constant 31 30 c1
                   , constant 29 21 c2
                   , constant 11 10 c3
                   , (n `eq'` t) `and'` (n `neq'` _31)
                   ]

loadpost c1 c2 c3 = [ constant 31 30 c1
                    , constant 29 21 c2
                    , constant 11 10 c3
                    ]

-- ldr imm

ldrpost32 = loadpost 0b10 0b111000010 0b01
ldrpost64 = loadpost 0b11 0b111000010 0b01
ldrpre32  = loadpre  0b10 0b111000010 0b11
ldrpre64  = loadpre  0b11 0b111000010 0b11

-- ldrb imm

ldrbpost = loadpost 0b00 0b111000010 0b01
ldrbpre  = loadpre  0b00 0b111000010 0b11

-- ldrh imm

ldrhpost = loadpost 0b01 0b111000010 0b01
ldrhpre  = loadpre  0b01 0b111000010 0b11

-- ldrsb imm

ldrsbpost32 = loadpost 0b00 0b111000110 0b01
ldrsbpost64 = loadpost 0b00 0b111000100 0b01
ldrsbpre32  = loadpre  0b00 0b111000110 0b11
ldrsbpre64  = loadpre  0b00 0b111000100 0b11

-- ldrsh imm

ldrshpost32 = loadpost 0b01 0b111000110 0b01
ldrshpost64 = loadpost 0b01 0b111000100 0b01
ldrshpre32  = loadpre  0b01 0b111000110 0b11
ldrshpre64  = loadpre  0b01 0b111000100 0b11

-- ldrsw imm

ldrswpost = loadpost 0b10 0b111000100 0b01
ldrswpre  = loadpre  0b10 0b111000100 0b11

-- stores

storepre c1 c2 c3 = [ constant 31 30 c1
                    , constant 29 21 c2
                    , constant 11 10 c3
                    , (n `eq'` t) `and'` (n `neq'` _31)
                    ]

storepost c1 c2 c3 = [ constant 31 30 c1
                     , constant 29 21 c2
                     , constant 11 10 c3
                     ]
-- str

strpost32 = storepost 0b10 0b111000000 0b01
strpost64 = storepost 0b11 0b111000000 0b01
strpre32  = storepre  0b10 0b111000000 0b11
strpre64  = storepre  0b11 0b111000000 0b11

-- strb

strpost = storepost 0b00 0b111000000 0b01
strpre  = storepre  0b00 0b111000000 0b11

-- strh imm

strhpost = storepost 0b01 0b111000000 0b01
strhpre  = storepre  0b01 0b111000000 0b11

-- exlusive stores

s = v 20 16

storex c0 = [ constant 31 21 c0
            , constant 15 15 0
            , s `eq'` t
            , (s `eq'` n) `and'` (n `eq'` _31)
            ]
-- stxr

stxr32 = storex 0b10001000000
stxr64 = storex 0b11001000000

-- stxrh

stxrh = storex 0b01001000000

-- stxrb

stxrb = storex 0b00001000000

-- Loads: ldr X , ldrb X , ldrsb, ldrh X , ldrsh X , ldrsw X

o1 = v 14 14
zero = c 0

load co = [ constant 31 21 co
          , constant 11 10 0b10
          , o1 `eq'` zero
          ]

ldrreg32   = load 0b10111000011
ldrreg64   = load 0b11111000011
ldrbreg    = load 0b00111000011
ldrhreg    = load 0b01111000011

ldrsb = [ constant 31 30 0b00
        , constant 29 23 0b1110001
        , constant 21 21 0b1
        , constant 11 10 0b10
        , o1 `eq'` zero
        ]

ldrshreg32 = load 0b01111000111
ldrshreg64 = load 0b01111000101
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

