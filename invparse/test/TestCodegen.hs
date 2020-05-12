module TestCodegen where
import           AST
import           Codegen
import           Control.Monad   (forM_)
import           Test.HUnit.Base

p    = v 24 24
w    = v 21 21
zero = c 0
one  = c 1
fifteen = c 15
n    = v 19 16
t    = v 15 12
m    = v 3 0
e = NoOp Encoding
num = NoOp . Val

testEq :: Test
testEq =
  let eqs = [ p `eq'` zero
            , m `eq'` one
            -- , n `eq'` t
            ]
      ex1 = UnaryOp NotBits (BinOp (BinOp (BinOp e ShiftBits (num 24)) AndBits (num 1)) XorBits (num 0))
      ex2 = UnaryOp NotBits (BinOp (BinOp (BinOp e ShiftBits (num 0)) AndBits (num 15)) XorBits (num 1))
      ex3 = undefined
      exs = [ex1, ex2]
  in testCodegenConstraints (instr eqs, "eqs") exs

testCodegenConstraints :: (Instruction, String) -> [BitTest] -> Test
testCodegenConstraints instr expected = TestCase $ do
  match <- genConstantMatchInstr instr
  assertEqual "Mismatched lengths" (length expected) (length $ constraints match)
  forM_ (zip expected (reverse $ map constraintTest $ constraints match)) $ \(e, a) ->
    assertEqual "Unexpected constraints" e a


