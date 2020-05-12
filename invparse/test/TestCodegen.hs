module TestCodegen where
import           AST
import           Codegen
import           Test.HUnit.Base

p    = v 24 24
w    = v 21 21
zero = c 0
one  = c 1
fifteen = c 15
n    = v 19 16
t    = v 15 12
m    = v 3 0

testEq :: Test
testEq = undefined

testCodegenConstraints :: (Instruction, String) -> [InstrConstraint] -> Test
testCodegenConstraints instr expected = TestCase $ do
  match <- genConstantMatchInstr instr
  assertEqual "Unexpected constraints" expected (constraints match)


