module TestCodegen where
import           AST
import           Codegen
import           Test.HUnit.Base

testEq :: Test
testEq = undefined

testCodegenConstraints :: (Instruction, String) -> [InstrConstraint] -> Test
testCodegenConstraints instr expected = TestCase $ do
  match <- genConstantMatchInstr instr
  assertEqual "Unexpected constraints" expected (constraints match)


