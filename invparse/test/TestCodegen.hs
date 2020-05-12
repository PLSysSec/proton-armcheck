module TestCodegen where
import           AST
import           Codegen
import           Test.HUnit.Base

testCodegenConstraints :: (Instruction, String) -> [InstrConstraint] -> IO Test
testCodegenConstraints instr expected = do
  match <- genConstantMatchInstr instr
  return $ expected ~=? constraints match
