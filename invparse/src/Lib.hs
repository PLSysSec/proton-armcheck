module Lib
    ( someFunc
    ) where
import           A32v8
import           AST
import           Codegen
import           Control.Monad (forM_)

someFunc :: IO ()
someFunc = do
  forM_ a32v8instrs $ \(instr, name) -> do
    let str = bitstring $ genConstantMatchInstr instr
    putStrLn name
    print $ length str
    print str
