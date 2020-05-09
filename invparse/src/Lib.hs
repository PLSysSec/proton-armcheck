module Lib
    ( someFunc
    ) where
import           A32v8
import           AST
import           Codegen
import           Control.Monad (forM_, unless)

someFunc :: IO ()
someFunc = do
  putStrLn "Checking generated code for correctness"
  forM_ a32v8instrs $ \(instr, name) -> do
    let str = bitstring $ genConstantMatchInstr instr
    unless (length str == 32) $ error $ unwords [ "Bad bitstring"
                                                , name
                                                , ":"
                                                , str
                                                , "("
                                                , show $ length str
                                                , ")"
                                                ]
