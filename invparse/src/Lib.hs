module Lib
    ( someFunc
    ) where
import           A32v8
import           A64v8
import           AST
import           Codegen
import           Control.Monad (forM, forM_, unless, when)
import           Data.List     (nub)

someFunc :: IO ()
someFunc = do
  putStrLn "Checking generated code for correctness"
  -- Get the match strings
  putStrLn "Getting match strings and checking for 32 bit lengths"
  strings <- forM a64v8instrs $ \is -> do
    instrMatch <- genConstantMatchInstr is
    let str = bitstring instrMatch
    print instrMatch
    unless (length str == 32) $ error $ unwords [ "Non-32 bit bitstring"
                                                , snd is
                                                , ":"
                                                , str
                                                , "("
                                                , show $ length str
                                                , ")"
                                                ]
    return str
  print "Done."


  -- None of the match strings should be the same. We can afford to do this
  -- slowly and dumbly
  -- putStrLn "Checking for overlapping bitpatterns"
  -- forM_ strings $ \(s1, name1) ->
  --   forM_ strings $ \(s2, name2 ) ->
  --     when (name1 /= name2 && s1 == s2) $
  --       error $ unwords [ "Overlapping bitstring for instructions"
  --                       , name1
  --                       , "and"
  --                       , name2
  --                       , ":\n"
  --                       , s1
  --                       ]


