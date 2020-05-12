import           Control.Monad   (mapM_)
import           Test.HUnit.Text
import           TestCodegen
import           TestEndToEnd

main :: IO ()
main = mapM_ runTestTT [ test0
                       , test1
                       , test2
                       ]
