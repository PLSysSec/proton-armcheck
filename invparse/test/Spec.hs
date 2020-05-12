import           Control.Monad   (mapM_)
import           Test.HUnit.Text
import           TestCodegen

main :: IO ()
main = mapM_ runTestTT [ testEq ]
