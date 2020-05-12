import           Control.Monad   (mapM_)
import           Test.HUnit.Text
import           TestEndToEnd

main :: IO ()
main = mapM_ runTestTT $ equality ++ ors ++ ands ++ compound
