module TestEndToEnd (test1) where
import           AST
import           Foreign.C
import           GHC.IO.Exception   (IOErrorType (..), IOException (..))
import           System.Posix.Temp

import           Codegen
import           Control.Concurrent
import           Control.DeepSeq    (rnf)
import           Control.Exception  (SomeException, mask, onException, throwIO,
                                     try)
import           Control.Exception  (bracket)
import qualified Control.Exception  as C
import           Control.Monad
import           Control.Monad      (forM_, unless)
import           System.Directory   (removeFile)
import           System.Exit        (ExitCode (..))
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO
import           System.Process
import           Test.HUnit.Base

p    = v 24 24
w    = v 21 21
zero = c 0
one  = c 1
fifteen = c 15
n    = v 19 16
t    = v 15 12
m    = v 3 0

test1 :: Test
test1 = testCodegenC (m `eq'` one) 1 1


testCodegenC :: Bits -- ^ constraint
             -> Int  -- ^ input
             -> Int  -- ^ expected result
             -> Test -- ^ actual result
testCodegenC c i expected = TestCase $ do
  let instr = (Instruction [c], "test")
  match <- genConstantMatchInstr instr
  let eq = constraints match
  assertEqual "Bad number of constraints" 1 (length eq)
  i <- cpp $ unlines [ "#include <stdio.h>"
                     , "#include <stdint.h>"
                     , "int main(int argc, char *argv[]) {"
                     , "uint32_t instr = " ++ show i ++ ";"
                     , "uint32_t x =" ++ showCompilable (head eq) ++ ";"
                     , "printf(\"%d\", x);"
                     , "}"
                     ]
  assertEqual "Expected equal" expected i

readCommand
    :: FilePath              -- ^ Filename of the executable (see 'proc' for details)
    -> [String]              -- ^ any arguments
    -> String                -- ^ standard input
    -> IO (ExitCode, String) -- ^ code, stdout+stderr
readCommand proc args input = do
    let cmd = unwords $ proc : (args ++ ["2>&1"])
    mask $ \restore -> do
      (inh, outh, _, pid) <- runInteractiveCommand cmd
      flip onException
        (do hClose inh; hClose outh;
            terminateProcess pid; waitForProcess pid) $ restore $ do
        -- fork off a thread to start consuming stdout
        out <- hGetContents outh
        waitOut <- forkWait $ C.evaluate $ rnf out

        -- now write and flush any input
        let writeInput = do
              unless (null input) $ do
                hPutStr inh input
                hFlush inh
              hClose inh

        C.catch writeInput $ \e -> case e of
          IOError { ioe_type = ResourceVanished
                  , ioe_errno = Just ioe }
            | Errno ioe == ePIPE -> return ()
          _ -> throwIO e

        -- wait on the output
        waitOut

        hClose outh

        -- wait on the process
        ex <- waitForProcess pid

        return (ex, out)

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)

cpp :: Read a => String -> IO a
cpp src = do
  let fp         = "testfile.cpp"
      executable = "testfile"
      cmd        = "./testfile"
  writeFile fp src

  -- compile
  (ccode,cout) <- readCommand cc ["-o", executable, fp] ""
  unless (ccode == ExitSuccess) $ fail cout

  -- run
  (code,out) <- readCommand cmd [] ""

  unless (code == ExitSuccess) $ fail out
  readIO out

    where cc = "c++"
