module TestEndToEnd where
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


testCodegenC :: (GlobalConstraint, Int) -- ^ Constraint, input value
             -> Int                     -- ^ Expected result
             -> Test                    -- ^ Actual result
testCodegenC (c, i) expected = TestCase $ do
  let instr = (Instruction [Global c], "test")
  match <- genConstantMatchInstr instr
  let eq = constraints match
  assertEqual "Bad number of constraints" 0 (length eq)
  i <- cpp $ unlines [ "int main(int argc, char *argv[]) {"
                     , "uint32_t instr = " ++ show i ++ ";"
                     , "uint32_t x =" ++ show eq ++ ";"
                     , "printf(\"%d\", x);"
                     , "}"
                     ]
  assertEqual "Expected equal" (show expected) i

readCommand
    :: FilePath             -- ^ Filename of the executable (see 'proc' for details)
    -> [String]             -- ^ any arguments
    -> String               -- ^ standard input
    -> IO (ExitCode,String) -- ^ exitcode, stdout+stderr
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
  fp <- bracket (mkstemps "/tmp/activeC" ".cpp")
                (hClose . snd)
                (\(f,h) -> hPutStr h src >> return (dropExtension f))

  -- compile
  (ccode,cout) <- readCommand cc ["-o", fp, fp ++ ".cpp"] ""
  removeFile $ fp ++ ".cpp"
  unless (ccode == ExitSuccess) $ fail cout
  -- run
  (code,out) <- readCommand fp [] ""
  removeFile fp
  unless (code == ExitSuccess) $ fail out
  readIO out

    where cc = "c++"
