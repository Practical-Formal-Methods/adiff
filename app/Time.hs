module Main where

import           RIO

import           System.Environment
import           System.IO          (putStrLn)
import           System.Process
import           Timed

main :: IO ()
main = do
  args <- getArgs
  let cp = case args of
        [cmd]           -> shell cmd
        (program:args') -> proc program args'
        _               -> error "missing arg"

  logOptions <- logOptionsHandle stderr True
  withLogFunc logOptions $ \logger -> runRIO logger $ do
    (_, out, _) <- execTimed cp ""
    liftIO $ putStrLn out
