module Main where

import           System.Environment
import           System.Process
import           Timed

main = do
  args <- getArgs
  let cp = case args of
        [cmd]           -> shell cmd
        (program:args') -> proc program args'
  (code,out, timed) <- execTimed cp ""
  putStrLn out
  print timed
