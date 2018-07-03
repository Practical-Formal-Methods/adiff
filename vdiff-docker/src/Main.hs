{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Int
import           Data.Text           (splitOn)
import qualified Data.Text           as T
import           Options.Applicative
import           Prelude
import           Safe                (readMay)
import           System.Environment
import           System.IO
import           System.Process

description :: String
description =
  "This is a small shell script that starts the docker image vdiff:latest that" ++
  "has all the verifiers and the vdiff tools installed. This used to be a shell " ++
  "script, but parsing arguments in consistent and self-documenting way is just " ++
  "much easier this way.\n\n" ++
  "IMPORTANT VARIABLES: $VDIFF_DOCKER: this is where the database and the benchmarks are stored"


data Parameters = Parameters
  { cpus        :: Int
  , cpusetCpus  :: Maybe String
  , memory      :: String
  , portMapping :: [(Int16,Int16)]
  , exec        :: Bool
  } deriving Show


parameterParser :: Parser Parameters
parameterParser =  Parameters <$> parseCpus <*> parseCPUSet <*> parseMemory <*> parsePortMappings <*> parseExec
  where
    parseCpus = option auto $ mconcat
      [ long "cpus" , help "limit the number of used cpus"
      , value 1, metavar "CPUS"
      ]
    parseMemory = option str $ mconcat
      [ long "memory"
      , help "limit the number of used memory"
      , value "8G"
      , metavar "MEM"
      ]
    parseCPUSet = optional $ option str $ mconcat
      [ long "cpuset-cpus"
      , help "limit the process to the comma separated list of cpus"
      , metavar "CPUSET"
      ]
    parsePortMappings = many $ option parsePair $ mconcat [short 'p', help "port mapping a la docker"]
    parseExec = flag False True $ mconcat [long "exec",  help "execute a command"]
    parsePair = str >>= \s -> do
      case splitOn ":" s of
        [x,y] -> case (readMay (T.unpack x), readMay (T.unpack y)) of
                   (Just x', Just y') -> return (x',y')
                   _                  -> readerError "wrong format"
        _     -> readerError "wrong format"


opts :: ParserInfo Parameters
opts = info (parameterParser <**> helper) (progDesc description)

main :: IO ()
main = do
  allArgs <- getArgs
  let regular = takeWhile (/= "--") allArgs
      passthrough = drop 1 $ dropWhile (/= "--") allArgs

  Parameters{..} <- handleParseResult $ execParserPure defaultPrefs opts regular

  prepareEnv
  let
    limits = [ "--cpus=" ++ show cpus
            , "--memory=" ++ memory
            , maybe "" ("--cpuset-cpus="++) cpusetCpus
            , "--memory-swap=0"
            ]
    other = map (\(x,y) -> "-p="++ show x ++ ":" ++ show y) portMapping
    cmd = concat' $ ["docker run -it"] ++ mounts ++ limits ++ other ++ ["vdiff/vdiff:latest"]
  if exec
    then callCommand $ cmd ++ " /bin/bash -c '" ++ concat' passthrough ++ "'"
    else callCommand $ cmd ++ " /bin/bash"

mounts :: [String]
mounts = map (\(a,b) -> "-v " ++ a ++ ":" ++ b)
  [ ("$VDIFF_DOCKER/docker-bash-history", "/root/.bash_history")
  , ("$VDIFF_DOCKER/sv-benchmarks", "/benchmarks")
  , ("$VDIFF_DOCKER/database", "/database")
  ]

prepareEnv :: IO ()
prepareEnv = do
  v <- lookupEnv "VDIFF_DOCKER"
  case v of
    Nothing -> error "VDIFF_DOCKER is not set"
    Just v' -> do
      callCommand "touch $VDIFF_DOCKER/docker-bash-history"
      callCommand "mkdir -p $VDIFF_DOCKER/database"


concat' :: [String] -> String
concat' = concatMap (++ " ")
