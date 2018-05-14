#!/usr/bin/env stack
-- stack --resolver lts-11.3 script --package optparse-applicative --package process
{-# LANGUAGE RecordWildCards #-}

import           Options.Applicative
import           System.Environment
import           System.Process

description :: String
description =
  "This is a small shell script that starts the docker image vdiff:latest that" ++
  "has all the verifiers and the vdiff tools installed. This used to be a shell " ++
  "script, but parsing arguments in consistent and self-documenting way is just " ++
  "much easier this way.\n\n" ++
  "IMPORTANT VARIABLES: $VDIFF_DOCKER: this is where the database and the benchmarks are stored"


data Parameters = Parameters
  { cpus       :: Int
  , cpusetCpus :: Maybe String
  , memory     :: String
  } deriving Show


parameterParser :: Parser Parameters
parameterParser =  Parameters <$> parseCpus <*> parseCPUSet <*> parseMemory
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

opts :: ParserInfo Parameters
opts = info (parameterParser <**> helper) (progDesc description)

main :: IO ()
main = do
  Parameters{..} <- execParser opts
  prepareEnv
  let
    limits = [ "--cpus=" ++ show cpus
             , "--memory=" ++ memory
             , maybe "" ("--cpuset-cpus="++) cpusetCpus
             , "--memory-swap=0"
             ]
    cmd = concat' $ ["docker run -it"] ++ mounts ++ limits ++ ["vdiff/vdiff:latest", "/bin/bash"]
  callCommand cmd

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
