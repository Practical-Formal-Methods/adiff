-- | the experimental data
module Data where

import           Data.ByteString
import qualified Data.ByteString.Char8          as C8
import           Database.SQLite.Simple.ToField

import           RIO

import           Timed

-- | An instrumented program
data CProgram = CProgram
  { programSource           :: String
  , programOriginalFilename :: String
  } deriving Show


type VerifierName = String

newtype Timeout = Timeout Int
  deriving (Show)

-- | A run of one verifier against one instrumented program
data VerifierRun = VerifierRun
  { runVerifierName :: VerifierName
  , verifierResult  :: VerifierResult
  , verifierCode    :: Hashed String
  } deriving (Show)



data Verdict = Sat | Unsat | Unknown
  deriving Show

-- | The result of the verification
data VerifierResult
  = VerifierTerminated Verdict Timing
  | VerifierTimedOut
  deriving Show

verdict :: VerifierResult -> Verdict
verdict (VerifierTerminated v _ ) = v
verdict VerifierTimedOut          = Unknown

timing :: VerifierResult -> Maybe Timing
timing (VerifierTerminated _ t) = Just t
timing (VerifierTimedOut)       = Nothing



instance ToField Verdict where
  toField Unsat   = toField ("unsat" :: String)
  toField Sat     = toField ("sat" :: String)
  toField Unknown = toField ("unknown" :: String)


newtype Hashed a = Hashed { getHash :: ByteString }
  deriving (Show, Eq)

instance ToField (Hashed a) where
  toField x = toField (C8.unpack $ getHash x)

