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

-- | A run of one verifier against one instrumented program
data VerifierRun = VerifierRun
  { runVerifierName :: VerifierName
  , verifierResult  :: VerifierResult
  , verifierTiming  :: Timing
  , verifierCode    :: Hashed String
  } deriving (Show)



-- | The result of the verification
data VerifierResult
  = VerificationSuccessful -- ^ the error location is unreachable
  | VerificationFailed -- ^ the error location is reachable
  | VerificationResultUnknown  -- ^ the verifier does not know
  deriving (Eq, Show)

instance ToField VerifierResult where
  toField VerificationSuccessful    = toField ("unsat" :: String)
  toField VerificationFailed        = toField ("sat" :: String)
  toField VerificationResultUnknown = toField ("unknown" :: String)


newtype Hashed a = Hashed { getHash :: ByteString }
  deriving (Show, Eq)

instance ToField (Hashed a) where
  toField x = toField (C8.unpack $ getHash x)

