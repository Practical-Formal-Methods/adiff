{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | the experimental data
module VDiff.Data where

import           Control.Lens
import           Data.ByteString
import qualified Data.ByteString.Base16         as Hex
import qualified Data.ByteString.Char8          as C8
import           Data.Text.Encoding
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField
import qualified Database.SQLite.Simple     as SQL

import           RIO

import           VDiff.Timed

newtype Hashed = Hashed { getHash :: ByteString }
  deriving (Eq)


instance ToField Hashed where
  toField x = toField (C8.unpack $ getHash x)

instance Display Hashed where
  display x =  display $ decodeUtf8 $ Hex.encode (getHash x)

instance Show Hashed where
  show x = C8.unpack $ Hex.encode $ getHash x

instance FromField Hashed where
  fromField x = do
    (s :: Text) <- fromField x
    let sbs = encodeUtf8 s
        unhex = fst $ Hex.decode sbs
    return $ Hashed unhex

-- | An instrumented program
data CProgram = CProgram
  { _source           :: !String
  , _originalFilename :: !String
  , _hash             :: !Hashed
  } deriving Show
makeFieldsNoPrefix ''CProgram


type VerifierName = String

newtype Timeout = Timeout Int
  deriving (Show)

-- | A run of one verifier against one instrumented program
data VerifierRun = VerifierRun
  { runVerifierName :: !VerifierName
  , verifierResult  :: !VerifierResult
  , verifierCode    :: !Hashed
  } deriving (Show)



data Verdict = Sat | Unsat | Unknown
  deriving (Eq, Show)

-- | The result of the verification
data VerifierResult
  = VerifierTerminated !Verdict !Timing
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


instance SQL.FromRow CProgram where
  fromRow = do
    h <- SQL.field
    o <- SQL.field
    c <- SQL.field
    return $ CProgram c o h

