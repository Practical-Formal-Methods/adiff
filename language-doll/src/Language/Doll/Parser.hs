{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Doll.Parser where

import qualified Control.Applicative        as A
import           Control.Monad.Reader
import           Data.Text
import qualified Data.Text                  as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

import qualified Data.Map                   as Map

type DollEnv = Map.Map Text Double

newtype ErrorMsg = ErrorMsg Text
  deriving (Show, Read, Eq, Ord)

instance ShowErrorComponent ErrorMsg  where
  showErrorComponent = show

type Parser a = ParsecT ErrorMsg Text (Reader DollEnv) a

sc :: Parser ()
sc = L.space space1 A.empty A.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc


constant :: Parser Double
constant = lexeme $ (try L.float <|> (fromIntegral <$> L.decimal))


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifierString :: Parser String
identifierString = (:) <$> letterChar <*> many alphaNumChar

identifier :: Parser Text
identifier = T.pack <$> identifierString


evalDoll :: DollEnv -> Text -> Either String Double
evalDoll m inp = case (runReader (runParserT expr "input" inp) m) of
  Right n  -> Right n
  Left err -> Left (parseErrorPretty err)

evalDollTest :: Text -> Either String Double
evalDollTest = evalDoll defMap
  where defMap = Map.fromList [("pi", 3), ("zero", 0)]

expr :: Parser Double
expr = makeExprParser term table <?> "expression"

term = parens expr <|> try funCall <|> constant <|> variable <?> "term"

funCall = do
  n <- identifier
  space
  case n of
    "max" -> max <$> lexeme term <*> lexeme term
    "min" -> min <$> lexeme term <*> lexeme term
    _     -> fail "not a fun call"

table = [ [ prefix  "-"  negate , prefix  "+"  id ]
        , [ binary  "*"  ((*) :: Double -> Double -> Double) , binary  "/"  (/) ]
        , [ binary  "+"  (+) , binary  "-"  (-)  ]
        ]


variable :: Parser Double
variable = do
  name <- identifier
  asks (Map.lookup name) >>= \case
    Nothing -> fail $ "undefined variable: "  ++ show name ++ "'"
    Just val -> return val

binary  name f = InfixL  (f <$ symbol name)
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)
