module ADiff.Instrumentation.Browser.Types where

import RIO

-- | Describes the position of a statement in the translation unit by encoding a "path" through the abstract syntax tree.
data AstPosition = AstPosition
  { functionName :: String
  , indices      :: [Int]
  } deriving (Eq, Ord, Show)

astDepth :: AstPosition -> Int
astDepth (AstPosition _ is) = length is


instance Display AstPosition where
 display (AstPosition fn xs) = display (tshow fn) <> "/" <> foldl' f "" xs
  where f b x = display b <> "/" <> display x
