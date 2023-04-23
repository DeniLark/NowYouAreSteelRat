module Book.Types.TypeChapter where

import           Data.Aeson
import           Data.Char                      ( toLower )
import           GHC.Generics                   ( Generic )

data Type = Simple | Random | AllPaths | Final
  deriving (Generic, Show)

lowerFirst :: String -> String
lowerFirst ""       = ""
lowerFirst (c : cs) = toLower c : cs

instance FromJSON Type where
  parseJSON =
    genericParseJSON defaultOptions { constructorTagModifier = lowerFirst }
