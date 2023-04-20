module Book.Types.NextRandomChapter where

import           Data.Aeson
import           GHC.Generics                   ( Generic )

data NextRandomChapter = NextRandomChapter
  { nextRandomChapterText :: String
  , nextRandomChapters    :: [Int]
  }
  deriving (Generic, Show)
instance FromJSON NextRandomChapter where
