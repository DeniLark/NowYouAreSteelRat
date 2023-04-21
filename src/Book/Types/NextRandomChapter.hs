module Book.Types.NextRandomChapter where

import           Data.Aeson
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data NextRandomChapter = NextRandomChapter
  { nextRandomChapterText :: Text
  , nextRandomChapters    :: [Int]
  }
  deriving (Generic, Show)
instance FromJSON NextRandomChapter where
