module Book.Types.NextChapter where

import           Data.Aeson
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

instance FromJSON NextChapter where

data NextChapter = NextChapter
  { nextChapterId   :: Int
  , nextChapterText :: Text
  }
  deriving (Generic, Show)
