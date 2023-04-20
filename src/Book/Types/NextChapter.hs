module Book.Types.NextChapter where

import           Data.Aeson
import           GHC.Generics                   ( Generic )

instance FromJSON NextChapter where

chapterFieldsMod :: String -> String
chapterFieldsMod "typeChapter" = "type"
chapterFieldsMod s             = s


data NextChapter = NextChapter
  { nextChapterId   :: Int
  , nextChapterText :: String
  }
  deriving (Generic, Show)
