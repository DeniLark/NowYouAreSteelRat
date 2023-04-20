module Book.Types.Chapter where

import           Data.Aeson
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

import           Book.Types.NextChapter
import           Book.Types.NextRandomChapter
import           Book.Types.TypeChapter


data Chapter = Chapter
  { chapterId          :: Int
  , typeChapter        :: Maybe Type -- Nothing == Simple
  , text               :: [Text]
  , nextChapters       :: Maybe [NextChapter]
  , nextChaptersRandom :: Maybe NextRandomChapter
  }
  deriving (Generic, Show)

instance FromJSON Chapter where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = chapterFieldsMod }
