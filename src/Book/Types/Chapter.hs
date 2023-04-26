module Book.Types.Chapter where

import           Data.Aeson
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )

import           Book.Types.TypeChapter


data Chapter = Chapter
  { chapterId    :: Int
  , typeChapter  :: Maybe Type -- Nothing == Simple
  , text         :: [Text]
  , nextChapters :: [Int]
  }
  deriving (Generic, Show)

chapterFieldsMod :: String -> String
chapterFieldsMod "typeChapter" = "type"
chapterFieldsMod s             = s

instance FromJSON Chapter where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = chapterFieldsMod }

textChapter :: Chapter -> Text
textChapter chapter =
  T.unlines $ T.pack (show $ chapterId chapter) : text chapter

isChapterRandom :: Chapter -> Bool
isChapterRandom chapter | justTypeChapter chapter == Random = True
                        | otherwise                         = False

justTypeChapter :: Chapter -> Type
justTypeChapter = fromMaybe Simple . typeChapter

justNextChapters :: Chapter -> [Int]
justNextChapters = nextChapters
