module Book.Types.Chapter where

import           Data.Aeson
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )

import           Book.Types.TypeChapter


data Chapter = Chapter
  { chapterId           :: Int
  , chapterType         :: Maybe Type -- Nothing == Simple
  , chapterText         :: [Text]
  , chapterNextChapters :: [Int]
  }
  deriving (Generic, Show)

chapterFieldsMod :: String -> String
chapterFieldsMod "chapterType"         = "type"
chapterFieldsMod "chapterText"         = "text"
chapterFieldsMod "chapterNextChapters" = "nextChapters"
chapterFieldsMod s                     = s

instance FromJSON Chapter where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = chapterFieldsMod }

textChapter :: Chapter -> Text
textChapter chapter =
  T.unlines $ T.pack (show $ chapterId chapter) : chapterText chapter

isChapterRandom :: Chapter -> Bool
isChapterRandom chapter | justTypeChapter chapter == Random = True
                        | otherwise                         = False

isChapterAllPaths :: Chapter -> Bool
isChapterAllPaths chapter | justTypeChapter chapter == AllPaths = True
                          | otherwise                           = False

justTypeChapter :: Chapter -> Type
justTypeChapter = fromMaybe Simple . chapterType

