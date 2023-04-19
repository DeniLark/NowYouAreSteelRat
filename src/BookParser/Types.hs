{-# LANGUAGE DeriveGeneric #-}

module BookParser.Types
  ( Chapter
  ) where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , Options
                                                  ( constructorTagModifier
                                                  , fieldLabelModifier
                                                  )
                                                , defaultOptions
                                                , genericParseJSON
                                                )
import           Data.Char                      ( toLower )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

instance FromJSON NextChapter where

chapterFieldsMod :: String -> String
chapterFieldsMod "typeChapter" = "type"
chapterFieldsMod s             = s


data Type = Simple | Random | AllPaths
  deriving (Generic, Show)

lowerFirst :: String -> String
lowerFirst ""       = ""
lowerFirst (c : cs) = toLower c : cs

instance FromJSON Type where
  parseJSON =
    genericParseJSON defaultOptions { constructorTagModifier = lowerFirst }

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

data NextChapter = NextChapter
  { nextChapterId   :: Int
  , nextChapterText :: String
  }
  deriving (Generic, Show)

data NextRandomChapter = NextRandomChapter
  { nextRandomChapterText :: String
  , nextRandomChapters    :: [Int]
  }
  deriving (Generic, Show)
instance FromJSON NextRandomChapter where
