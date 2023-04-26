module Bot.Model where

import qualified Data.Map                      as M
import           Data.Maybe

import           Book.IO
import           Book.Types.Book                ( Book )
import           Book.Types.Chapter             ( Chapter )

type UserId = Integer

data Model = Model
  { modelBook           :: Book
  , modelCurrentChapter :: M.Map UserId Int
  }
  deriving Show

initialModel :: IO Model
initialModel = do
  book <- getBook
  pure $ Model book M.empty

currentChapter :: UserId -> Model -> Chapter
currentChapter userId model =
  fromJust $ M.lookup (currentChapterInt userId model) $ modelBook model

currentChapterInt :: UserId -> Model -> Int
currentChapterInt userId = fromMaybe 0 . M.lookup userId . modelCurrentChapter

newCurrentChapter :: UserId -> Int -> Model -> Model
newCurrentChapter userId newChapter model = model
  { modelCurrentChapter = newMapCurrentChapter
  }
 where
  newMapCurrentChapter :: M.Map UserId Int
  newMapCurrentChapter = M.insert userId newChapter $ modelCurrentChapter model
