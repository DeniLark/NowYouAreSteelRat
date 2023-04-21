module Bot.Model where


import           Book.Types.Book

data Model = Model
  { modelBook      :: Book
  , currentChapter :: Int
  }
  deriving Show
