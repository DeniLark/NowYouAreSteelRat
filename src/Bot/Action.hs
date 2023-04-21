module Bot.Action where

import           Data.Text                      ( Text )

data Action = NoAction
            | Start
            -- | ViewChapter
            | Reply Text
  deriving Show
