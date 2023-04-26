module Bot.Action where

import           Bot.Model
import           Data.Text                      ( Text )

data Action = NoAction
            | NewChapter UserId Int
            | Prev UserId
            | Next UserId
            | Start UserId
            | ShowChapter UserId
            | Reply UserId Text
  deriving (Show, Read)
