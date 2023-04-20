module Bot.Action where

import           Data.Text                      ( Text )

data Action = NoAction
            | Reply Text
