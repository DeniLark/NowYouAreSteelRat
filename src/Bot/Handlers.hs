module Bot.Handlers where

import qualified Telegram.Bot.API              as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser

import           Bot.Action
import           Bot.Model

handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate $ Reply <$> text

handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction    model = pure model
handleAction (Reply msg) model = model <# do
  replyText msg
  pure NoAction
