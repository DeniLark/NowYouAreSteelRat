module Bot.Bot where

import qualified Telegram.Bot.API              as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser

import           Bot.BotToken                   ( getTokenFromEnvOrCLI )
import           Data.Text                      ( Text )

data Model = Model
data Action = NoAction
            | Reply Text

initialBot :: BotApp Model Action
initialBot = BotApp { botInitialModel = Model
                    , botAction       = flip handleUpdate
                    , botHandler      = handleAction
                    , botJobs         = []
                    }

handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate $ Reply <$> text

handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction    model = pure model
handleAction (Reply msg) model = model <# do
  replyText msg
  pure NoAction


run :: IO ()
run = do
  token <- getTokenFromEnvOrCLI
  env   <- Telegram.defaultTelegramClientEnv token
  startBot_ initialBot env

