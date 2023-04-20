module Bot.Bot where

import qualified Telegram.Bot.API              as Telegram
import           Telegram.Bot.Simple


import           Bot.Action
import           Bot.BotToken                   ( getTokenFromEnvOrCLI )
import           Bot.Handlers
import           Bot.Model

initialBot :: BotApp Model Action
initialBot = BotApp { botInitialModel = Model []
                    , botAction       = flip handleUpdate
                    , botHandler      = handleAction
                    , botJobs         = []
                    }

run :: IO ()
run = do
  token <- getTokenFromEnvOrCLI
  env   <- Telegram.defaultTelegramClientEnv token
  startBot_ initialBot env

