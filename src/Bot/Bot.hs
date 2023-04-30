module Bot.Bot where

import qualified Telegram.Bot.API              as Telegram
import           Telegram.Bot.Simple


import           Bot.Action
import           Bot.BotToken                   ( getTokenFromEnvOrCLI )
import           Bot.Handlers
import           Bot.Model


initialBot :: IO (BotApp Model Action)
initialBot = do
  model <- initialModel
  pure $ BotApp { botInitialModel = model
                , botAction       = flip handleUpdate
                , botHandler      = handleAction
                , botJobs         = []
                }

run :: IO ()
run = do
  token   <- getTokenFromEnvOrCLI
  env     <- Telegram.defaultTelegramClientEnv token
  initBot <- initialBot
  startBot_ initBot env

