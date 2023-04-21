module Bot.Bot where

import qualified Telegram.Bot.API              as Telegram
import           Telegram.Bot.Simple


import           Bot.Action
import           Bot.BotToken                   ( getTokenFromEnvOrCLI )
import           Bot.Handlers
import           Bot.Model

import           Book.IOTests
import           Telegram.Bot.Simple.Debug      ( traceBotDefault )

initialBot :: IO (BotApp Model Action)
initialBot = do
  book <- getBookTest
  pure $ BotApp { botInitialModel = Model book 66
                , botAction       = flip handleUpdate
                , botHandler      = handleAction
                , botJobs         = []
                }

run :: IO ()
run = do
  token   <- getTokenFromEnvOrCLI
  env     <- Telegram.defaultTelegramClientEnv token
  initBot <- initialBot
  -- startBot_ (traceBotDefault initBot) env
  startBot_ initBot env

