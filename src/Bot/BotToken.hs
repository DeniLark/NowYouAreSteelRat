module Bot.BotToken
  ( getTokenFromEnvOrCLI
  ) where

import           Control.Applicative            ( Alternative((<|>)) )
import qualified Data.Text.IO                  as T
import qualified Telegram.Bot.API              as Telegram
import           Telegram.Bot.Simple            ( getEnvToken )

getTokenFromEnvOrCLI :: IO Telegram.Token
getTokenFromEnvOrCLI =
  getEnvToken "TELEGRAM_BOT_TOKEN"
    <|> (putStrLn "Input your token: " >> (Telegram.Token <$> T.getLine))
