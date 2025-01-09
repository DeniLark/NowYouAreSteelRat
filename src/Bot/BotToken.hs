module Bot.BotToken
  ( getTokenFromEnvOrCLI
  ) where

import           Control.Applicative            ( Alternative((<|>)) )
import qualified Data.Text.IO                  as T
import qualified Telegram.Bot.API              as Telegram
import           Telegram.Bot.Simple            ( getEnvToken )

getTokenFromEnvOrCLI :: IO Telegram.Token
getTokenFromEnvOrCLI =
  getEnvToken "NOW_YOU_ARE_STEEL_RAT__BOT_TOKEN"
    <|> (putStrLn "Input your token: " >> (Telegram.Token <$> T.getLine))
