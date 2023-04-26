{-# LANGUAGE OverloadedStrings #-}
module Bot.Keyboard where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Telegram.Bot.API              as Telegram

keyboardSimpleChapter :: [Int] -> Telegram.ReplyKeyboardMarkup
keyboardSimpleChapter chapters = Telegram.ReplyKeyboardMarkup
  { Telegram.replyKeyboardMarkupKeyboard           = buttons
  , Telegram.replyKeyboardMarkupResizeKeyboard     = Just True
  , Telegram.replyKeyboardMarkupOneTimeKeyboard    = Just True
  , Telegram.replyKeyboardMarkupSelective          = Just True
  , Telegram.replyKeyboardMarkupInputFieldSelector = Nothing
  }
 where
  buttons :: [[Telegram.KeyboardButton]]
  buttons = pure . addKeyboardButton . T.pack . show <$> chapters

addKeyboardButton :: Text -> Telegram.KeyboardButton
addKeyboardButton text = Telegram.KeyboardButton
  { Telegram.keyboardButtonText            = text
  , Telegram.keyboardButtonRequestContact  = Nothing
  , Telegram.keyboardButtonRequestLocation = Nothing
  , Telegram.keyboardButtonRequestPoll     = Nothing
  , Telegram.keyboardButtonWebApp          = Nothing
  }
