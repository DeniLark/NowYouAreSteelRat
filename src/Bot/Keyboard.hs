{-# LANGUAGE OverloadedStrings #-}
module Bot.Keyboard where

import           Book.Types.NextChapter
import           Data.Text                      ( Text )
import qualified Telegram.Bot.API              as Telegram

keyboardSimpleChapter :: [NextChapter] -> Telegram.ReplyKeyboardMarkup
keyboardSimpleChapter chapters = Telegram.ReplyKeyboardMarkup
  { Telegram.replyKeyboardMarkupKeyboard           = buttons
  , Telegram.replyKeyboardMarkupResizeKeyboard     = Just True
  , Telegram.replyKeyboardMarkupOneTimeKeyboard    = Just True
  , Telegram.replyKeyboardMarkupSelective          = Just True
  , Telegram.replyKeyboardMarkupInputFieldSelector = Nothing
  }
 where
  buttons :: [[Telegram.KeyboardButton]]
  buttons =
    (pure . addKeyboardButton . nextChapterText <$> chapters)
      <> [["Prev", "Next"]]


addKeyboardButton :: Text -> Telegram.KeyboardButton
addKeyboardButton text = Telegram.KeyboardButton
  { Telegram.keyboardButtonText            = text
  , Telegram.keyboardButtonRequestContact  = Nothing
  , Telegram.keyboardButtonRequestLocation = Nothing
  , Telegram.keyboardButtonRequestPoll     = Nothing
  , Telegram.keyboardButtonWebApp          = Nothing
  }
