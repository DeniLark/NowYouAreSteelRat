{-# LANGUAGE OverloadedStrings #-}
module Bot.Keyboard where

import           Data.List.Split
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Telegram.Bot.API              as Telegram

import           Book.Types.Chapter
import           Book.Types.TypeChapter

countBtnInLine :: Int
countBtnInLine = 5 -- кнопок в ряду

keyboardChapter :: Chapter -> Telegram.ReplyKeyboardMarkup
keyboardChapter chapter = Telegram.ReplyKeyboardMarkup
  { Telegram.replyKeyboardMarkupKeyboard           = buttons chapter
  , Telegram.replyKeyboardMarkupResizeKeyboard     = Just True
  , Telegram.replyKeyboardMarkupOneTimeKeyboard    = Just True
  , Telegram.replyKeyboardMarkupSelective          = Just True
  , Telegram.replyKeyboardMarkupInputFieldSelector = Nothing
  }
 where
  buttons :: Chapter -> [[Telegram.KeyboardButton]]
  buttons ch
    | justTypeChapter ch == Random
    = pure $ pure $ addKeyboardButton $ textForRandom $ chapterNextChapters ch
    | otherwise
    = intsToKeyboardButtons $ chapterNextChapters ch

textForRandom :: [Int] -> Text
textForRandom = T.intercalate " или " . map (T.pack . show)

intsToKeyboardButtons :: [Int] -> [[Telegram.KeyboardButton]]
intsToKeyboardButtons =
  chunksOf countBtnInLine . map (addKeyboardButton . T.pack . show)

intsToKeyboard :: [Int] -> Telegram.ReplyKeyboardMarkup
intsToKeyboard ints = Telegram.ReplyKeyboardMarkup
  { Telegram.replyKeyboardMarkupKeyboard           = intsToKeyboardButtons ints
  , Telegram.replyKeyboardMarkupResizeKeyboard     = Just True
  , Telegram.replyKeyboardMarkupOneTimeKeyboard    = Just True
  , Telegram.replyKeyboardMarkupSelective          = Just True
  , Telegram.replyKeyboardMarkupInputFieldSelector = Nothing
  }

addKeyboardButton :: Text -> Telegram.KeyboardButton
addKeyboardButton text = Telegram.KeyboardButton
  { Telegram.keyboardButtonText            = text
  , Telegram.keyboardButtonRequestContact  = Nothing
  , Telegram.keyboardButtonRequestLocation = Nothing
  , Telegram.keyboardButtonRequestPoll     = Nothing
  , Telegram.keyboardButtonWebApp          = Nothing
  }
