{-# LANGUAGE OverloadedStrings #-}
module Bot.Keyboard where

import           Data.List.Split
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Telegram.Bot.API              as Telegram

import           Book.Types.Chapter
import           Book.Types.TypeChapter

keyboardSimpleChapter :: Chapter -> Telegram.ReplyKeyboardMarkup
keyboardSimpleChapter chapter = Telegram.ReplyKeyboardMarkup
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
    = pure $ pure $ addKeyboardButton $ textForRandom $ justNextChapters ch
    | otherwise
    = chunksOf 4 -- 4 кнопки в ряду 
      $   (addKeyboardButton . T.pack . show)
      <$> justNextChapters ch

textForRandom :: [Int] -> Text
textForRandom = T.intercalate " или " . map (T.pack . show)

addKeyboardButton :: Text -> Telegram.KeyboardButton
addKeyboardButton text = Telegram.KeyboardButton
  { Telegram.keyboardButtonText            = text
  , Telegram.keyboardButtonRequestContact  = Nothing
  , Telegram.keyboardButtonRequestLocation = Nothing
  , Telegram.keyboardButtonRequestPoll     = Nothing
  , Telegram.keyboardButtonWebApp          = Nothing
  }
