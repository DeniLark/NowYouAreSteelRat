{-# LANGUAGE OverloadedStrings #-}
module Bot.Handlers where

import           Control.Applicative
import           Control.Monad.Trans            ( liftIO )
import           Data.Maybe
import qualified Telegram.Bot.API              as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser

import           Book.Types.Book
import           Book.Types.Chapter             ( justNextChapters
                                                , textChapter
                                                )
import           Bot.Action
import           Bot.Keyboard                   ( keyboardSimpleChapter )
import           Bot.Model

handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ update = parseUpdate parser update
 where
  user = fromJust $ Telegram.updateMessage update >>= Telegram.messageFrom
  userId = Telegram.userId user
  Telegram.UserId userIdInteger = userId
  parser =
    (Start userIdInteger <$ command "start") <|> (Reply userIdInteger <$> text)


handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction       model = pure model
handleAction (Start userId) model = model <# do
  let chapter = lookupChapter (currentChapter userId model) $ modelBook model
  reply (toReplyMessage $ textChapter chapter)
    { replyMessageReplyMarkup = Just
                                $ Telegram.SomeReplyKeyboardMarkup
                                $ keyboardSimpleChapter
                                $ justNextChapters chapter
    }
  pure NoAction

handleAction (Reply userId msg) model = newModel <# do
  replyText msg
  pure $ Start userId
 where
  newModel :: Model
  newModel = case msg of
    "Next" -> newCurrentChapter userId (currentChapter userId model + 1) model
    "Prev" -> newCurrentChapter userId (currentChapter userId model - 1) model
    _      -> model
