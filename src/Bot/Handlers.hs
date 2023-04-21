{-# LANGUAGE OverloadedStrings #-}
module Bot.Handlers where

import           Control.Applicative
import           Control.Monad.Trans            ( liftIO )
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
handleUpdate _ = parseUpdate $ (Start <$ command "start") <|> (Reply <$> text)

handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = pure model
handleAction Start    model = model <# do
  let chapter = lookupChapter (currentChapter model) $ modelBook model
  reply (toReplyMessage $ textChapter chapter)
    { replyMessageReplyMarkup = Just
                                $ Telegram.SomeReplyKeyboardMarkup
                                $ keyboardSimpleChapter
                                $ justNextChapters chapter
    }
  pure NoAction

handleAction (Reply msg) model = newModel <# do
  replyText msg
  pure Start
 where
  newModel :: Model
  newModel = case msg of
    "Next" -> model { currentChapter = currentChapter model + 1 }
    "Prev" -> model { currentChapter = currentChapter model - 1 }
    -- _ -> model { currentChapter = currentChapter model + 1 }
    _      -> model
