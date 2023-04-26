{-# LANGUAGE OverloadedStrings #-}
module Bot.Handlers where

import           Control.Applicative
import           Data.Maybe
import           Data.Text.Read
import qualified Telegram.Bot.API              as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser

import           Book.Types.Book
import           Book.Types.Chapter             ( Chapter(nextChapters)
                                                , justNextChapters
                                                , textChapter
                                                )
import           Bot.Action
import           Bot.Keyboard                   ( keyboardSimpleChapter )
import           Bot.Model
import           Control.Monad                  ( when )
import           Data.Bool                      ( bool )


handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ update = parseUpdate parser update
 where
  user = fromJust $ Telegram.updateMessage update >>= Telegram.messageFrom
  userId = Telegram.userId user
  Telegram.UserId userIdInteger = userId
  parser =
    (Start userIdInteger <$ command "start")
      <|> (Prev userIdInteger <$ command "prev")
      <|> (Next userIdInteger <$ command "next")
      <|> (Reply userIdInteger <$> text)


handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = pure model
handleAction (Prev userId) model =
  newCurrentChapter userId (currentChapter userId model - 1) model
    <# pure (ShowChapter userId)
handleAction (Next userId) model =
  newCurrentChapter userId (currentChapter userId model + 1) model
    <# pure (ShowChapter userId)
handleAction (Start       userId) model = model <# pure (ShowChapter userId)
handleAction (ShowChapter userId) model = model <# do
  let chapter = lookupChapter (currentChapter userId model) $ modelBook model
  reply (toReplyMessage $ textChapter chapter)
    { replyMessageReplyMarkup = Just
                                $ Telegram.SomeReplyKeyboardMarkup
                                $ keyboardSimpleChapter
                                $ justNextChapters chapter
    }
  pure NoAction
handleAction (Reply userId msg) model = do
  let chapterInt           = currentChapter userId model
      chapter              = lookupChapter chapterInt $ modelBook model
      possibleNextChapters = nextChapters chapter
      eitherInt            = fst <$> (decimal :: Reader Int) msg
      maybeInt             = either
        (const Nothing)
        (\a -> bool Nothing (Just a) $ a `elem` possibleNextChapters)
        eitherInt
      newModel = maybe model (flip (newCurrentChapter userId) model) maybeInt
  eff $ do
    when (isNothing maybeInt) (replyText "Неверный ввод")
  eff $ pure $ ShowChapter userId
  pure newModel
