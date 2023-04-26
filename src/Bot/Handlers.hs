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
                                                , isChapterRandom
                                                , justNextChapters
                                                , justTypeChapter
                                                , textChapter
                                                )
import           Bot.Action
import           Bot.Keyboard                   ( keyboardSimpleChapter )
import           Bot.Model
import           Control.Monad                  ( when )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Bool                      ( bool )
import           System.Random


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
      <|> callbackQueryDataRead
      <|> (Reply userIdInteger <$> text)


handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = pure model
handleAction (Prev userId) model =
  newCurrentChapter userId (currentChapterInt userId model - 1) model
    <# pure (ShowChapter userId)
handleAction (Next userId) model =
  newCurrentChapter userId (currentChapterInt userId model + 1) model
    <# pure (ShowChapter userId)
handleAction (Start userId) model = -- обнуляет прогресс
  newCurrentChapter userId 0 model <# pure (ShowChapter userId)
handleAction (ShowChapter userId) model = model <# do
  let chapter = currentChapter userId model
  reply (toReplyMessage $ textChapter chapter)
    { replyMessageReplyMarkup =
      Just $ Telegram.SomeReplyKeyboardMarkup $ keyboardSimpleChapter chapter
    }
  pure NoAction
handleAction (NewChapter userId newChapter) model =
  newCurrentChapter userId newChapter model <# pure (ShowChapter userId)
handleAction (Reply userId msg) model = do
  let chapterInt           = currentChapterInt userId model
      chapter              = lookupChapter chapterInt $ modelBook model
      possibleNextChapters = nextChapters chapter
      eitherInt            = fst <$> (decimal :: Reader Int) msg
      maybeInt             = either
        (const Nothing)
        (\a -> bool Nothing (Just a) $ a `elem` possibleNextChapters)
        eitherInt
  eff $ do
    newChapter <- if isChapterRandom chapter
      then liftIO $ getRandomFromList possibleNextChapters
      else maybe (replyText "Неверный ввод" >> pure chapterInt) pure maybeInt
    pure $ NewChapter userId newChapter
  pure model


getRandomFromList :: [Int] -> IO Int
getRandomFromList list = do
  gen <- newStdGen
  let pos = fst $ randomR (0, length list - 1) gen
  pure $ list !! pos
