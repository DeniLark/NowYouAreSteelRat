{-# LANGUAGE ScopedTypeVariables #-}
module Book.IOTests where

import           Data.Aeson
import qualified Data.ByteString               as B

import           Book.IO
import           Book.Types.Book
import           Book.Types.Chapter


getBookTest :: IO Book
getBookTest = listChapterToBook <$> getChaptersFromFile "chapter66_74.json"

getChaptersTest' :: IO ()
getChaptersTest' = do
  chapterTemplateJSON :: Either String [Chapter] <- eitherDecodeStrict -- decodeStrict
    -- <$> B.readFile "./book/chapters/chapter0_11.json"
    <$> B.readFile "./book/chapters/chapter12_23.json"
  either putStrLn (mapM_ print) chapterTemplateJSON
