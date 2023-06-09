{-# LANGUAGE ScopedTypeVariables #-}
module Book.IO where

import           Data.Aeson
import qualified Data.ByteString               as B
import           System.Directory

import           Book.Types.Book
import           Book.Types.Chapter

pathBook :: FilePath
pathBook = "./book/chapters/"

getBook :: IO Book
getBook = listChapterToBook <$> getAllChapters

getAllChapters :: IO [Chapter]
getAllChapters = do
  filePaths <- listDirectory pathBook
  foldMap getChaptersFromFile filePaths

getChaptersFromFile :: FilePath -> IO [Chapter]
getChaptersFromFile filePath = do
  eitherChapters <- eitherDecodeStrict <$> B.readFile (pathBook <> filePath)
  either (\err -> putStrLn (filePath <> ": " <> err) >> pure [])
         pure
         eitherChapters


