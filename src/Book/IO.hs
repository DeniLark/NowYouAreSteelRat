{-# LANGUAGE ScopedTypeVariables #-}
module Book.IO where

import           Data.Aeson
import qualified Data.ByteString               as B

import           Book.Types.Chapter

pathBook :: FilePath
pathBook = "./book/chapters/"

getChaptersFromFile :: FilePath -> IO [Chapter]
getChaptersFromFile filePath = do
  eitherChapters <- eitherDecodeStrict <$> B.readFile (pathBook <> filePath)
  either (\err -> putStrLn (filePath <> ": " <> err) >> pure [])
         pure
         eitherChapters


