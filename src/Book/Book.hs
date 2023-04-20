{-# LANGUAGE ScopedTypeVariables #-}
module Book.Book where
import           Data.Aeson
import qualified Data.ByteString               as B

import           Book.Types.Chapter

pathBook :: FilePath
pathBook = "./book/chapters/"

getChaptersTest :: IO [Chapter]
getChaptersTest = getChaptersFromFile "chapter66_74.json"

getChaptersFromFile :: FilePath -> IO [Chapter]
getChaptersFromFile filePath = do
  eitherChapters <- eitherDecodeStrict <$> B.readFile (pathBook <> filePath)
  either (\err -> putStrLn (filePath <> ": " <> err) >> pure [])
         pure
         eitherChapters

getChaptersTest' :: IO ()
getChaptersTest' = do
  chapterTemplateJSON :: Either String [Chapter] <- eitherDecodeStrict -- decodeStrict
    -- <$> B.readFile "./book/chapters/chapter0_11.json"
    <$> B.readFile "./book/chapters/chapter12_23.json"
  either putStrLn (mapM_ print) chapterTemplateJSON

