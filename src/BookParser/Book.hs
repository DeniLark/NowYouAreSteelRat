{-# LANGUAGE ScopedTypeVariables #-}
module BookParser.Book where
import           Data.Aeson
import qualified Data.ByteString               as B

import           BookParser.Types



getChaptersTest :: IO ()
getChaptersTest = do
  chapterTemplateJSON :: Either String [Chapter] <- eitherDecodeStrict -- decodeStrict
    -- <$> B.readFile "./book/chapters/chapter0_11.json"
    <$> B.readFile "./book/chapters/chapter12_23.json"
  either putStrLn (mapM_ print) chapterTemplateJSON

