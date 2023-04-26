import           Data.Foldable
import           Data.Function
import           Data.List                      ( sortBy )

import           Book.IO
import           Book.Types.Chapter

main :: IO ()
main = checkAllChaptersForExistence

checkAllChaptersForExistence :: IO ()
checkAllChaptersForExistence = do
  chapters <- sortBy (compare `on` chapterId) <$> getAllChapters
  _        <- foldlM checkOneChapter 0 chapters
  pure ()
 where
  checkOneChapter :: Int -> Chapter -> IO Int
  checkOneChapter n ch
    | n == chapterId ch
    = pure $ n + 1
    | otherwise
    = putStrLn ("Chapter " <> show n <> " is not exist")
      >> checkOneChapter (n + 1) ch
