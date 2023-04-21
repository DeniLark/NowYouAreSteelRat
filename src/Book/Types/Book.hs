module Book.Types.Book where

import           Book.Types.Chapter             ( Chapter(chapterId) )
import qualified Data.Map                      as M
import           Data.Maybe

type Book = M.Map Int Chapter

lookupChapter :: Int -> Book -> Chapter
lookupChapter i = fromJust . M.lookup i

listChapterToBook :: [Chapter] -> Book
listChapterToBook = foldr addChapterInBook M.empty

addChapterInBook :: Chapter -> Book -> Book
addChapterInBook chapter = M.insert (chapterId chapter) chapter
