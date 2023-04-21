module Book.Types.Book where

import           Book.Types.Chapter             ( Chapter(chapterId) )
import qualified Data.Map                      as M

type Book = M.Map Int Chapter

listChapterToBook :: [Chapter] -> Book
listChapterToBook = foldr addChapterInBook M.empty

addChapterInBook :: Chapter -> Book -> Book
addChapterInBook chapter = M.insert (chapterId chapter) chapter
