module Bot.Model where


import           Book.Types.Chapter             ( Chapter )

data Model = Model
  { modelBook :: [Chapter]
  }
