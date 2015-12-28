module BasicTypes (Time(Time), Slot(Slot, NoPreference), Doodle(Doodle), UserRank(..)) where

data Time = Time Int Int Int Int Int deriving (Show)
data Slot = Slot Time Time
          | NoPreference deriving(Show)
data Doodle = Doodle [Slot] deriving(Show)
data UserRank = Teacher
              | Student
              deriving(Show, Eq, Ord)