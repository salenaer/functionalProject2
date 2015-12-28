module BasicTypes (Time(Time), Slot(Slot, NoPreference), Doodle(Doodle), UserRank(..), sortDoodle) where

import Data.List

data Time = Time Int Int Int Int Int deriving (Show, Eq, Ord)
data Slot = Slot Time Time
          | NoPreference deriving(Show, Eq)
data Doodle = Doodle [Slot] deriving(Show)
data UserRank = Teacher
              | Student
              deriving(Show, Eq, Ord)

sortDoodle::Doodle->Doodle
sortDoodle (Doodle slots) = 
    Doodle $ sortOn (\(Slot begin end)->end) slots