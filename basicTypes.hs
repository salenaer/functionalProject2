{- origine of all data types to prevent cycli in imports. 
Exports all types that need to be constructed in different files. 
-}

module BasicTypes (Time(Time), Slot(Slot, NoPreference), Doodle(Doodle), UserRank(..), Schedule(Schedule), sortDoodle) where

import Data.List
import Text.Printf (printf)

data Time = Time Int Int Int Int Int deriving (Eq, Ord)

zeroPad::Int->String
zeroPad = printf "%02d"

instance Show Time where
    show (Time year month day hour minute) = 
        foldr (++) [] [(show year), "-", (zeroPad month), "-", zeroPad(day),
                        "T", zeroPad(hour), ":", zeroPad(minute)]

--------------------------------------------------------------------------------------------------------------
data Slot = Slot Time Time
          | NoPreference deriving(Eq)

instance Show Slot where
    show (Slot begin end) = unwords [(show begin), "/", (show end)]
    show NoPreference = "No preference"

--------------------------------------------------------------------------------------------------------------
--structure to store all possible slots for some exam
data Doodle = Doodle [Slot]

sortDoodle::Doodle->Doodle
sortDoodle (Doodle slots) = 
    Doodle $ sortOn (\(Slot begin end)->end) slots

insertComma :: String->String->String
insertComma a [] = a ++ "\n]"
insertComma a b = a ++ ", \n" ++ b

instance Show Doodle where
    show (Doodle slots)=
        "[\n" ++ (foldr insertComma [] $ map (\x->"     " ++ show x) slots)

--------------------------------------------------------------------------------------------------------------
--clean way to encode user privileges
data UserRank = Teacher
              | Student
              deriving(Show, Eq, Ord)

--------------------------------------------------------------------------------------------------------------
--is returned to user when the exam schedule is asked
--list of course names and slots together with the number of votes for this schedule
--votes are only used in the calculation of the best schedule
data Schedule = Schedule [(String, Slot)] Int

instance (Eq Schedule) where
    (==) (Schedule _ cost1) (Schedule _ cost2) = cost1 == cost2 

instance (Ord Schedule) where
    (<=) (Schedule _ cost1) (Schedule _ cost2) = cost1 <= cost2

instance Show Schedule where
    show (Schedule list _) = 
      "[\n" ++ (foldr insertComma [] $ 
        map (\(name, slot)->name++" : " ++ show slot) list)