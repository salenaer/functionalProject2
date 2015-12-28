module Pool (UserPool, ExamPool, FollowerPool, Pool(Pool), Exam(Exam), User(User), addUser, changePassword, login, 
    emptyUserPool, addExam, getExamDoodle, subscribe, prefer, emptyExamPool) where

import BasicTypes(Time(Time), Slot(Slot, NoPreference), Doodle(Doodle), UserRank, sortDoodle)
import Error

import qualified Data.Map
import qualified Data.List
import Control.Monad

data User = User String String UserRank deriving (Show, Eq, Ord)
data Exam = Exam String Doodle FollowerPool deriving (Show)
data Pool key value = Pool (Data.Map.Map key value) deriving(Show) --brings map into either monad

type UserPool = Pool String User
type ExamPool = Pool String Exam
type FollowerPool = Pool String Slot

get::(Ord key)=>key->Pool key value->Either Error value
get id (Pool pool) = 
    case Data.Map.lookup id pool
        of Just value -> Right value 
           Nothing -> Left NoSuchId

member::(Ord key)=>key ->Pool key value->Bool
member id (Pool pool) = Data.Map.member id pool

add::(Ord key)=>key->value->Pool key value->Pool key value
add id x (Pool pool) = Pool $ Data.Map.insert id x pool

--------------------------UserPool---------------------------------

addUser::UserPool->String->String->UserRank->Either Error UserPool
addUser pool name password rank=
    if (member name pool)
        then Left IdTaken
        else Right $ add name (User name password rank) pool

changePassword::UserPool->String->String->UserRank->UserPool
changePassword pool name password rank=add name (User name password rank) pool
--changePassword is only called after the login has succeeded, we already know it is in there

login::UserPool->String->String->Either Error User
login pool name triedpassword= do
    user<-get name pool
    (\(User _ storedPassword _)-> if storedPassword == triedpassword
                                then return user
                                else Left WrongLogin) user

emptyUserPool::UserPool
emptyUserPool = Pool Data.Map.empty

--------------------------ExamPool----------------------------------
addExam::ExamPool -> String -> String -> Doodle -> Either Error ExamPool
addExam pool name teacher doodle = 
    case get name pool 
        of  Left _ -> Right $ add name (Exam teacher (sortDoodle doodle) emptyFollowerPool) pool
            Right (Exam storedTeacher _ followers) -> 
                if storedTeacher == teacher
                    then return $ add name (Exam teacher doodle followers) pool
                    else Left IdTaken

getExamDoodle::ExamPool -> String -> Either Error Doodle
getExamDoodle pool examName= do
    Exam _ doodle _ <-get examName pool
    return doodle

subscribe::ExamPool->User->String->Either Error ExamPool
subscribe pool (User userName _ _) examName = do
    Exam teacher doodle followers <- get examName pool
    if (member userName followers)
        then Left AlreadySubscribed
        else return $ add examName (Exam teacher doodle (add userName NoPreference followers)) pool

prefer::ExamPool->String->String->Slot->Either Error ExamPool
prefer pool userName examName slot = do
    Exam teacher doodle@(Doodle slots)followers <- get examName pool
    if not (member userName followers)
        then Left NotSubscribed 
        else if (Data.List.elem slot slots)
            then return $ add examName (Exam teacher doodle (add userName slot followers)) pool
            else Left NoSuchSlot

emptyExamPool::ExamPool
emptyExamPool = Pool Data.Map.empty

emptyFollowerPool::FollowerPool
emptyFollowerPool = Pool Data.Map.empty
-----------------------------------------------------------------------

increase::[(Slot, Int)]->Slot->[(Slot, Int)]
increase [] _ = []
increase x NoPreference = x
increase ((slot, x):rest) target
    |slot == target = (slot, x+1):rest
    |otherwise = (slot, x):(increase rest target)

slotsForExam::Exam->String->[(String, Slot, Int)]
slotsForExam (Exam _ (Doodle slots) (Pool followers)) examName = 
    map (\(x,y)->(examName, x, y))
        (foldl increase 
                    (zip slots
                        (cycle [0]))
               followers)

data Schedule = Schedule [(String, Slot)] Int deriving(Show)

instance (Eq Schedule) where
    (==) (Schedule _ cost1) (Schedule _ cost2) = cost1 == cost2 

instance (Ord Schedule) where
    (<=) (Schedule _ cost1) (Schedule _ cost2) = cost1 <= cost2 

overlapping::Slot->Slot->Bool
overlapping (Slot beginA endA)(Slot beginB endB)=
    (beginA <= beginB) && (endA > beginB) ||
    ((beginB <= beginA) && (endB > beginA))

someOverlap::Slot->[(String, Slot)]->Bool
someOverlap slot list = 
    foldl (||) False $ map (\(_, x) -> overlapping slot x) list

combine::[Schedule]->[(String, Slot, Int)]->[Schedule]
combine [] slots = do
    (examName, slot, slotVotes)<-slots
    return $ Schedule [(examName, slot)] slotVotes
combine schedules slots = do
    (examName, slot, slotVotes)<-slots
    Schedule planned scheduleVotes<-schedules
    guard $ not (someOverlap slot planned)
    return $ Schedule ((examName,slot):planned) (slotVotes+scheduleVotes)

schedulesForPool::ExamPool->[Schedule]
schedulesForPool (Pool exams) = 
    foldl collect [] $ Data.Map.assocs exams where
        collect schedules (name, exam)=
            combine schedules $ slotsForExam exam name

bestScheduleForPool::ExamPool->Either Error Schedule
bestScheduleForPool pool = 
    case schedulesForPool pool
        of []->Left NoPossibleExamSchedule
           x->return $ maximum x
----------------------fails if slot is not in list

slot1 = Slot (Time 2016 1 4 12 0)(Time 2016 1 4 14 0)
slot2 = Slot (Time 2016 1 4 14 0)(Time 2016 1 4 16 0)
slot3 = Slot (Time 2016 1 4 16 0)(Time 2016 1 4 18 0)

exam1 = Exam "Mark" (Doodle [slot1, slot2, slot3])
                    (Pool (Data.Map.fromList [("Tim",slot1)
                                             ,("Johny", slot3)
                                             ,("Gabrial", slot1)
                                             ,("Anny", slot1)
                                             ,("Hoday", slot3)]))
exam2 = Exam "Mark" (Doodle [slot1, slot2, slot3])
                    (Pool (Data.Map.fromList [("Tim",slot1)
                                             ,("Johny", slot2)
                                             ,("Gabrial", NoPreference)
                                             ,("Anny", slot1)
                                              ,("Hoday", NoPreference)]))
exam3 = Exam "Mark" (Doodle [slot1, slot2, slot3])
                    (Pool (Data.Map.fromList [("Tim",slot1)
                                             ,("Anny", slot1)
                                             ,("Hoday", NoPreference)]))
testExamPool = Pool (Data.Map.fromList [("Math", exam1)
                                       ,("English", exam2)
                                       ,("Dutch", exam3)])
