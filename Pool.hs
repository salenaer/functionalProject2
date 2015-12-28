module Pool (UserPool, ExamPool, FollowerPool, Pool(Pool), Exam(Exam), User(User), addUser, changePassword, login, 
    emptyUserPool, addExam, getExamDoodle, subscribe, prefer, emptyExamPool) where
{-
module Pool (UserPool, ExamPool, FollowerPool, Exam, User, addUser, changePassword, login, 
    emptyUserPool, addExam, getExamDoodle, emptyExamPool) where
-}
import BasicTypes(Time(Time), Slot(Slot, NoPreference), Doodle(Doodle), UserRank)
import Error

import qualified Data.Map

data User = User String String UserRank deriving (Show, Eq, Ord)
data Exam = Exam String Doodle FollowerPool deriving (Show)
data Pool key value = Pool (Data.Map.Map key value) deriving(Show) --brings map into either monad

type UserPool = Pool String User
type ExamPool = Pool String Exam
type FollowerPool = Pool User Slot

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
        of  Left _ -> Right $ add name (Exam teacher doodle emptyFollowerPool) pool
            Right (Exam storedTeacher _ followers) -> 
                if storedTeacher == teacher
                    then return $ add name (Exam teacher doodle followers) pool
                    else Left IdTaken

getExamDoodle::ExamPool -> String -> Either Error Doodle
getExamDoodle pool examName= do
    Exam _ doodle _ <-get examName pool
    return doodle

subscribe::ExamPool->User->String->Either Error ExamPool
subscribe pool user examName = do
    Exam teacher doodle followers <- get examName pool
    if (member user followers)
        then Left AlreadySubscribed
        else return $ add examName (Exam teacher doodle (add user NoPreference followers)) pool

prefer::ExamPool->User->String->Slot->Either Error ExamPool
prefer pool user examName slot = do
    Exam teacher doodle followers <- get examName pool
    if (member user followers)
        then return $ add examName (Exam teacher doodle (add user slot followers)) pool
        else Left NotSubscribed 


emptyExamPool::ExamPool
emptyExamPool = Pool Data.Map.empty

emptyFollowerPool::FollowerPool
emptyFollowerPool = Pool Data.Map.empty