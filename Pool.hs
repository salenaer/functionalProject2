module Pool (UserPool, ExamPool, FollowerPool, Pool(Pool), Exam(Exam), User(User), addUser, changePassword, login, 
    emptyUserPool, addExam, getExamDoodle, emptyExamPool) where
{-
module Pool (UserPool, ExamPool, FollowerPool, Exam, User, addUser, changePassword, login, 
    emptyUserPool, addExam, getExamDoodle, emptyExamPool) where
-}
import BasicTypes(Time(Time), Slot(Slot, NoPreference), Doodle(Doodle))
import Error

import qualified Data.Map

data User = User String String Bool deriving (Show, Eq, Ord)
data Exam = Exam String Doodle FollowerPool deriving (Show)
data Pool key value = Pool (Data.Map.Map key value) deriving(Show) --brings map into either monad

type UserPool = Pool String User
type ExamPool = Pool String Exam
type FollowerPool = Pool User Slot

get::(Ord key)=>key->Pool key value->Either Error value
get id (Pool pool) = 
    iter $ Data.Map.lookup id pool where 
        iter Nothing = Left NoSuchId
        iter (Just value) = Right value 

member::(Ord key)=>key ->Pool key value->Bool
member id (Pool pool) = Data.Map.member id pool

add::(Ord key)=>key->value->Pool key value->Pool key value
add id x (Pool pool) = Pool $ Data.Map.insert id x pool

--------------------------UserPool---------------------------------

addUser::UserPool->String->String->Bool->Either Error UserPool
addUser pool name password teacher=
    if (member name pool)
        then Left IdTaken
        else Right $ add name (User name password teacher) pool

changePassword::UserPool->String->String->Bool->UserPool
changePassword pool name password teacher=add name (User name password teacher) pool
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
    iter $ get name pool where 
        iter :: Either Error Exam -> Either Error ExamPool
        iter (Left _) = Right $ add name (Exam teacher doodle emptyFollowerPool) pool
        iter (Right (Exam storedTeacher _ followers)) = 
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
{-
prefere::ExamPool->User->String->Slot->Either Error ExamPool
prefere pool user examName slot = do
    Exam teacher doodle followers <- get pool examName
-}

emptyExamPool::ExamPool
emptyExamPool = Pool Data.Map.empty

emptyFollowerPool::FollowerPool
emptyFollowerPool = Pool Data.Map.empty