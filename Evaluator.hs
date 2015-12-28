--data constructors need to be explicitly shared and imported
import RequestParser (Login(Login), Request(..))
import BasicTypes(Time(Time), Slot(Slot, NoPreference), Doodle(Doodle), UserRank(..))

import qualified Pool 
import Error

import System.Random  
import System.IO

adminPassword :: String
adminPassword = "123"

--student pool, teacher pool, exam pool
data ServerData = ServerData Pool.UserPool Pool.ExamPool deriving (Show)

eval::ServerData ->Request->IO(ServerData)
eval input (AddTeacher login name) = do
    password <- makePassword --makes password even when login fails (optimize if possible)
    handleAddUser password input $ addUser input login name password Teacher 
    
eval input (AddStudent login name) = do
    password <- makePassword --makes password even when login fails (optimize if possible)
    handleAddUser password input $ addUser input login name password Student 

eval input (ChangePassword login newpass)=
    handleChangePassword input $ changePassword input login newpass

eval input (SetDoodle login examName doodle)=
   handleSetDoodle input $ setDoodle input login examName doodle

eval input (GetDoodle login examName)=
   handleGetDoodle input $ getDoodle input login examName

eval input (Subscribe login examName)=
    handleSubscribe input $ subscribe input login examName

eval input (Prefer login examName slot)=
    handlePrefer input $ prefer input login examName slot

--------------------adding users
-- add some user to the pool of users
addUser::ServerData->Login->String->String->UserRank->Either Error ServerData
addUser (ServerData userPool examPool) login userName password rank= do
    admin login
    updatedUserPool <- Pool.addUser userPool userName password rank
    return $ ServerData updatedUserPool examPool

handleAddUser :: String->ServerData->Either Error ServerData->IO(ServerData)
handleAddUser _ input (Left error) = do
    System.IO.putStrLn $ show error
    return input
handleAddUser password _ (Right output) = do
    System.IO.putStr "ok "
    System.IO.putStrLn password
    return output

-----------------------------changing password
changePassword::ServerData->Login->String->Either Error ServerData
changePassword (ServerData userPool examPool) login newPassword = do
    Pool.User name password teacher<-checkUser userPool login
    return $ ServerData (Pool.changePassword userPool name newPassword teacher) examPool

handleChangePassword :: ServerData->Either Error ServerData->IO(ServerData)
handleChangePassword input (Left error) = do
    System.IO.putStrLn $ show error
    return input
handleChangePassword _ (Right output) = do
    System.IO.putStrLn "ok"
    return output

-----------------------------setting and getting doodles

setDoodle::ServerData->Login->String->Doodle->Either Error ServerData
setDoodle (ServerData userPool examPool) login examName doodle = do
    Pool.User teacherName _ teacher<-checkSpecificUser userPool login Teacher
    updatedExamPool <- Pool.addExam examPool examName teacherName doodle
    return $ ServerData userPool updatedExamPool

handleSetDoodle::ServerData->Either Error ServerData->IO(ServerData)
handleSetDoodle input (Left error) = do
    System.IO.putStrLn $ show error
    return input
handleSetDoodle _ (Right output) = do
    System.IO.putStrLn "ok"
    return output

getDoodle::ServerData->Login->String->Either Error Doodle
getDoodle (ServerData userPool examPool) login examName =
    checkUser userPool login
    >> Pool.getExamDoodle examPool examName

handleGetDoodle::ServerData->Either Error Doodle -> IO(ServerData)
handleGetDoodle input (Left error) = do
    System.IO.putStrLn $ show error
    return input
handleGetDoodle input (Right doodle) = do
    System.IO.putStr "ok"
    System.IO.putStrLn $ show doodle
    return input

--------------------------- subscribe and prefere
subscribe::ServerData->Login->String->Either Error ServerData
subscribe (ServerData userPool examPool) login examName = do
    user<-checkSpecificUser userPool login Student
    updatedExamPool<-Pool.subscribe examPool user examName
    return $ ServerData userPool updatedExamPool

handleSubscribe::ServerData->Either Error ServerData -> IO(ServerData)
handleSubscribe input (Left error) = do
    System.IO.putStrLn $ show error
    return input
handleSubscribe _ (Right output) = do
    System.IO.putStrLn "ok"
    return output

prefer::ServerData->Login->String->Slot->Either Error ServerData
prefer (ServerData userPool examPool) login examName slot = do
    Pool.User userName _ _<-checkSpecificUser userPool login Student
    updatedExamPool<-Pool.prefer examPool userName examName slot
    return $ ServerData userPool updatedExamPool

handlePrefer::ServerData->Either Error ServerData -> IO(ServerData)
handlePrefer input (Left error) = do
    System.IO.putStrLn $ show error
    return input
handlePrefer _ (Right output) = do
    System.IO.putStrLn "ok"
    return output



---------------------------supporting functions
admin::Login->Either Error ()
admin (Login name password) =
    if name =="admin"&&password==adminPassword
        then Right ()
        else Left WrongLogin

--if login succeeds return User
--if login fails returns nothing
checkUser::Pool.UserPool -> Login -> Either Error Pool.User
checkUser pool (Login name password) = Pool.login pool name password

checkSpecificUser::Pool.UserPool -> Login -> UserRank-> Either Error Pool.User
checkSpecificUser pool login rank = do
    user@(Pool.User name password foundRank) <- checkUser pool login
    if (foundRank == rank)
        then return user
        else Left WrongLogin


makePassword = do      
    gen <- newStdGen      
    return $ take 5 (randomRs ('a','z') gen) 

--test code--
{-
login (Pool.addUser Pool.emptyUserPool "Tim" "pswr")(Login "Tim" "pswr")
eval (ServerData Pool.emptyUserPool emptyExamPool) (AddTeacher (Login "admin" "123") "walter")
eval (ServerData advancedUserPool Pool.emptyExamPool) (AddStudent (Login "admin" "123") "Tim")
eval (ServerData advancedUserPool Pool.emptyExamPool) (ChangePassword (Login "Tim" "luck") "timmy123")
eval (ServerData advancedUserPool emptyExamPool) (SetDoodle (Login "Tim" "luck") "Math" (Doodle [Slot (Time 2016 1 4 15 0)(Time 2016 1 4 17 0), Slot (Time 2016 1 4 14 0)(Time 2016 1 4 16 0)]))
eval (ServerData advancedUserPool advancedExamPool) (SetDoodle (Login "John" "pswr") "Math" (Doodle [Slot (Time 2016 1 4 15 0)(Time 2016 1 4 17 0), Slot (Time 2016 1 4 14 0)(Time 2016 1 4 16 0)]))
eval (ServerData advancedUserPool advancedExamPool) (GetDoodle (Login "John" "pswr") "Math")
eval (ServerData advancedUserPool advancedExamPool) (Subscribe (Login "Anny" "pswr") "German")
eval (ServerData advancedUserPool expertExamPool) (Prefer (Login "Anny" "pswr") "German" (Slot (Time 2016 1 4 15 0)(Time 2016 1 4 17 0)))
-}

advancedUserPool = getPool (Pool.addUser (getPool (Pool.addUser (getPool (Pool.addUser Pool.emptyUserPool "Tim" "luck" Teacher)) "John" "pswr" Teacher)) "Anny" "pswr" Student)
advancedExamPool = getPool (Pool.addExam (getPool (Pool.addExam Pool.emptyExamPool "Math" "Tim" (Doodle []))) "German" "Tim" (Doodle [Slot (Time 2016 1 4 15 0)(Time 2016 1 4 17 0), Slot (Time 2016 1 4 14 0)(Time 2016 1 4 16 0)]))
expertExamPool = getPool(Pool.subscribe advancedExamPool (Pool.User "Anny" "pswr" Student) "German")
getPool::Either Error (Pool.Pool x y) -> Pool.Pool x y
getPool (Right pool) = pool

