--data constructors need to be explicitly shared and imported
import RequestParser (Login(Login), Request(..))
import BasicTypes(Time(Time), Slot(Slot, NoPreference), Doodle(Doodle))

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
    handleAddUser password input $ addUser input login name password True 
    
eval input (AddStudent login name) = do
    password <- makePassword --makes password even when login fails (optimize if possible)
    handleAddUser password input $ addUser input login name password False 

eval input (ChangePassword login newpass)=
    handleChangePassword input $ changePassword input login newpass

eval input (SetDoodle login examName doodle)=
   handleSetDoodle input $ setDoodle input login examName doodle

eval input (GetDoodle login examName)=
   handleGetDoodle input $ getDoodle input login examName

--------------------adding users
-- add some user to the pool of users
addUser::ServerData->Login->String->String->Bool->Either Error ServerData
addUser (ServerData userPool examPool) login userName password teacher= do
    admin login
    updatedUserPool <- Pool.addUser userPool userName password teacher
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
    Pool.User teacherName _ teacher<-checkSpecificUser userPool login True
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

checkSpecificUser::Pool.UserPool -> Login -> Bool-> Either Error Pool.User
checkSpecificUser pool login teacher = do
    user@(Pool.User name password rank) <- checkUser pool login
    if (rank == teacher)
        then return user
        else Left WrongLogin


makePassword = do      
    gen <- newStdGen      
    return $ take 5 (randomRs ('a','z') gen) 

--test code--
{-
login (Pool.addUser emptyUserPool "Tim" "pswr")(Login "Tim" "pswr")
eval (ServerData emptyUserPool emptyExamPool) (AddTeacher (Login "admin" "123") "walter")
eval (ServerData advanceUserPool emptyExamPool) (AddStudent (Login "admin" "123") "Tim")
eval (ServerData advanceUserPool emptyExamPool) (ChangePassword (Login "Tim" "luck") "timmy123")
eval (ServerData advanceUserPool emptyExamPool) (SetDoodle (Login "Tim" "luck") "Math" (Doodle [Slot (Time 2016 1 4 15 0)(Time 2016 1 4 17 0), Slot (Time 2016 1 4 14 0)(Time 2016 1 4 16 0)]))
eval (ServerData advanceUserPool advanceExamPool) (SetDoodle (Login "John" "pswr") "Math" (Doodle [Slot (Time 2016 1 4 15 0)(Time 2016 1 4 17 0), Slot (Time 2016 1 4 14 0)(Time 2016 1 4 16 0)]))
eval (ServerData advanceUserPool advanceExamPool) (GetDoodle (Login "John" "pswr") "Math")

-}

timPool = Pool.addUser Pool.emptyUserPool "Tim" "pswr" True
advanceUserPool = getPool (Pool.addUser (getPool (Pool.addUser (getPool (Pool.addUser Pool.emptyUserPool "Tim" "luck" True)) "John" "pswr" True)) "Anny" "pswr" False)
advanceExamPool = getPool (Pool.addExam (getPool (Pool.addExam Pool.emptyExamPool "Math" "Tim" (Doodle []))) "German" "Tim" (Doodle [Slot (Time 2016 1 4 15 0)(Time 2016 1 4 17 0), Slot (Time 2016 1 4 14 0)(Time 2016 1 4 16 0)]))

getPool::Either Error (Pool.Pool x y) -> Pool.Pool x y
getPool (Right pool) = pool

