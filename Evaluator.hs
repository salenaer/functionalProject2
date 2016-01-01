module Evaluator (eval, evalWithPassword, ServerData(ServerData)) where

--data constructors need to be explicitly shared and imported
import RequestParser (Login(Login), Request(..))
import BasicTypes(Slot, Doodle, UserRank(..), Schedule)

import qualified Pool 
import Error
 
import System.IO

--student pool, teacher pool, exam pool
data ServerData = ServerData Pool.UserPool Pool.ExamPool String

--boolean indicates if the data has been changed, it is has an writeTvar needs to be done
--only writeTvarChecks if some other write has done in past all reads can happen concurrently
evalWithPassword::ServerData->Request->String->(Bool, String, ServerData)
evalWithPassword input (AddTeacher login name) password= do
    handleAddUser password input $ addUser input login name password Teacher 
    
evalWithPassword input (AddStudent login name) password= do
    handleAddUser password input $ addUser input login name password Student 

eval::ServerData->Request->(Bool, String, ServerData)
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

eval input (ExamSchedule login)=
    handleExamSchedule input $ examSchedule input login

--------------------adding users
-- add some user to the pool of users
addUser::ServerData->Login->String->String->UserRank->Either Error ServerData
addUser (ServerData userPool examPool adminPassword) login userName password rank= do
    admin login adminPassword
    updatedUserPool <- Pool.addUser userPool userName password rank
    return $ ServerData updatedUserPool examPool adminPassword

handleAddUser :: String->ServerData->Either Error ServerData->(Bool, String, ServerData)
handleAddUser _ input (Left error) = (False, show error, input)
handleAddUser password _ (Right output) = (True, "ok "++ password, output)

-----------------------------changing password
changePassword::ServerData->Login->String->Either Error ServerData
changePassword (ServerData userPool examPool adminPassword) login newPassword = do
    Pool.User name password teacher<-checkUser userPool login
    return $ ServerData (Pool.changePassword userPool name newPassword teacher) examPool adminPassword

handleChangePassword :: ServerData->Either Error ServerData->(Bool, String, ServerData)
handleChangePassword input (Left error) = (False, show error, input)
handleChangePassword _ (Right output) = (True, "ok", output)

-----------------------------setting and getting doodles

setDoodle::ServerData->Login->String->Doodle->Either Error ServerData
setDoodle (ServerData userPool examPool adminPassword) login examName doodle = do
    Pool.User teacherName _ teacher<-checkSpecificUser userPool login Teacher
    updatedExamPool <- Pool.addExam examPool examName teacherName doodle
    return $ ServerData userPool updatedExamPool adminPassword

handleSetDoodle::ServerData->Either Error ServerData->(Bool, String, ServerData)
handleSetDoodle input (Left error) = (False, show error, input)
handleSetDoodle _ (Right output) = (True, "ok", output)


getDoodle::ServerData->Login->String->Either Error Doodle
getDoodle (ServerData userPool examPool _) login examName =
    checkUser userPool login
    >> Pool.getExamDoodle examPool examName

handleGetDoodle::ServerData->Either Error Doodle -> (Bool, String, ServerData)
handleGetDoodle input (Left error) = (False, show error, input)
handleGetDoodle input (Right doodle) = (False, "ok "++ show doodle, input)

--------------------------- subscribe ,prefere and examSchedule
subscribe::ServerData->Login->String->Either Error ServerData
subscribe (ServerData userPool examPool adminPassword) login examName = do
    user<-checkSpecificUser userPool login Student
    updatedExamPool<-Pool.subscribe examPool user examName
    return $ ServerData userPool updatedExamPool adminPassword

handleSubscribe::ServerData->Either Error ServerData -> (Bool, String, ServerData)
handleSubscribe input (Left error) = (False, show error, input)
handleSubscribe _ (Right output) = (True, "ok", output)


prefer::ServerData->Login->String->Slot->Either Error ServerData
prefer (ServerData userPool examPool adminPassword) login examName slot = do
    Pool.User userName _ _<-checkSpecificUser userPool login Student
    updatedExamPool<-Pool.prefer examPool userName examName slot
    return $ ServerData userPool updatedExamPool adminPassword

handlePrefer::ServerData->Either Error ServerData -> (Bool, String, ServerData)
handlePrefer input (Left error) = (False, show error, input)
handlePrefer _ (Right output) = (True, "ok", output)

examSchedule::ServerData->Login->Either Error Schedule
examSchedule (ServerData userPool examPool adminPassword) login =
    checkUser userPool login
    >> Pool.examSchedule examPool

handleExamSchedule::ServerData->Either Error Schedule -> (Bool, String, ServerData)
handleExamSchedule input (Left error) = (False, show error, input)
handleExamSchedule input (Right schedule) = (True, "ok "++ show schedule, input)
---------------------------supporting functions
admin::Login->String->Either Error ()
admin (Login name password) adminPassword =
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

