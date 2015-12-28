module RequestParser(Login(Login), Request(..)) where
import DoodleParser(slot, doodle)
import BasicTypes(Time(Time), Slot(Slot, NoPreference), Doodle(Doodle))

--module RequestParser(Time(Time), Slot(Slot), Doodle, Login(Login), Request(..)) where
--import DoodleParser(Doodle, Slot(Slot), Time(Time))

import Parser
import Data.Time
import Data.Char


data Login = Login String String deriving (Show)

data Request = AddTeacher Login String
             | AddStudent Login String
             | ChangePassword Login String
             | GetDoodle Login String
             | SetDoodle Login String Doodle
             | Subscribe Login String
             | Prefer Login String Slot
             | ExamSchedule Login
             deriving (Show)

requestParser :: Parser.Parser Request
requestParser = Parser.blank >> Parser.oneof [addTeacher, addStudent, changePassword, getDoodle, setDoodle, subscribe, prefer, examSchedule]

keywordAndLogin :: String->Parser.Parser Login
keywordAndLogin keyword=
    Parser.keyword(keyword)>>login

---------------------------Parse commands-----------------------------------------

addTeacher :: Parser.Parser Request
addTeacher = do
    admin <- keywordAndLogin("add-teacher")
    name <- Parser.token
    return $ AddTeacher admin name

addStudent :: Parser.Parser Request
addStudent = do
    admin <- keywordAndLogin("add-student")
    name <- Parser.token
    return $ AddStudent admin name

changePassword::Parser.Parser Request
changePassword = do
    user <- keywordAndLogin("change-password")
    password <- password
    return $ ChangePassword user password

getDoodle::Parser.Parser Request
getDoodle = do
    user <- keywordAndLogin("get-doodle")
    name <- Parser.token
    return $ GetDoodle user name

setDoodle::Parser.Parser Request
setDoodle = do
    teacher <- keywordAndLogin("set-doodle")
    name <- Parser.token
    newDoodle <- doodle
    return $ SetDoodle teacher name newDoodle

subscribe :: Parser.Parser Request
subscribe = do
    user <- keywordAndLogin"subscribe"
    name <- Parser.token
    return $ Subscribe user name

prefer :: Parser.Parser Request
prefer = do
    user <- keywordAndLogin"subscribe"
    name <- Parser.token
    slot <- slot
    return $ Prefer user name slot

examSchedule :: Parser.Parser Request
examSchedule = 
    keywordAndLogin"exam-schedule">>=return . ExamSchedule 

-----------------supporting parsers-----------------------------------


password :: Parser.Parser String
password = Parser.blank >>  (Parser.some $ sat isAlphaNum)

login :: Parser.Parser Login
login = do
    name <- Parser.token
    Parser.char('@')
    pass <- password
    return $ Login name pass

{-
    Parser.apply requestParser "add-teacher admin@1234 walter"
    Parser.apply requestParser "set-doodle walter@jk62k math [2016-01-04T14:00+01:00 / 2016-01-04T16:00+01:00, 2016-01-04T13:00+01:00 / 2016-01-04T15:00+01:00]"
-}
--2015-11-30T10:28+01:00/2015-11-31T10:28+01:00

