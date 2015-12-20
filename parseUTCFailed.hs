import Parser
import Data.Time
import Data.Char

data Slot = Slot UTCTime UTCTime deriving(Show)

data Doodle = Doodle [Slot] deriving(Show)

zone :: Parser.Parser String
zone = positiveZone `Parser.orelse` negativeZone `Parser.orelse` noZone

positiveZone :: Parser.Parser String
positiveZone=do
    Parser.char('+')
    hour<-integer
    Parser.char(':')
    minutes<-integer
    return $ '+': (show hour) ++":"++(show minutes)

negativeZone :: Parser.Parser String
negativeZone=do
    Parser.char('-')
    hour<-integer
    Parser.char(':')
    minutes<-integer
    return $ '-': (show hour) ++":"++(show minutes)

--eat nothing and return the nutral zone
noZone::Parser.Parser String
noZone = return $ ""

day::Parser.Parser String
day=do
    year<-integer
    Parser.char('-')
    month<-integer
    Parser.char('-')
    day<-integer
    Parser.char('T')
    return $ (show year) ++ "-" ++ (show month) ++ "-" ++ (show day)

hour::Parser.Parser String
hour=do
    hour<-integer
    Parser.char(':')
    minutes<-integer
    timeZone<-zone
    return $ (show hour) ++ ":" ++ (show minutes) ++ timeZone

time :: Parser.Parser UTCTime
time = do 
    Parser.blank
    day<-day
    hour<-hour
    return $ read (day++" "++hour)