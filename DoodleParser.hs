{- parses doodle slot and time strings
-}

module DoodleParser(slot, doodle) where

import BasicTypes(Time(Time), Slot(Slot, NoPreference), Doodle(Doodle))

import Parser
import Data.Time
import Data.Char

data Zone = PositiveZone Int Int
          | NegativeZone Int Int
          | NoZone
           deriving(Show)

zone :: Parser.Parser Zone
zone = positiveZone `Parser.orelse` negativeZone

positiveZone :: Parser.Parser Zone
positiveZone=do
    Parser.char('+')
    hour<-integer
    Parser.char(':')
    minutes<-integer
    return $ PositiveZone hour minutes

negativeZone :: Parser.Parser Zone
negativeZone=do
    Parser.char('-')
    hour<-integer
    Parser.char(':')
    minutes<-integer
    return $ NegativeZone hour minutes

noZone :: Parser.Parser Zone
noZone = return NoZone

dayTime::Parser.Parser (Int, Int) 
dayTime = do
    hour<-integer
    Parser.char(':')
    minutes<-integer
    myZone<-zone
    return $ incorporateTimeZone hour minutes myZone

incorporateTimeZone::Int->Int->Zone->(Int, Int)
incorporateTimeZone hour minutes NoZone = (hour, minutes)
incorporateTimeZone hour minutes (PositiveZone incHour incMinutes)=
    (hour+incHour, minutes+incMinutes)
incorporateTimeZone hour minutes (NegativeZone decHour decMinutes)=
    (hour-decHour, minutes-decMinutes)

time :: Parser.Parser Time
time = do 
    Parser.blank
    year<-integer
    Parser.char('-')
    month<-integer
    Parser.char('-')
    day<-integer
    Parser.char('T')
    (hour, minutes)<-dayTime
    return $ Time year month day hour minutes

slot :: Parser.Parser Slot
slot = do
    start <- time
    Parser.blank
    Parser.char('/')
    end <- time
    return $ Slot start end

slots :: Parser.Parser [Slot]
slots = 
    multiSlots `Parser.orelse` oneSlot

oneSlot :: Parser.Parser [Slot]
oneSlot = do
    slot <- slot
    return [slot]

multiSlots :: Parser.Parser [Slot]
multiSlots = do 
    slot<-slot
    Parser.blank
    Parser.char(',')
    slots<-slots
    return $ slot:slots

doodle :: Parser.Parser Doodle
doodle = do
    Parser.blank
    Parser.char('[')
    slots <- slots
    Parser.char(']')
    return $ Doodle slots

--Parser.apply time "2015-11-30T10:28+01:00"
--Parser.apply doodle "[2016-01-04T14:00+01:00 / 2016-01-04T16:00+01:00, 2016-01-04T13:00+01:00 / 2016-01-04T15:00+01:00]"