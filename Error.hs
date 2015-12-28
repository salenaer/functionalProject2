module Error (Error(..)) where

data Error = IdTaken
           | WrongLogin
           | NoSuchId
           | NoSuchSlot
           | NotSubscribed
           | AlreadySubscribed
           | NoPossibleExamSchedule
           deriving (Show)