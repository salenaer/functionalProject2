module Error (Error(..)) where

data Error = IdTaken
           | WrongLogin
           | NoSuchId
           | NoSuchSlot
           | NotSubscribed
           | AlreadySubscribed
           | NoPossibleExamSchedule

instance Show Error where
    show IdTaken = "id-taken"
    show WrongLogin = "wrong-login"
    show NoSuchId = "no-such-id"
    show NoSuchSlot = "no-such-slot"
    show NotSubscribed = "not-subscribed"
    show NoPossibleExamSchedule = "no-possible-exam-schedule"
    show AlreadySubscribed = "already-subscribed" 