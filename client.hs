import Network
import System.IO (hGetContents, hPutStrLn, hClose, Handle)

send msg = do
    h <- connectTo "localhost" $ PortNumber 5555
    hPutStrLn h msg
    response <- hGetContents h
    putStrLn $ "SERVER RESPONSE: " ++ response
    hClose h

setup = do
    send "add-teacher admin@123 olga"
    send "add-teacher admin@123 wolf"
    send "add-student admin@123 janne"
    send "add-student admin@123 tim"
    send "add-student admin@123 anny"
    send "add-student admin@123 john"

{-
send "change-password olga@RANDOM teacher"
send "change-password wolf@RANDOM teacher"
send "change-password tim@RANDOM student"
send "change-password anny@RANDOM student"
send "change-password john@RANDOM student"
send "set-doodle olga@teacher Cooking [2016-01-04T14:00+01:00 / 2016-01-04T16:00+01:00,2016-01-04T13:00+01:00 / 2016-01-04T15:00+01:00]"
send "set-doodle wolf@teacher Cooking []" =>id taken
send "subscribe tim@student Cooking"
send "prefer tim@student Cooking 2016-01-04T13:00+01:00 / 2016-01-04T15:00+01:00"

     -}
