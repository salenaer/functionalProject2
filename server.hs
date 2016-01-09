{- 
This file the hart of the application. 
This file runs all other functions and handles the IO part. 
This file is responsible for tracking the values of the TVar. 
-}


import Pool
import RequestParser
import Parser
import Evaluator(eval, evalWithPassword, ServerData(ServerData))

--Network.TCP is meant for intern usage, as such I use the network package instead
import Network 
import System.Environment (getArgs)
import System.IO (hGetLine, hPutStrLn, hClose, hSetBuffering, Handle, BufferMode(NoBuffering))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad 
import System.Random 

--starts the server and creates the TVar.
--to start up server type in cmd: runhaskell server.hs adminpassword 
--the password can be anything, to run the commands as writen in client.hs use 123
main::IO()
main = withSocketsDo $ do
    args <- getArgs
    let adminPassword = head args
    --newTVarIO is IO variant of newTVar 
    serverData <- newTVarIO $ ServerData emptyUserPool emptyExamPool adminPassword
    socket <- listenOn $ PortNumber 5555
    loop socket serverData

--accept connection, asynchronously handle connection. This loops forever
loop :: Socket -> TVar(ServerData)->IO ()
loop socket serverData = do
    (handle, _, _) <- accept socket
    hSetBuffering handle NoBuffering
    forkIO $ inputProcessor handle serverData
    loop socket serverData

--parse input
--if exacly one expression is returned and the whole string is eaten then handle the expression. 
inputProcessor::Handle->TVar(ServerData)->IO()
inputProcessor handle serverData = do
    line <- hGetLine handle
    let input = Parser.apply requestParser line
    case input of [(string, "")] -> handleCorrectParse handle string serverData
                  otherwise -> hPutStrLn handle "wrong command" 
    hClose handle

--handle the expressions, when adding user we need to generate a new password. 
--This is an IO action so we do it in this layer. We don't want to generate a password for every command
--so we split expressions in the once that need a password (addStudent and addTeacher and the onces that don't)
handleCorrectParse::Handle->Request->TVar(ServerData)->IO()
handleCorrectParse handle request@(AddTeacher login name) serverData =
    handleAddUser handle request serverData
handleCorrectParse handle request@(AddStudent login name) serverData =
    handleAddUser handle request serverData
handleCorrectParse handle request tVarServerData = do
    string<-atomically $ do 
        serverData <- readTVar tVarServerData
        let (writeNeeded, output, newServerData) = eval serverData request
        when (writeNeeded) $ writeTVar tVarServerData newServerData
        return output
    hPutStrLn handle string

handleAddUser::Handle->Request->TVar(ServerData)->IO()
handleAddUser handle request tVarServerData  = do
    password <- makePassword --makes password even when login fails (optimize if possible)
    string<-atomically $ do 
        serverData <- readTVar tVarServerData
        let (writeNeeded, output, newServerData) = evalWithPassword serverData request password
        when (writeNeeded) $ writeTVar tVarServerData newServerData
        return output
    hPutStrLn handle string

makePassword::IO(String)
makePassword = do      
    gen <- newStdGen      
    return $ take 5 (randomRs ('a','z') gen) 


--to start up server type in cmd: runhaskell server.hs "123"