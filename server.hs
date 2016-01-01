import Pool
import RequestParser
import Parser
import Evaluator(eval, evalWithPassword, ServerData(ServerData))

import Network
import System.Environment (getArgs)
import System.IO (hGetLine, hPutStrLn, hClose, hSetBuffering, Handle, BufferMode(NoBuffering))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad 
import System.Random 

--Network.TCP is meant for intern usage

main::IO()
main = withSocketsDo $ do
    args <- getArgs
    let adminPassword = head args
    --newTVarIO is IO variant of newTVar 
    serverData <- newTVarIO $ ServerData emptyUserPool emptyExamPool adminPassword
    socket <- listenOn $ PortNumber 5555
    loop socket serverData

loop :: Socket -> TVar(ServerData)->IO ()
loop socket serverData = do
    (handle, _, _) <- accept socket
    hSetBuffering handle NoBuffering
    forkIO $ inputProcessor handle serverData
    loop socket serverData

inputProcessor::Handle->TVar(ServerData)->IO()
inputProcessor handle serverData = do
    line <- hGetLine handle
    let input = Parser.apply requestParser line
    case input of [(string, "")] -> handleCorrectParse handle string serverData
                  otherwise -> hPutStrLn handle "parse failed" 
    hClose handle

handleCorrectParse::Handle->Request->TVar(ServerData)->IO()
handleCorrectParse handle request@(AddTeacher login name) serverData =
    handleAddUser handle request serverData
handleCorrectParse handle request@(AddStudent login name) serverData =
    handleAddUser handle request serverData
handleCorrectParse handle request tVarServerData = do
    string<-atomically $ do 
        serverData <- readTVar tVarServerData
        let (bool, output, newServerData) = eval serverData request
        when (bool) $ writeTVar tVarServerData newServerData
        return output
    hPutStrLn handle string

handleAddUser::Handle->Request->TVar(ServerData)->IO()
handleAddUser handle request tVarServerData  = do
    password <- makePassword --makes password even when login fails (optimize if possible)
    string<-atomically $ do 
        serverData <- readTVar tVarServerData
        let (bool, output, newServerData) = evalWithPassword serverData request password
        when (bool) $ writeTVar tVarServerData newServerData
        return output
    hPutStrLn handle string

makePassword::IO(String)
makePassword = do      
    gen <- newStdGen      
    return $ take 5 (randomRs ('a','z') gen) 


--to start up server type in cmd: runhaskell server.hs "123"