import System.Random    
makePassword = do      
	gen <- newStdGen      
	return $ take 20 (randomRs ('a','z') gen) 
