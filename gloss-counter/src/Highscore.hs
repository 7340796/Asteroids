module Highscore where 

readHighscore :: IO Int
readHighscore = do
    contents <- readFile "content/highscore.txt"
    return $ readInt contents

writeHighscore :: Int -> IO()
writeHighscore score = do
    oldScore <- readHighscore
    if score > oldScore then writeFile "content/highscore.txt" (show score)
    else return ()

readInt :: String -> Int
readInt = read 