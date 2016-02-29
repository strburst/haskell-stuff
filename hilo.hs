import System.Random

main = do
    gen <- getStdGen
    let num = take 10 $ randomRs (1, 5) gen :: [Int]
    putStrLn $ show num
