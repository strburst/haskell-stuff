-- This experiment generates LaTex code representing the Stirling number of the
-- second kind, which is the number of surjections from m things to k things
main = do
    input <- getLine
    let m = read (words input !! 0)
        k = read (words input !! 1)
    putStrLn $ surjectionLatex m k

surjectionLatex :: Integer -> Integer -> String
surjectionLatex m k = if m < k
    then error $ "No surjections from " ++ show m ++ " things to " ++ show k
        ++ " things by the Pigeonhole Principle"
    else drop 2 $ unwords $ map (surjectionTerm m k) [0..k]

surjectionTerm :: Integer -> Integer -> Integer -> String
surjectionTerm m k i = if i `mod` 2 == 0
    then "+ " ++ term
    else "- " ++ term
        where term = (k `choose` i) ++ "(" ++ show k ++ " - " ++ show i ++ ")^{" ++ show m ++ "}"

choose :: Integer -> Integer -> String
choose n k = "{" ++ show n ++ " \\choose " ++ show k ++ "}"
