mult3 a b c = a*b*c
--heronArea :: (Int a, Int b, Int c) => Int -> Int -> Double
heronArea a b c = sqrt (s*(s-a)*(s-b)*(s-c))
	where s = 0.5*(a+b+c)

coordDist (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

absProbs probs = [head probs,
	(1-(probs !! 0))*(probs !! 1),
	(1-(probs !! 0))*(1-(probs !! 1))*(probs !! 2),
	(1-(probs !! 0))*(1-(probs !! 1))*(1-(probs !! 2))]
{-
absProbs0 probs = [probs !! 0, absProbs1 (drop 1 probs)]

absProbs1 ans probs = ans ++ [(1-(probs !! 0))]-}
{-
main = do
	putStrLn "Hello world! Gimme three numbers!"
	
	putStrLn "x: "
	xStr <- getLine
	let x = read xStr :: Int
	putStrLn "y: "
	yStr <- getLine
	let y = read yStr :: Int
	putStrLn "z: "
	zStr <- getLine
	let z = read zStr :: Int
	
	putStrLn ("x*y*z = " ++ show (mult3 x y z))
-}
