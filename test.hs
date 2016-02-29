mult3 a b c = a*b*c

coordDist :: (Floating a) => (a, a) -> (a, a) -> a
coordDist (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

heronArea :: (Floating a) => a -> a -> a -> a
heronArea a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where s = 0.5*(a+b+c)

serialProbability :: (Floating a) => [a] -> [a]
serialProbability probs = [head probs,
    (1-(probs !! 0))*(probs !! 1),
    (1-(probs !! 0))*(1-(probs !! 1))*(probs !! 2),
    (1-(probs !! 0))*(1-(probs !! 1))*(1-(probs !! 2))]

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

flatten :: [[a]] -> [a]
flatten []      = []
flatten (xs:[]) = xs
flatten (xs:ys) = xs ++ flatten ys

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

dot :: Num a => [a] -> [a] -> a
dot xs ys = foldr (+) 0 (zipWith (*) xs ys)

sublists :: [a] -> [[a]]
sublists []       = []
sublists x@(_:xs) = x : sublists xs

main = do
    putStrLn "Hello world! Gimme three numbers!"

    putStr "x: "
    xStr <- getLine
    let x = read xStr :: Int
    putStr "y: "
    yStr <- getLine
    let y = read yStr :: Int
    putStr "z: "
    zStr <- getLine
    let z = read zStr :: Int

    putStrLn ("x * y * z = " ++ show (mult3 x y z))
