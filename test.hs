fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

flatten :: [[a]] -> [a]
flatten []      = []
flatten (xs:[]) = xs
flatten (xs:ys) = xs ++ flatten ys

fib2 n = fibs !! n
         where fibs = 0 : scanl (+) 1 fibs

-- uncurry3 :: ((a, b, c) -> d) -> (a -> b -> c -> d)
-- uncurry3 f =

dot :: Num a => [a] -> [a] -> a
dot xs ys = foldr (+) 0 (zipWith (*) xs ys)
