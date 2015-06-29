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
