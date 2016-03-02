module Golf where

skips :: [a] -> [[a]]
skips base = map ((flip everyNth) base) [1..(length base)]

everyNth :: Int -> [a] -> [a]
everyNth n xs =
  if (length xs) < n
    then []
    else (head $ drop (n - 1) xs) : everyNth n (drop n xs)
