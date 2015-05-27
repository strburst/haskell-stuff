splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'

-- seq1 0 = 10
seq1 x = 10 - 3*x

seq2 1 = 1
seq2 2 = 5
seq2 x = seq2 (x - 1) + seq2 (x - 2)

seq3 0 = -1
seq3 x = -2 * seq3 (x - 1)

seq4 0 = -1
seq4 1 = 0
seq4 x = x * (seq4 (x - 1)) + (seq4 (x - 2)) ^ 2

seq5 0 = 1000
seq5 x = seq5 (x - 1) * 1.09

compound :: Double -> Double -> Integer -> Double
compound principal rate time = principal * (rate + 1) ^ time
