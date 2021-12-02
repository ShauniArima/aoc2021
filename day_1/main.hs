module Main where

countIncreasedValues :: [Integer] -> Integer
countIncreasedValues values = sum [ if x < y then 1 else 0 | (x, y) <- zip values (tail values) ]

slidingNums :: [Integer] -> [Integer]
slidingNums values = zipWith3 (\a b c -> a + b + c) values (tail values) (tail (tail values))

main = do
    inputFile <- readFile "input"
    let input = read <$> lines inputFile
    print "Part One:"
    print (countIncreasedValues input)
    print "Part Two:"
    print (countIncreasedValues (slidingNums input))