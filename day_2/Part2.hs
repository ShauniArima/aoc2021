module Main where

import Text.Parsec
    ( digit, letter, newline, space, many1, sepEndBy, parse, Parsec )
import Text.Parsec.String

type Position       = (Integer, Integer)
type SubmarineState = (Integer, Integer, Integer)

main :: IO ()
main = do
    inputFile <- readFile "input"
    let (Right states) = parse parser "" inputFile
    print "Part Two:"
    print (computeAnswer states)

{- Compute answer -}
computeAnswer :: [SubmarineState] -> Integer
computeAnswer = uncurry (*) . stateToPosition . foldl computeState (0, 0, 0)

{- Convert state to position -}
stateToPosition :: SubmarineState -> Position
stateToPosition (h, d, _) = (h, d)

{- Apply the desired operations on two states -}
computeState :: SubmarineState -> SubmarineState -> SubmarineState
computeState (sourceH, sourceDepth, sourceAim) (hToAdd, depthToApply, aimToAdd) = (sourceH + hToAdd, sourceDepth + (depthToApply * sourceAim), sourceAim + aimToAdd)

{- Parse input -}
parser :: Parsec String () [SubmarineState]
parser = do
    (do directionString <- many1 letter
        space
        getState directionString <$> number) `sepEndBy` newline

number :: Parser Integer
number = read <$> many1 digit

getState :: String -> Integer  -> SubmarineState
getState "up" x    = (0, 0, -x)
getState "down" x    = (0, 0, x)
getState "forward" x = (x, x, 0)
getState s _         = error $ "Invalid input : " ++ s
