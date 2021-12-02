module Main where

import Text.Parsec
    ( digit, letter, newline, space, many1, sepEndBy, parse, Parsec )
import Text.Parsec.String

type Position   = (Integer, Integer)
type Vector     = (Integer, Integer)

data Command    = Forward Integer | Down Integer | Up Integer deriving (Eq)

main :: IO ()
main = do
    inputFile <- readFile "input"
    let (Right directions) = parse parser "" inputFile
    print "Part One:"
    print (computeAnswer directions)

{- Compute answer -}
computeAnswer :: [Command] -> Integer
computeAnswer directions = uncurry (*) (computeMovement directions)

{- Compute movement -}
computeMovement :: [Command] -> Position
computeMovement directions = foldr addVectors (0, 0) (commandsToVectors directions)

{- Commands to positions -}
commandsToVectors :: [Command] -> [Vector]
commandsToVectors = map commandToVector

{- Command to vector -}
commandToVector :: Command -> Position
commandToVector (Forward value) = (value, 0)
commandToVector (Up value) = (0, -value)
commandToVector (Down value) = (0, value)

{- Add two vectors -}
addVectors :: Position -> Position -> Position
addVectors (h1, d1) (h2, d2) = (h1 + h2, d1 + d2)

{- Parse input -}
parser :: Parsec String () [Command]
parser = do
    (do directionString <- many1 letter
        space
        getDirection directionString <$> number) `sepEndBy` newline

number :: Parser Integer
number = read <$> many1 digit

getDirection :: String -> Integer  -> Command
getDirection "up" x    = Up x
getDirection "forward" x = Forward x
getDirection "down" x    = Down x
getDirection s _         = error $ "Invalid input : " ++ s
