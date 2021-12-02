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
    let (Right commands) = parse parser "" inputFile
    print "Part One:"
    print (computeAnswer commands)

{- Compute answer -}
computeAnswer :: [Command] -> Integer
computeAnswer commands = uncurry (*) (computeMovement commands)

{- Compute movement -}
computeMovement :: [Command] -> Position
computeMovement commands = foldr addVectors (0, 0) (commandsToVectors commands)

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
    (do commandstring <- many1 letter
        space
        getCommand commandstring <$> number) `sepEndBy` newline

number :: Parser Integer
number = read <$> many1 digit

getCommand :: String -> Integer  -> Command
getCommand "up" x    = Up x
getCommand "forward" x = Forward x
getCommand "down" x    = Down x
getCommand s _         = error $ "Invalid input : " ++ s
