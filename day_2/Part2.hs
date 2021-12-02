module Main where

import Text.Parsec
    ( digit, letter, newline, space, many1, sepEndBy, parse, Parsec )
import Text.Parsec.String

type Position       = (Integer, Integer)
type SubmarimeState = (Integer, Integer, Integer)

data Command =    Forward Integer
                    | Down Integer
                    | Up Integer deriving (Eq)

main :: IO ()
main = do
    inputFile <- readFile "input"
    let (Right commands) = parse parser "" inputFile
    print "Part Two:"
    print (computeAnswer commands)

{- Compute final position -}
computeAnswer :: [Command] -> Integer
computeAnswer commands = uncurry (*) (computeFinalPosition commands)

computeFinalPosition :: [Command] -> Position
computeFinalPosition commands = (stateToPosition . foldl computeState (0, 0, 0)) (commandsToStates commands)


stateToPosition :: SubmarimeState -> Position
stateToPosition (h, d, _) = (h, d) 

computeState :: SubmarimeState -> SubmarimeState -> SubmarimeState
computeState (h1, d1, aim1) (h2, d2, aim2) = (h1 + h2, d1 + (d2 * aim1), aim1 + aim2)

commandsToStates :: [Command] -> [SubmarimeState]
commandsToStates = map commandToState

commandToState :: Command -> SubmarimeState
commandToState (Forward value) = (value, value, 0)
commandToState (Up value) = (0, 0, -value)
commandToState (Down value) = (0, 0, value)

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
