{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Day2a where

import Text.Parsec
import Text.Parsec.String

type Cube = (Int, String)

integer :: Parser Int
integer = do 
    spaces
    read <$> many1 digit

cube :: Parser Cube
cube = do
    cubeNum <- integer
    spaces
    cubeColor <- string "red" <|> string "green" <|> string "blue"
    return (cubeNum, cubeColor)

play :: Parser [Cube]
play = cube `sepBy1` char ',' 

game :: Parser [Cube]
game = concat <$> (play `sepBy1` char ';')

wholeGame :: Parser (Int, [Cube])
wholeGame = do
    string "Game "
    gameNum <- integer
    char ':'
    cubes <- game
    return (gameNum, cubes)

input :: Parser [(Int, [Cube])]
input = wholeGame `sepBy` char '\n'

passing :: Cube -> Bool
passing (n, "red") = n <= 12
passing (n, "green") = n <= 13
passing (n, "blue") = n <= 14
passing (_, _) = error "missing color 🥶"

possible :: (Int, [Cube]) -> Bool
possible (_, cubes) = all passing cubes

solution :: IO ()
solution = do
    inputStr <- readFile "src/input.txt"
    putStrLn inputStr
    case parse input "" inputStr of
        Left err -> putStrLn $ "Error: " ++ show err
        Right games -> print $ sum $ map fst $ filter possible games
