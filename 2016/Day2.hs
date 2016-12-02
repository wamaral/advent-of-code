module Day2 where

-- Types
data Action = U | D | L | R
  deriving (Show)

-- Logic
move :: Integer -> Action -> Integer
move current act = case act of
  U -> if current > 3 then current - 3 else current
  D -> if current < 7 then current + 3 else current
  L -> if current `elem` [1, 4, 7] then current else current - 1
  R -> if current `elem` [3, 6, 9] then current else current + 1

findButton :: Integer -> [Action] -> Integer
findButton = foldl move

-- Parse
parseAction :: Char -> Action
parseAction 'U' = U
parseAction 'D' = D
parseAction 'L' = L
parseAction 'R' = R

-- Main
main :: IO ()
main = do
  c <- readFile "input/2"
  let inputs = foldl (\acc i -> acc ++ [findButton (last acc) (map parseAction i)]) [5] $ lines c
  putStr "1. "
  putStrLn $ show $ tail inputs
  putStr "2. "
  putStrLn "TODO"
