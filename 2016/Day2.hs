module Day2 where

-- Types
data Action = U | D | L | R
  deriving (Show)

-- Logic
move1 :: Integer -> Action -> Integer
move1 current act = case act of
  U -> if current > 3 then current - 3 else current
  D -> if current < 7 then current + 3 else current
  L -> if current `elem` [1, 4, 7] then current else current - 1
  R -> if current `elem` [3, 6, 9] then current else current + 1

move2 :: Char -> Action -> Char
move2 current act = case act of
  U -> neighbour current "121452349678B"
  D -> neighbour current "36785ABC9ADCD"
  L -> neighbour current "122355678AABD"
  R -> neighbour current "134467899BCCD"
  where
    base = "123456789ABCD"
    neighbour i lst = snd $ head $ filter (\x -> i == fst x) $ zip base lst

-- Parse
parseAction :: Char -> Action
parseAction 'U' = U
parseAction 'D' = D
parseAction 'L' = L
parseAction 'R' = R
parseAction _   = error "Invalid input"

-- Main
main :: IO ()
main = do
  inputs <- readFile "input/2"
  putStr "1. "
  putStrLn $ show $ tail $ getButtons move1 [5] (lines inputs)
  putStr "2. "
  putStrLn $ show $ tail $ getButtons move2 "5" (lines inputs)
  where getButtons fn = foldl (\acc i -> acc ++ [foldl fn (last acc) (map parseAction i)])
