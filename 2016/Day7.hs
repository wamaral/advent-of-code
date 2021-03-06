module Day7 where

import           Data.List       (isInfixOf)
import           Data.List.Split (divvy, split, startsWithOneOf)

-- Types
data Ipv7 = Ipv7 { insides  :: [String]
                 , outsides :: [String]
                 } deriving Show

-- Logic
abba :: String -> Bool
abba = any (\(x:y:z:w:_) -> x == w && y == z && x /= y) . divvy 4 1

tls :: Ipv7 -> Bool
tls i = (any abba $ outsides i) && (all (not . abba) $ insides i)

ssl :: Ipv7 -> Bool
ssl i = any bab aba
  where
    aba = filter (\(x:y:z:_) -> x == z && x /= y) $ concatMap (divvy 3 1) (outsides i)
    bab (r:s:_) = any (\x -> [s, r, s] `isInfixOf` x) (insides i)

-- Parse
makeIpv7 :: String -> Ipv7
makeIpv7 input = foldl append Ipv7 { insides = [], outsides = [] } $ split (startsWithOneOf "[]") input
  where append ipv7 elem = case (head elem) of
          '[' -> ipv7 { insides = (insides ipv7) ++ [(tail elem)] }
          ']' -> ipv7 { outsides = (outsides ipv7) ++ [(tail elem)] }
          _   -> ipv7 { outsides = (outsides ipv7) ++ [elem] }

-- Main
main :: IO ()
main = do
  input <- lines <$> readFile "input/7"
  let inputs = map makeIpv7 input
  putStr "1. "
  putStrLn $ show $ length $ filter tls inputs
  putStr "2. "
  putStrLn $ show $ length $ filter ssl inputs
