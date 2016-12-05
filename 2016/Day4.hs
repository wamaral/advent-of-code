module Day4 where

import           Data.Function   (on)
import           Data.List       (group, groupBy, sort, sortOn)
import           Data.List.Split (splitOn)

-- Types
data Room = Room { roomName :: [String]
                 , sectorId :: Integer
                 , checksum :: String
                 } deriving Show

-- Logic
realRoom :: Room -> Bool
realRoom room = (==) (checksum room) $ map head $ take 5 $ concat $ reverse $ groupBy ((==) `on` length) $ sortOn length $ group $ sort (concat $ roomName room)

decryptName :: Room -> String
decryptName room = map (rotateChar (sectorId room)) $ unwords $ roomName room

rotateChar :: Integer -> Char -> Char
rotateChar _ ' ' = ' '
rotateChar i c = last $ take ((fromIntegral i) + 1) $ dropWhile (/= c) $ cycle ['a'..'z']

-- Parse
makeRoom :: [String] -> Room
makeRoom input = Room { roomName = init input
                      , sectorId = read $ head roomId
                      , checksum = last roomId
                      }
  where
    roomId = splitOn "[" $ init $ last input

-- Main
main :: IO ()
main = do
  inputs <- readFile "input/4"
  let rooms = map (makeRoom . splitOn "-") $ lines inputs
  putStr "1. "
  putStrLn $ show $ foldr (+) 0 $ map sectorId $ filter realRoom rooms
  putStr "2. "
  putStrLn $ show $ sectorId $ head $ filter (\r -> decryptName r == "northpole object storage") $ filter realRoom rooms
