module Day5 where

import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Digest.Pure.MD5       (md5)
import           Data.List                  (isPrefixOf)

-- Logic
hash :: String -> String
hash = show . md5 . pack

isKey :: String -> Bool
isKey = isPrefixOf "00000" . hash

-- Main
main :: IO ()
main = do
  input <- readFile "input/5"
  putStr "1. "
  putStrLn $ show $ map ((!! 5) . hash) $ take 8 $ filter isKey $ map ((input ++) . show) [0..]
  putStr "2. "
  putStrLn "TODO"
