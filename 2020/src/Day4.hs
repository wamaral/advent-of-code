module Day4
  (day4part1, day4part2)
  where

import           Common
import qualified Data.Map             as M
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Passport = M.Map String String

inputParser :: Parser [Passport]
inputParser = passportParser `sepBy` newline

passportParser :: Parser Passport
passportParser = M.fromList <$> passportFieldParser `sepEndBy` separator
  where separator = try $ newline <|> spaceChar

passportFieldParser :: Parser (String, String)
passportFieldParser = do
  k <- choice $ map (try . string) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]
  _ <- char ':'
  v <- some (alphaNumChar <|> char '#')
  return (k, v)

parseInput :: String -> [Passport]
parseInput = fromMaybe [] . parseMaybe inputParser

requiredFieldsPresent :: Passport -> Bool
requiredFieldsPresent c = all (== True) $ map (\x -> M.member x c) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

day4part1 :: String -> String
day4part1 = show . length . filter requiredFieldsPresent . parseInput

day4part2 :: String -> String
day4part2 _ = ""
