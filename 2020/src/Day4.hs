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

validHeight :: String -> Bool
validHeight s = case scale of
  "cm" -> height >= 150 && height <= 193
  "in" -> height >= 59 && height <= 76
  _    -> False
  where
    (height, scale) = fromMaybe (0, "") $ parseMaybe heightParser s
    heightParser :: Parser (Int, String)
    heightParser = (,) <$> intParser <*> choice (map string ["cm", "in"])

validField :: String -> String -> Bool
validField "byr" v = (\x -> x >= 1920 && x <= 2002) . stringToInt0 $ v
validField "iyr" v = (\x -> x >= 2010 && x <= 2020) . stringToInt0 $ v
validField "eyr" v = (\x -> x >= 2020 && x <= 2030) . stringToInt0 $ v
validField "hgt" v = validHeight v
validField "hcl" v = isJust $ parseMaybe (char '#' >> count 6 hexDigitChar :: Parser String) v
validField "ecl" v = v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validField "pid" v = isJust $ parseMaybe (count 9 digitChar :: Parser String) v
validField "cid" _ = True
validField _ _     = False

validFields :: Passport -> Bool
validFields = all (== True) . M.elems . M.mapWithKey validField

day4part1 :: String -> String
day4part1 = show . length . filter requiredFieldsPresent . parseInput

day4part2 :: String -> String
day4part2 = show . length . filter validFields . filter requiredFieldsPresent . parseInput
