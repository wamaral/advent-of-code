module Day2
  (day2part1, day2part2)
  where

import Common
import Data.List
import Safe
import Text.Megaparsec
import Text.Megaparsec.Char

type Level = Int
type Report = [Level]

reportParser :: Parser Report
reportParser = intParser `sepBy` hspace

safeReport :: Report -> Bool
safeReport report = isSequential && isCloseEnough
  where
    sorted = sort report
    isSequential = report == sorted || report == reverse sorted
    isCloseEnough = all (\n -> abs n >= 1 && abs n <= 3) $ zipWith (-) report (tail report)

safeReportDampened :: Report -> Bool
safeReportDampened report = any safeReport (report : dampenedReports)
  where
    dampen n = (\(a,b) -> a ++ tailDef [] b) $ splitAt n report
    dampenedReports = map dampen [0..(length report)]

day2part1 :: String -> String
day2part1 = show . length . filter safeReport . readListOf reportParser

day2part2 :: String -> String
day2part2 = show . length . filter safeReportDampened . readListOf reportParser
