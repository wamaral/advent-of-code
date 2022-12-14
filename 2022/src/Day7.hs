module Day7
  (day7part1, day7part2)
  where

import           Common
import           Data.List
import qualified Data.Map.Strict      as M
import qualified Data.Set             as S
import           Text.Megaparsec
import           Text.Megaparsec.Char

type FileName = String
data Command = CdDir String | CdParent | CdRoot | Ls deriving Show
data CommandOutput = LsDir String | LsFile Int String deriving Show
data InputLine = Cmd Command | Out CommandOutput deriving Show

type Path = [String]
data File = File Int String deriving (Eq, Ord, Show)
type FileMap = M.Map Path (S.Set File)

fileNameParser :: Parser FileName
fileNameParser = many (alphaNumChar <|> punctuationChar)

commandParser :: Parser Command
commandParser = string "$ " *> choice
  [ CdParent <$ try (string "cd ..")
  , CdRoot <$ try (string "cd /")
  , CdDir <$> (string "cd " *> fileNameParser)
  , Ls <$ string "ls"
  ] <* optional newline

commandOutputParser :: Parser CommandOutput
commandOutputParser = choice
  [ LsDir <$> (string "dir " *> fileNameParser)
  , LsFile <$> intParser <*> (spaceChar *> fileNameParser)
  ]

inputLineParser :: Parser InputLine
inputLineParser = choice
  [ Cmd <$> commandParser
  , Out <$> commandOutputParser
  ]

mapInput :: FileMap -> Path -> [InputLine] -> FileMap
mapInput fm _ [] = fm
mapInput fm path (i:is) = case i of
  Cmd (CdDir dir) -> mapInput fm (path ++ [dir]) is
  Cmd CdParent -> mapInput fm (init path) is
  Cmd CdRoot -> mapInput fm ["/"] is
  Cmd Ls -> mapInput fm path is
  Out (LsDir _dir) -> mapInput (M.insertWith S.union path S.empty fm) path is
  Out (LsFile size name) -> mapInput (M.insertWith S.union path (S.fromList [File size name]) fm) path is

fileSize :: File -> Int
fileSize (File size _) = size

dirSizes :: FileMap -> M.Map Path Int
dirSizes = M.map (sum . S.map fileSize)

dirSizesAgg :: FileMap -> M.Map Path Int
dirSizesAgg fm = M.mapWithKey(\path _ -> sum $ M.elems $ M.filterWithKey (\k _ -> path `isPrefixOf` k) fms) fms
  where fms = dirSizes fm

day7part1 :: String -> String
day7part1 = show . sum . filter (<= 100000) . M.elems. dirSizesAgg . mapInput M.empty ["/"] . readListOf inputLineParser

day7part2 :: String -> String
day7part2 _ = ""
