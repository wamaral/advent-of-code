module Day8 where

import           Data.Either            (rights)
import           Data.List              (isPrefixOf)
import           Data.List.Split
import           Data.Matrix            hiding ((<|>))
import qualified Data.Matrix            as M
import           Data.Vector            hiding (filter, foldl, length, map,
                                         mapM_, reverse, take)
import qualified Data.Vector            as V
import           Text.Megaparsec
import           Text.Megaparsec.String

-- Types
data Pixel = Off | On deriving Eq
instance Show Pixel where
  show On  = "#"
  show Off = "."

data Operation = Rect { x :: Int , y :: Int }
  | RotateRow { row :: Int, count :: Int }
  | RotateCol { col :: Int, count :: Int }
  deriving Show

-- Logic
runOp :: Matrix Pixel -> Operation -> Matrix Pixel
runOp origin op = case op of
  Rect x y            -> joinBlocks $ (matrix y x (const On), b, c, d) where (a,b,c,d) = splitBlocks y x origin
  RotateRow row count -> setSize Off 6 50 $ switchRows (row + 1) 7 $ origin <-> (rowVector $ vecRotate count $ getRow (row + 1) origin)
  RotateCol col count -> setSize Off 6 50 $ switchCols (col + 1) 51 $ origin M.<|> (colVector $ vecRotate count $ getCol (col + 1) origin)

vecRotate :: Int -> Vector Pixel -> Vector Pixel
vecRotate i v = V.reverse $ V.concat $ [b, a] where (a,b) = V.splitAt (i `mod` (V.length v)) $ V.reverse v

-- Parse
val :: Parser Int
val = read <$> many numberChar

parseRect :: Parser Operation
parseRect = Rect <$> (string "rect " *> val) <*> (string "x" *> val)

parseRotateRow :: Parser Operation
parseRotateRow = RotateRow <$> (string "rotate row y=" *> val) <*> (string " by " *> val)

parseRotateCol :: Parser Operation
parseRotateCol = RotateCol <$> (string "rotate column x=" *> val) <*> (string " by " *> val)

makeOp :: Parser Operation
makeOp = try parseRect <|> parseRotateRow <|> parseRotateCol

-- Main
main :: IO ()
main = do
  input <- lines <$> readFile "input/8"
  let ops = rights $ map (parse makeOp "") input
  let display = matrix 6 50 (const Off)
  let applied = foldl runOp display ops
  putStr "1. "
  putStrLn $ show $ length $ filter (== On) $ M.toList applied
  putStrLn "2. "
  putStrLn $ show applied
