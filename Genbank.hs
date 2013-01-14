module Genbank
    (
    ) where

import System.Environment
import Control.Monad
import Control.Applicative ((<$>),(<*>))
import Debug.Trace
import qualified Data.Char

import Text.Parsec hiding (space,spaces)
import Text.Parsec.String

data Tag = Tag { name :: String
               , value :: String
               } deriving Show

data Location = Location String
              deriving Show

data Feature = Feature { feat_type_ :: String
                       , feat_loc_ :: Location
                       , feat_tags_ :: [Tag]
                       } deriving Show

data Genbank = Genbank { hdr_ :: String
                       , tags_ :: [Tag]
                       , features_ :: [Feature]
                       , seq_ :: String
                       } deriving (Show)

parseGenbank :: Parser Genbank
parseGenbank = do
  r <- Genbank <$> header
               <*> many tag
               <*> features
               <*> seq_origin
  eof
  return r

tr m = trace m

header :: Parser String
header = do
  string "LOCUS"
  skip_line

tag :: Parser Tag
tag = do
  spaces
  n<- try $ do
        key <- many upper
        guard $ key `notElem` ["FEATURES","ORIGIN"]
        return key
  s <- many1 space
  l <- skip_line
  rest <- many indented_lines
  return $ Tag n (concat $ l : rest)

features :: Parser [Feature]
features = do
  string "FEATURES"
  skip_line
  many feature

feature :: Parser Feature
feature = do
  s <- many1 space
  typ <- many letter
  spaces
  loc1 <- skip_line
  ls <- many (indented_lines' $ length s + 1)
  let (loc:quals) = parse_qual ls loc1
  return $ Feature typ (Location loc) (map qual_to_tag quals)
  where
    parse_qual [] prev = [prev]
    parse_qual (l:ls) prev | head l=='/' = prev : parse_qual ls l
                           | otherwise = parse_qual ls (prev++l)
    qual_to_tag l = let (k,('=':v)) = break (=='=') l
                    in Tag k v

seq_origin :: Parser String
seq_origin = do
  string "ORIGIN"
  skip_line
  l <- many indented_lines
  string "//"
  eol
  return $ concat l

indented_lines :: Parser String
indented_lines = indented_lines' 1

indented_lines' :: Int -> Parser String
indented_lines' n = do
  try $ do
    count n space
    spaces
  skip_line

eol = spaces >> char '\n'


-- Redefine isSpace,space,spaces to *not* include newline
isSpace c = c /= '\n' && Data.Char.isSpace c
space = satisfy isSpace <?> "space"
spaces = skipMany space <?> "white space"

skip_line :: Parser String
skip_line = do s <- many (noneOf "\n")
               eol
               return s


main = do
  (fname:_) <- getArgs
  gb <- parseFromFile parseGenbank fname
  print $ (last . take 6 . features_) <$> gb