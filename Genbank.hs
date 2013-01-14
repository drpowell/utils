module Genbank
    ( main
    ) where

import System.Environment
import Control.Monad
import Control.Applicative ((<$>),(<*>))
import Debug.Trace
import qualified Data.Char
import Text.Printf

import Text.Regex.PCRE ((=~))
import Data.List.Split
import Data.Maybe

import Text.Parsec hiding (space,spaces)
import Text.Parsec.String

data Tag = Tag { name :: String
               , value :: String
               } deriving Show

data Location = Location { complement_ :: Bool
                         , joins_ :: [Loc]
                         } deriving (Show)

data Loc = LocExact Int Int
         | LocOther String
           deriving (Show)

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

tr :: (Monad m) => String -> m ()
tr m = trace m (return ())

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

fromRight (Right r) = r
fromRight e = error $ show e

feature :: Parser Feature
feature = do
  s <- many1 space
  typ <- many letter
  spaces
  loc1 <- skip_line
  ls <- many (indented_lines' $ length s + 1)
  let (loc:quals) = parse_qual ls loc1
  return $ Feature typ (fromRight $ parse parse_loc "" loc) (map qual_to_tag quals)
  where
    parse_qual [] prev = [prev]
    parse_qual (l:ls) prev | head l=='/' = prev : parse_qual ls (tail l)
                           | otherwise = parse_qual ls (prev++l)
    qual_to_tag l = case break (=='=') l of
                      (k,('=':v)) -> Tag k (may_remove_quot v)
                      _ -> Tag l ""
    may_remove_quot s | head s=='"' = init (tail s)
                      | otherwise   = s

seq_origin :: Parser String
seq_origin = do
  string "ORIGIN"
  skip_line
  l <- many seq_line
  string "//"
  eol
  return $ concat l

seq_line :: Parser String
seq_line = do
  skipMany1 (char ' ' <|> digit)
  s <- skip_line
  return $ filter (not . isSpace) s

parse_loc :: Parser Location
parse_loc = do
  (between (string "complement(") (string ")") (Location True <$> joins))
  <|>
  (Location False <$> joins)
  where
    range :: Parser Loc
    range = do low <- many1 digit
               hi <- option low (do string ".."
                                    many1 digit)
               return $ LocExact (read low) (read hi)
    joins = join_simple <|> join_complex
    join_simple  = range >>= \r -> return [r]
    join_complex = between (string "join(") (string ")") (sepBy1 range (char ','))

indented_lines :: Parser String
indented_lines = indented_lines' 1

indented_lines' :: Int -> Parser String
indented_lines' n = do
  try $ do
    count n space
    spaces
  skip_line

eol :: Parser ()
eol = spaces >> char '\n' >> return ()


-- Redefine isSpace,space,spaces to *not* include newline
isSpace :: Char -> Bool
isSpace c = c /= '\n' && Data.Char.isSpace c
space :: Parser Char
space = satisfy isSpace <?> "space"
spaces :: Parser ()
spaces = skipMany space <?> "white space"

skip_line :: Parser String
skip_line = do s <- many (noneOf "\n")
               eol
               return s

extract_loc :: String -> Location -> String
extract_loc str loc | complement_ loc = rev_comp joined
                    | otherwise       = joined
  where
    joined = concat $ map one_loc (joins_ loc)
    one_loc (LocExact lo hi) = take (hi-lo+1) $ drop (lo-1) str
    one_loc e = error $ show e

extract_feat :: String -> Feature -> String
extract_feat str feat = extract_loc str . feat_loc_ $ feat

rev_comp :: String -> String
rev_comp = reverse . map comp
  where comp x = case x of {'T'->'A'; 'A'->'T'; 'C'->'G'; 'G'->'C'
                           ;'t'->'a'; 'a'->'t'; 'c'->'g'; 'g'->'c'
                           ; x -> x }

find_tag :: String -> Feature -> Maybe Tag
find_tag tag feat = listToMaybe $ filter (\t -> name t == tag) $ feat_tags_ feat

feat_by_type :: String -> Genbank -> [Feature]
feat_by_type typ genbank = filter (\f -> feat_type_ f == typ) $ features_ genbank

check_cds_translation :: Genbank -> [String]
check_cds_translation genbank = map check_feat $ feat_by_type "CDS" genbank
  where
    check_feat f = case find_tag "translation" f of
                     Nothing -> printf "%s: No translation" (feat_name f)
                     Just trans -> printf "%s: %s"
                                     (feat_name f)
                                     (match_str f $ value trans)
    feat_name f = value . head $ catMaybes [find_tag "standard_name" f
                                           ,find_tag "gene" f
                                           ,find_tag "locus_tag" f
                                           ,Just $ Tag "" "No name"
                                           ]
    match_str f trans1 = let trans2 = rna2prot . dna2rna . extract_feat (seq_ genbank) $ f
                         in if (trans1++"*")==trans2
                            then "MATCH"
                            else "MISMATCH : "++trans1++" /= "++trans2

main :: IO ()
main = do
  (fname:_) <- getArgs
  gb <- parseFromFile parseGenbank fname
  case gb of
    Left e -> error $ "Parse failed : "++show e
    Right gb -> do print $ (last . take 6 . features_) gb
                   print $ (take 100 . seq_) gb
                   putStrLn $ unlines $ check_cds_translation gb








----------------------------------------------------------------------


coding_str :: String
coding_str = "UUU F      CUU L      AUU I      GUU V \
             \UUC F      CUC L      AUC I      GUC V \
             \UUA L      CUA L      AUA I      GUA V \
             \UUG L      CUG L      AUG M      GUG V \
             \UCU S      CCU P      ACU T      GCU A \
             \UCC S      CCC P      ACC T      GCC A \
             \UCA S      CCA P      ACA T      GCA A \
             \UCG S      CCG P      ACG T      GCG A \
             \UAU Y      CAU H      AAU N      GAU D \
             \UAC Y      CAC H      AAC N      GAC D \
             \UAA *      CAA Q      AAA K      GAA E \
             \UAG *      CAG Q      AAG K      GAG E \
             \UGU C      CGU R      AGU S      GGU G \
             \UGC C      CGC R      AGC S      GGC G \
             \UGA *      CGA R      AGA R      GGA G \
             \UGG W      CGG R      AGG R      GGG G "


codons :: [(String, String)]
codons =  map conv (coding_str =~ "(\\w{3})\\s(\\S+)" :: [[String]])
    where
      conv ([_,c,a]) = (c,a)

rna2prot :: String -> String
rna2prot s = concatMap (\c -> fromMaybe (error $ "Unknown codon:"++c++":") $ lookup c codons)
             . chunksOf 3
             . map Data.Char.toUpper $ s

dna2rna :: String -> String
dna2rna s = map (\c -> case c of {'T' -> 'U'; 't'->'u'; x->x}) s
