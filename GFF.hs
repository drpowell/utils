{-# LANGUAGE OverloadedStrings #-}

module GFF
    ( GFF(..), Scaffold(..), Feature(..), Seq, Str
    , isGff3, parseGff, gffOutput, setFeatAttrib
    , isFasta, parseFasta
    , featAttrib, featLen, featSel, featSeq, featId
    , featureToLine
    ) where

{-
  Reference for GFF3 : http://www.sequenceontology.org/gff3.shtml
  Reference for GFF2 : http://www.sanger.ac.uk/resources/software/gff/spec.html#t_2
-}

import Data.Maybe
import Text.Printf
import Data.List
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as T

type Seq = BS.ByteString
type Str = T.Text

data GFF = GFF { scaffolds :: [Scaffold]
               , features :: [Feature]
               }

data Scaffold = Scaffold { s_name :: Str, dna :: Seq }
              deriving (Show)

data Feature = Feature { seq_id, source, typ :: Str
                       , start, end :: Int
                       , score, strand, phase :: Str
                       , attributes :: M.Map Str [Str]
                       } deriving (Show)

lineToFeature :: Str -> Feature
lineToFeature l = Feature {seq_id = cols!!0, source = cols!!1, typ = cols!!2
                          ,start = read . T.unpack $ cols!!3, end = read . T.unpack $ cols!!4
                          ,score = cols!!5, strand = cols!!6, phase = cols!!7
                          ,attributes = attribToMap (cols!!8)
                          }
  where
    cols = T.splitOn "\t" l
    attribToMap s = M.fromListWith (++) $ mapMaybe toPair $ T.splitOn ";" s
    toPair s = case T.splitOn "=" s of
                 [k,v] -> Just (k,[v])
                 [""] -> Nothing
                 x -> error $ "Bad attribute pair : "++show x++" : "++show s

featureToLine :: Feature -> Str
featureToLine f = T.intercalate "\t" $ map ($f) [seq_id, source, typ
                                                , T.pack.show.start
                                                , T.pack.show.end
                                                , score, strand, phase
                                                ,mapToAttrib . attributes]
    where
      mapToAttrib :: M.Map Str [Str] -> Str
      mapToAttrib mp = T.intercalate ";" $
                         map (\(k,vs) -> T.concat $ [k,"=",T.intercalate " " vs]) $ M.toList mp

featId :: Feature -> Str
featId f = featAttrib "ID" f

featAttrib :: Str -> Feature -> Str
featAttrib k f = maybe "" head $ M.lookup k $ attributes f

setFeatAttrib :: Str -> Feature -> Str -> Feature
setFeatAttrib k f v = f { attributes = M.insert k [v] (attributes f) }

parseScaffold :: [Str] -> (Scaffold, [Str])
parseScaffold (l:ls) = let name = T.tail (T.dropWhile (' ' /=) l)
                           (dnaLines,rest) = break ("##end-DNA" `T.isPrefixOf`) ls
                       in (Scaffold { s_name = name
                                    , dna = BS.pack $ concatMap (T.unpack . T.drop 2) dnaLines
                                    }
                          , rest)

parseFasta :: [Str] -> [Scaffold]
parseFasta ls
    | null ls = []
    | ">" `T.isPrefixOf` head ls = let (dna,rest) = break (">" `T.isPrefixOf`) (tail ls)
                                   in Scaffold { s_name = T.tail (head ls)
                                               , dna = BS.pack . T.unpack . T.concat $ dna}
                                        : parseFasta rest
    | otherwise = error $ "Bad FASTA section : "++show (take 5 ls)

isFasta :: [Str] -> Bool
isFasta ls = case ls of
               (l:_) -> ">" `T.isPrefixOf` l
               _ -> False

parseLines :: [Str] -> GFF
parseLines [] = GFF [] []
parseLines lss@(l:ls)
    | "##DNA " `T.isPrefixOf` l = let (s,rem) = parseScaffold lss in addScaffold s (parseLines rem)
    | "##FASTA" == l          = GFF (parseFasta ls) []
    | "##" `T.isPrefixOf` l   = parseLines ls
    | otherwise               = addFeature (lineToFeature l) (parseLines ls)
  where
    addFeature f gff = gff { features  = f : features gff }
    addScaffold s gff = gff { scaffolds = s : scaffolds gff }

getScaffold :: GFF -> Str -> Maybe Scaffold
getScaffold gff name = lookup name $ map (\s -> (s_name s, s)) (scaffolds gff)

featSeq :: GFF -> Feature -> Seq
featSeq gff f = case getScaffold gff (seq_id f) of
                  Nothing -> error $ "No scaffold by the name : "++(T.unpack $ seq_id f)
                  Just sc -> BS.take (featLen f) . BS.drop (start f - 1) . dna $ sc

-- | TODO, should check ##gff-version
parseGff :: [Str] -> GFF
parseGff ls = parseLines ls

isGff3 :: [Str] -> Bool
isGff3 ls = case ls of
              ("##gff-version 3":_) -> True
              _ -> False

gffOutput :: Bool -> GFF -> [Str]
gffOutput asFasta gff = ["##gff-version 3"]
                        ++ seqInline
                        ++ map featureToLine (features gff)
                        ++ (if null (scaffolds gff) then [] else seqAsFasta)
  where
    seqInline | asFasta = []
              | otherwise = map (\s -> T.pack $ printf "##Type DNA %s" (T.unpack $ s_name s))
                              (scaffolds gff)
                            ++ concatMap scaffoldOutput (scaffolds gff)
    seqAsFasta | asFasta = ["###","##FASTA"]
                           ++ concatMap scaffoldAsFasta (scaffolds gff)
               | otherwise = []
    scaffoldOutput s = [T.pack $ printf "##DNA %s" (T.unpack $ s_name s)]
                       ++ map (T.append "##") (T.chunksOf 40 . T.pack . BS.unpack $ dna s)
                       ++ ["##end-DNA"]
    scaffoldAsFasta s = (">" `T.append` s_name s) : (T.chunksOf 60 . T.pack . BS.unpack $ dna s)

toChunks _ [] = []
toChunks n s = let (a,b) = splitAt n s
               in a : toChunks n b

featLen :: Feature -> Int
featLen f = end f - start f

featSel :: Str -> [Feature] -> [Feature]
featSel f = filter ((f==).typ)

