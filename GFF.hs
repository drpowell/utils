module GFF
    ( GFF(..), Scaffold(..), Feature(..), Seq
    , parseGff, gffOutput, setFeatAttrib
    , featAttrib, featLen, featSel, featSeq, featId
    ) where

import Data.Maybe
import Text.Printf
import Data.List
import qualified Data.Map as M
import Data.List.Split
import qualified Data.ByteString.Char8 as BS

type Seq = BS.ByteString

data GFF = GFF { scaffolds :: [Scaffold]
               , features :: [Feature]
               }

data Scaffold = Scaffold { s_name :: String, dna :: Seq }
              deriving (Show)

data Feature = Feature { seq_id, source, typ :: String
                       , start, end :: Int
                       , score, strand, phase :: String
                       , attributes :: M.Map String [String]
                       } deriving (Show)

lineToFeature :: String -> Feature
lineToFeature l = Feature {seq_id = cols!!0, source = cols!!1, typ = cols!!2
                          ,start = read (cols!!3), end = read (cols!!4)
                          ,score = cols!!5, strand = cols!!6, phase = cols!!7
                          ,attributes = attribToMap (cols!!8)
                          }
  where
    cols = splitOn "\t" l
    attribToMap s = M.fromListWith (++) $ mapMaybe toPair $ splitOn ";" s
    toPair s = case splitOn "=" s of
                 [k,v] -> Just (k,[v])
                 [""] -> Nothing
                 x -> error $ "Bad attribute pair : "++show x++" : "++show s

featureToLine :: Feature -> String
featureToLine f = intercalate "\t" $ map ($f) [seq_id, source, typ, show.start, show.end, score, strand, phase
                                              ,mapToAttrib . attributes]
    where
      mapToAttrib mp = intercalate ";" $ map (\(k,vs) -> concatMap (\v -> k++"="++v) vs) $ M.toList mp

featId :: Feature -> String
featId f = featAttrib "ID" f

featAttrib :: String -> Feature -> String
featAttrib k f = maybe "" head $ M.lookup k $ attributes f

setFeatAttrib :: String -> Feature -> String -> Feature
setFeatAttrib k f v = f { attributes = M.insert k [v] (attributes f) }

parseScaffold :: [String] -> (Scaffold, [String])
parseScaffold (l:ls) = let name = tail (dropWhile (' ' /=) l)
                           (dnaLines,rest) = break ("##end-DNA" `isPrefixOf`) ls
                       in (Scaffold {s_name = name, dna = BS.pack $ concatMap (drop 2) dnaLines}
                          , rest)
parseFastaScaffolds :: [String] -> [Scaffold]
parseFastaScaffolds ls
    | null ls = []
    | ">" `isPrefixOf` head ls = let (dna,rest) = break (">" `isPrefixOf`) (tail ls)
                                 in Scaffold {s_name = tail (head ls), dna = BS.pack $ concat dna}
                                        : parseFastaScaffolds rest
    | otherwise = error $ "Bad FASTA section : "++show (take 5 ls)

parseLines :: [String] -> GFF
parseLines [] = GFF [] []
parseLines lss@(l:ls)
    | "##DNA " `isPrefixOf` l = let (s,rem) = parseScaffold lss in addScaffold s (parseLines rem)
    | "##FASTA" == l          = GFF (parseFastaScaffolds ls) []
    | "##" `isPrefixOf` l     = parseLines ls
    | otherwise               = addFeature (lineToFeature l) (parseLines ls)
  where
    addFeature f gff = gff { features  = f : features gff }
    addScaffold s gff = gff { scaffolds = s : scaffolds gff }

getScaffold :: GFF -> String -> Maybe Scaffold
getScaffold gff name = lookup name $ map (\s -> (s_name s, s)) (scaffolds gff)

featSeq :: GFF -> Feature -> Seq
featSeq gff f = case getScaffold gff (seq_id f) of
                  Nothing -> error $ "No scaffold by the name : "++seq_id f
                  Just sc -> BS.take (featLen f) . BS.drop (start f - 1) . dna $ sc

-- | TODO, should check ##gff-version
parseGff :: [String] -> GFF
parseGff ls = parseLines ls

gffOutput :: Bool -> GFF -> [String]
gffOutput asFasta gff = ["##gff-version 3"]
                        ++ seqInline
                        ++ map featureToLine (features gff)
                        ++ seqAsFasta
  where
    seqInline | asFasta = []
              | otherwise = map (\s -> printf "##Type DNA %s" (s_name s)) (scaffolds gff)
                            ++ concatMap scaffoldOutput (scaffolds gff)
    seqAsFasta | asFasta = ["###","##FASTA"]
                           ++ concatMap scaffoldAsFasta (scaffolds gff)
               | otherwise = []
    scaffoldOutput s = [printf "##DNA %s" (s_name s)]
                       ++ map ("##"++) (toChunks 40 . BS.unpack $ dna s)
                       ++ ["##end-DNA"]
    scaffoldAsFasta s = (">"++s_name s) : (toChunks 60 . BS.unpack $ dna s)

toChunks _ [] = []
toChunks n s = let (a,b) = splitAt n s
               in a : toChunks n b

featLen :: Feature -> Int
featLen f = end f - start f

featSel :: String -> [Feature] -> [Feature]
featSel f = filter ((f==).typ)

