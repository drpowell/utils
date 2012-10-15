#!/usr/bin/env runghc

{-# LANGUAGE DeriveDataTypeable,TupleSections #-}

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Ord
import System.Console.CmdArgs
import System.Exit

import Useful
import GFF

data Options = Options { files :: [FilePath]
                       , num_genes :: Int
                       } deriving (Show,Eq,Data,Typeable)

options = Options { files = [] &= args
                  , num_genes = 10 &= help "Number of genes to output per-group (-1 means unlimited)"
                  }

mode = cmdArgsMode $ options
                    &= helpArg [explicit, name "h", name "help"]
                    &= program progName
                    &= summary (progName ++ " V1.0 (C) David R. Powell <david@drp.id.au>")
                    &= help "Calculate the 'venn diagram' of genes across a set of GFF3 or Fasta files"

geneSetFasta :: [Scaffold] -> S.Set String
geneSetFasta sc = S.fromList . map (takeWhile (' ' /=) . s_name) $ sc

geneSetGff :: GFF -> S.Set String
geneSetGff gff = S.fromList . map myName . featSel "gene" . features $ gff
  where
    myName f = case featAttrib "Note" f =~ "Similar to (\\S+):" of
                 [[_,name]] -> name
                 [] -> featAttrib "ID" f

loadFile :: FilePath -> IO (S.Set String)
loadFile f = do
    ls <- lines <$> readFile f
    return $ parseFile ls
  where
    parseFile ls | isFasta ls = geneSetFasta $ parseFasta ls
                 | isGff3 ls = geneSetGff $ parseGff ls
                 | otherwise = error $ "Unknown file format : "++f

membership :: Ord a => [(b, S.Set a)] -> a -> [b]
membership sets g = mapMaybe (\(f,set) -> if S.member g set then Just f else Nothing) sets

groupByMembership :: [(FilePath, S.Set String)] -> [([FilePath], [String])]
groupByMembership sets = let allSets = S.unions . map snd $ sets
                             allMemberships = map (\g -> (membership sets g, g)) . S.toList $ allSets
                         in map (\l -> (fst $ head l, map snd l)) . grpBy fst $ allMemberships

main = do
  opts <- cmdArgsRun mode

  when (null $ files opts) $ do
    printf "No files specified\n\n"
    putStrLn $ show mode
    exitFailure

  sets <- mapM (\f -> loadFile f >>= \s -> return $ (f,s)) (files opts)

  forM_ sets $ \(f, set) -> do
         printf "%s: %d\n" f (S.size set)
         return ()

  {-
  let [(_,a),(_,b)] = sets
  printf "intersection : %d\n" (S.size $ S.intersection a b)
  printf "A-B : %d\n" (S.size $ S.difference a b)
  printf "B-A : %d\n" (S.size $ S.difference b a)
  -}

  let byGroup = groupByMembership sets
  forM_ (reverse $ sortBy (comparing $ length . fst) byGroup) $ \(files, grp) -> do
      printf "%s : %d\n" (intercalate " " files) (length grp)
      let genes = (take (num_genes opts) grp) ++ if num_genes opts <= length grp then ["..."] else []
      printf "%s\n" (intercalate "\n" . map ("    "++) $ genes)




grpBy :: Ord k => (a -> k) -> [a] -> [[a]]
grpBy f ls = grpBy' (Just . f) ls

grpBy' :: Ord k => (a -> Maybe k) -> [a] -> [[a]]
grpBy' f ls = M.elems . M.fromListWith (++) . mapMaybe (\l -> f l >>= \k -> return (k,[l])) $ ls
