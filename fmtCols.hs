#!/usr/bin/env runghc

{-# LANGUAGE DeriveDataTypeable #-}

import Data.Maybe
import Data.List
import Data.List.Split
import Text.Printf
import System.Environment
import System.Console.CmdArgs
import System.Process
import Control.Exception (try,handle,IOException)
import System.IO.Error (ioeGetErrorType)

import System.IO
import System.Posix.IO
import System.Posix.Terminal
import HsShellScript.ProcErr

type ColInfo = (ColType,Int)
data ColType = Dbl | Int | Str deriving (Show,Eq,Ord)

data Opts = Opts
    { guess :: Int
    , separator :: String
    , only :: Maybe Int
    , pager :: String
    , files :: [String]
    } deriving (Show,Data,Typeable)

defOpts = Opts
       { guess = 50 &= help "Number of lines to use to guess column types (50)"
       , separator = "\t" &= help "Separator character (\\t)"
       , only = Nothing &= help "Number of lines to show.  Negative means skip that many lines"
       , pager = "less -FRX" &= help "Default PAGER to use (less -FRX), set to empty to disable"
       , files = [] &= args &= typFile
       } &= help "Format a tab (or comma) separated file"
         &= program "fmtCols"
         &= summary "fmtCols V1.0, (C) David R. Powell"
         &= helpArg [explicit, name "h", name "help"]

main = do
  opts <- cmdArgs defOpts
  ls <- lines `fmap` case files opts of
                       [] -> getContents
                       (f:_) -> readFile f
  let typs = guessFmts opts . take (guess opts) . tail $ ls
  (outH, chld) <- setupPager opts
  -- print opts
  -- print typs
  handle pipeClosed $
    mapM_ (\l -> hPutStrLn outH $ fmt opts typs l)
          (case only opts of
             Nothing -> ls
             Just n -> if n<0
                         then drop (abs n) ls
                         else take n ls)
  try (hClose outH) :: IO (Either IOException ())
  waitPager chld

fmt opts typs l = intercalate "\t" . zipWith fmtCol typs . splitOn (separator opts) $ l

fmtCol (Dbl,w) str = maybe str (printf "%*.2f" w) (readDbl str)
fmtCol (Int,w) str = maybe str (printf "%*d" w) (readInt str)
fmtCol (Str,w) str = printf "%-*s" w str

guessFmts :: Opts -> [String] -> [ColInfo]
guessFmts opts ls = let cols = transpose $ map (splitOn (separator opts)) ls
                        typs = map guessColumn cols
                        widths = zipWith guessWidth typs cols
                    in zip typs widths

guessType str = case readInt str of
                  Just _ -> Int
                  Nothing -> case readDbl str of
                               Just _ -> Dbl
                               Nothing -> Str

guessColumn strs = case sort $ nub $ map guessType strs of
                     [e] -> e
                     [Dbl, Int] -> Dbl
                     _ -> Str

guessWidth t strs
    | t==Dbl = maximum . map lenDbl . mapMaybe readDbl $ strs
    | t==Int = maximum . map lenInt . mapMaybe readInt $ strs
    | t==Str = maximum . map length $ strs
  where
    lenDbl s = length (printf "%.2f" s :: String)
    lenInt s = length (printf "%d" s :: String)

readDbl = maybeRead :: String -> Maybe Double
readInt = maybeRead :: String -> Maybe Integer

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                [(v, "")] -> Just v
                _ -> Nothing

setupPager opts
    | pager opts == [] = return (stdout, Nothing)
    | otherwise = do
  tty <- isatty stdout
  if not tty
     then return (stdout, Nothing)
     else do
       let cmd = pager opts
       (Just inH, _, _, ph) <- createProcess $ (shell cmd) {std_in = CreatePipe}
       return (inH, Just ph)

waitPager Nothing = return ()
waitPager (Just ph) = waitForProcess ph >> return ()

pipeClosed :: IOException -> IO ()
pipeClosed e = if "vanished" `isInfixOf` (show $ ioeGetErrorType e)
               then return ()
               else print ("EXCEPTION : "++show e) >> return ()