module CmdArgs_zsh
    where

import System.Console.CmdArgs.Explicit (flagHelp,flagNames,modeFlags,FlagInfo(..),flagInfo,flagType,modeArgs,argType,Mode)
import Data.List
import Control.Applicative ((<$>))
import System.Environment
import System.Exit

zshMayOutputDef mode = do env <- getEnvironment
                          case lookup "CMDARGS_COMPLETE_ZSH" env of
                            Just x -> do zshCompletion mode
                                         exitWith ExitSuccess
                            Nothing -> return ()


zshCompletion :: Mode a -> IO ()
zshCompletion mode = do p <- getProgName
                        putStrLn . unlines . zsh p $ mode

zsh :: String -> Mode a -> [String]
zsh prog mode = ["#compdef "++prog
                ,""
                ,"# zsh completion for '"++prog++"'"
                ,"# To use, put this file somewhere in your $fpath and call it '_"++prog++"'"
                ,""
                ,"_arguments -C -S -s : \\"]
                ++ optspecs ++
                ["  && ret=0"]
  where
    optspecs = map def flags ++ finalArgs
    def f = "  "++mutuallyExclusive f ++ "{" ++ intercalate "," (asOpt f) ++ "}'["++flagHelp f++"]" ++ arg f ++ "' \\"
    flags = modeFlags mode
    asOpt f = map (\n -> (if length n==1 then "-" else "--") ++ n) $ flagNames f
    arg f = case flagInfo f of
              FlagReq   -> ":argument:"++argTyp f   -- Required argument
              FlagOpt _ -> "::argument:"++argTyp f  -- Optional argument
              _ -> ""
    argTyp f = case flagType f of
                 "FILE" -> "_files"
                 "DIR" -> "_directories"
                 _ -> "( )"
    mutuallyExclusive f = "'("++intercalate " " (asOpt f)++")'"
    finalArgs = case argType <$> snd (modeArgs mode) of
                  Just "[FILE]" -> ["  '*:files:_files' \\"]
                  Just "FILE"   -> ["  '*:files:_files' \\"]
                  _ -> []
