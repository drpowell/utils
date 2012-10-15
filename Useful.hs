module Useful
    ( progName
    , module Text.Printf
    , module Text.Regex.PCRE
    , module Control.Monad
    , module Control.Applicative
    , module Data.Maybe
    , module Data.List
    , module Debug.Trace
    ) where

import Text.Regex.PCRE ((=~))
import Text.Printf
import Control.Applicative ((<$>))
import Control.Monad (forM,forM_,when)
import Data.Maybe
import Data.List
import Debug.Trace

import System.Environment
import System.IO.Unsafe

progName = unsafePerformIO getProgName
