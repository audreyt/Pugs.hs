module Pugs.Compat.Regex (
    compilePCRE,
    executePCRE
) where

import Data.Array (Array)
import qualified Text.Regex.PCRE.String as PCRE

compilePCRE :: PCRE.CompOption -> PCRE.ExecOption -> String -> IO (Either (PCRE.MatchOffset, String) PCRE.Regex) 
compilePCRE = PCRE.compile

executePCRE :: PCRE.Regex -> String -> IO (Either PCRE.WrapError (Maybe (Array Int (PCRE.MatchOffset, PCRE.MatchLength)))) 
executePCRE = PCRE.execute
