module Rules (rules) where

import qualified Rules.Perl5
import qualified Rules.JSON
import qualified Rules.YAML
import qualified Rules.Perl6Class

rules = concat
    [ Rules.Perl5.rules
    , Rules.JSON.rules
    , Rules.YAML.rules
    , Rules.Perl6Class.rules
    ]
