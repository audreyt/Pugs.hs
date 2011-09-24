{-# OPTIONS_GHC -fglasgow-exts #-}
module MO.Capture where

import Data.Typeable
import StringTable.Atom
import StringTable.AtomMap as AtomMap hiding (map)
import Data.Monoid

-- | a Capture is a frozen version of the arguments to an application.
data Capt a
    = CaptMeth
        { c_invocant :: a
        , c_feeds    :: [Feed a]
        }
    | CaptSub
        { c_feeds    :: [Feed a]
        }
    deriving (Show, Eq, Ord, Typeable)


-- | non-invocant arguments.
data Feed a = MkFeed
    { f_positionals :: [a]
    , f_nameds      :: AtomMap [a] 
        -- ^ maps to [:a:] and not a since if the Sig stipulates
        --   @x, "x => 1, x => 2" constructs @x = (1, 2).
    }
    deriving (Show, Eq, Ord, Typeable)

instance Monoid (Feed a) where
    mempty = MkFeed mempty mempty
    mappend (MkFeed x1 x2) (MkFeed y1 y2) = MkFeed (mappend x1 y1) (mappend x2 y2)
    mconcat xs = MkFeed (mconcat (map f_positionals xs)) (mconcat (map f_nameds xs))

emptyFeed :: Feed a
emptyFeed = mempty

concatFeeds :: [Feed a] -> Feed a
concatFeeds xs = MkFeed (concatMap f_positionals xs) (foldl AtomMap.union mempty (map f_nameds xs))
