{-# OPTIONS_GHC -fglasgow-exts #-}
module MO.Capture where
import Prelude hiding (foldl, foldr)
import Data.Typeable
import StringTable.Atom
import StringTable.AtomMap as AtomMap hiding (map)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Monoid
import Data.Foldable (foldl, foldr)

-- | a Capture is a frozen version of the arguments to an application.
data Capt a
    = CaptMeth
        { c_invocant :: a
        , c_feeds    :: Seq (Feed a)
        }
    | CaptSub
        { c_feeds    :: Seq (Feed a)
        }
    deriving (Show, Eq, Ord, Typeable)


-- | non-invocant arguments.
data Feed a = MkFeed
    { f_positionals :: Seq a
    , f_nameds      :: AtomMap (Seq a)
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

concatFeeds :: Seq (Feed a) -> Feed a
concatFeeds xs = MkFeed (foldr mappend mempty (fmap f_positionals xs)) (foldl AtomMap.union mempty (fmap f_nameds xs))
