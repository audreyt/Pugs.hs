{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}
{-# LANGUAGE KindSignatures #-}
module MO.Run where
import Data.Typeable
data AnyResponder (m :: * -> *)
emptyResponder :: (Typeable1 m, Monad m) => AnyResponder m

