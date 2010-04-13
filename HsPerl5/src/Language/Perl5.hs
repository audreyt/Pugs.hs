{-# INCLUDE "p5embed.h" #-}
module Language.Perl5
    ( Context(..)
    , ToSV(..)
    , FromSV(..)
    , withPerl5
    , callSub,      (.:), (.!)
    , callMethod,   (.$), (.$!)
    , eval
    , eval_
    , SV
    , use
    ) where
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Exception (bracket, throwIO, Exception(..))
import Data.Dynamic (toDyn)
import Data.List (intersperse)

-- | Perl 5's calling context.
data Context = Void | Item | List

enumContext :: (Num a) => Context -> a
enumContext Void = 128
enumContext Item = 0
enumContext List = 1

type Interpreter = Ptr ()
type SV = Ptr ()

class ToCV a where
    toCV :: a -> Int -> IO SV

instance ToSV a => ToCV a where
    toCV x _ = toSV x

instance ToCV String where
    toCV sub count = do
        cv <- withCString sub perl5_get_cv
        if cv /= nullPtr then return cv else do
            let prms = map (\i -> "$_[" ++ show i ++ "]") [0 .. count-1]
            eval ("sub { " ++ sub ++ "(" ++ (concat $ intersperse ", " prms) ++ ") }")

(.:) :: (ToCV sub, ToArgs args, FromArgs ret) => sub -> args -> IO ret
(.:) = callSub

(.!) :: (ToCV sub, ToArgs args) => sub -> args -> IO ()
(.!) = callSub

-- | Call a Perl 5 subroutine.
callSub :: forall s a r. (ToCV s, ToArgs a, FromArgs r) => s -> a -> IO r
callSub sub args = do
    args'   <- toArgs args
    sub'    <- toCV sub (length args')
    rv      <- withArray0 nullPtr args' $ \argsPtr -> do
        perl5_apply sub' nullPtr argsPtr (enumContext $ contextOf (undefined :: r))
    returnPerl5 rv

(.$) :: (ToSV meth, ToArgs args, FromArgs ret) => SV -> meth -> args -> IO ret
(.$) = callMethod

(.$!) :: (ToSV meth, ToArgs args) => SV -> meth -> args -> IO ()
(.$!) = callMethod

-- | Call a Perl 5 method.
callMethod :: forall i m a r. (ToSV i, ToSV m, ToArgs a, FromArgs r) => i -> m -> a -> IO r
callMethod inv meth args = do
    inv'    <- toSV inv
    args'   <- toArgs args
    sub'    <- toSV meth
    rv      <- withArray0 nullPtr args' $ \argsPtr -> do
        perl5_apply sub' inv' argsPtr (enumContext $ contextOf (undefined :: r))
    returnPerl5 rv

-- | Use a module.  Returns a prototype object representing the module.
use :: String -> IO SV
use m = eval $ "use " ++ m ++ "; q[" ++ m ++ "]"

-- | Run a computation within the context of a Perl 5 interpreter. 
withPerl5 :: IO a -> IO a
withPerl5 f = do
    withCString "-e" $ \prog -> withCString "" $ \cstr -> do
        withArray [prog, prog, cstr] $ \argv -> do
            bracket (perl5_init 3 argv) (\interp -> do
                perl_destruct interp
                perl_free interp) (const f)

-- | Evaluate a snippet of Perl 5 code.
eval :: forall a. FromArgs a => String -> IO a
eval str = withCStringLen str $ \(cstr, len) -> do
    rv  <- perl5_eval cstr (toEnum len) (enumContext $ contextOf (undefined :: a))
    returnPerl5 rv 

-- | Same as 'eval' but always in void context.
eval_ :: String -> IO ()
eval_ str = eval str

returnPerl5 :: forall a. FromArgs a => Ptr SV -> IO a
returnPerl5 rv = do
    svs <- peekArray0 nullPtr rv
    case svs of
        []      -> fromArgs =<< peekArray0 nullPtr (rv `advancePtr` 1)
        [err]   -> throwIO (DynException $ toDyn err)
        (_:x:_) -> fail =<< fromSV x

-- | Data types that can be casted into a Perl 5 value (SV).
class ToSV a where
    toSV :: a -> IO SV

-- | Data types that can be casted from a Perl 5 value (SV).
class FromSV a where
    fromSV :: SV -> IO a

instance ToSV SV where toSV = return
instance FromSV SV where fromSV = return

instance ToSV () where
    toSV _ = perl5_sv_undef
instance FromSV () where
    fromSV x = seq x (return ())

instance ToArgs [String] where
    toArgs = mapM toSV

instance FromArgs [String] where
    fromArgs = mapM fromSV

instance ToSV String where
    toSV str = withCStringLen str $ \(cstr, len) -> do
        perl5_newSVpvn cstr (toEnum len)
instance FromSV String where
    fromSV sv = do
        cstr <- perl5_SvPV sv
        peekCString cstr

instance ToSV Int where
    toSV = perl5_newSViv . toEnum

instance ToSV Double where
    toSV = perl5_newSVnv . realToFrac

instance FromSV Int where
    fromSV = fmap fromEnum . perl5_SvIV

instance FromSV Double where
    fromSV = fmap realToFrac . perl5_SvNV

instance FromSV Bool where
    fromSV = perl5_SvTRUE

instance ToSV Bool where
    toSV True = perl5_sv_yes
    toSV False = perl5_sv_no

class ToArgs a where
    toArgs :: a -> IO [SV]

class FromArgs a where
    fromArgs :: [SV] -> IO a
    contextOf :: a -> Context
    contextOf _ = Item

instance FromArgs () where
    fromArgs _ = return ()
    contextOf _ = Void

instance ToArgs () where
    toArgs _ = return []

instance ToSV a => ToArgs a where
    toArgs = fmap (:[]) . toSV

instance (ToSV a, ToSV b) => ToArgs (a, b) where
    toArgs (x, y) = do
        x' <- toSV x
        y' <- toSV y
        return [x', y']

instance FromSV a => FromArgs a where
    fromArgs [] = fail "Can't convert an empty return list!"
    fromArgs (x:_) = fromSV x
    contextOf _ = Item

instance (FromSV a, FromSV b) => FromArgs (a, b) where
    fromArgs [] = fail "Can't convert an empty return list!"
    fromArgs [_] = fail "Can't convert a single  return list!"
    fromArgs (x:y:_) = do
        x' <- fromSV x
        y' <- fromSV y
        return (x', y')
    contextOf _ = List

instance FromArgs r => FromSV (IO r) where
    -- Callback code.
    fromSV x = do
        return $ callSub x ()

instance (ToArgs a, FromArgs r) => FromSV (a -> IO r) where
    -- Callback code.
    fromSV x = do
        return $ callSub x

instance (ToArgs a, ToArgs b, FromArgs r) => FromSV (a -> b -> IO r) where
    -- Callback code.
    fromSV x = do
        -- First we obtain x as a CV
        return $ \arg1 arg2 -> do
            as1  <- toArgs arg1
            as2  <- toArgs arg2
            callSub x (as1 ++ as2)

instance ToArgs [SV] where
    toArgs = return
instance FromArgs [SV] where
    fromArgs = return

instance ToArgs a => ToSV (IO a) where
    toSV f = do
        sp <- newStablePtr $ \_ _ -> do
            svs <- toArgs =<< f 
            newArray0 nullPtr svs
        perl5_make_cv sp

instance (ToArgs a, FromArgs r) => ToSV (r -> IO a) where
    toSV f = do
        sp <- newStablePtr $ \args _ -> do
            args'   <- fromArgs =<< peekArray0 nullPtr args
            svs     <- toArgs =<< f args'
            newArray0 nullPtr svs
        perl5_make_cv sp

instance (ToArgs a, FromArgs (r1, r2)) => ToSV (r1 -> r2 -> IO a) where
    toSV f = do
        sp <- newStablePtr $ \args _ -> do
            (a1, a2)    <- fromArgs =<< peekArray0 nullPtr args
            svs         <- toArgs =<< f a1 a2
            newArray0 nullPtr svs
        perl5_make_cv sp

instance (ToArgs a, FromArgs r) => ToSV (r -> a) where
    toSV f = do
        sp <- newStablePtr $ \args _ -> do
            args'   <- fromArgs =<< peekArray0 nullPtr args
            svs     <- toArgs $ f args'
            newArray0 nullPtr svs
        perl5_make_cv sp

instance (ToArgs a, FromArgs (r1, r2)) => ToSV (r1 -> r2 -> a) where
    toSV f = do
        sp <- newStablePtr $ \args _ -> do
            (a1, a2)    <- fromArgs =<< peekArray0 nullPtr args
            svs         <- toArgs $ f a1 a2
            newArray0 nullPtr svs
        perl5_make_cv sp

type Callback = Ptr SV -> CInt -> IO (Ptr SV)

hsPerl5Apply :: StablePtr Callback -> Ptr SV -> CInt -> IO (Ptr SV)
hsPerl5Apply ptr args cxt = do
    f <- deRefStablePtr ptr
    f args cxt

foreign export ccall "hsPerl5Apply"
    hsPerl5Apply :: StablePtr Callback -> Ptr SV -> CInt -> IO (Ptr SV)

foreign import ccall "perl5_make_cv"
    perl5_make_cv :: StablePtr Callback -> IO SV
foreign import ccall "perl5_init"
    perl5_init :: CInt -> Ptr CString -> IO Interpreter
foreign import ccall "perl5_sv_undef"
    perl5_sv_undef :: IO SV
foreign import ccall "perl5_sv_yes"
    perl5_sv_yes :: IO SV
foreign import ccall "perl5_sv_no"
    perl5_sv_no :: IO SV
foreign import ccall "perl5_eval"
    perl5_eval :: CString -> CInt -> CInt -> IO (Ptr SV)
foreign import ccall "perl5_newSVpvn"
    perl5_newSVpvn :: CString -> CInt -> IO SV
foreign import ccall "perl5_SvPV"
    perl5_SvPV :: SV -> IO CString
foreign import ccall "perl5_SvIV"
    perl5_SvIV :: SV -> IO CInt
foreign import ccall "perl5_SvNV"
    perl5_SvNV :: SV -> IO CDouble
foreign import ccall "perl5_newSViv"
    perl5_newSViv :: CInt -> IO SV
foreign import ccall "perl5_newSVnv"
    perl5_newSVnv :: CDouble -> IO SV
foreign import ccall "perl_destruct"
    perl_destruct :: Interpreter -> IO CInt
foreign import ccall "perl_free"
    perl_free :: Interpreter -> IO ()
foreign import ccall "perl5_apply"
    perl5_apply :: SV -> SV -> Ptr SV -> CInt -> IO (Ptr SV)
foreign import ccall "perl5_SvTRUE"
    perl5_SvTRUE :: SV -> IO Bool
foreign import ccall "perl5_get_sv"
    perl5_get_sv :: CString -> IO SV
foreign import ccall "perl5_get_cv"
    perl5_get_cv :: CString -> IO SV
