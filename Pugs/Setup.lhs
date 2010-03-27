#!/usr/bin/env runghc
> import Distribution.Simple
> import System.Cmd (rawSystem)
> 
> main :: IO ()
> main = writeBuildInfo >> defaultMainWithHooks defaultUserHooks
>     where
>     writeBuildInfo = rawSystem "perl" ["Configure.PL"]
