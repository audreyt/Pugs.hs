{-# LANGUAGE ExtendedDefaultRules #-}
import Language.Perl5

main :: IO ()
main = withPerl5 $ do
    _Test_  <- use "Test"

    "plan".!("tests", 11)

    x1 <- "crypt".:("salt", "plaintext")
    x2 <- "crypt".:("salt", "plaintext") :: IO String

    "ok".![x1, x2]

    ver <- _Test_.$"VERSION"$(1.00)
    "ok".!(ver :: String)

    code <- eval "sub { ok 1 }" :: IO (IO ())
    code

    code' <- eval "sub { ok $_[0] }" :: IO (Int -> IO ())
    code' 123

    foo <- eval "sub { $_[0]->() }" :: IO (IO () -> IO ())
    foo (putStrLn "ok 5")

    foo <- eval "sub { print $_[0]->(q[ok]) }" :: IO ((String -> IO String) -> IO ())
    foo (\x -> return (x ++ " 6\n"))

    foo <- eval "sub { print $_[0]->(q[ok]) }" :: IO ((String -> String) -> IO ())
    foo (++ " 7\n")

    foo <- eval "sub { print $_[0]->(q[ok], qq[ 8\n]) }" :: IO ((String -> String -> String) -> IO ())
    foo (++)

    say <- eval "sub { print qq[$_[0] $_[1]\n] }" :: IO (String -> Int -> IO ())
    say "ok" 9

    rec <- eval "sub { $_[0]->(sub { $_[0]->() }) }"
    rec (($ (putStrLn "ok 10")) :: (IO () -> IO ()) -> IO ()) :: IO ()

    eval_ "sub my_ok { print qq[ok @_\n] }"
    "my_ok".!11

dbiDemo :: IO ()
dbiDemo = do
    _DBI_ <- use "DBI"

    dbh <- _DBI_.$"connect"$"dbi:SQLite:foo"
    dbh.$!"do"$"CREATE TABLE foo (id, moose)"
