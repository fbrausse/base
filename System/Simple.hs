
{- some wrappers around System stuff that is unnecessarily complicated -}

module System.Simple ( Flag(..), ArgSpec(..), Option, ArgsException(..)
                     , getopt, exit_code, die, exit, flag
                     ) where

import Control.Exception (Exception, throw)
import System.IO (hPutStr, stdout, stderr)
import System.Exit hiding (die)

data Flag = Short Char | Long String deriving (Eq)

instance Show Flag where
	show (Short c) = '-':[c]
	show (Long s) = "--" ++ s

data ArgSpec a = NoArg a | ArgReq (String -> a)

type Option a = (Flag, ArgSpec a)

data ArgsException = UnknownFlag Flag
                   | MissingParam Flag
                   deriving (Show)

instance Exception ArgsException

{- unexported class, only 'flag' is,
   see <https://wiki.haskell.org/List_instance> -}
class FlagSpec t where
	{-| Transforms t into a `Flag`. Instances for `Char` and `String` are
	    provided, mapping to the `Short` and `Long` constructors,
	    respectively. -}
	flag :: t -> Flag
	list_flag :: [t] -> Flag

instance FlagSpec Char where
	flag = Short
	list_flag = Long

instance (FlagSpec a) => FlagSpec [a] where
	flag = list_flag
	list_flag = undefined

{-| A simplified version of @System.Console.GetOpt.getOpt@.

    As with POSIX getopt(3), both, fused short flags without parameters (e.g.
    "-xx" in the examples below) as well as a short flag fused with a
    parameter (e.g. "-y5" in the examples below), are supported. Also a single
    "--" element in the argument list terminates flag recognition. In addition
    long flags (e.g. "--version" via `Long` "version") work out of the box.

    Differences to @System.Console.GetOpt.getOpt@:

    - `flag` is a convenience method from the private type class `FlagSpec`
      instantiated on both, `Char` and `String` parameters, and constructing the
      respective `Short` or `Long` `Flag` meant to be used to construct the
      first component of the pair `Option`.

    - The second component of `Option` specifies the action to be invoked for
      the corresponding flag. For `NoArg` flags, it is a constant and for
      `ArgReq` flags it is expected to take the parameter `String` and map it
      to a constant.

    - There is no permutation, options are processed from beginning to end of
      the given list and the corresponding actions are returned, in the same
      order. Any `String` in the list not starting with "-" is passed to the
      default handler given as first argument to `getopt` allowing to parse
      parameters, e.g., "status" in "git status".

    - There also are no flags with optional parameters as that possibility
      introduces ambiguities in usage, which, once observed, can only be worked
      around either by "un-fusing" parameters (which collides with the semantics
      of the default handler) or by shuffling passed flags on invocation, which
      conflicts with context-sensitive option processing negating the use case
      shown in the second example below.

        Optional parameters can usually be avoided by introducing a differently
        named flag. In case no sensible flags are left, it might be worth
        consolidating some of them into a parameter of a new one encompassing
        them.

    - `getopt` throws an `ArgsException` in two cases:

         1. An `ArgReq` flag `f` without a parameter occurs in the list ->
            `MissingParam` `f`.

         2. A flag "-f" occurs in the list, but no `Short` 'f' was given ->
            `UnknownFlag` `$` `Short` 'f'.

         The choice for exceptions was made in order to simplify the usage:
         both, the flag actions, as well as `getopt` itself can throw exceptions
         and caller of it can then transparently handle all of them. Inside
         `getopt` no strict evaluation is performed. {- which means that
         a `catch` around the evaluation of all actions (e.g. 'parse' in the
         second example below or 'evaluate $ parse' in the first) will catch both
         types of exceptions.-}

    - There is no usage information attached to `Option` since formatting of
      a human readable help message according to whatever circumstances
      `getopt` is run in is just beyond the scope: `getopt` as a pure function
      requires no knowledge of the environment simplifying its usage.
      In many cases there is also additional information to be displayed to the
      user on "--help", e.g., grouping of certain flags or dependencies on
      environment variables, which generally is impossible to capture in this
      signature. Thus, for simple cases, it is better to just add an action
      displaying the help message for flag "-h".

    As with @System.Console.GetOpt.getOpt@, both versions of example usage work:

    The first one creates a list of @Prelude.IO@ `Int`, one for each encountered
    flag and evaluates those via @Prelude.sequence@.

> opts :: [Option (IO Int)]
> opts = [(flag 'x', NoArg $ pure 42)
>        ,(flag 'y', ArgReq $ evaluate . read)
>        ]
>
> parse :: IO [Int]
> parse = getArgs >>= sequence . getopt (\_ -> pure 23) opts
>
> -- for ["-x","bla","-y 7"], parse returns [42,23,7] in IO.

    In the second example, each flag yields a function transforming a previous
    state into the next one allowing to easily specify context-sensitivity in
    the definition of each `Option`:

> opts :: [Option (Int -> IO Int)]
> opts = [(flag 'x', NoArg $ \i -> pure $ i+1)
>        ,(flag 'y', ArgReq $ \s i -> evaluate $ i + read s)
>        ]
>
> parse :: IO Int
> parse = do
> 	args <- getArgs
> 	foldl (>>=) (pure 5) $ getopt (\_ i -> pure i) opts args
>
> -- parse ["-x","bla","-y 7"], parse returns 13 in IO.

    In both examples here the return type for the flags was chosen to be
    @Prelude.IO@ `Int`
    in order to accomodate some potential I/O informing the user about
    non-expected parameters or invalid options in the current state.
 -}
getopt :: (String -> a) -> [Option a] -> [String] -> [a]
getopt uh opts args = parse args []
  where na (NoArg  _) = 0
	na (ArgReq _) = 1

	fa (NoArg a)  = const a
	fa (ArgReq a) = a . head

	parse [] ac = ac
	parse ("--":xs) ac = ac ++ map uh xs
	parse (('-':'-':os):xs) ac =
		let a = lookup_just (Long os) opts in
		interp (Long os) a ac xs
	parse (('-':o:os):xs) ac =
		let a = lookup_just (Short o) opts in
		interp (Short o) a ac $ short_args (na a) os xs
	parse (x:xs) ac = parse xs (ac ++ [uh x])

	short_args :: Int -> String -> [String] -> [String]
	short_args _ []  xs = xs
	short_args 0 rem xs = (('-':rem):xs)
	short_args _ rem xs = (rem:xs)

	interp f a ac xs = if length xs >= na a
	                   then let (as,xs') = splitAt (na a) xs in
	                        parse xs' (ac ++ [fa a as])
	                   else throw $ MissingParam f

	lookup_just f l = maybe (throw $ UnknownFlag f) id $ lookup f l

{-| Simple mapping of '0' to `ExitSuccess` and anything else to `ExitFailure`.
 -}
exit_code :: Int -> ExitCode
exit_code 0 = ExitSuccess
exit_code n = ExitFailure n

{-| Terminates the program with the given exit code. -}
exit :: Int -> IO a
exit = exitWith . exit_code

{-| Prints the given string to `stdout` or `stderr` depending on whether the
    given exit code is zero or not before terminating the program with that
    code. -}
die :: Int -> String -> IO a
die code msg = do
	let hdl = if code == 0 then stdout else stderr
	hPutStr hdl msg
	exit code
