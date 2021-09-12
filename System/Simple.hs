
{- some wrappers around System stuff that is unnecessarily complicated -}

module System.Simple ( Flag(..), ArgSpec(..), Option, ArgsException(..)
                     , getopt, exit_code, die, exit
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

getopt :: (String -> a) -> [Option a] -> [String] -> [a]
getopt uh opts args = parse' args []
  where na (NoArg  _) = 0
	na (ArgReq _) = 1

	fa (NoArg a)  = const a
	fa (ArgReq a) = a

	parse' [] ac = ac
	parse' ("--":xs) ac = ac ++ map uh xs
	parse' (('-':'-':os):xs) ac =
		let a = lookup_just (Long os) opts in
		interp (Long os) a ac xs
	parse' (('-':o:os):xs) ac =
		let a = lookup_just (Short o) opts in
		interp (Short o) a ac $ short_args (na a) os xs
	parse' (x:xs) ac = parse' xs (ac ++ [uh x])

	short_args :: Int -> String -> [String] -> [String]
	short_args _ []  xs = xs
	short_args 0 rem xs = (('-':rem):xs)
	short_args _ rem xs = (rem:xs)

	interp f a ac xs = if length xs >= na a
	                   then let (as,xs') = splitAt (na a) xs in
	                        parse' xs' (ac ++ [fa a $ head as])
	                   else throw $ MissingParam f

	lookup_just f l = case lookup f l of Nothing -> throw $ UnknownFlag f
	                                     Just x -> x

exit_code :: Int -> ExitCode
exit_code 0 = ExitSuccess
exit_code n = ExitFailure n

exit :: Int -> IO a
exit = exitWith . exit_code

die :: Int -> String -> IO a
die code msg = do
	let hdl = if code == 0 then stdout else stderr
	hPutStr hdl msg
	exit code
