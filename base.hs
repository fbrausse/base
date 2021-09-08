
import Control.Exception

import Text.Read (readMaybe)

import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Lazy as L

import System.Environment (getProgName, getArgs, getEnvironment)
import System.Exit hiding (die)
import System.IO (hPutStr, stdin, stdout, stderr, hSetBinaryMode, hGetContents)

import Data.String.Conv.Base as CB (Base(..), part, encode, decode)

data Wrap = NoWrap | Wrap Int

io_encode :: Base -> Wrap -> IO ()
io_encode base wrap = do
	hSetBinaryMode stdin True
	buf <- L.hGetContents stdin
	let res = encode base buf
	hPutStr stdout $
		case wrap of
			NoWrap -> if null res then res else app_ln res
			Wrap n -> concatMap app_ln $ part n res
		where app_ln = flip (++) "\n"

io_decode :: Base -> Bool -> IO ()
io_decode base ign = do
	hSetBinaryMode stdout True
	txt <- hGetContents stdin
	let ftxt = filter (\c -> (not ign || c `elem` alph base) && (c /= '\n')) txt
	case decode base ftxt of
		Left err -> die 2 $ "error: " ++ err ++ "\n"
		Right bl -> BL.hPutBuilder stdout bl

-- c2w = fromIntegral . ord
-- w2c = chr . fromIntegral

numbers, symbols, symbols_lc :: String
numbers    = "0123456789"
symbols    = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
symbols_lc = "abcdefghijklmnopqrstuvwxyz"

pad_eq :: Char
pad_eq     = '='

-- incomplete pattern matching, failure is caught and handled in parse_args below
named :: String -> Base
named "16"    = Base Nothing         1 2 True  $ take 16 $ numbers ++ symbols
named "32"    = Base (Just $ pad_eq) 5 8 True  $           symbols ++ "234567"
named "32hex" = Base (Just $ pad_eq) 5 8 True  $ take 32 $ numbers ++ symbols
named "45"    = Base Nothing         2 3 False $           numbers ++ symbols ++ " $%*+-./:"
named "64"    = Base (Just $ pad_eq) 3 4 True  $           symbols ++ symbols_lc ++ numbers ++ "+/"
named "64url" = Base (Just $ pad_eq) 3 4 True  $           symbols ++ symbols_lc ++ numbers ++ "-_"
named _ = throw $ PatternMatchFail "named"

{- some wrappers around System.Exit stuff that is unnecessarily complicated -}
exit_code :: Int -> ExitCode
exit_code 0 = ExitSuccess
exit_code n = ExitFailure n

die :: Int -> String -> IO a
die code msg = do
	let hdl = if code == 0 then stdout else stderr
	hPutStr hdl msg
	exitWith $ exit_code code

usage :: IO String
usage = do
	prog <- getProgName
	return $ "\
\usage: " ++ prog ++ " [OPTS] {-d|-e} {-a ALPH I O|NAMED} [-p PAD]\n\
\\n\
\Options:\n\
\  -a ALPH I O  use the ordered ALPH as alphabet, I and O are the input and\n\
\               output group lengths in bytes and characters, respectively\n\
\  -d           decode from base-ALPH\n\
\  -e           encode to base-ALPH\n\
\  -i           ignore non-alphabet characters in input\n\
\  -p PAD       use PAD as padding character\n\
\  -r           RFC-4648 interpretation of chunks to encode\n\
\  -w COLS      wrap encoded lines after COLS characters (default 76);\n\
\               use 0 to disable line wrapping\n\
\\n\
\As an alternative to '-a ALPH I O' the NAMED parameter refers to one of:\n\
\  16           hex encoding (RFC-4648 section 8)\n\
\  32           same as 'base32' program (RFC-4648 section 6)\n\
\  32hex        extended hex alphabet base32 (RFC-4648 section 7)\n\
\  45           base45 encoding (draft-faltstrom-base45-07)\n\
\  64           same as 'base64' program (RFC-4648 section 4)\n\
\  64url        file- and URL-safe base64 (RFC-4648 section 5)\n\
\\n\
\Environment variables affecting the behaviour:\n\
\  BASE_COMPAT  if set, the default mode is to encode; otherwise there is no\n\
\               default\n\
\\n\
\Written by Franz Brauße <fb@paxle.org>; License: BSD-2\n"

data Mode = Encode | Decode

data Params = Params { base :: Maybe Base
                     , mode :: Maybe Mode
                     , wrap :: Wrap
                     , ign :: Bool
                     }

parse_int_opt :: String -> String -> String -> IO Int
parse_int_opt opt label value =
	case readMaybe value of
		Nothing -> die 1 $ "error: parameter " ++ label ++
		                   " for option '" ++ opt ++
		                   "' is non-numeric: " ++ value ++ "\n"
		Just n -> return n

parse_nat_opt :: String -> String -> String -> IO Int
parse_nat_opt opt label value = do
	n <- parse_int_opt opt label value
	if n < 0
	then
		die 1 $ "error: parameter " ++ label ++ " for option '" ++
		        opt ++ "' is less than 0: " ++ value ++ "\n"
	else
		return n

parse_args :: Maybe Base
           -> Maybe Mode
           -> Maybe Char
           -> Maybe Bool
           -> Wrap
           -> Bool
           -> [String]
           -> IO Params

parse_args _ _ _ _ _ _ ("-h":_) = usage >>= die 0

parse_args alph _    pad rfc wrap ign ("-e":xs) =
	parse_args alph (Just Encode) pad rfc wrap ign xs
parse_args alph _    pad rfc wrap ign ("-d":xs) =
	parse_args alph (Just Decode) pad rfc wrap ign xs
parse_args _    mode pad rfc wrap ign ("-a":alph:i:o:xs) = do
	ni <- parse_nat_opt "-a" "I" i
	no <- parse_nat_opt "-a" "O" o
	parse_args (Just $ Base Nothing ni no False alph)
	           mode pad rfc wrap ign xs
parse_args _ _ _ _ _ _ ("-a":_) =
	die 1 $ "error: option '-a' requires parameters ALPH, I and O\n"

parse_args base mode _ rfc wrap ign ("-p":[p]:xs) =
	parse_args base mode (Just p) rfc wrap ign xs
parse_args _ _ _ _ _ _ ["-p"] =
	die 1 $ "error: option '-p' requires parameter PAD\n"
parse_args _ _ _ _ _ _ ("-p":_) =
	die 1 $ "error: option '-p' requires a single byte character for padding\n"

parse_args base mode pad _ wrap ign ("-r":xs) =
	parse_args base mode pad (Just True) wrap ign xs

parse_args base mode pad rfc _ ign ("-w":s:xs) = do
	n <- parse_nat_opt "-w" "COLS" s
	let wrap = if n == 0 then NoWrap else Wrap n in
		parse_args base mode pad rfc wrap ign xs
parse_args _ _ _ _ _ _ ["-w"] =
	die 1 $ "error: option '-w' requires a numeric parameter COLS >= 0\n"

parse_args base mode pad rfc wrap _ ("-i":xs) =
	parse_args base mode pad rfc wrap True xs

parse_args _    mode pad rfc wrap ign (x:xs) = do
	base <- catch (evaluate $ named x) $ \(PatternMatchFail _) ->
		die 1 $ "error: unrecognized option '" ++ x ++ "'\n"
	parse_args (Just base) mode pad rfc wrap ign xs

parse_args (Just base) mode (Just p) rfc wrap ign [] | pad base == Nothing =
	parse_args (Just base { pad = Just p }) mode Nothing rfc wrap ign []

parse_args (Just base) mode pad (Just rfc) wrap ign [] | rfc4648 base == False =
	parse_args (Just base { rfc4648 = rfc }) mode pad Nothing wrap ign []

parse_args base mode _ _ wrap ign [] =
	return $ Params base mode wrap ign

parse_argv :: IO Params
parse_argv = do
	env <- getEnvironment
	args <- getArgs
	let def_mode = fmap (const Encode) $ lookup "BASE_COMPAT" env
	parse_args Nothing def_mode Nothing Nothing (Wrap 76) False args

main :: IO ()
main = do
	Params base mode wrap ign <- parse_argv
	base <- case base of
		Nothing ->
			die 1 $ "error: ALPH not specified\n"
		Just base | length (alph base) < 2 ->
			die 1 $ "error: at least 2 symbols are required in ALPH\n"
		Just base | otherwise ->
			return base
	case mode of Nothing -> die 1 $ "error: decode/encode mode not specified\n"
	             Just Encode -> io_encode base wrap
	             Just Decode -> io_decode base ign
	exitWith $ exit_code 0
