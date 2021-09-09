
import Control.Exception

import Text.Read (readMaybe)
import Data.List (partition, intercalate)

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
named s = throw $ PatternMatchFail $ "named " ++ s

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
\usage: " ++ prog ++ " [OPTS] {-d | -e} {-a ALPH I O | NAMED} [-p PAD]\n\
\\n\
\Options:\n\
\  -a ALPH I O  use the ordered ALPH as alphabet, I and O are the input and\n\
\               output group lengths in bytes and characters, respectively\n\
\  -d           decode from base-ALPH\n\
\  -e           encode to base-ALPH\n\
\  -h           display this help message\n\
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

data Flag = Short Char | Long String deriving (Eq)

instance Show Flag where
	show (Short c) = '-':[c]
	show (Long s) = "--" ++ s

data Option a = Option Flag [String] ([String] -> a)

data ArgsException = UnknownFlag Flag
                   | MissingParams Flag [String]
                   deriving (Show)

instance Exception ArgsException

parse :: [Option a] -> [String] -> (String -> a) -> [a]
parse opts args uh = parse' args []
  where is_short f = case f of Short _ -> True ; _ -> False
	(short,long) = partition (is_short . fst) [(f,(p,e)) | Option f p e <- opts]

	parse' [] ac = ac
	parse' ("--":xs) ac = ac ++ map uh xs
	parse' (('-':'-':os):xs) ac =
		let (p,e) = lookup_just (Long os) long in
		interp (Long os) p e ac xs
	parse' (('-':o:os):xs) ac =
		let (p,e) = lookup_just (Short o) short in
		interp (Short o) p e ac $ short_args (length p) os xs
	parse' (x:xs) ac = parse' xs (ac ++ [uh x])

	short_args _ []  xs = xs
	short_args 0 rem xs = (('-':rem):xs)
	short_args _ rem xs = (rem:xs)

	interp f p e ac xs = if length xs >= length p
	                     then let (as,xs') = splitAt (length p) xs in
	                          parse' xs' (ac ++ [e as])
	                     else throw $ MissingParams f $ drop (length xs) p

	lookup_just f l = case lookup f l of Nothing -> throw $ UnknownFlag f
	                                     Just x -> x


opts :: [Option ((Params, Maybe Char, Maybe Bool) -> IO (Params, Maybe Char, Maybe Bool))]
opts = [ Option (Short 'a') ["ALPH","I","O"] (\(alph:i:[o]) (par,pad,rfc) -> do
		i <- parse_nat_opt "-a" "I" i
		o <- parse_nat_opt "-a" "O" o
		return (par { base = Just $ Base Nothing i o False alph },pad,rfc))
       , Option (Short 'd') [] (\_ (par,pad,rfc) -> return (par { mode = Just Decode },pad,rfc))
       , Option (Short 'e') [] (\_ (par,pad,rfc) -> return (par { mode = Just Encode },pad,rfc))
       , Option (Short 'h') [] (\_ _ -> usage >>= die 0)
       , Option (Short 'i') [] (\_ (par,pad,rfc) -> return (par { ign = True },pad,rfc))
       , Option (Short 'p') ["PAD"] (\[pad] (par,_,rfc) ->
		case pad of [p] -> return (par,Just p,rfc)
		            _   -> die 1 $ "error: option '-p' requires a \
		                           \single character for padding\n")
       , Option (Short 'r') [] (\_ (par,pad,_) -> return (par,pad,Just True))
       , Option (Short 'w') ["COLS"] (\[s] (par,pad,rfc) -> do
		n <- parse_nat_opt "-w" "COLS" s
		let wrap = if n == 0 then NoWrap else Wrap n
		return (par { wrap = wrap },pad,rfc))
       ]

{-
parse_args :: Params
           -> Maybe Char -- pad
           -> Maybe Bool -- rfc
           -> [String]
           -> IO Params

parse_args _ _ _ ("-h":_) = usage >>= die 0

parse_args par pad rfc ("-e":xs) =
	parse_args (par { mode = Just Encode }) pad rfc xs
parse_args par pad rfc ("-d":xs) =
	parse_args (par { mode = Just Decode }) pad rfc xs
parse_args par pad rfc ("-a":alph:i:o:xs) = do
	i <- parse_nat_opt "-a" "I" i
	o <- parse_nat_opt "-a" "O" o
	parse_args (par { base = Just $ Base Nothing i o False alph }) pad rfc xs
parse_args _ _ _ ("-a":_) =
	die 1 $ "error: option '-a' requires parameters ALPH, I and O\n"

parse_args par _ rfc ("-p":[p]:xs) =
	parse_args par (Just p) rfc xs
parse_args _ _ _ ["-p"] =
	die 1 $ "error: option '-p' requires parameter PAD\n"
parse_args _ _ _ ("-p":_) =
	die 1 $ "error: option '-p' requires a single byte character for padding\n"

parse_args par pad _ ("-r":xs) =
	parse_args par pad (Just True) xs

parse_args par pad rfc ("-w":s:xs) = do
	n <- parse_nat_opt "-w" "COLS" s
	let wrap = if n == 0 then NoWrap else Wrap n
	parse_args (par { wrap = wrap }) pad rfc xs
parse_args _ _ _ ["-w"] =
	die 1 $ "error: option '-w' requires a numeric parameter COLS >= 0\n"

parse_args par pad rfc ("-i":xs) =
	parse_args (par { ign = True }) pad rfc xs

parse_args par pad rfc (x:xs) = do
	base <- catch (evaluate $ named x) $ \(PatternMatchFail _) ->
		die 1 $ "error: unrecognized option '" ++ x ++ "'\n"
	parse_args (par { base = Just base }) pad rfc xs

parse_args par@(Params (Just base) _ _ _) (Just p) rfc [] | pad base == Nothing =
	parse_args (par { base = Just base { pad = Just p } }) Nothing rfc []

parse_args par@(Params (Just base) _ _ _) pad (Just rfc) [] | rfc4648 base == False =
	parse_args (par { base = Just base { rfc4648 = rfc } }) pad Nothing []

parse_args par _ _ [] = return par
-}

parse_argv :: IO Params
parse_argv = do
	env <- getEnvironment
	args <- getArgs
	let def_mode = fmap (const Encode) $ lookup "BASE_COMPAT" env
	let def_params = Params Nothing def_mode (Wrap 76) False
	let uh x (par,pad,rfc) = do
		base <- catch (evaluate $ named x) $ \(PatternMatchFail _) ->
			die 1 $ "error: unrecognized name '" ++ x ++ "'\n"
		return (par { base = Just base },pad,rfc)
	ac <- catch (evaluate $ parse opts args uh) $ \ex -> case ex of
		UnknownFlag f -> die 1 $ "error: unknown option '" ++ show f ++ "'\n"
		MissingParams f p -> die 1 $ "error: parameters " ++ intercalate ", " p ++
		                             " of option '" ++ show f ++ "' missing\n"
	(par,pad,rfc) <- foldl (>>=) (pure (def_params,Nothing,Nothing)) ac
	let do_pad (Just pad) (Just base) = Just base { pad = Just pad }
	    do_pad _ b = b
	    do_rfc (Just rfc) (Just base) = Just base { rfc4648 = rfc }
	    do_rfc _ b = b
	-- parse_args def_params Nothing Nothing args
	return $ par { base = do_pad pad $ do_rfc rfc $ base par }

main :: IO ()
main = do
	Params base mode wrap ign <- parse_argv
	base <- case base of
		Nothing ->
			die 1 $ "error: ALPH not specified\n"
		Just base | length (alph base) < 2 ->
			die 1 $ "error: at least 2 symbols are required in ALPH\n"
		          | otherwise ->
			return base
	case mode of Nothing -> die 1 $ "error: decode/encode mode not specified\n"
	             Just Encode -> io_encode base wrap
	             Just Decode -> io_decode base ign
	exitWith $ exit_code 0
