
import Control.Exception (PatternMatchFail(..), throw, catch, evaluate)

import Text.Read (readMaybe)

import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Lazy as L

import System.Environment (getProgName, getArgs, getEnvironment)
import System.IO (hPutStr, stdin, stdout, hSetBinaryMode, hGetContents)
import System.Simple

import Data.String.Conv.Base as CB (Base(..), part, encode, decode)

uerr' :: Int -> String -> IO a
uerr' code msg = die code $ "error: " ++ msg ++ "\n"

uerr :: String -> IO a
uerr = uerr' 1

data Wrap = NoWrap | Wrap Int

io_encode :: Base -> Wrap -> IO ()
io_encode base wrap = do
	hSetBinaryMode stdin True
	hPutStr stdout . out wrap . encode base
		=<< L.hGetContents stdin
  where out NoWrap   res = if null res then res else app_ln res
	out (Wrap n) res = concatMap app_ln $ part n res
	app_ln           = flip (++) "\n"

io_decode :: Base -> Bool -> IO ()
io_decode base ign = do
	hSetBinaryMode stdout True
	either (uerr' 2) (BL.hPutBuilder stdout) . decode base . filter nonign
		=<< hGetContents stdin
  where nonign c = (not ign || c `elem` alph base) && (c /= '\n')

-- c2w = fromIntegral . ord
-- w2c = chr . fromIntegral

numbers, symbols, symbols_lc :: String
numbers    = "0123456789"
symbols    = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
symbols_lc = "abcdefghijklmnopqrstuvwxyz"

named :: String -> Base
named "16"    = Base Nothing    1 2 True  $ take 16 $ numbers ++ symbols
named "32"    = Base (Just '=') 5 8 True  $           symbols ++ "234567"
named "32hex" = Base (Just '=') 5 8 True  $ take 32 $ numbers ++ symbols
named "45"    = Base Nothing    2 3 False $           numbers ++ symbols ++ " $%*+-./:"
named "64"    = Base (Just '=') 3 4 True  $           symbols ++ symbols_lc ++ numbers ++ "+/"
named "64url" = Base (Just '=') 3 4 True  $           symbols ++ symbols_lc ++ numbers ++ "-_"
named "z85"   = Base Nothing    4 5 True  $           numbers ++ symbols_lc ++ symbols ++ ".-:+=^!/*?&<>()[]{}@%$#"
named s = throw $ PatternMatchFail $ "named " ++ s

usage :: IO String
usage = do
	prog <- getProgName
	return $ "\
\usage: " ++ prog ++ " [OPTS] {-d | -e} {-a I:O:ALPH | NAMED}\n\
\\n\
\Options:\n\
\  -a I:O:ALPH  use the ordered ALPH as alphabet, I and O are the input and\n\
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
\As an alternative to '-a I:O:ALPH' the NAMED parameter refers to one of:\n\
\  16           hex encoding (RFC-4648 section 8)\n\
\  32           same as 'base32' program (RFC-4648 section 6)\n\
\  32hex        extended hex alphabet base32 (RFC-4648 section 7)\n\
\  45           base45 encoding (draft-faltstrom-base45-07)\n\
\  64           same as 'base64' program (RFC-4648 section 4)\n\
\  64url        file- and URL-safe base64 (RFC-4648 section 5)\n\
\  z85          ascii85-like encoding (ZeroMQ 32/Z85)\n\
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
parse_int_opt opt label value = maybe (uerr msg) return $ readMaybe value
	where msg = "parameter " ++ label ++ " for option '" ++ opt ++
	            "' is non-numeric: " ++ value

parse_nat_opt :: String -> String -> String -> IO Int
parse_nat_opt opt label value = do
	n <- parse_int_opt opt label value
	if n < 0 then uerr msg else return n
	where msg = "parameter " ++ label ++ " for option '" ++ opt ++
	            "' is less than 0: " ++ value

type Par = (Params, Maybe Char, Maybe Bool)

non_opt :: String -> Par -> IO Par
non_opt x (par,pad,rfc) = do
	base <- catch (evaluate $ named x) $ \(PatternMatchFail _) ->
		uerr $ "unrecognized name '" ++ x ++ "'"
	return (par { base = Just base },pad,rfc)

opts :: [Option (Par -> IO Par)]
opts =
   [ (flag 'a', ArgReq $ \s (par,pad,rfc) -> do
	let (i,oa) = break ((==) ':') s
	let (o,a') = break ((==) ':') $ tail oa
	let a      = tail a'
	i <- parse_nat_opt "-a" "I" i
	o <- parse_nat_opt "-a" "O" o
	if length a < 2
	then uerr "at least 2 symbols are required in ALPH"
	else ret par { base = Just $ Base Nothing i o False a } pad rfc)
  , (flag 'd', NoArg $ \(par,pad,rfc) -> ret par { mode = Just Decode } pad rfc)
  , (flag 'e', NoArg $ \(par,pad,rfc) -> ret par { mode = Just Encode } pad rfc)
  , (flag 'h', NoArg $ \_ -> usage >>= die 0)
  , (flag 'i', NoArg $ \(par,pad,rfc) -> ret par { ign = True } pad rfc)
  , (flag 'p', ArgReq $ \pad (par,_,rfc) ->
	case pad of [p] -> ret par (Just p) rfc
		    _   -> uerr $ "option '-p' requires a single \
		                  \character for padding")
  , (flag 'r', NoArg $ \(par,pad,_) -> ret par pad (Just True))
  , (flag 'w', ArgReq $ \s (par,pad,rfc) -> do
	n <- parse_nat_opt "-w" "COLS" s
	let wrap = if n == 0 then NoWrap else Wrap n
	ret par { wrap = wrap } pad rfc)
  ]
  where ret a b c = return (a,b,c)

parse_argv :: IO Params
parse_argv = do
	env <- getEnvironment
	args <- getArgs
	let def_mode = fmap (const Encode) $ lookup "BASE_COMPAT" env
	let def_params = Params Nothing def_mode (Wrap 76) False

	(par,pad,rfc) <- foldl (>>=) (pure (def_params,Nothing,Nothing)) $
		getopt non_opt opts args

	let do_pad (Just pad) (Just base) = Just base { pad = Just pad }
	    do_pad _ b = b
	    do_rfc (Just rfc) (Just base) = Just base { rfc4648 = rfc }
	    do_rfc _ b = b
	return $ par { base = do_pad pad $ do_rfc rfc $ base par }

main :: IO ()
main = do
	Params base mode wrap ign <- catch parse_argv $ uerr . msg
	base <- maybe (uerr "ALPH not specified") return base
	let run Encode = io_encode base wrap
	    run Decode = io_decode base ign
	maybe (uerr "decode/encode mode not specified") run mode
	exit 0
  where
	msg (UnknownFlag  f) = "unknown option '" ++ show f ++ "'"
	msg (MissingParam f) = "option '" ++ show f ++ "' requires a parameter"
