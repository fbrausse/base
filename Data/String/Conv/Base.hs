
module Data.String.Conv.Base ( Splittable(..)
                             , part
                             , Base(..)
                             , in_base
                             , encode
                             , decode
                             ) where

import Prelude hiding (splitAt, null)

import Data.Maybe
import Data.List (elemIndex)
import qualified Data.List (splitAt, null)

import Data.Word (Word8)

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Short as S
import qualified Data.ByteString.Lazy as L

class Splittable a where splitAt :: Int -> a -> (a, a)
                         null :: a -> Bool

instance Splittable [a] where splitAt = Data.List.splitAt
                              null    = Data.List.null
instance Splittable B.ByteString where splitAt = B.splitAt
                                       null    = B.null
instance Splittable L.ByteString where splitAt = L.splitAt . fromIntegral
                                       null    = L.null

part :: (Splittable a) => Int -> a -> [a]
part n l
	| null l = []
	| otherwise = let (x,xs) = splitAt n l in (x : part n xs)

data Base = Base { pad :: Maybe Char -- additional padding
                 , igr :: Int -- input group size in bytes
                 , ogr :: Int -- output group size in characters
                 , rfc4648 :: Bool
                 , alph :: String -- decoder alphabet
                 }

log' :: Int -> Int -> Int
log' b x = acc 1
	where acc :: Integer -> Int
	      acc a = if fromIntegral x <= a
	              then 0
	              else 1 + acc (a * fromIntegral b)

in_base :: Integral t => t -> t -> [t]
in_base b v = let (q,r) = quotRem v b in r : in_base b q

interpret_chunk :: Base -> [Word8] -> [Int]
interpret_chunk base u
	| rfc4648 base = reverse . f . take (igr base) $ u ++ repeat 0
	| otherwise    = f u
  where
	eval acc b = acc * 256 + fromIntegral b
	n          = length $ alph base
	f s        = take (ogr base) . in_base n $ foldl eval 0 s

encode_chunk :: Base -> L.ByteString -> String
encode_chunk base x | null x    = []
                    | otherwise =
	let n   = fromIntegral $ L.length x
	    k   = log' (length $ alph base) (256 ^ n)
	    ev  = take k $ interpret_chunk base $ L.unpack x
	    suf = case pad base of Just c | n < igr base -> repeat c
	                           _      | otherwise    -> []
	in take (ogr base) $ map ((!!) $ alph base) ev ++ suf

encode :: Base -> L.ByteString -> String
encode base buf = concatMap (encode_chunk base) $ part (igr base) buf

decode_chunk :: Base -> [Int] -> [Word8]
decode_chunk base ch | rfc4648 base =
	take k . reverse . to_bytes
	       . foldl eval 0 . take (ogr base) $ ch ++ repeat 0
                     | otherwise =
	reverse . take k . to_bytes . foldr (flip eval) 0 $ ch
  where
	n            = length $ alph base
	eval acc x   = x + acc * n
	to_bytes v   = map fromIntegral $ take (igr base) $ in_base 256 v
	full_info_ch = length ch - if length ch < ogr base then 1 else 0
	k            = log' 256 (n ^ full_info_ch)

decode_idcs :: Base -> [Int] -> BL.Builder
decode_idcs base idcs =
	mconcat $ map (BL.shortByteString . S.pack . decode_chunk base) $
		part (ogr base) idcs

strip_padded_suf :: Base -> String -> Either String String
strip_padded_suf base txt =
	case pad base of
		Nothing -> Right txt
		Just p -> case elemIndex p txt of
			Nothing -> Right txt
			Just i ->
				let (pre,suf) = splitAt i txt in
				if all (\c -> c == p) suf
				then Right pre
				else Left "intermediate padding characters not at end of input"

decode :: Base -> String -> Either String BL.Builder
decode base txt = do
	stxt <- strip_padded_suf base txt
	let idcs = map (\c -> elemIndex c $ alph base) stxt
	if any isNothing idcs
	then Left "non-alphabet character(s) in input"
	else Right $ decode_idcs base $ catMaybes idcs
