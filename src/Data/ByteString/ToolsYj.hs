{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.ToolsYj (

	fromBits, fromBits', toBits, toBits',

	fromBitsBE', toBitsBE

	) where

import Data.Bits
import Data.Bits.ToolsYj
import Data.Bool
import Data.ByteString qualified as BS

fromBits :: Bits b => b -> BS.ByteString
fromBits = BS.unfoldr \b ->
	bool Nothing (Just (bitsToBits 8 b, b `shiftR` 8)) (b /= zeroBits)

fromBits' :: FiniteBits b => b -> BS.ByteString
fromBits' b0 = go (finiteBitSize b0 `div` 8) b0
	where
	go 0 _ = ""
	go n b = bitsToBits 8 b `BS.cons` go (n - 1) (b `shiftR` 8)

toBits :: Bits b => BS.ByteString -> b
toBits = BS.foldr (\b s -> bitsToBits 8 b .|. s `shiftL` 8) zeroBits

toBits' :: forall b . FiniteBits b => BS.ByteString -> Maybe b
toBits' bs = bool
	Nothing
	(Just $ BS.foldr (\b s -> bitsToBits 8 b .|. s `shiftL` 8) zeroBits bs)
	(8 * BS.length bs <= finiteBitSize @b undefined)

toBitsBE :: Bits b => BS.ByteString -> b
toBitsBE = BS.foldl (\s b -> bitsToBits 8 b .|. s `shiftL` 8) zeroBits

fromBitsBE' :: FiniteBits b => b -> BS.ByteString
fromBitsBE' b0 = go (finiteBitSize b0 `div` 8) b0
	where
	go 0 _ = ""
	go n b = go (n - 1) (b `shiftR` 8) `BS.snoc` bitsToBits 8 b
