{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.Lazy.ToolsYj (
	splitAt' ) where

import Data.Int
import Data.ByteString.Lazy qualified as LBS

splitAt' :: Int64 -> LBS.ByteString -> Maybe (LBS.ByteString, LBS.ByteString)
splitAt' n bs
	| LBS.length bs < n = Nothing
	| otherwise = Just $ LBS.splitAt n bs
