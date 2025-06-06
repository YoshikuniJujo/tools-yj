{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Sequence.ToolsYj (splitAt') where

import Data.Sequence qualified as Seq

splitAt' :: Int -> Seq.Seq a -> Maybe (Seq.Seq a, Seq.Seq a)
splitAt' n s
	| Seq.length s < n = Nothing
	| otherwise = Just $ Seq.splitAt n s
