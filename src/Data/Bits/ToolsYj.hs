{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Bits.ToolsYj (checkBits, bitsList) where

import Data.Bits

checkBits :: Bits bs => bs -> bs -> Bool
checkBits wnt = (== wnt) . (.&. wnt)

bitsList :: FiniteBits bs => bs -> [bs]
bitsList bs =
	filter (/= zeroBits) $ map ((bs .&.) . bit) [0 .. finiteBitSize bs - 1]
