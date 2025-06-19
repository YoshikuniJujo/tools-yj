{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.C.ByteArray where

import Foreign.Ptr
import Foreign.C.String
import Data.Word
import Data.ByteString qualified as BS

type CByteArray = (Ptr Word8, Int)

fromCStringLen :: CStringLen -> CByteArray
fromCStringLen (p, n) = (castPtr p, n)

toCStringLen :: CByteArray -> CStringLen
toCStringLen (p, n) = (castPtr p, n)

packToByteString :: CByteArray -> IO BS.ByteString
packToByteString = BS.packCStringLen . toCStringLen

useAsFromByteString :: BS.ByteString -> (CByteArray -> IO a) -> IO a
useAsFromByteString bs f = BS.useAsCStringLen bs $ f . fromCStringLen
