{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Data.BAM.Version1_6.Internal
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this library are expected to track development
-- closely.
--
-- All credit goes to the author(s)/maintainer(s) of the
-- [containers](https://hackage.haskell.org/package/containers) library
-- for the above warning text.
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.Internal ( -- * BAM version 1.6 internal functions
                                      bytestringToFloatLE
                                    , byteStringToIntLE 
                                    , byteStringToWord16LE
                                    , floatToByteStringLE
                                    , intToByteStringLE
                                    , intToWord16LE
                                    , intToWord32LE
                                    , int8ToByteString
                                    , int16ToByteStringLE
                                    , int32ToByteStringLE
                                    , maybeOption
                                    , splitByteString
                                    , word8sToInt8LE
                                    , word8sToInt16LE
                                    , word8sToInt32LE
                                    , word8sToWord8LE
                                    , word8sToWord16LE
                                    , word8sToWord32LE
                                    , word32ToByteStringLE
                                    , word16ToByteStringLE
                                    , word16ToByteString
                                    , word16ToIntLE
                                    ) where

import Data.Attoparsec.ByteString.Lazy
import Data.Binary.Get                 (runGet,getFloatle)
import Data.Binary.Put                 (runPut,putFloatle,putInt64le)
import Data.Bits
import Data.ByteString as DB
import Data.ByteString.Builder
import Data.Int
import Data.Word

-- | Make a parser optional, return Nothing if there is no match.
maybeOption :: Parser a
            -> Parser (Maybe a)
maybeOption p =
  option Nothing
         (Just <$> p)

-- | Convert a little-endian Word16 to an Int.
word16ToIntLE :: Word16
              -> Int
word16ToIntLE w = do
  let signedValue = fromIntegral w :: Int
      -- Check if the most significant bit (sign bit) is set
      isNegative = testBit signedValue 15
  case isNegative of
    True  ->
      signedValue - 65536  -- If negative, subtract 2^16
    False ->
      signedValue

-- | Split a ByteString into chunks of n bytes.
splitByteString :: Int
                -> ByteString
                -> [ByteString]
splitByteString n bs =
  case (DB.null bs) of
    True  ->
      []
    False ->
      case (DB.length bs `mod` n /= 0) of
        True  ->
          error $
            "ByteString length must be a multiple of " ++
            (show n)                                   ++
            "."
        False ->
          let (chunk,rest) = DB.splitAt n
                                        bs
            in chunk : splitByteString n
                                       rest

-- | Convert a ByteString to a Float (IEEE 754 binary32).
bytestringToFloatLE :: ByteString
                    -> Float
bytestringToFloatLE bs =
  case (DB.length bs == 4) of
    True  ->
      runGet getFloatle $
        fromStrict bs
    False ->
      error "ByteString must contain exactly 4 bytes"

-- | Convert a ByteString (little-endian) to an Int.
byteStringToIntLE :: ByteString
                  -> Int
byteStringToIntLE bs =
  Prelude.foldr go 0 (Prelude.zip [0..]
                                  (DB.unpack bs)
                     )
  where
    go :: (Int,Word8)
       -> Int
       -> Int
    go (i,b)
       acc = acc
             .|.
             ( fromIntegral b
               `shiftL`
               (8 * i)
             )

-- | Convert a ByteString to [Word16] in little-endian format.
byteStringToWord16LE :: ByteString
                     -> [Word16]
byteStringToWord16LE bs
  | DB.length bs `mod` 2 /= 0 = error "ByteString must have an even length."
  | otherwise = go bs
                   []
  where
    go :: ByteString
       -> [Word16]
       -> [Word16]
    go input acc
      | DB.null input = Prelude.reverse acc -- Stop processing when no bytes are left
      | otherwise     =
          let byte1  = fromIntegral (DB.index input 0) :: Word16 -- LSB
              byte2  = fromIntegral (DB.index input 1) :: Word16 -- MSB
              word16 = byte1 .|. (byte2 `shiftL` 8) -- Combine bytes in little-endian order
            in go ( DB.drop 2
                            input
                  )
                  ( word16 : acc
                  ) -- Recursively process the rest

-- | Convert a [Word8] to a Int8 (little endian).
word8sToInt8LE :: [Word8]
               -> Int8
word8sToInt8LE [ b0
               ] = fromIntegral b0
word8sToInt8LE _ = error "List must contain exactly 1 Word8 element."

-- | Convert a [Word8] to a Int16 (little endian).
word8sToInt16LE :: [Word8]
                -> Int16
word8sToInt16LE [ b0
                , b1
                ] = 
  ( fromIntegral b0
  ) .|.
  ( fromIntegral b1
    `shiftL`
    8
  )
word8sToInt16LE _ = error "List must contain exactly 2 Word8 elements."

-- | Convert a [Word8] to a Int32 (little endian).
word8sToInt32LE :: [Word8]
              -> Int32
word8sToInt32LE [ b0
                , b1
                , b2
                , b3
                ] = 
  (fromIntegral b0
  ) .|.
  ( fromIntegral b1
    `shiftL`
    8
  ) .|.
  ( fromIntegral b2
    `shiftL`
    16
  ) .|.
  ( fromIntegral b3
    `shiftL`
    24
  )
word8sToInt32LE _ = error "List must contain exactly 4 Word8 elements."

-- | Convert a [Word8] to a Word8 (little endian).
word8sToWord8LE :: [Word8]
                -> Word8
word8sToWord8LE [ b0
                ] = b0
word8sToWord8LE _ = error "List must contain exactly 1 Word8 element."

-- | Convert a [Word8] to a Word16 (little endian).
word8sToWord16LE :: [Word8]
                 -> Word16
word8sToWord16LE [ b0
                 , b1
                 ] = 
  ( fromIntegral b0
  ) .|.
  ( fromIntegral b1
    `shiftL`
    8
  )
word8sToWord16LE _ = error "List must contain exactly 2 Word8 elements."

-- | Convert a [Word8] to a Word32 (little endian).
word8sToWord32LE :: [Word8]
                 -> Word32
word8sToWord32LE [ b0
                 , b1
                 , b2
                 , b3
                 ] =
  ( fromIntegral b0
  ) .|.
  ( fromIntegral b1
    `shiftL`
    8
  ) .|.
  ( fromIntegral b2
    `shiftL`
    16
  ) .|.
  ( fromIntegral b3
    `shiftL`
    24
  )
word8sToWord32LE _ = error "List must contain exactly 4 Word8 elements."

-- Convert Word16 to ByteString.
word16ToByteString :: Word16
                   -> ByteString
word16ToByteString w =
  DB.pack [ fromIntegral (w `shiftR` 8)
          , fromIntegral (w .&. 0xFF)
          ]

-- | Convert a Word16 in little endian
-- byte order to a ByteString.
word16ToByteStringLE :: Word16
                     -> ByteString
word16ToByteStringLE w =
  DB.toStrict        $
    toLazyByteString $
      word16LE w

-- | Convert a Word32 in little endian
-- byte order to a ByteString.
word32ToByteStringLE :: Word32
                     -> ByteString
word32ToByteStringLE w =
  DB.toStrict        $
    toLazyByteString $
      word32LE w

-- | Convert a Int in little endian
-- byte order to a ByteString.
intToByteStringLE :: Int
                  -> ByteString
intToByteStringLE i =
  DB.toStrict    $
    runPut       $
      putInt64le $
        fromIntegral i

-- | Convert an Int in little-endian
-- byte order to a Word16.
intToWord16LE :: Int
              -> Word16
intToWord16LE i
  | i >= 0 && i <= 0xFFFF = fromIntegral i
  | otherwise             = error "Int value is out of Word16 range"

-- | Convert an Int little-endian
-- byte order to a Word32.
intToWord32LE :: Int
              -> Word32
intToWord32LE i
  | i >= 0    = fromIntegral i
  | otherwise = error "Int value must be non-negative"

-- | Convert a Int8 in little endian
-- byte order to a ByteString.
int8ToByteString :: Int8
                 -> ByteString
int8ToByteString i =
  DB.toStrict        $
    toLazyByteString $
      int8 i

-- | Convert a Int16 in little endian
-- byte order to a ByteString.
int16ToByteStringLE :: Int16
                    -> ByteString
int16ToByteStringLE i =
  DB.toStrict        $
    toLazyByteString $
      int16LE i

-- | Convert a Int32 in little endian
-- byte order to a ByteString.
int32ToByteStringLE :: Int32
                    -> ByteString
int32ToByteStringLE i =
  DB.toStrict        $
    toLazyByteString $
      int32LE i

-- | Convert a Float in little endian
-- byte order to a ByteString.
-- See https://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-floa
floatToByteStringLE :: Float
                    -> ByteString
floatToByteStringLE f =
  DB.toStrict $
    runPut    $
      putFloatle f
