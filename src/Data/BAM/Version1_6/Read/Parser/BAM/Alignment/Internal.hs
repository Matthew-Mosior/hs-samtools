-- |
-- Module      :  Data.BAM.Version1_6.Read.Parser.BAM.Alignment.Internal
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

module Data.BAM.Version1_6.Read.Parser.BAM.Alignment.Internal ( -- * BAM_V1_6_BAM_BAMAlignments parser - internal functions
                                                                decodeSeqField  
                                                              , dropUpTo
                                                              , takeUpTo
                                                              ) where

import Data.Bits
import Data.ByteString as DB hiding (concatMap)
import Data.Word

-- | Decode SEQ field to a [Word8]
-- (each byte represents two bases).
decodeSeqField :: DB.ByteString
               -> [Word8]
decodeSeqField bytes =
  concatMap decodeByte (DB.unpack bytes)
  where
    decodeByte :: Word8
               -> [Word8]
    decodeByte byte =
      [ byte `shiftR` 4
      , byte .&. 0x0F
      ]

-- | Extract a sub ByteString starting from
-- and including the first occurrence of the given byte.
dropUpTo :: Word8
         -> ByteString
         -> ByteString
dropUpTo byte bs = 
  let (_,suffix) = DB.break (== byte) bs
    in case DB.uncons suffix of
         Nothing ->
           DB.empty -- Specified byte not found, return an empty ByteString.
         Just _  ->
           suffix

-- | Extract a sub ByteString up to
-- and including the first occurrence of a given byte.
takeUpTo :: Word8
         -> ByteString
         -> ByteString
takeUpTo byte
         bs = 
  let (prefix,suffix) = DB.break (== byte) bs
    in case DB.uncons suffix of
         Nothing    ->
           prefix -- No 0x00 found, return the entire ByteString.
         Just (_,_) ->
           DB.concat [ prefix
                     , DB.singleton byte
                     ]
