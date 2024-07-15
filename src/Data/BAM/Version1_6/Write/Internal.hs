-- |
-- Module      :  Data.BAM.Version1_6.Write.Internal
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

module Data.BAM.Version1_6.Write.Internal ( -- * BAM_V1_6 writing - internal functions
                                            encodeSeqField  
                                          ) where

import Data.ByteString as DB hiding (concatMap,length)
import Data.Bits
import Data.List       as DL
import Data.Word

-- | Encode a [Word8] (each byte represents two bases)
-- into a ByteString representing the SEQ field.
encodeSeqField :: [Word8]
               -> DB.ByteString
encodeSeqField bases =
  DB.pack $
    concatMap encodeByte pairs
  where
    pairs =
      case ( odd $ DL.length bases
           ) of
        True  ->
          pairsWithPadding bases
        False ->
          pairsWithoutPadding bases
    encodeByte :: (Word8,Word8)
               -> [Word8]
    encodeByte (high,low) =
      [ (high `shiftL` 4)
        .|.
        low
      ]
    pairsWithPadding :: [Word8]
                     -> [(Word8,Word8)]
    pairsWithPadding []       = []
    pairsWithPadding [x]      = [(x,0)]
    pairsWithPadding (x:y:xs) = (x,y) : pairsWithPadding xs
    pairsWithoutPadding :: [Word8]
                        -> [(Word8,Word8)]
    pairsWithoutPadding []       = []
    pairsWithoutPadding [_]      = []
    pairsWithoutPadding (x:y:xs) = (x,y) : pairsWithoutPadding xs
