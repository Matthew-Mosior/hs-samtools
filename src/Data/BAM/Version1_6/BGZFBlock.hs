{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.BGZFBlock
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.BGZFBlock ( -- * BAM_V1_6_BGZFBlock version 1.6 data type
                                       BAM_V1_6_BGZFBlock(..)
                                     ) where

import Data.BAM.Version1_6.BAM
import Data.BAM.Version1_6.GZipHeader

import Data.Data
import Data.Word
import Generics.Deriving.Base

-- | Custom @"BAM_V1_6_BGZFBlock"@ (BAM version 1.6) data type.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BGZFBlock = BAM_V1_6_BGZFBlock
  { bam_v1_6_bgzfblock_gzip_header                :: BAM_V1_6_GZipHeader -- ^ GZip 10 byte header.
                                                                         -- This contains a magic number (1f 08),
                                                                         -- the compression method (08 for DEFLATE),
                                                                         -- 1-byte of header flags,
                                                                         -- a 4-byte timestamp,
                                                                         -- compression flags,
                                                                         -- and the operating system ID.
  , bam_v1_6_bgzfblock_subfield_identifier_one    :: Word8               -- ^ This should be 'B'.
  , bam_v1_6_bgzfblock_subfield_identifier_two    :: Word8               -- ^ This should be 'C'.
  , bam_v1_6_bgzfblock_subfield_length            :: Word16              -- ^ This should be '02 00'.
  , bam_v1_6_bgzfblock_total_block_size_minus_one :: Word16              -- ^ The payload of the BGZF
                                                                         -- extra field is a 16-bit unsigned
                                                                         -- integer in little endian format.
                                                                         -- This integer gives the size of the
                                                                         -- containing BGZF block minus one.
  , bam_v1_6_bgzfblock_cdata                      :: BAM_V1_6_BAM        -- ^ Compressed DATA by zlib::deflate().
                                                                         -- The data in this field will be
                                                                         -- in a decompressed and
                                                                         -- useable state.
  , bam_v1_6_bgzfblock_crc32                      :: Word32              -- ^ CRC-32.
  , bam_v1_6_bgzfblock_isize                      :: Word32              -- ^ Input SIZE (length of uncompressed data).
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BGZFBlock where
  BAM_V1_6_BGZFBlock bam_v1_6_bgzfblock_gzip_header1
                     bam_v1_6_bgzfblock_subfield_identifier_one1
                     bam_v1_6_bgzfblock_subfield_identifier_two1
                     bam_v1_6_bgzfblock_subfield_length1
                     bam_v1_6_bgzfblock_total_block_size_minus_one1
                     bam_v1_6_bgzfblock_cdata1
                     bam_v1_6_bgzfblock_crc321
                     bam_v1_6_bgzfblock_isize1 ==
    BAM_V1_6_BGZFBlock bam_v1_6_bgzfblock_gzip_header2
                       bam_v1_6_bgzfblock_subfield_identifier_one2
                       bam_v1_6_bgzfblock_subfield_identifier_two2
                       bam_v1_6_bgzfblock_subfield_length2
                       bam_v1_6_bgzfblock_total_block_size_minus_one2
                       bam_v1_6_bgzfblock_cdata2
                       bam_v1_6_bgzfblock_crc322
                       bam_v1_6_bgzfblock_isize2 =
      bam_v1_6_bgzfblock_gzip_header1                == bam_v1_6_bgzfblock_gzip_header2                &&
      bam_v1_6_bgzfblock_subfield_identifier_one1    == bam_v1_6_bgzfblock_subfield_identifier_one2    &&
      bam_v1_6_bgzfblock_subfield_identifier_two1    == bam_v1_6_bgzfblock_subfield_identifier_two2    &&
      bam_v1_6_bgzfblock_subfield_length1            == bam_v1_6_bgzfblock_subfield_length2            &&
      bam_v1_6_bgzfblock_total_block_size_minus_one1 == bam_v1_6_bgzfblock_total_block_size_minus_one2 &&
      bam_v1_6_bgzfblock_cdata1                      == bam_v1_6_bgzfblock_cdata2                      &&
      bam_v1_6_bgzfblock_crc321                      == bam_v1_6_bgzfblock_crc322                      &&
      bam_v1_6_bgzfblock_isize1                      == bam_v1_6_bgzfblock_isize2

instance Show BAM_V1_6_BGZFBlock where
  show (BAM_V1_6_BGZFBlock gzip_header
                           subfield_identifier_one
                           subfield_identifier_two
                           subfield_length
                           total_block_size_minus_one
                           cdata
                           crc32
                           isize
       ) =
    "BAM_V1_6_BGZFBlock { "                               ++
    "bam_v1_6_bgzfblock_gzip_header = "                   ++
    (show gzip_header)                                    ++
    " , bam_v1_6_bgzfblock_subfield_identifier_one = "    ++
    (show subfield_identifier_one)                        ++
    " , bam_v1_6_bgzfblock_subfield_identifier_two = "    ++
    (show subfield_identifier_two)                        ++
    " , bam_v1_6_bgzfblock_subfield_length = "            ++
    (show subfield_length)                                ++
    " , bam_v1_6_bgzfblock_total_block_size_minus_one = " ++
    (show total_block_size_minus_one)                     ++
    " , bam_v1_6_bgzfblock_cdata = "                      ++
    (show cdata)                                          ++
    " , bam_v1_6_bgzfblock_crc32 = "                      ++
    (show crc32)                                          ++
    " , bam_v1_6_bgzfblock_isize = "                      ++
    (show isize)                                          ++
    " }"
