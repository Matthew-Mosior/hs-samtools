{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.GZipHeader
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.GZipHeader ( -- * BAM_V1_6_GZipHeader version 1.6 data type
                                        BAM_V1_6_GZipHeader(..)
                                      ) where

import Data.Data
import Data.Word
import Generics.Deriving.Base

-- | Custom @"BAM_V1_6_GZipHeader"@ (BAM version 1.6) data type.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_GZipHeader = BAM_V1_6_GZipHeader
  { bam_v1_6_gzip_header_gzip_identifier_1   :: Word8  -- ^ Magic number 1f (0x1f).
  , bam_v1_6_gzip_header_gzip_identifier_2   :: Word8  -- ^ Magic number 08 (0x08).
  , bam_v1_6_gzip_header_compression_method  :: Word8  -- ^ Compression method (08 for DEFLATE).
  , bam_v1_6_gzip_header_header_flags        :: Word8  -- ^ 1-byte for header flags.
  , bam_v1_6_gzip_header_modification_time   :: Word32 -- ^ 4-byte timestamp.
  , bam_v1_6_gzip_header_extra_flags         :: Word8  -- ^ 1-byte eXtra FLags.
  , bam_v1_6_gzip_header_operating_system    :: Word8  -- ^ The operating system id.
  , bam_v1_6_gzip_header_extra_length        :: Word16 -- ^ If FLG.FEXTRA is set,
                                                       -- this gives the length of the optional
                                                       -- extra field.
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_GZipHeader where
  BAM_V1_6_GZipHeader bam_v1_6_gzip_header_gzip_identifier_11
                      bam_v1_6_gzip_header_gzip_identifier_21
                      bam_v1_6_gzip_header_compression_method1
                      bam_v1_6_gzip_header_header_flags1
                      bam_v1_6_gzip_header_modification_time1
                      bam_v1_6_gzip_header_extra_flags1
                      bam_v1_6_gzip_header_operating_system_id1
                      bam_v1_6_gzip_header_extra_length1 ==
    BAM_V1_6_GZipHeader bam_v1_6_gzip_header_gzip_identifier_12
                        bam_v1_6_gzip_header_gzip_identifier_22
                        bam_v1_6_gzip_header_compression_method2
                        bam_v1_6_gzip_header_header_flags2
                        bam_v1_6_gzip_header_modification_time2
                        bam_v1_6_gzip_header_extra_flags2
                        bam_v1_6_gzip_header_operating_system_id2
                        bam_v1_6_gzip_header_extra_length2 =
      bam_v1_6_gzip_header_gzip_identifier_11   == bam_v1_6_gzip_header_gzip_identifier_12   &&
      bam_v1_6_gzip_header_gzip_identifier_21   == bam_v1_6_gzip_header_gzip_identifier_22   &&
      bam_v1_6_gzip_header_compression_method1  == bam_v1_6_gzip_header_compression_method2  &&
      bam_v1_6_gzip_header_header_flags1        == bam_v1_6_gzip_header_header_flags2        &&
      bam_v1_6_gzip_header_modification_time1   == bam_v1_6_gzip_header_modification_time2   &&
      bam_v1_6_gzip_header_extra_flags1         == bam_v1_6_gzip_header_extra_flags2         &&
      bam_v1_6_gzip_header_operating_system_id1 == bam_v1_6_gzip_header_operating_system_id2 &&
      bam_v1_6_gzip_header_extra_length1        == bam_v1_6_gzip_header_extra_length2

instance Show BAM_V1_6_GZipHeader where
  show (BAM_V1_6_GZipHeader gzip_identifier_1
                            gzip_identifier_2
                            compression_method
                            header_flags
                            modification_time
                            extra_flags
                            operating_system_id
                            extra_length
       ) =
    "BAM_V1_6_GZipHeader { "                         ++
    "bam_v1_6_gzip_header_gzip_identifier_1 = "      ++
    (show gzip_identifier_1)                         ++
    " , bam_v1_6_gzip_header_gzip_identifier_2 = "   ++
    (show gzip_identifier_2)                         ++
    " , bam_v1_6_gzip_header_compression_method = "  ++
    (show compression_method)                        ++
    " , bam_v1_6_gzip_header_header_flags = "        ++
    (show header_flags)                              ++
    " , bam_v1_6_gzip_header_modification_time = "   ++
    (show modification_time)                         ++
    " , bam_v1_6_gzip_header_extra_flags = "         ++
    (show extra_flags)                               ++
    " , bam_v1_6_gzip_header_operating_system_id = " ++
    (show operating_system_id)                       ++
    " , bam_v1_6_gzip_header_extra_length = "        ++
    (show extra_length)                              ++
    " }"
