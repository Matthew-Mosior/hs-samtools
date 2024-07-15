{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.BAM.BAMHeader
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.BAM.BAMHeader ( -- * BAM_V1_6_BAM_BAMHeader version 1.6 data type
                                           BAM_V1_6_BAM_BAMHeader(..)
                                         ) where

import Data.BAM.Version1_6.BAM.ReferenceInformation

import Data.ByteString (ByteString)
import Data.Data
import Data.Sequence
import Data.Word
import Generics.Deriving.Base

-- | Custom @"BAM_V1_6_BAM_BAMHeader"@ (BAM version 1.6) data type.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BAM_BAMHeader = BAM_V1_6_BAM_BAMHeader
  { bam_v1_6_bam_bamheader_magic                 :: ByteString                             -- ^ BAM magic string (magic).
                                                                                           -- This should be BAM\1.
  , bam_v1_6_bam_bamheader_l_text                :: Word32                                 -- ^ Length of the header text, including any NUL padding (l_text).
  , bam_v1_6_bam_bamheader_text                  :: ByteString                             -- ^ Plain header text in SAM;
                                                                                           -- not necessarily NUL-terminated.
  , bam_v1_6_bam_bamheader_n_ref                 :: Word32                                 -- ^ Number of reference sequences (n_ref).
  , bam_v1_6_bam_bamheader_reference_information :: Seq BAM_V1_6_BAM_Reference_Information -- ^ List of reference information (n = bam_v1_6_bam_n_ref).
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM_BAMHeader where
  BAM_V1_6_BAM_BAMHeader bam_v1_6_bam_bamheader_magic1
                         bam_v1_6_bam_bamheader_l_text1
                         bam_v1_6_bam_bamheader_text1
                         bam_v1_6_bam_bamheader_n_ref1
                         bam_v1_6_bam_bamheader_reference_information1 ==
    BAM_V1_6_BAM_BAMHeader bam_v1_6_bam_bamheader_magic2
                           bam_v1_6_bam_bamheader_l_text2
                           bam_v1_6_bam_bamheader_text2
                           bam_v1_6_bam_bamheader_n_ref2
                           bam_v1_6_bam_bamheader_reference_information2 =
      bam_v1_6_bam_bamheader_magic1                 == bam_v1_6_bam_bamheader_magic2                         &&
      bam_v1_6_bam_bamheader_l_text1                == bam_v1_6_bam_bamheader_l_text2                        &&
      bam_v1_6_bam_bamheader_text1                  == bam_v1_6_bam_bamheader_text2                          &&
      bam_v1_6_bam_bamheader_n_ref1                 == bam_v1_6_bam_bamheader_n_ref2                         &&
      bam_v1_6_bam_bamheader_reference_information1 == bam_v1_6_bam_bamheader_reference_information2

instance Show BAM_V1_6_BAM_BAMHeader where
  show (BAM_V1_6_BAM_BAMHeader magic
                               l_text
                               text
                               n_ref
                               reference_information
       ) =
    "BAM_V1_6_BAM_BAMHeader { "                          ++
    "bam_v1_6_bam_bamheader_magic = "                    ++
    (show magic)                                         ++
    " , bam_v1_6_bam_bamheader_l_text = "                ++
    (show l_text)                                        ++
    " , bam_v1_6_bam_bamheader_text = "                  ++
    (show text)                                          ++
    " , bam_v1_6_bam_bamheader_n_ref = "                 ++
    (show n_ref)                                         ++
    " , bam_v1_6_bam_bamheader_reference_information = " ++
    (show reference_information)                         ++
    " }"
