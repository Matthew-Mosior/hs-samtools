{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.BAM.Alignment.OptionalFields.SmallSOPT
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

module Data.BAM.Version1_6.BAM.Alignment.OptionalFields.SmallSOPT ( -- * BAM version 1.6 alignment optional fields data type
                                                                    BAM_V1_6_BAM_Alignment_OptionalFields_SmallSOPT(..)
                                                                  ) where

import Data.ByteString
import Data.Int
import Data.Data
import Generics.Deriving.Base

-- | Custom BAM (version 1.6) @"BAM_V1_6_Alignment_OptionalFields_SmallSOPT"@ data type.
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BAM_Alignment_OptionalFields_SmallSOPT = BAM_V1_6_BAM_Alignment_OptionalFields_SmallSOPT
  { bam_v1_6_bam_alignment_optionalfields_smallsopt_tag   :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_smallsopt_value :: Int16
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM_Alignment_OptionalFields_SmallSOPT where
  BAM_V1_6_BAM_Alignment_OptionalFields_SmallSOPT bam_v1_6_bam_alignment_optionalfields_smallsopt_tag1
                                                  bam_v1_6_bam_alignment_optionalfields_smallsopt_value1 ==
    BAM_V1_6_BAM_Alignment_OptionalFields_SmallSOPT bam_v1_6_bam_alignment_optionalfields_smallsopt_tag2
                                                    bam_v1_6_bam_alignment_optionalfields_smallsopt_value2 =
      bam_v1_6_bam_alignment_optionalfields_smallsopt_tag1   == bam_v1_6_bam_alignment_optionalfields_smallsopt_tag2 &&
      bam_v1_6_bam_alignment_optionalfields_smallsopt_value1 == bam_v1_6_bam_alignment_optionalfields_smallsopt_value2

instance Show BAM_V1_6_BAM_Alignment_OptionalFields_SmallSOPT where
  show (BAM_V1_6_BAM_Alignment_OptionalFields_SmallSOPT tag
                                                        value
       ) =
    "BAM_V1_6_BAM_Alignment_OptionalFields_SmallSOPT { "          ++
    "bam_v1_6_bam_alignment_optionalfields_smallsopt_tag = "      ++
    (show tag)                                                    ++
    " , bam_v1_6_bam_alignment_optionalfields_smallsopt_value = " ++
    (show value)                                                  ++
    " }"
