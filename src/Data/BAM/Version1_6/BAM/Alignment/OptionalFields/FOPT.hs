{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.BAM.Alignment.OptionalFields.FOPT
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

module Data.BAM.Version1_6.BAM.Alignment.OptionalFields.FOPT ( -- * BAM version 1.6 alignment optional fields data type
                                                               BAM_V1_6_BAM_Alignment_OptionalFields_FOPT(..)
                                                             ) where

import Data.ByteString
import Data.Word
import Data.Data
import Generics.Deriving.Base

-- | Custom BAM (version 1.6) @"BAM_V1_6_Alignment_OptionalFields_FOPT"@ data type.
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BAM_Alignment_OptionalFields_FOPT = BAM_V1_6_BAM_Alignment_OptionalFields_FOPT
  { bam_v1_6_bam_alignment_optionalfields_fopt_tag   :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_fopt_value :: Word32
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM_Alignment_OptionalFields_FOPT where
  BAM_V1_6_BAM_Alignment_OptionalFields_FOPT bam_v1_6_bam_alignment_optionalfields_fopt_tag1
                                             bam_v1_6_bam_alignment_optionalfields_fopt_value1 ==
    BAM_V1_6_BAM_Alignment_OptionalFields_FOPT bam_v1_6_bam_alignment_optionalfields_fopt_tag2
                                               bam_v1_6_bam_alignment_optionalfields_fopt_value2 =
      bam_v1_6_bam_alignment_optionalfields_fopt_tag1 == bam_v1_6_bam_alignment_optionalfields_fopt_tag2 &&
      bam_v1_6_bam_alignment_optionalfields_fopt_value1 == bam_v1_6_bam_alignment_optionalfields_fopt_value2

instance Show BAM_V1_6_BAM_Alignment_OptionalFields_FOPT where
  show (BAM_V1_6_BAM_Alignment_OptionalFields_FOPT tag
                                                   value
       ) =
    "BAM_V1_6_BAM_Alignment_OptionalFields_FOPT { "          ++
    "bam_v1_6_bam_alignment_optionalfields_fopt_tag = "      ++
    (show tag)                                               ++
    " , bam_v1_6_bam_alignment_optionalfields_fopt_value = " ++
    (show value)                                             ++
    " }"
