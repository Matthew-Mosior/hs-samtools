{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.BAM.BAMAlignments
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.BAM.BAMAlignments ( -- * BAM_V1_6_BAM_BAMAlignment version 1.6 data type
                                               BAM_V1_6_BAM_BAMAlignments(..)
                                             ) where

import Data.BAM.Version1_6.BAM.Alignment

import Data.Data
import Data.Sequence
import Generics.Deriving.Base

-- | Custom @"BAM_V1_6_BAM_BAMAlignments"@ (BAM version 1.6) data type.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
newtype BAM_V1_6_BAM_BAMAlignments = BAM_V1_6_BAM_BAMAlignments
  { bam_v1_6_bam_bamalignments_alignments :: Seq BAM_V1_6_BAM_Alignment -- ^ List of alignments.
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM_BAMAlignments where
  BAM_V1_6_BAM_BAMAlignments bam_v1_6_bamalignments_alignments1 ==
    BAM_V1_6_BAM_BAMAlignments bam_v1_6_bamalignments_alignments2 =
      bam_v1_6_bamalignments_alignments1 == bam_v1_6_bamalignments_alignments2 

instance Show BAM_V1_6_BAM_BAMAlignments where
  show (BAM_V1_6_BAM_BAMAlignments alignments
       ) =
    "BAM_V1_6_BAM_BAMAlignments { "        ++
    "bam_v1_6_bamalignments_alignments = " ++
    (show alignments)                      ++
    " }"
