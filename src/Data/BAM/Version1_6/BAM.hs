{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.BAM
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.BAM ( -- * BAM_V1_6_BAM version 1.6 data type
                                 BAM_V1_6_BAM(..)
                               ) where

import Data.BAM.Version1_6.BAM.BAMHeader
import Data.BAM.Version1_6.BAM.BAMAlignments

import Data.Data
import Generics.Deriving.Base

-- | Custom @"BAM_V1_6_BAM"@ (BAM version 1.6) data type.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BAM = BAM_V1_6_BAM
  { bam_v1_6_bam_bamheader          :: Maybe BAM_V1_6_BAM_BAMHeader
  , bam_v1_6_bam_bamalignments      :: Maybe BAM_V1_6_BAM_BAMAlignments
  , bam_v1_6_bam_endoffilemarker    :: Bool
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM where
  BAM_V1_6_BAM bam_v1_6_bam_bamheader1
               bam_v1_6_bam_bamalignments1
               bam_v1_6_bam_endoffilemarker1 ==
    BAM_V1_6_BAM bam_v1_6_bam_bamheader2
                 bam_v1_6_bam_bamalignments2
                 bam_v1_6_bam_endoffilemarker2 =
      bam_v1_6_bam_bamheader1          == bam_v1_6_bam_bamheader2     &&
      bam_v1_6_bam_bamalignments1      == bam_v1_6_bam_bamalignments2 &&
      bam_v1_6_bam_endoffilemarker1    == bam_v1_6_bam_endoffilemarker2

instance Show BAM_V1_6_BAM where
  show (BAM_V1_6_BAM header
                     alignments
                     endoffilemarker
       ) =
    "BAM_V1_6_BAM { "                       ++
    "bam_v1_6_bam_bamheader = "             ++
    (show header)                           ++
    " , bam_v1_6_bam_alignments = "         ++
    (show alignments)                       ++
    " , bam_v1_6_bam_endoffilemarker = "    ++
    (show endoffilemarker)                  ++
    " }"
