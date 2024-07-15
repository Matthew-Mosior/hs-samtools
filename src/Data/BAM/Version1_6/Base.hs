{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.Base
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.Base ( -- * BAM_V1_6 version 1.6 data type
                                  BAM_V1_6(..)
                                ) where

import Data.BAM.Version1_6.BGZFBlock

import Data.Data
import Data.Sequence
import Generics.Deriving.Base

-- | Custom @"BAM_V1_6"@ (BAM version 1.6) newtype.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
newtype BAM_V1_6 = BAM_V1_6
  { bam_v1_6 :: Seq BAM_V1_6_BGZFBlock -- ^ Sequence holding all
                                       -- BGZF blocks of data
                                       -- within the BAM file.
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6 where
  BAM_V1_6 bam_v1_61 ==
    BAM_V1_6 bam_v1_62 =
      bam_v1_61 == bam_v1_62

instance Show BAM_V1_6 where
  show (BAM_V1_6 bam
       ) =
    "BAM_V1_6 { " ++
    "bam_v1_6 = " ++
    (show bam)    ++
    " }"
