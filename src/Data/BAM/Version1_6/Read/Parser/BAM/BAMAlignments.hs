{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.Read.Parser.BAM.BAMAlignments
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.Read.Parser.BAM.BAMAlignments ( -- * BAM_V1_6_BAM parser
                                                           parse_BAM_V1_6_BAM_BAMAlignments
                                                         ) where

import Data.BAM.Version1_6.BAM.BAMAlignments
import Data.BAM.Version1_6.Read.Parser.BAM.Alignment.Base

import Data.Attoparsec.ByteString.Lazy as DABL
import Data.Sequence                   as DSeq

-- | Define the @"BAM_V1_6_BAM_BAMAlignments"@ parser.
parse_BAM_V1_6_BAM_BAMAlignments :: Parser BAM_V1_6_BAM_BAMAlignments
parse_BAM_V1_6_BAM_BAMAlignments = do
  alignments <-
    DABL.many' parse_BAM_V1_6_BAM_Alignment
  return BAM_V1_6_BAM_BAMAlignments
           { bam_v1_6_bam_bamalignments_alignments = DSeq.fromList alignments
           }
