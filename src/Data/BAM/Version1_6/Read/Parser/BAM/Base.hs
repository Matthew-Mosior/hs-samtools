{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.Read.Parser.BAM.Base
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.Read.Parser.BAM.Base ( -- * BAM_V1_6_BAM parser
                                                  parse_BAM_V1_6_BAM
                                                ) where

import Data.BAM.Version1_6.Internal
import Data.BAM.Version1_6.BAM
import Data.BAM.Version1_6.Read.Parser.BAM.BAMAlignments
import Data.BAM.Version1_6.Read.Parser.BAM.BAMHeader

import Data.Attoparsec.ByteString.Lazy as DABL

-- | Define the @"BAM_V1_6_BAM"@ parser.
parse_BAM_V1_6_BAM :: Parser BAM_V1_6_BAM
parse_BAM_V1_6_BAM = do
  emptycdatablock <-
    peekWord8
  case emptycdatablock of
    Nothing ->
      return BAM_V1_6_BAM
               { bam_v1_6_bam_bamheader       = Nothing
               , bam_v1_6_bam_bamalignments   = Nothing
               , bam_v1_6_bam_endoffilemarker = True 
               }      
    Just _ -> do
      header <-
        maybeOption parse_BAM_V1_6_BAM_BAMHeader
      case header of
        Nothing      -> do
          alignments <-
            parse_BAM_V1_6_BAM_BAMAlignments
          return BAM_V1_6_BAM
               { bam_v1_6_bam_bamheader       = Nothing
               , bam_v1_6_bam_bamalignments   = Just alignments
               , bam_v1_6_bam_endoffilemarker = False
               }
        Just header' ->
          return BAM_V1_6_BAM
                   { bam_v1_6_bam_bamheader       = Just header'
                   , bam_v1_6_bam_bamalignments   = Nothing
                   , bam_v1_6_bam_endoffilemarker = False
                   }          
