{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# Language QuasiQuotes           #-}

-- |
-- Module      :  Data.SAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.ZOPT
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

module Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.ZOPT ( -- * BAM_V1_6 parser - alignment section - zopt field 
                                                                           parse_BAM_V1_6_BAM_Alignment_OptionalFields_ZOPT
                                                                         ) where

import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.ZOPT
import Data.BAM.Version1_6.Read.Error

import Data.ByteString                 as DB
import Data.Attoparsec.ByteString.Lazy as DABL
import Data.Sequence                   as DSeq
import Text.Regex.PCRE.Heavy

-- | Defines a parser for the optional zopt field of alignment section of the BAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_BAM_V1_6_BAM_Alignment_OptionalFields_ZOPT :: Parser BAM_V1_6_BAM_Alignment_OptionalFields_ZOPT
parse_BAM_V1_6_BAM_Alignment_OptionalFields_ZOPT = do
  alignmentzoptfieldtag <- do
    alignmentzoptfieldtagp <-
      DABL.take 2
    -- Parse ZOPT tag of the alignment section.
    case (alignmentzoptfieldtagp =~ [re|[A-Za-z][A-Za-z0-9]|]) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_Alignment_ZOPT_Tag_Incorrect_Format
      True  ->
        -- ZOPT tag is in the accepted format. 
        return alignmentzoptfieldtagp
  _ <- do
    alignmentzoptfieldtypep <-
      DABL.take 1
    -- Parse ZOPT type of the alignment section.
    case (alignmentzoptfieldtypep =~ [re|[Z]|]) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_Alignment_ZOPT_Type_Incorrect_Format
      True  ->
        -- ZOPT type is in the accepted format.
        return ()
  alignmentzoptfieldvalue <- do
    alignmentzoptfieldvaluep <-
      DABL.takeTill (\x -> x == 0x00)
    -- Parse ZOPT value of the alignment section.
    case (alignmentzoptfieldvaluep =~ [re|[ !-~]*|]) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_Alignment_ZOPT_Value_Incorrect_Format
      True  ->
        -- ZOPT value is in the accepted format.
        return alignmentzoptfieldvaluep
  _ <-
    DABL.take 1
  return BAM_V1_6_BAM_Alignment_OptionalFields_ZOPT
           { bam_v1_6_bam_alignment_optionalfields_zopt_tag   = alignmentzoptfieldtag
           , bam_v1_6_bam_alignment_optionalfields_zopt_value = DSeq.fromList $
                                                                  DB.unpack alignmentzoptfieldvalue
           }
