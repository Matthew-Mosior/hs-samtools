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
-- Module      :  Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.BigSOPT
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

module Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.BigSOPT ( -- * BAM_V1_6_BAM parser - alignment section - bigsopt field 
                                                                              parse_BAM_V1_6_BAM_Alignment_OptionalFields_BigSOPT
                                                                            ) where

import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.BigSOPT
import Data.BAM.Version1_6.Internal
import Data.BAM.Version1_6.Read.Error

import Data.ByteString                  as DB
import Data.Attoparsec.ByteString.Lazy  as DABL
import Text.Regex.PCRE.Heavy

-- | Defines a parser for the optional bigsopt field of alignment section of the BAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_BAM_V1_6_BAM_Alignment_OptionalFields_BigSOPT :: Parser BAM_V1_6_BAM_Alignment_OptionalFields_BigSOPT
parse_BAM_V1_6_BAM_Alignment_OptionalFields_BigSOPT = do
  alignmentbigsoptfieldtag <- do
    alignmentbigsoptfieldtagp <-
      DABL.take 2
    -- Parse BigSOPT tag of the alignment section.
    case (alignmentbigsoptfieldtagp =~ [re|[A-Za-z][A-Za-z0-9]|]) of
      False ->
        fail $ show BAM_V1_6_Read_Error_Alignment_BigSOPT_Tag_Incorrect_Format
      True  ->
        -- BigSOPT tag is in the accepted format. 
        return alignmentbigsoptfieldtagp
  _ <- do
    alignmentbigsoptfieldtypep <-
      DABL.take 1
    -- Parse BigSOPT type of the alignment section.
    case (alignmentbigsoptfieldtypep =~ [re|[S]|]) of
      False ->
        fail $ show BAM_V1_6_Read_Error_Alignment_BigSOPT_Type_Incorrect_Format
      True  ->
        -- BigSOPT type is in the accepted format.
        return ()
  alignmentbigsoptfieldvalue <-
    DABL.take 2
  return BAM_V1_6_BAM_Alignment_OptionalFields_BigSOPT
           { bam_v1_6_bam_alignment_optionalfields_bigsopt_tag   = alignmentbigsoptfieldtag
           , bam_v1_6_bam_alignment_optionalfields_bigsopt_value = word8sToWord16LE $
                                                                     DB.unpack alignmentbigsoptfieldvalue
           }
