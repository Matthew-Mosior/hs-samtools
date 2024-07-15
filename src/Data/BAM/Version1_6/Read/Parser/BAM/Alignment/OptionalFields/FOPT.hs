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
-- Module      :  Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.FOPT
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

module Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.FOPT ( -- * BAM_V1_6 parser - alignment section - fopt field 
                                                                           parse_BAM_V1_6_BAM_Alignment_OptionalFields_FOPT
                                                                         ) where

import           Data.BAM.Version1_6.BAM.Alignment.OptionalFields.FOPT
import           Data.BAM.Version1_6.Internal
import           Data.BAM.Version1_6.Read.Error

import           Data.ByteString                 as DB
import           Data.Attoparsec.ByteString.Lazy as DABL
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the optional fopt field of alignment section of the BAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_BAM_V1_6_BAM_Alignment_OptionalFields_FOPT :: Parser BAM_V1_6_BAM_Alignment_OptionalFields_FOPT
parse_BAM_V1_6_BAM_Alignment_OptionalFields_FOPT = do
  alignmentfoptfieldtag <- do
    alignmentfoptfieldtagp <-
      DABL.take 2
    -- Parse FOPT tag of the alignment section.
    case (alignmentfoptfieldtagp =~ [re|[A-Za-z][A-Za-z0-9]|]) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_Alignment_FOPT_Tag_Incorrect_Format
      True  ->
        -- FOPT tag is in the accepted format. 
        return alignmentfoptfieldtagp
  _ <- do
    alignmentfoptfieldtypep <-
      DABL.take 1
    -- Parse FOPT type of the alignment section.
    case (alignmentfoptfieldtypep =~ [re|[f]|]) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_Alignment_FOPT_Type_Incorrect_Format
      True  ->
        -- FOPT type is in the accepted format.
        return ()
  alignmentfoptfieldvalue <- do
    alignmentfoptfieldvaluep <-
      DABL.take 4
    -- Parse FOPT value of the alignment section.
    case (alignmentfoptfieldvaluep =~ [re|[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?|]) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_Alignment_FOPT_Value_Incorrect_Format
      True  ->
        -- FOPT value is in the accepted format.
        return alignmentfoptfieldvaluep
  return BAM_V1_6_BAM_Alignment_OptionalFields_FOPT
           { bam_v1_6_bam_alignment_optionalfields_fopt_tag   = alignmentfoptfieldtag
           , bam_v1_6_bam_alignment_optionalfields_fopt_value = word8sToWord32LE $
                                                                  DB.unpack alignmentfoptfieldvalue
           }
