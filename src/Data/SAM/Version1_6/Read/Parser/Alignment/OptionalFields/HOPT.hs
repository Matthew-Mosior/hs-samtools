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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Alignment.OptionalFields.HOPT
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

module Data.SAM.Version1_6.Read.Parser.Alignment.OptionalFields.HOPT ( -- * SAM_V1_6 parser - alignment section - hopt field 
                                                                       parse_SAM_V1_6_Alignment_OptionalFields_HOPT
                                                                     ) where

import Data.SAM.Version1_6.Alignment.OptionalFields.HOPT
import Data.SAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Char8  as DABC8 (isEndOfLine)
import           Data.Attoparsec.ByteString.Lazy   as DABL
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the optional hopt field of alignment section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Alignment_OptionalFields_HOPT :: Parser SAM_V1_6_Alignment_OptionalFields_HOPT
parse_SAM_V1_6_Alignment_OptionalFields_HOPT = do
  alignmenthoptfieldtag <- do
    alignmenthoptfieldtagp <-
      DABL.takeTill (== 58)
    -- Parse HOPT tag of the alignment section.
    case (alignmenthoptfieldtagp =~ [re|[A-Za-z][A-Za-z0-9]|]) of
      False ->
        fail $ show SAM_V1_6_Error_Alignment_HOPT_Tag_Incorrect_Format
      True  ->
        -- HOPT tag is in the accepted format. 
        return alignmenthoptfieldtagp
  _ <-
    word8 58
  _ <- do
    alignmenthoptfieldtypep <-
      DABL.takeTill (== 58)
    -- Parse HOPT type of the alignment section.
    case (alignmenthoptfieldtypep =~ [re|[H]|]) of
      False ->
        fail $ show SAM_V1_6_Error_Alignment_HOPT_Type_Incorrect_Format
      True  ->
        -- HOPT type is in the accepted format.
        return ()
  _ <-
    word8 58
  alignmenthoptfieldvalue <- do
    alignmenthoptfieldvaluep <-
      DABL.takeTill (\x -> x == 09 || isEndOfLine x)
    -- Parse HOPT value of the alignment section.
    case (alignmenthoptfieldvaluep =~ [re|([0-9A-F][0-9A-F])*|]) of
      False ->
        fail $ show SAM_V1_6_Error_Alignment_HOPT_Value_Incorrect_Format
      True  ->
        -- HOPT value is in the accepted format.
        return alignmenthoptfieldvaluep
  return SAM_V1_6_Alignment_OptionalFields_HOPT
           { sam_v1_6_alignment_optionalfields_hopt_tag   = alignmenthoptfieldtag
           , sam_v1_6_alignment_optionalfields_hopt_value = alignmenthoptfieldvalue
           }
