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
-- Module      :  Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.HOPT
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

module Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.HOPT ( -- * BAM_V1_6 parser - alignment section - hopt field 
                                                                           parse_BAM_V1_6_BAM_Alignment_OptionalFields_HOPT
                                                                         ) where

import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.HOPT
import Data.BAM.Version1_6.Internal
import Data.BAM.Version1_6.Read.Error

import Data.ByteString.Base16            as DBB16
import Data.Base16.Types                 as DBB16T
import Data.Attoparsec.ByteString.Lazy   as DABL
import Data.Sequence                     as DSeq
import Text.Regex.PCRE.Heavy

-- | Defines a parser for the optional hopt field of alignment section of the BAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_BAM_V1_6_BAM_Alignment_OptionalFields_HOPT :: Parser BAM_V1_6_BAM_Alignment_OptionalFields_HOPT
parse_BAM_V1_6_BAM_Alignment_OptionalFields_HOPT = do
  alignmenthoptfieldtag <- do
    alignmenthoptfieldtagp <-
      DABL.take 2
    -- Parse HOPT tag of the alignment section.
    case (alignmenthoptfieldtagp =~ [re|[A-Za-z][A-Za-z0-9]|]) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_Alignment_HOPT_Tag_Incorrect_Format
      True  ->
        -- HOPT tag is in the accepted format. 
        return alignmenthoptfieldtagp
  _ <- do
    alignmenthoptfieldtypep <-
      DABL.take 1
    -- Parse HOPT type of the alignment section.
    case (alignmenthoptfieldtypep =~ [re|[H]|]) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_Alignment_HOPT_Type_Incorrect_Format
      True  ->
        -- HOPT type is in the accepted format.
        return ()
  alignmenthoptfieldvalue <- do
    alignmenthoptfieldvaluep <-
      DABL.takeTill (\x -> x == 0x00)
    -- Parse HOPT value of the alignment section.
    case (alignmenthoptfieldvaluep =~ [re|([0-9A-F][0-9A-F])*|]) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_Alignment_HOPT_Value_Incorrect_Format
      True  ->
        -- HOPT value is in the accepted format.
        return $
          Prelude.map (\currenthexvalue -> do
                         case (DBB16.isBase16 alignmenthoptfieldvaluep) of
                           False ->
                             error $
                               show BAM_V1_6_Read_Error_Alignment_HOPT_Value_Incorrect_Format
                           True  ->
                             DBB16.decodeBase16    $
                               DBB16T.assertBase16 currenthexvalue
                      ) $
          ( splitByteString 2
                            alignmenthoptfieldvaluep
          )
  _ <-
    DABL.take 1
  return BAM_V1_6_BAM_Alignment_OptionalFields_HOPT
           { bam_v1_6_bam_alignment_optionalfields_hopt_tag   = alignmenthoptfieldtag
           , bam_v1_6_bam_alignment_optionalfields_hopt_value = DSeq.fromList alignmenthoptfieldvalue
           }
