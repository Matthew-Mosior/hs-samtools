{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}

-- |
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.SQ.Base
-- Copyright   :  (c) Matthew Mosior 2023
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

module Data.SAM.Version1_6.Read.Parser.Header.SQ.Base ( -- * SAM_V1_6 parser - header section (Reference sequence dictionary)
                                                        parse_SAM_V1_6_Reference_Sequence_Dictionary
                                                      ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error
import Data.SAM.Version1_6.Read.Parser.Header.SQ.SN
import Data.SAM.Version1_6.Read.Parser.Header.SQ.LN
import Data.SAM.Version1_6.Read.Parser.Header.SQ.AH
import Data.SAM.Version1_6.Read.Parser.Header.SQ.AN
import Data.SAM.Version1_6.Read.Parser.Header.SQ.AS
import Data.SAM.Version1_6.Read.Parser.Header.SQ.DS
import Data.SAM.Version1_6.Read.Parser.Header.SQ.M5
import Data.SAM.Version1_6.Read.Parser.Header.SQ.SP
import Data.SAM.Version1_6.Read.Parser.Header.SQ.TP
import Data.SAM.Version1_6.Read.Parser.Header.SQ.UR

import Data.Attoparsec.ByteString.Lazy   as DABL
import Text.Regex.PCRE.Heavy

-- | Make a parser optional, return Nothing if there is no match.
maybeOption :: Parser a
            -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

-- | @"SAM_V1_6_Reference_Sequence_Dictionary"@ parser.
--
-- Defines a parser for @SQ tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Reference_Sequence_Dictionary :: Parser SAM_V1_6_Reference_Sequence_Dictionary
parse_SAM_V1_6_Reference_Sequence_Dictionary = do
  _         <- do sqheaderp <- DABL.takeTill (== 09)
                  -- Parse @SQ tag of the header section.
                  case (sqheaderp =~ [re|[@][S][Q]|]) of
                    False -> fail $ show SAM_V1_6_Error_File_Level_Metadata_Tag_Incorrect_Format
                    True  -> -- @SQ tag is in the accepted format.
                             return sqheaderp
  _         <- word8 09
  -- This parser assumes that the SN tag always appears first, followed by
  -- the LN tag, followed by the AH, AN, AS, DS, M5,
  -- SP, TP and UR tags if they exist, in that order.
  sn <- parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_SN
  _  <- word8 09
  ln <- parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_LN
  _  <- word8 09
  ah <- maybeOption parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_AH
  _  <- word8 09
  an <- maybeOption parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_AN
  _  <- word8 09
  as <- maybeOption parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_AS
  _  <- word8 09
  ds <- maybeOption parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_DS
  _  <- word8 09
  m5 <- maybeOption parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_M5
  _  <- word8 09
  sp <- maybeOption parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_SP
  _  <- word8 09
  tp <- maybeOption parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_TP
  _  <- word8 09
  ur <- maybeOption parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_UR 
  return SAM_V1_6_Reference_Sequence_Dictionary { sam_v1_6_reference_sequence_dictionary_reference_sequence_name                        = sn  
                                                , sam_v1_6_reference_sequence_dictionary_reference_sequence_length                      = ln
                                                , sam_v1_6_reference_sequence_dictionary_reference_alternative_locus                    = ah
                                                , sam_v1_6_reference_sequence_dictionary_reference_alternative_reference_sequence_names = an
                                                , sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier                     = as
                                                , sam_v1_6_reference_sequence_dictionary_description                                    = ds
                                                , sam_v1_6_reference_sequence_dictionary_md5_checksum                                   = m5
                                                , sam_v1_6_reference_sequence_dictionary_species                                        = sp
                                                , sam_v1_6_reference_sequence_dictionary_molecule_topology                              = tp
                                                , sam_v1_6_reference_sequence_dictionary_uri                                            = ur
                                                } 
