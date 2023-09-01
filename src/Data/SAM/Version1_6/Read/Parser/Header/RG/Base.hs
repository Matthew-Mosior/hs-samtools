{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedLists             #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE MultiWayIf                  #-}
{-# LANGUAGE PackageImports              #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE QuasiQuotes                 #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.RG.Base
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

module Data.SAM.Version1_6.Read.Parser.Header.RG.Base ( -- * SAM_V1_6 parser - header section (Read group)
                                                        parse_SAM_V1_6_Read_Group
                                                      ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error
import Data.SAM.Version1_6.Read.Parser.Header.RG.ID
import Data.SAM.Version1_6.Read.Parser.Header.RG.BC
import Data.SAM.Version1_6.Read.Parser.Header.RG.CN
import Data.SAM.Version1_6.Read.Parser.Header.RG.DS
import Data.SAM.Version1_6.Read.Parser.Header.RG.DT
import Data.SAM.Version1_6.Read.Parser.Header.RG.FO
import Data.SAM.Version1_6.Read.Parser.Header.RG.KS
import Data.SAM.Version1_6.Read.Parser.Header.RG.LB
import Data.SAM.Version1_6.Read.Parser.Header.RG.PG
import Data.SAM.Version1_6.Read.Parser.Header.RG.PI
import Data.SAM.Version1_6.Read.Parser.Header.RG.PL
import Data.SAM.Version1_6.Read.Parser.Header.RG.PM
import Data.SAM.Version1_6.Read.Parser.Header.RG.PU
import Data.SAM.Version1_6.Read.Parser.Header.RG.SM

import Data.Attoparsec.ByteString.Lazy   as DABL
import Text.Regex.PCRE.Heavy

-- | Make a parser optional, return Nothing if there is no match.
maybeOption :: Parser a
            -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

-- | @"SAM_V1_6_Read_Group"@ parser.
--
-- Defines a parser for @RG tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Read_Group :: Parser SAM_V1_6_Read_Group
parse_SAM_V1_6_Read_Group = do
  _         <- do rgheaderp <- DABL.takeTill (== 09)
                  -- Parse @RG tag of the header section.
                  case (rgheaderp =~ [re|[@][R][G]|]) of
                    False -> fail $ show SAM_V1_6_Error_Read_Group_Tag_Incorrect_Format
                    True  -> -- @RG tag is in the accepted format.
                             return rgheaderp
  _         <- word8 09
  -- This parser assumes that the ID tag always appears first, followed by
  -- the BC, CN, DS, DT, FO, KS, LB, PG, PI, PL,
  -- PM, PU and SM tags if they exist, in that order.
  id <- parse_SAM_V1_6_SAM_V1_6_Read_Group_ID
  _  <- word8 09
  bc <- maybeOption parse_SAM_V1_6_SAM_V1_6_Read_Group_BC
  _  <- word8 09
  cn <- maybeOption parse_SAM_V1_6_SAM_V1_6_Read_Group_CN
  _  <- word8 09
  ds <- maybeOption parse_SAM_V1_6_SAM_V1_6_Read_Group_DS
  _  <- word8 09
  dt <- maybeOption parse_SAM_V1_6_SAM_V1_6_Read_Group_DT
  _  <- word8 09
  fo <- maybeOption parse_SAM_V1_6_SAM_V1_6_Read_Group_FO
  _  <- word8 09
  ks <- maybeOption parse_SAM_V1_6_SAM_V1_6_Read_Group_KS
  _  <- word8 09
  lb <- maybeOption parse_SAM_V1_6_SAM_V1_6_Read_Group_LB
  _  <- word8 09
  pg <- maybeOption parse_SAM_V1_6_SAM_V1_6_Read_Group_PG
  _  <- word8 09
  pi <- maybeOption parse_SAM_V1_6_SAM_V1_6_Read_Group_PI
  _  <- word8 09
  pl <- maybeOption parse_SAM_V1_6_SAM_V1_6_Read_Group_PL
  _  <- word8 09
  pm <- maybeOption parse_SAM_V1_6_SAM_V1_6_Read_Group_PM
  _  <- word8 09
  pu <- maybeOption parse_SAM_V1_6_SAM_V1_6_Read_Group_PU
  _  <- word8 09
  sm <- maybeOption parse_SAM_V1_6_SAM_V1_6_Read_Group_SM
  return SAM_V1_6_Read_Group { sam_v1_6_read_group_identifer                    = id
                             , sam_v1_6_read_group_barcode_sequence             = bc
                             , sam_v1_6_read_group_sequencing_center            = cn
                             , sam_v1_6_read_group_description                  = ds
                             , sam_v1_6_read_group_run_date                     = dt
                             , sam_v1_6_read_group_flow_order                   = fo
                             , sam_v1_6_read_group_key_sequence                 = ks
                             , sam_v1_6_read_group_library                      = lb
                             , sam_v1_6_read_group_programs                     = pg
                             , sam_v1_6_read_group_predicted_median_insert_size = pi
                             , sam_v1_6_read_group_platform                     = pl
                             , sam_v1_6_read_group_platform_model               = pm
                             , sam_v1_6_read_group_platform_unit                = pu
                             , sam_v1_6_read_group_sample                       = sm
                             }
