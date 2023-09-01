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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.PG.Base
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

module Data.SAM.Version1_6.Read.Parser.Header.PG.Base ( -- * SAM_V1_6 parser - header section (Program)
                                                        parse_SAM_V1_6_Program
                                                      ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error
import Data.SAM.Version1_6.Read.Parser.Header.PG.ID
import Data.SAM.Version1_6.Read.Parser.Header.PG.PN
import Data.SAM.Version1_6.Read.Parser.Header.PG.CL
import Data.SAM.Version1_6.Read.Parser.Header.PG.PP
import Data.SAM.Version1_6.Read.Parser.Header.PG.DS
import Data.SAM.Version1_6.Read.Parser.Header.PG.VN

import Data.Attoparsec.ByteString.Lazy   as DABL
import Text.Regex.PCRE.Heavy

-- | Make a parser optional, return Nothing if there is no match.
maybeOption :: Parser a
            -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

-- | @"SAM_V1_6_Program"@ parser.
--
-- Defines a parser for @PG tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Program :: Parser SAM_V1_6_Program
parse_SAM_V1_6_Program = do
  _         <- do pgheaderp <- DABL.takeTill (== 09)
                  -- Parse @PG tag of the header section.
                  case (pgheaderp =~ [re|[@][P][G]|]) of
                    False -> fail $ show SAM_V1_6_Error_Program_Tag_Incorrect_Format 
                    True  -> -- @PG tag is in the accepted format.
                             return pgheaderp
  _         <- word8 09
  -- This parser assumes that the ID tag always appears first, followed by
  -- the PN, CL, PP,
  -- DS and VN tags if they exist, in that order.
  id <- parse_SAM_V1_6_SAM_V1_6_Program_ID
  _  <- word8 09
  pn <- maybeOption parse_SAM_V1_6_SAM_V1_6_Program_PN
  _  <- word8 09
  cl <- maybeOption parse_SAM_V1_6_SAM_V1_6_Program_CL
  _  <- word8 09
  pp <- maybeOption parse_SAM_V1_6_SAM_V1_6_Program_PP
  _  <- word8 09
  ds <- maybeOption parse_SAM_V1_6_SAM_V1_6_Program_DS
  _  <- word8 09
  vn <- maybeOption parse_SAM_V1_6_SAM_V1_6_Program_VN
  return SAM_V1_6_Program { sam_v1_6_program_record_identifier = id
                          , sam_v1_6_program_name              = pn
                          , sam_v1_6_program_command_line      = cl
                          , sam_v1_6_program_previous_pg_id    = pp
                          , sam_v1_6_program_description       = ds
                          , sam_v1_6_program_version           = vn
                          }
