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
{-# Language QuasiQuotes           #-}

-- |
-- Module      :  Data.SAM.Version1_6.Read.Parser.Alignment.AOPT
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

module Data.SAM.Version1_6.Read.Parser.Alignment.AOPT ( -- * SAM_V1_6 parser - alignment section - aopt field 
                                                        parse_SAM_V1_6_Alignment_AOPT
                                                      ) where

import Data.SAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Lazy   as DABL
import qualified Data.ByteString                   as DB
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the optional aopt field of alignment section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Alignment_AOPT :: Parser DB.ByteString
parse_SAM_V1_6_Alignment_AOPT = do
  _ <- do alignmentaoptfieldtagp <- DABL.takeTill (== 58)
          -- Parse AOPT tag of the alignment section.
          case (alignmentaoptfieldtagp =~ [re|/[A-Za-z][A-Za-z0-9]/|]) of
            False -> fail $ show SAM_V1_6_Error_Alignment_AOPT_Tag_Incorrect_Format
            True  -> -- AOPT tag is in the accepted format. 
                     return alignmentaoptfieldtagp
  _ <- word8 58
  _ <- do alignmentaoptfieldtypep <- DABL.takeTill (== 58)
          -- Parse AOPT type of the alignment section.
          case (alignmentaoptfieldtypep =~ [re|[A]|]) of
            False -> fail $ show SAM_V1_6_Error_Alignment_AOPT_Type_Incorrect_Format
            True  -> -- AOPT type is in the accepted format.
                     return alignmentaoptfieldtypep
  _ <- word8 58
  alignmentaoptfieldvalue <- do alignmentaoptfieldvaluep <- DABL.takeTill (== 09)
                                -- Parse AOPT value of the alignment section.
                                case (alignmentaoptfieldvaluep =~ [re|[!-~]|]) of
                                  False -> fail $ show SAM_V1_6_Error_Alignment_AOPT_Value_Incorrect_Format
                                  True  -> -- AOPT value is in the accepted format.
                                           return alignmentaoptfieldvaluep
  return alignmentaoptfieldvalue
