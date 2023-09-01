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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.HD.VN
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

module Data.SAM.Version1_6.Read.Parser.Header.HD.VN ( -- * SAM_V1_6 parser - header section (File-level metadata) - VN tag
                                                      parse_SAM_V1_6_File_Level_Metadata_VN
                                                    ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Lazy   as DABL
import qualified Data.ByteString                   as DB   (unpack)
import           Data.Sequence                     as DSeq
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the VN tag of the @HD tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_File_Level_Metadata_VN :: Parser SAM_V1_6_File_Level_Metadata_Format_Version
parse_SAM_V1_6_File_Level_Metadata_VN = do
  hdheaderversiontag <- do hdheaderversiontagp <- DABL.takeTill (== 58)
                           -- Parse VN tag of the header section.
                           case (hdheaderversiontagp =~ [re|[V][N]|]) of
                             False -> fail $ show SAM_V1_6_Error_File_Level_Metadata_Format_Version_Tag_Incorrect_Format
                             True  -> -- VN tag is in the accepted format. 
                                      return hdheaderversiontagp
  _ <- word8 58
  hdheaderversionvalue <- do hdheaderversionvaluep <- DABL.takeTill (== 09)
                             -- Parse VN value of the header section.
                             case (hdheaderversionvaluep =~ [re|/^[0-9]+\.[0-9]+$/.|]) of
                               False -> fail $ show SAM_V1_6_Error_File_Level_Metadata_Format_Version_Value_Incorrect_Format
                               True  -> -- VN value is in the accepted format.
                                        return hdheaderversionvaluep  
  return SAM_V1_6_File_Level_Metadata_Format_Version { sam_v1_6_file_level_metadata_format_version_tag   = DSeq.fromList $ DB.unpack hdheaderversiontag
                                                     , sam_v1_6_file_level_metadata_format_version_value = hdheaderversionvalue
                                                     }
