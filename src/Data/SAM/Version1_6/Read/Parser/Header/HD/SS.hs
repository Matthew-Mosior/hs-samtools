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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.HD.SS
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

module Data.SAM.Version1_6.Read.Parser.Header.HD.SS ( -- * SAM_V1_6 parser - header section (File-level metadata) - SS tag
                                                      parse_SAM_V1_6_File_Level_Metadata_SS
                                                    ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Lazy   as DABL
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the SS tag of the @HD tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_File_Level_Metadata_SS :: Parser SAM_V1_6_File_Level_Metadata_SubSorting_Order
parse_SAM_V1_6_File_Level_Metadata_SS = do
  _ <- do hdheadersubsortingordertagp <- DABL.takeTill (== 58)
          -- Parse SS tag of the header section.
          case (hdheadersubsortingordertagp =~ [re|[S][S]|]) of
            False -> fail $ show SAM_V1_6_Error_File_Level_Metadata_Subsorting_Order_Tag_Incorrect_Format
            True  -> -- SS tag is in the accepted format.
                     return hdheadersubsortingordertagp
  _ <- word8 58
  hdheadersubsortingordervalue <- do hdheadersubsortingordervaluep <- DABL.takeTill (== 09)
                                     -- Parse SS value of the header section.
                                     case (hdheadersubsortingordervaluep =~ [re|(coordinate|queryname|unsorted)(:[A-Za-z0-9_-]+)+|]) of
                                       False -> fail $ show SAM_V1_6_Error_File_Level_Metadata_Subsorting_Order_Incorrect_Format 
                                       True  -> -- SS value is in the accepted format.
                                                return hdheadersubsortingordervaluep 
  return SAM_V1_6_File_Level_Metadata_SubSorting_Order { sam_v1_6_file_level_metadata_subsorting_order_value = hdheadersubsortingordervalue
                                                       }
