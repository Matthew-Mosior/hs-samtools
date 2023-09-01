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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.HD.SO
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

module Data.SAM.Version1_6.Read.Parser.Header.HD.SO ( -- * SAM_V1_6 parser - header section (File-level metadata) - SO tag
                                                      parse_SAM_V1_6_File_Level_Metadata_SO
                                                    ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Lazy   as DABL
import qualified Data.ByteString                   as DB   (unpack)
import           Data.Sequence                     as DSeq
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the SO tag of the @HD tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_File_Level_Metadata_SO :: Parser SAM_V1_6_File_Level_Metadata_Sorting_Order
parse_SAM_V1_6_File_Level_Metadata_SO = do
  hdheadersortingordertag <- do hdheadersortingordertagp <- DABL.takeTill (== 58)
                                -- Parse SO tag of the header section.
                                case (hdheadersortingordertagp =~ [re|[S][O]|]) of
                                  False -> fail $ show SAM_V1_6_Error_File_Level_Metadata_Sorting_Order_Tag_Incorrect_Format
                                  True  -> -- SO tag is in the accepted format.
                                           return hdheadersortingordertagp
  _ <- word8 58
  hdheadersortingordervalue <- do hdheadersortingordervaluep <- DABL.takeTill (== 09)
                                  -- Parse SO value of the header section.
                                  case (hdheadersortingordervaluep =~ [re|[u][n][k][n][o][w][n]|[u][n][s][o][r][t][e][d]|[q][u][e][r][y][n][a][m][e]|[c][o][o][r][d][i][n][a][t][e]|]) of
                                    False -> fail $ show SAM_V1_6_Error_File_Level_Metadata_Sorting_Order_Invalid_Value
                                    True  -> -- SO value is in the accepted format.
                                             return hdheadersortingordervaluep  
  return SAM_V1_6_File_Level_Metadata_Sorting_Order { sam_v1_6_file_level_metadata_sorting_order_tag   = DSeq.fromList $ DB.unpack hdheadersortingordertag
                                                    , sam_v1_6_file_level_metadata_sorting_order_value = hdheadersortingordervalue
                                                    }
