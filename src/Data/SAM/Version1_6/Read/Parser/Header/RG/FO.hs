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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.RG.FO
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

module Data.SAM.Version1_6.Read.Parser.Header.RG.FO ( -- * SAM_V1_6 parser - header section (Read group) - FO tag
                                                      parse_SAM_V1_6_SAM_V1_6_Read_Group_FO
                                                    ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Lazy   as DABL
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the FO tag of the @RG tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_SAM_V1_6_Read_Group_FO :: Parser SAM_V1_6_Read_Group_Flow_Order 
parse_SAM_V1_6_SAM_V1_6_Read_Group_FO = do
  _ <- do rgheaderflowordertagp <- DABL.takeTill (== 58)
          -- Parse FO tag of the header section.
          case (rgheaderflowordertagp =~ [re|[F][O]|]) of
            False -> fail $ show SAM_V1_6_Error_Read_Group_Flow_Order_Incorrect_Format
            True  -> -- FO tag is in the accepted format. 
                     return rgheaderflowordertagp
  _ <- word8 58
  rgheaderflowordervalue <- do rgheaderflowordervaluep <- DABL.takeTill (== 09)
                               -- Parse FO value of the header section.
                               case (rgheaderflowordervaluep =~ [re|/\*|[ACMGRSVTWYHKDBN]+/|]) of
                                 False -> fail $ show SAM_V1_6_Error_Read_Group_Flow_Order_Incorrect_Format
                                 True  -> -- FO value is in the accepted format.
                                          return rgheaderflowordervaluep
  return SAM_V1_6_Read_Group_Flow_Order { sam_v1_6_read_group_flow_order_value = rgheaderflowordervalue
                                        }
