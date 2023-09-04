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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.RG.PL
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

module Data.SAM.Version1_6.Read.Parser.Header.RG.PL ( -- * SAM_V1_6 parser - header section (Read group) - PL tag
                                                      parse_SAM_V1_6_SAM_V1_6_Read_Group_PL
                                                    ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Lazy   as DABL
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the PL tag of the @RG tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_SAM_V1_6_Read_Group_PL :: Parser SAM_V1_6_Read_Group_Platform 
parse_SAM_V1_6_SAM_V1_6_Read_Group_PL = do
  _ <- do rgheaderplatformtagp <- DABL.takeTill (== 58)
          -- Parse PL tag of the header section.
          case (rgheaderplatformtagp =~ [re|[P][L]|]) of
            False -> fail $ show SAM_V1_6_Error_Read_Group_Platform_Incorrect_Format
            True  -> -- PL tag is in the accepted format. 
                     return rgheaderplatformtagp
  _ <- word8 58
  rgheaderplatformvalue <- do rgheaderplatformvaluep <- DABL.takeTill (== 09)
                              -- Parse PL value of the header section.
                              case (rgheaderplatformvaluep =~ [re|[C][A][P][I][L][L][A][R][Y]|[D][N][B][S][E][Q]|[E][L][E][M][E][N][T]|[H][E][L][I][C][O][S]|[I][L][L][U][M][I][N][A]|[I][O][N][T][O][R][R][E][N][T]|[L][S][4][5][4]|[O][N][T]|[P][A][C][B][I][O]|[S][O][L][I][D]|[U][L][T][I][M][A]|]) of
                                False -> fail $ show SAM_V1_6_Error_Read_Group_Platform_Incorrect_Format
                                True  -> -- PL value is in the accepted format.
                                         return rgheaderplatformvaluep
  return SAM_V1_6_Read_Group_Platform { sam_v1_6_read_group_platform_value = rgheaderplatformvalue
                                      }
