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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.RG.DT
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

module Data.SAM.Version1_6.Read.Parser.Header.RG.DT ( -- * SAM_V1_6 parser - header section (Read group) - DT tag
                                                      parse_SAM_V1_6_SAM_V1_6_Read_Group_DT
                                                    ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Lazy   as DABL
import qualified Data.ByteString                   as DB   (unpack)
import           Data.Sequence                     as DTeq
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the DT tag of the @RG tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_SAM_V1_6_Read_Group_DT :: Parser SAM_V1_6_Read_Group_Run_Date 
parse_SAM_V1_6_SAM_V1_6_Read_Group_DT = do
  rgheaderrundatetag <- do rgheaderrundatetagp <- DABL.takeTill (== 58)
                           -- Parse DT tag of the header section.
                           case (rgheaderrundatetagp =~ [re|[D][T]|]) of
                             False -> fail $ show SAM_V1_6_Error_Read_Group_Date_Run_Produced_Incorrect_Format
                             True  -> -- DT tag is in the accepted format. 
                                      return rgheaderrundatetagp
  _ <- word8 58
  rgheaderrundatevalue <- DABL.takeTill (== 09)
  return SAM_V1_6_Read_Group_Run_Date { sam_v1_6_read_group_run_date_tag   = DTeq.fromList $ DB.unpack rgheaderrundatetag
                                      , sam_v1_6_read_group_run_date_value = rgheaderrundatevalue
                                      }
