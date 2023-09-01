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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.PG.ID
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

module Data.SAM.Version1_6.Read.Parser.Header.PG.ID ( -- * SAM_V1_6 parser - header section (Program) - ID tag
                                                      parse_SAM_V1_6_SAM_V1_6_Program_ID
                                                    ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Lazy   as DABL
import qualified Data.ByteString                   as DB   (unpack)
import           Data.Sequence                     as DSeq
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the ID tag of the @PG tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_SAM_V1_6_Program_ID :: Parser SAM_V1_6_Program_Record_Identifier
parse_SAM_V1_6_SAM_V1_6_Program_ID = do
  pgheaderidentifiertag <- do pgheaderidentifiertagp <- DABL.takeTill (== 58)
                              -- Parse ID tag of the header section.
                              case (pgheaderidentifiertagp =~ [re|[I][D]|]) of
                                False -> fail $ show SAM_V1_6_Error_Program_Identifier_Incorrect_Format 
                                True  -> -- ID tag is in the accepted format. 
                                         return pgheaderidentifiertagp
  _ <- word8 58
  pgheaderidentifiervalue <- DABL.takeTill (== 09)
  return SAM_V1_6_Program_Record_Identifier { sam_v1_6_program_record_identifier_tag   = DSeq.fromList $ DB.unpack pgheaderidentifiertag
                                            , sam_v1_6_program_record_identifier_value = pgheaderidentifiervalue
                                            }