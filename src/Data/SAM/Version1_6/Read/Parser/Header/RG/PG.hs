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
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}

-- |
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.RG.PG
-- Copyright   :  (c) Matthew Mosior 2024
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

module Data.SAM.Version1_6.Read.Parser.Header.RG.PG ( -- * SAM_V1_6 parser - header section (Read group) - PG tag
                                                      parse_SAM_V1_6_Read_Group_PG
                                                    ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error

import Data.Attoparsec.ByteString.Char8 (isEndOfLine)
import Data.Attoparsec.ByteString.Lazy as DABL
import Text.Regex.PCRE.Heavy

-- | Defines a parser for the PG tag of the @RG tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Read_Group_PG :: Parser SAM_V1_6_Read_Group_Programs 
parse_SAM_V1_6_Read_Group_PG = do
  _ <- do
    rgheaderprogramstagp <-
      DABL.takeTill (== 58)
    -- Parse PG tag of the header section.
    case (rgheaderprogramstagp =~ [re|[P][G]|]) of
      False ->
        fail $ show SAM_V1_6_Error_Read_Group_Programs_Incorrect_Format
      True  ->
        -- PG tag is in the accepted format. 
        return ()
  _                     <-
    word8 58
  rgheaderprogramsvalue <-
    DABL.takeTill (\x -> x == 09 || isEndOfLine x)
  return SAM_V1_6_Read_Group_Programs { sam_v1_6_read_group_programs_value = rgheaderprogramsvalue
                                      }
