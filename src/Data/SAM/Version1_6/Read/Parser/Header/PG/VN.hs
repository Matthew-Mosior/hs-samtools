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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.PG.VN
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

module Data.SAM.Version1_6.Read.Parser.Header.PG.VN ( -- * SAM_V1_6 parser - header section (Program) - VN tag
                                                      parse_SAM_V1_6_Program_VN
                                                    ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error

import Data.Attoparsec.ByteString.Char8 (isEndOfLine)
import Data.Attoparsec.ByteString.Lazy as DABL
import Text.Regex.PCRE.Heavy

-- | Defines a parser for the VN tag of the @PG tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Program_VN :: Parser SAM_V1_6_Program_Version
parse_SAM_V1_6_Program_VN = do
  _                    <- do
    pgheaderversiontagp <-
      DABL.takeTill (== 58)
    -- Parse VN tag of the header section.
    case (pgheaderversiontagp =~ [re|[V][N]|]) of
      False ->
        fail $ show SAM_V1_6_Error_Program_Version_Incorrect_Format 
      True  ->
        -- VN tag is in the accepted format. 
        return ()
  _                    <-
    word8 58
  pgheaderversionvalue <-
    DABL.takeTill (\x -> x == 09 || isEndOfLine x)
  return SAM_V1_6_Program_Version { sam_v1_6_program_version_value = pgheaderversionvalue
                                  }
