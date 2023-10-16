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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.HD.Base
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

module Data.SAM.Version1_6.Read.Parser.Header.HD.Base ( -- * SAM_V1_6 parser - header section (File-level metadata)
                                                        parse_SAM_V1_6_File_Level_Metadata
                                                      ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error
import Data.SAM.Version1_6.Read.Parser.Header.HD.VN
import Data.SAM.Version1_6.Read.Parser.Header.HD.SO
import Data.SAM.Version1_6.Read.Parser.Header.HD.GO
import Data.SAM.Version1_6.Read.Parser.Header.HD.SS

import Control.Applicative.Permutations           (intercalateEffect,toPermutation,toPermutationWithDefault)
import Data.Attoparsec.ByteString.Char8  as DABC8 (endOfLine)
import Data.Attoparsec.ByteString.Lazy   as DABL
import Text.Regex.PCRE.Heavy

-- | @"SAM_V1_6_File_Level_Metadata"@ parser.
--
-- Defines a parser for @HD tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_File_Level_Metadata :: Parser SAM_V1_6_File_Level_Metadata
parse_SAM_V1_6_File_Level_Metadata = do
  _         <- do hdheaderp <- DABL.takeTill (== 09)
                  -- Parse @HD tag of the header section.
                  case (hdheaderp =~ [re|[@][H][D]|]) of
                    False -> fail $ show SAM_V1_6_Error_File_Level_Metadata_Tag_Incorrect_Format
                    True  -> -- @HD tag is in the accepted format.
                             return () 
  _         <- word8 09
  -- This parser assumes that the
  -- VN, SO, GO and SS tags can appear in any order.
  hd <- intercalateEffect (word8 09) $
          SAM_V1_6_File_Level_Metadata
            <$> toPermutation parse_SAM_V1_6_File_Level_Metadata_VN
            <*> toPermutationWithDefault Nothing 
                                         (Just <$> parse_SAM_V1_6_File_Level_Metadata_SO)
            <*> toPermutationWithDefault Nothing
                                         (Just <$> parse_SAM_V1_6_File_Level_Metadata_GO)
            <*> toPermutationWithDefault Nothing
                                         (Just <$> parse_SAM_V1_6_File_Level_Metadata_SS)
  _ <- endOfLine
  return hd 
