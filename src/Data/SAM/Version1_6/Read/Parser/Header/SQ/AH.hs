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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.SQ.AH
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

module Data.SAM.Version1_6.Read.Parser.Header.SQ.AH ( -- * SAM_V1_6 parser - header section (Reference sequence dictionary) - AH tag
                                                      parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_AH
                                                    ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Lazy   as DABL
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the AH tag of the @SQ tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_AH :: Parser SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus
parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_AH = do
  _ <- do sqheaderalternativelocustagp <- DABL.takeTill (== 58)
          -- Parse AH tag of the header section.
          case (sqheaderalternativelocustagp =~ [re|[A][H]|]) of
            False -> fail $ show SAM_V1_6_Error_Reference_Sequence_Dictionary_Alternative_Locus_Incorrect_Format
            True  -> -- AH tag is in the accepted format.
                     return sqheaderalternativelocustagp
  _ <- word8 58
  sqheaderalternativelocusvalue <- DABL.takeTill (== 09) 
  return SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus { sam_v1_6_reference_sequence_dictionary_alternative_locus_value = sqheaderalternativelocusvalue
                                                                  }
