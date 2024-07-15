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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.SQ.TP
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

module Data.SAM.Version1_6.Read.Parser.Header.SQ.TP ( -- * SAM_V1_6 parser - header section (Reference sequence dictionary) - TP tag
                                                      parse_SAM_V1_6_Reference_Sequence_Dictionary_TP
                                                    ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error

import Data.Attoparsec.ByteString.Char8 (isEndOfLine)
import Data.Attoparsec.ByteString.Lazy as DABL
import Text.Regex.PCRE.Heavy

-- | Defines a parser for the TP tag of the @SQ tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Reference_Sequence_Dictionary_TP :: Parser SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology
parse_SAM_V1_6_Reference_Sequence_Dictionary_TP = do
  _                             <- do
    sqheadermoleculetopologytagp <-
      DABL.takeTill (== 58)
    -- Parse TP tag of the header section.
    case (sqheadermoleculetopologytagp =~ [re|[T][P]|]) of
      False ->
        fail $ show SAM_V1_6_Error_Reference_Sequence_Dictionary_Molecule_Topology_Incorrect_Format
      True  ->
        -- TP tag is in the accepted format. 
        return ()
  _                             <-
    word8 58
  sqheadermoleculetopologyvalue <- do
    sqheadermoleculetopologyvaluep <-
      DABL.takeTill (\x -> x == 09 || isEndOfLine x)
    -- Parse TP value of the header section.
    case (sqheadermoleculetopologyvaluep =~ [re|[l][i][n][e][a][r]|[c][i][r][c][u][l][a][r]|]) of
      False ->
        fail $ show SAM_V1_6_Error_Reference_Sequence_Dictionary_Molecule_Topology_Invalid_Value
      True  ->
        -- TP value is in the accepted format.
        return sqheadermoleculetopologyvaluep
  return SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology { sam_v1_6_reference_sequence_dictionary_molecule_topology_value = sqheadermoleculetopologyvalue
                                                                  }
