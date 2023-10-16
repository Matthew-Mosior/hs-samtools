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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.SQ.AN
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

module Data.SAM.Version1_6.Read.Parser.Header.SQ.AN ( -- * SAM_V1_6 parser - header section (Reference sequence dictionary) - AN tag
                                                      parse_SAM_V1_6_Reference_Sequence_Dictionary_AN
                                                    ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error

import Data.Attoparsec.ByteString.Char8 (isEndOfLine)
import Data.Attoparsec.ByteString.Lazy   as DABL
import Text.Regex.PCRE.Heavy

-- | Defines a parser for the AN tag of the @SQ tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Reference_Sequence_Dictionary_AN :: Parser SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names
parse_SAM_V1_6_Reference_Sequence_Dictionary_AN = do
  _ <- do sqheaderalternativereferencesequencenamestagp <- DABL.takeTill (== 58)
          -- Parse AN tag of the header section.
          case (sqheaderalternativereferencesequencenamestagp =~ [re|[A][N]|]) of
            False -> fail $ show SAM_V1_6_Error_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names_Incorrect_Format
            True  -> -- AN tag is in the accepted format.
                     return ()
  _ <- word8 58
  sqheaderalternativereferencesequencenamesvalue <- do sqheaderalternativereferencesequencenamesvaluep <- DABL.takeTill (\x -> x == 09 || isEndOfLine x)
                                                       -- Parse AN value of the header section.
                                                       case (sqheaderalternativereferencesequencenamesvaluep =~ [re|[0-9A-Za-z!#$%&+.:;?@^_|~-][0-9A-Za-z!#$%&*+.:;=?@^_|~-]*(,[0-9A-Za-z!#$%&+.:;?@^_|~-][0-9A-Za-z!#$%&*+.:;=?@^_|~-]*)*|]) of
                                                         False -> fail $ show SAM_V1_6_Error_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names_Invalid_Value
                                                         True  -> -- AN value is in the accepted format.
                                                                  return sqheaderalternativereferencesequencenamesvaluep
  return SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names { sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value = sqheaderalternativereferencesequencenamesvalue
                                                                                     }
