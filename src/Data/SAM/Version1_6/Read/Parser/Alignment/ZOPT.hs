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
{-# Language QuasiQuotes           #-}

-- |
-- Module      :  Data.SAM.Version1_6.Read.Parser.Alignment.ZOPT
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

module Data.SAM.Version1_6.Read.Parser.Alignment.ZOPT ( -- * SAM_V1_6 parser - alignment section - zopt field 
                                                        parse_SAM_V1_6_Alignment_ZOPT
                                                      ) where

import Data.SAM.Version1_6.Alignment.ZOPT
import Data.SAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Char8  as DABC8 (isEndOfLine)
import           Data.Attoparsec.ByteString.Lazy   as DABL
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the optional zopt field of alignment section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Alignment_ZOPT :: Parser SAM_V1_6_Alignment_ZOPT
parse_SAM_V1_6_Alignment_ZOPT = do
  alignmentzoptfieldtag <- do alignmentzoptfieldtagp <- DABL.takeTill (== 58)
                              -- Parse ZOPT tag of the alignment section.
                              case (alignmentzoptfieldtagp =~ [re|[A-Za-z][A-Za-z0-9]|]) of
                                False -> fail $ show SAM_V1_6_Error_Alignment_ZOPT_Tag_Incorrect_Format
                                True  -> -- ZOPT tag is in the accepted format. 
                                         return alignmentzoptfieldtagp
  _ <- word8 58
  _ <- do alignmentzoptfieldtypep <- DABL.takeTill (== 58)
          -- Parse ZOPT type of the alignment section.
          case (alignmentzoptfieldtypep =~ [re|[Z]|]) of
            False -> fail $ show SAM_V1_6_Error_Alignment_ZOPT_Type_Incorrect_Format
            True  -> -- ZOPT type is in the accepted format.
                     return ()
  _ <- word8 58
  alignmentzoptfieldvalue <- do alignmentzoptfieldvaluep <- DABL.takeTill (\x -> x == 09 || isEndOfLine x)
                                -- Parse ZOPT value of the alignment section.
                                case (alignmentzoptfieldvaluep =~ [re|[ !-~]*|]) of
                                  False -> fail $ show SAM_V1_6_Error_Alignment_ZOPT_Value_Incorrect_Format
                                  True  -> -- ZOPT value is in the accepted format.
                                           return alignmentzoptfieldvaluep
  return SAM_V1_6_Alignment_ZOPT { sam_v1_6_alignment_zopt_tag   = alignmentzoptfieldtag
                                 , sam_v1_6_alignment_zopt_value = alignmentzoptfieldvalue
                                 }
