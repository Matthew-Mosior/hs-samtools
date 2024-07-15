{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# Language QuasiQuotes           #-}

-- |
-- Module      :  Data.SAM.Version1_6.Read.Parser.Alignment.OptionalFields.FOPT
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

module Data.SAM.Version1_6.Read.Parser.Alignment.OptionalFields.FOPT ( -- * SAM_V1_6 parser - alignment section - fopt field 
                                                                       parse_SAM_V1_6_Alignment_OptionalFields_FOPT
                                                                     ) where

import Data.SAM.Version1_6.Alignment.OptionalFields.FOPT
import Data.SAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Char8  as DABC8 (isEndOfLine)
import           Data.Attoparsec.ByteString.Lazy   as DABL
import qualified Data.ByteString.Char8             as DBC8
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the optional fopt field of alignment section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Alignment_OptionalFields_FOPT :: Parser SAM_V1_6_Alignment_OptionalFields_FOPT
parse_SAM_V1_6_Alignment_OptionalFields_FOPT = do
  alignmentfoptfieldtag <- do
    alignmentfoptfieldtagp <-
      DABL.takeTill (== 58)
    -- Parse FOPT tag of the alignment section.
    case (alignmentfoptfieldtagp =~ [re|[A-Za-z][A-Za-z0-9]|]) of
      False ->
        fail $ show SAM_V1_6_Error_Alignment_FOPT_Tag_Incorrect_Format
      True  ->
        -- FOPT tag is in the accepted format. 
        return alignmentfoptfieldtagp
  _ <-
    word8 58
  _ <- do
    alignmentfoptfieldtypep <-
      DABL.takeTill (== 58)
    -- Parse FOPT type of the alignment section.
    case (alignmentfoptfieldtypep =~ [re|[f]|]) of
      False ->
        fail $ show SAM_V1_6_Error_Alignment_FOPT_Type_Incorrect_Format
      True  ->
        -- FOPT type is in the accepted format.
        return ()
  _ <-
    word8 58
  alignmentfoptfieldvalue <- do
    alignmentfoptfieldvaluep <-
      DABL.takeTill (\x -> x == 09 || isEndOfLine x)
    -- Parse FOPT value of the alignment section.
    case (alignmentfoptfieldvaluep =~ [re|[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?|]) of
      False ->
        fail $ show SAM_V1_6_Error_Alignment_FOPT_Value_Incorrect_Format
      True  ->
        -- FOPT value is in the accepted format.
        case (DBC8.readInteger alignmentfoptfieldvaluep) of
          Nothing                                 -> return (-1)
          Just (alignmentfoptfieldvalueinteger,_) -> return $ (fromInteger alignmentfoptfieldvalueinteger :: Float)
  return SAM_V1_6_Alignment_OptionalFields_FOPT
           { sam_v1_6_alignment_optionalfields_fopt_tag   = alignmentfoptfieldtag
           , sam_v1_6_alignment_optionalfields_fopt_value = alignmentfoptfieldvalue
           }
