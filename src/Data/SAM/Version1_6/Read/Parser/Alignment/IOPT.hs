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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Alignment.IOPT
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

module Data.SAM.Version1_6.Read.Parser.Alignment.IOPT ( -- * SAM_V1_6 parser - alignment section - iopt field 
                                                        parse_SAM_V1_6_Alignment_IOPT
                                                      ) where

import Data.SAM.Version1_6.Alignment.IOPT
import Data.SAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Char8  as DABC8 (isEndOfLine)
import           Data.Attoparsec.ByteString.Lazy   as DABL
import qualified Data.ByteString.Char8             as DBC8
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the optional iopt field of alignment section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Alignment_IOPT :: Parser SAM_V1_6_Alignment_IOPT
parse_SAM_V1_6_Alignment_IOPT = do
  alignmentioptfieldtag <- do alignmentioptfieldtagp <- DABL.takeTill (== 58)
                              -- Parse IOPT tag of the alignment section.
                              case (alignmentioptfieldtagp =~ [re|[A-Za-z][A-Za-z0-9]|]) of
                                False -> fail $ show SAM_V1_6_Error_Alignment_IOPT_Tag_Incorrect_Format
                                True  -> -- IOPT tag is in the accepted format. 
                                         return alignmentioptfieldtagp
  _ <- word8 58
  _ <- do alignmentioptfieldtypep <- DABL.takeTill (== 58)
          -- Parse IOPT type of the alignment section.
          case (alignmentioptfieldtypep =~ [re|[i]|]) of
            False -> fail $ show SAM_V1_6_Error_Alignment_IOPT_Type_Incorrect_Format
            True  -> -- IOPT type is in the accepted format.
                     return ()
  _ <- word8 58
  alignmentioptfieldvalue <- do alignmentioptfieldvaluep <- DABL.takeTill (\x -> x == 09 || isEndOfLine x)
                                -- Parse IOPT value of the alignment section.
                                case (alignmentioptfieldvaluep =~ [re|[-+]?[0-9]+|]) of
                                  False -> fail $ show SAM_V1_6_Error_Alignment_IOPT_Value_Incorrect_Format
                                  True  -> -- IOPT value is in the accepted format.
                                           case (DBC8.readInteger alignmentioptfieldvaluep) of
                                             Nothing                                 -> return (-1)
                                             Just (alignmentioptfieldvalueinteger,_) -> return alignmentioptfieldvalueinteger
  return SAM_V1_6_Alignment_IOPT { sam_v1_6_alignment_iopt_tag   = alignmentioptfieldtag
                                 , sam_v1_6_alignment_iopt_value = alignmentioptfieldvalue
                                 }
