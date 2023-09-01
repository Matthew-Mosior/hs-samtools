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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.SQ.M5
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

module Data.SAM.Version1_6.Read.Parser.Header.SQ.M5 ( -- * SAM_V1_6 parser - header section (Reference sequence dictionary) - M5 tag
                                                      parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_M5
                                                    ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Lazy   as DABL
import qualified Data.ByteString                   as DB   (unpack)
import           Data.Sequence                     as DSeq
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the M5 tag of the @SQ tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_M5 :: Parser SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum
parse_SAM_V1_6_SAM_V1_6_Reference_Sequence_Dictionary_M5 = do
  sqheadermd5checksumtag <- do sqheadermd5checksumtagp <- DABL.takeTill (== 58)
                               -- Parse M5 tag of the header section.
                               case (sqheadermd5checksumtagp =~ [re|[M][5]|]) of
                                 False -> fail $ show SAM_V1_6_Error_Reference_Sequence_Dictionary_MD5_Checksum_Incorrect_Format 
                                 True  -> -- M5 tag is in the accepted format.
                                          return sqheadermd5checksumtagp
  _ <- word8 58
  sqheadermd5checksumvalue <- DABL.takeTill (== 09)
  return SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum { sam_v1_6_reference_sequence_dictionary_md5_checksum_tag   = DSeq.fromList $ DB.unpack sqheadermd5checksumtag
                                                             , sam_v1_6_reference_sequence_dictionary_md5_checksum_value = sqheadermd5checksumvalue
                                                             }
