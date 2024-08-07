{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedLists             #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE MultiWayIf                  #-}
{-# LANGUAGE PackageImports              #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE QuasiQuotes                 #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.CO.Base
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

module Data.SAM.Version1_6.Read.Parser.Header.CO.Base ( -- * SAM_V1_6 parser - header section (One-line text comment)
                                                        parse_SAM_V1_6_One_Line_Comment
                                                      ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error

import Data.Attoparsec.ByteString.Char8  as DABC8 (endOfLine,isEndOfLine)
import Data.Attoparsec.ByteString.Lazy   as DABL
import Text.Regex.PCRE.Heavy

-- | @"SAM_V1_6_One_Line_Comment"@ parser.
--
-- Defines a parser for @CO tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_One_Line_Comment :: Parser SAM_V1_6_One_Line_Comment
parse_SAM_V1_6_One_Line_Comment = do
  _     <- do
    coheaderp <-
      DABL.takeTill (== 09)
    -- Parse @PG tag of the header section.
    case (coheaderp =~ [re|[@][C][O]|]) of
      False ->
        fail $ show SAM_V1_6_Error_One_Line_Comment_Tag_Incorrect_Format
      True  ->
        -- @CO tag is in the accepted format.
        return ()
  _     <-
    word8 09
  value <-
    DABL.takeTill isEndOfLine
  _     <-
    endOfLine
  return SAM_V1_6_One_Line_Comment { sam_v1_6_one_line_comment_value = value
                                   }
