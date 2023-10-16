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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.PG.PP
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

module Data.SAM.Version1_6.Read.Parser.Header.PG.PP ( -- * SAM_V1_6 parser - header section (Program) - PP tag
                                                      parse_SAM_V1_6_Program_PP
                                                    ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error

import Data.Attoparsec.ByteString.Char8 (isEndOfLine)
import Data.Attoparsec.ByteString.Lazy   as DABL
import Text.Regex.PCRE.Heavy

-- | Defines a parser for the PP tag of the @PG tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Program_PP :: Parser SAM_V1_6_Program_Previous_PG_ID
parse_SAM_V1_6_Program_PP = do
  _ <- do pgheaderpreviouspgidtagp <- DABL.takeTill (== 58)
          -- Parse PP tag of the header section.
          case (pgheaderpreviouspgidtagp =~ [re|[P][P]|]) of
            False -> fail $ show SAM_V1_6_Error_Program_Previous_PG_ID_Incorrect_Format 
            True  -> -- PP tag is in the accepted format. 
                     return ()
  _ <- word8 58
  pgheaderpreviouspgidvalue <- DABL.takeTill (\x -> x == 09 || isEndOfLine x)
  return SAM_V1_6_Program_Previous_PG_ID { sam_v1_6_program_previous_pg_id_value = pgheaderpreviouspgidvalue
                                         }
