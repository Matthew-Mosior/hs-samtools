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
-- Module      :  Data.SAM.Version1_6.Read.Parser.Header.RG.Base
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

module Data.SAM.Version1_6.Read.Parser.Header.RG.Base ( -- * SAM_V1_6 parser - header section (Read group)
                                                        parse_SAM_V1_6_Read_Group
                                                      ) where

import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Error
import Data.SAM.Version1_6.Read.Parser.Header.RG.ID
import Data.SAM.Version1_6.Read.Parser.Header.RG.BC
import Data.SAM.Version1_6.Read.Parser.Header.RG.CN
import Data.SAM.Version1_6.Read.Parser.Header.RG.DS
import Data.SAM.Version1_6.Read.Parser.Header.RG.DT
import Data.SAM.Version1_6.Read.Parser.Header.RG.FO
import Data.SAM.Version1_6.Read.Parser.Header.RG.KS
import Data.SAM.Version1_6.Read.Parser.Header.RG.LB
import Data.SAM.Version1_6.Read.Parser.Header.RG.PG
import Data.SAM.Version1_6.Read.Parser.Header.RG.PI
import Data.SAM.Version1_6.Read.Parser.Header.RG.PL
import Data.SAM.Version1_6.Read.Parser.Header.RG.PM
import Data.SAM.Version1_6.Read.Parser.Header.RG.PU
import Data.SAM.Version1_6.Read.Parser.Header.RG.SM

import Control.Applicative.Permutations           (intercalateEffect,toPermutation,toPermutationWithDefault)
import Data.Attoparsec.ByteString.Char8  as DABC8 (endOfLine)
import Data.Attoparsec.ByteString.Lazy   as DABL
import Text.Regex.PCRE.Heavy

-- | @"SAM_V1_6_Read_Group"@ parser.
--
-- Defines a parser for @RG tag section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Read_Group :: Parser SAM_V1_6_Read_Group
parse_SAM_V1_6_Read_Group = do
  _  <- do
    rgheaderp <-
      DABL.takeTill (== 09)
    -- Parse @RG tag of the header section.
    case (rgheaderp =~ [re|[@][R][G]|]) of
      False ->
        fail $ show SAM_V1_6_Error_Read_Group_Tag_Incorrect_Format
      True  ->
        -- @RG tag is in the accepted format.
        return ()
  _  <-
    word8 09
  -- This parser assumes that the
  -- ID, BC, CN, DS, DT, FO, KS, LB, PG, PI, PL,
  -- PM, PU and SM tags can appear in any order.
  rg <-
    intercalateEffect (word8 09) $
      SAM_V1_6_Read_Group
        <$> toPermutation parse_SAM_V1_6_Read_Group_ID
        <*> toPermutationWithDefault Nothing
                                     (Just <$> parse_SAM_V1_6_Read_Group_BC)
        <*> toPermutationWithDefault Nothing
                                     (Just <$> parse_SAM_V1_6_Read_Group_CN)
        <*> toPermutationWithDefault Nothing
                                     (Just <$> parse_SAM_V1_6_Read_Group_DS)
        <*> toPermutationWithDefault Nothing
                                     (Just <$> parse_SAM_V1_6_Read_Group_DT)
        <*> toPermutationWithDefault Nothing
                                     (Just <$> parse_SAM_V1_6_Read_Group_FO)
        <*> toPermutationWithDefault Nothing
                                     (Just <$> parse_SAM_V1_6_Read_Group_KS)
        <*> toPermutationWithDefault Nothing
                                     (Just <$> parse_SAM_V1_6_Read_Group_LB)
        <*> toPermutationWithDefault Nothing
                                     (Just <$> parse_SAM_V1_6_Read_Group_PG)
        <*> toPermutationWithDefault Nothing
                                     (Just <$> parse_SAM_V1_6_Read_Group_PI)
        <*> toPermutationWithDefault Nothing
                                     (Just <$> parse_SAM_V1_6_Read_Group_PL)
        <*> toPermutationWithDefault Nothing
                                     (Just <$> parse_SAM_V1_6_Read_Group_PM)
        <*> toPermutationWithDefault Nothing
                                     (Just <$> parse_SAM_V1_6_Read_Group_PU)
        <*> toPermutationWithDefault Nothing
                                     (Just <$> parse_SAM_V1_6_Read_Group_SM)
  _  <-
    endOfLine
  return rg 
