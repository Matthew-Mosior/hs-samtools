{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StrictData                  #-}
{-# LANGUAGE TypeFamilies                #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Module      :  Data.BAM.Version1_6.BAM.Alignment.OptionalFields.Base
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.BAM.Alignment.OptionalFields.Base ( -- * BAM version 1.6 alignment optional fields data type
                                                               BAM_V1_6_BAM_Alignment_OptionalFields(..)
                                                             , BAM_V1_6_BAM_Alignment_OptionalFields_SumType(..)
                                                             ) where

import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.AOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.BigCOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.BigIOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.BigSOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.SmallCOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.SmallIOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.SmallSOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.FOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.ZOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.HOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.BOPT

import Data.Data
import Generics.Deriving.Base

-- | Custom BAM (version 1.6) @"BAM_V1_6_BAM_Alignment_Optional_Fields"@ data type.
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BAM_Alignment_OptionalFields = BAM_V1_6_BAM_Alignment_OptionalFields
  { bam_v1_6_bam_alignment_optionalfields_aopt        :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_AOPT      -- ^ A - [!-~] -
                                                                                                               -- Printable character.
  , bam_v1_6_bam_alignment_optionalfields_smallcopt   :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_SmallCOPT -- ^ c - [0-9]+ -
                                                                                                               -- Int8 (signed).
  , bam_v1_6_bam_alignment_optionalfields_bigcopt     :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_BigCOPT   -- ^ C - [0-9]+ -
                                                                                                               -- Word8 (unsigned).
  , bam_v1_6_bam_alignment_optionalfields_smalliopt   :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_SmallIOPT -- ^ i - [0-9]+ -
                                                                                                               -- Int32 (signed).
  , bam_v1_6_bam_alignment_optionalfields_bigiopt     :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_BigIOPT   -- ^ I - [0-9]+ -
                                                                                                               -- Word32 (unsigned).
  , bam_v1_6_bam_alignment_optionalfields_smallsopt   :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_SmallSOPT -- ^ s - [0-9]+ -
                                                                                                               -- Int16 (signed).
  , bam_v1_6_bam_alignment_optionalfields_bigsopt     :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_BigSOPT   -- ^ S - [0-9]+ -
                                                                                                               -- Word16 (unsigned).
  , bam_v1_6_bam_alignment_optionalfields_fopt        :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_FOPT      -- ^ f - [-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)? -
                                                                                                               -- Single-precision floating number.
  , bam_v1_6_bam_alignment_optionalfields_zopt        :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_ZOPT      -- ^ Z - [ !-~]* -
                                                                                                               -- Printable string, including space.
  , bam_v1_6_bam_alignment_optionalfields_hopt        :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_HOPT      -- ^ H - ([0-9A-F][0-9A-F])* -
                                                                                                               -- Byte array in the Hex format.
  , bam_v1_6_bam_alignment_optionalfields_bopt        :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_BOPT      -- ^ B - [cCsSiIf]&#8203;(,[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)* -
                                                                                                               -- Integer or numeric array.
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM_Alignment_OptionalFields where
  BAM_V1_6_BAM_Alignment_OptionalFields bam_v1_6_bam_alignment_optionalfields_aopt1
                                        bam_v1_6_bam_alignment_optionalfields_smallcopt1
                                        bam_v1_6_bam_alignment_optionalfields_bigcopt1
                                        bam_v1_6_bam_alignment_optionalfields_smalliopt1
                                        bam_v1_6_bam_alignment_optionalfields_bigiopt1
                                        bam_v1_6_bam_alignment_optionalfields_smallsopt1
                                        bam_v1_6_bam_alignment_optionalfields_bigsopt1
                                        bam_v1_6_bam_alignment_optionalfields_fopt1
                                        bam_v1_6_bam_alignment_optionalfields_zopt1
                                        bam_v1_6_bam_alignment_optionalfields_hopt1
                                        bam_v1_6_bam_alignment_optionalfields_bopt1 ==
    BAM_V1_6_BAM_Alignment_OptionalFields bam_v1_6_bam_alignment_optionalfields_aopt2
                                          bam_v1_6_bam_alignment_optionalfields_smallcopt2
                                          bam_v1_6_bam_alignment_optionalfields_bigcopt2
                                          bam_v1_6_bam_alignment_optionalfields_smalliopt2
                                          bam_v1_6_bam_alignment_optionalfields_bigiopt2
                                          bam_v1_6_bam_alignment_optionalfields_smallsopt2
                                          bam_v1_6_bam_alignment_optionalfields_bigsopt2
                                          bam_v1_6_bam_alignment_optionalfields_fopt2
                                          bam_v1_6_bam_alignment_optionalfields_zopt2
                                          bam_v1_6_bam_alignment_optionalfields_hopt2
                                          bam_v1_6_bam_alignment_optionalfields_bopt2 =
      bam_v1_6_bam_alignment_optionalfields_aopt1      ==  bam_v1_6_bam_alignment_optionalfields_aopt2        &&
      bam_v1_6_bam_alignment_optionalfields_smallcopt1 ==  bam_v1_6_bam_alignment_optionalfields_smallcopt2   &&
      bam_v1_6_bam_alignment_optionalfields_bigcopt1   ==  bam_v1_6_bam_alignment_optionalfields_bigcopt2     &&
      bam_v1_6_bam_alignment_optionalfields_smalliopt1 ==  bam_v1_6_bam_alignment_optionalfields_smalliopt2   &&
      bam_v1_6_bam_alignment_optionalfields_bigiopt1   ==  bam_v1_6_bam_alignment_optionalfields_bigiopt2     &&
      bam_v1_6_bam_alignment_optionalfields_smallsopt1 ==  bam_v1_6_bam_alignment_optionalfields_smallsopt2   &&
      bam_v1_6_bam_alignment_optionalfields_bigsopt1   ==  bam_v1_6_bam_alignment_optionalfields_bigsopt2     &&
      bam_v1_6_bam_alignment_optionalfields_fopt1      ==  bam_v1_6_bam_alignment_optionalfields_fopt2        &&
      bam_v1_6_bam_alignment_optionalfields_zopt1      ==  bam_v1_6_bam_alignment_optionalfields_zopt2        &&
      bam_v1_6_bam_alignment_optionalfields_hopt1      ==  bam_v1_6_bam_alignment_optionalfields_hopt2        &&
      bam_v1_6_bam_alignment_optionalfields_bopt1      ==  bam_v1_6_bam_alignment_optionalfields_bopt2

instance Show BAM_V1_6_BAM_Alignment_OptionalFields where
  show ( BAM_V1_6_BAM_Alignment_OptionalFields aopt
                                                smallcopt
                                                bigcopt
                                                smalliopt
                                                bigiopt
                                                smallsopt
                                                bigsopt
                                                fopt
                                                zopt
                                                hopt
                                                bopt
       ) =
    "BAM_V1_6_BAM_Alignment_OptionalFields { "             ++
    "bam_v1_6_bam_alignment_optionalfields_aopt = "         ++
    (show aopt)                                             ++
    " , bam_v1_6_bam_alignment_optionalfields_smallcopt = " ++
    (show smallcopt)                                        ++
    " , bam_v1_6_bam_alignment_optionalfields_bigcopt = "   ++
    (show bigcopt)                                          ++
    " , bam_v1_6_bam_alignment_optionalfields_smalliopt = " ++
    (show smalliopt)                                        ++
    " , bam_v1_6_bam_alignment_optionalfields_bigiopt = "   ++
    (show bigiopt)                                          ++
    " , bam_v1_6_bam_alignment_optionalfields_smallsopt = " ++
    (show smallsopt)                                        ++
    " , bam_v1_6_bam_alignment_optionalfields_bigsopt = "   ++
    (show bigsopt)                                          ++
    " , bam_v1_6_bam_alignment_optionalfields_fopt = "      ++
    (show fopt)                                             ++
    " , bam_v1_6_bam_alignment_optionalfields_zopt = "      ++
    (show zopt)                                             ++
    " , bam_v1_6_bam_alignment_optionalfields_hopt = "      ++
    (show hopt)                                             ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt = "      ++
    (show bopt)                                             ++
    " }"

-- | Sum type that can represent any of the
-- BAM_V1_6_BAM_Alignment_OptionalFields types.
data BAM_V1_6_BAM_Alignment_OptionalFields_SumType =
    AOPT
  | SmallCOPT
  | BigCOPT
  | SmallIOPT
  | BigIOPT
  | SmallSOPT
  | BigSOPT
  | FOPT
  | ZOPT
  | HOPT
  | BOPT
  | Empty
  deriving (Eq,Generic,Typeable)
