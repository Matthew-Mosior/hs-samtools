{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedLists             #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE PackageImports              #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# Language QuasiQuotes                 #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Module      :  Data.SAM.Version1_6.Alignment.Base
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

module Data.SAM.Version1_6.Alignment.Base ( -- * SAM version 1.6 alignment mandatory and optional data types
                                            SAM_V1_6_Alignment(..)
                                          ) where

import Data.SAM.Version1_6.Alignment.BOPT

import Data.ByteString
import Data.Data
import Data.Sequence
import Data.Word
import Generics.Deriving.Base


-- | Custom SAM (version 1.6) @"SAM_V1_6_Alignment"@ data type.
-- See section 1.4 and 1.5 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Alignment = SAM_V1_6_Alignment { sam_v1_6_alignment_qname :: ByteString
                                             , sam_v1_6_alignment_flag  :: Int
                                             , sam_v1_6_alignment_rname :: ByteString
                                             , sam_v1_6_alignment_pos   :: Integer
                                             , sam_v1_6_alignment_mapq  :: Int
                                             , sam_v1_6_alignment_cigar :: ByteString
                                             , sam_v1_6_alignment_rnext :: ByteString
                                             , sam_v1_6_alignment_pnext :: Integer
                                             , sam_v1_6_alignment_tlen  :: Integer
                                             , sam_v1_6_alignment_seq   :: ByteString
                                             , sam_v1_6_alignment_qual  :: ByteString
                                             , sam_v1_6_alignment_aopt  :: Maybe ByteString
                                             , sam_v1_6_alignment_iopt  :: Maybe Integer
                                             , sam_v1_6_alignment_fopt  :: Maybe Float
                                             , sam_v1_6_alignment_zopt  :: Maybe ByteString
                                             , sam_v1_6_alignment_hopt  :: Maybe (Seq Word8)
                                             , sam_v1_6_alignment_bopt  :: Maybe SAM_V1_6_Alignment_BOPT
                                             }
  deriving (Generic,Typeable)

instance Show SAM_V1_6_Alignment where
  show (SAM_V1_6_Alignment qname flag rname pos mapq cigar rnext pnext tlen seq qual aopt iopt fopt zopt hopt bopt) =
    "SAM_V1_6_Alignment { " ++
    "qname = "              ++
    (show qname)            ++
    " , flag = "            ++
    (show flag)             ++
    " , rname = "           ++
    (show rname)            ++
    " , pos = "             ++
    (show pos)              ++
    " , mapq = "            ++
    (show mapq)             ++
    " , cigar = "           ++
    (show cigar)            ++
    " , rnext = "           ++
    (show rnext)            ++
    " , pnext = "           ++
    (show pnext)            ++
    " , tlen = "            ++
    (show tlen)             ++
    " , seq = "             ++
    (show seq)              ++
    " , qual = "            ++
    (show qual)             ++
    " , aopt = "            ++
    ( show aopt)            ++
    " , iopt = "            ++
    (show iopt)             ++
    " , fopt = "            ++
    (show fopt)             ++
    " , zopt = "            ++
    (show zopt)             ++
    " , hopt = "            ++
    (show hopt)             ++
    " , bopt = "            ++
    (show bopt)             ++
    " }"
