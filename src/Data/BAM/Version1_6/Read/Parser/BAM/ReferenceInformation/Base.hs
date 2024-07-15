{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeFamilies                #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Module      :  Data.BAM.Version1_6.Read.Parser.BAM.ReferenceInformation.Base
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

module Data.BAM.Version1_6.Read.Parser.BAM.ReferenceInformation.Base ( -- * BAM_V1_6_BAM parser - reference information section
                                                                       parse_BAM_V1_6_BAM_Reference_Information
                                                                     ) where

import Data.BAM.Version1_6.BAM.ReferenceInformation
import Data.BAM.Version1_6.Internal

import Data.Attoparsec.ByteString.Lazy as DABL
import Data.ByteString                 as DB

-- | @"BAM_V1_6_BAM_Reference_Information"@ parser.
--
-- Defines a parser for the alignment section of the BAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_BAM_V1_6_BAM_Reference_Information :: Parser BAM_V1_6_BAM_Reference_Information
parse_BAM_V1_6_BAM_Reference_Information = do
  l_name <-
    DABL.take 4
  name   <-
    DABL.take            $
      fromIntegral       $
        word8sToWord32LE $
          DB.unpack l_name
  l_ref  <-
    DABL.take 4
  return BAM_V1_6_BAM_Reference_Information
           { bam_v1_6_bam_reference_information_l_name = word8sToWord32LE $
                                                           DB.unpack l_name
           , bam_v1_6_bam_reference_information_name   = name
           , bam_v1_6_bam_reference_information_l_ref  = word8sToWord32LE $
                                                           DB.unpack l_ref
           }
