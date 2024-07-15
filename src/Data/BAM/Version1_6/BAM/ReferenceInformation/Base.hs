{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.BAM.ReferenceInformation.Base
-- Copyright   :  (c) Matthew Mosior 2023
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.BAM.ReferenceInformation.Base ( -- * BAM_V1_6_BAM_Reference_Information version 1.6 data type
                                                           BAM_V1_6_BAM_Reference_Information(..)
                                                         ) where

import Data.ByteString
import Data.Data
import Data.Word
import Generics.Deriving.Base

-- | Custom @"BAM_V1_6_BAM_Reference_Information"@ (BAM version 1.6) data type.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BAM_Reference_Information = BAM_V1_6_BAM_Reference_Information
  { bam_v1_6_bam_reference_information_l_name :: Word32     -- ^ Length of the reference name plus 1 (including NUL).
  , bam_v1_6_bam_reference_information_name   :: ByteString -- ^ Reference sequence name; NUL terminated.
  , bam_v1_6_bam_reference_information_l_ref  :: Word32     -- ^ Length of the reference sequence.
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM_Reference_Information where
  BAM_V1_6_BAM_Reference_Information bam_v1_6_bam_reference_information_l_name1
                                     bam_v1_6_bam_reference_information_name1
                                     bam_v1_6_bam_reference_information_l_ref1 ==
    BAM_V1_6_BAM_Reference_Information bam_v1_6_bam_reference_information_l_name2
                                       bam_v1_6_bam_reference_information_name2
                                       bam_v1_6_bam_reference_information_l_ref2 =
      bam_v1_6_bam_reference_information_l_name1 == bam_v1_6_bam_reference_information_l_name2 &&
      bam_v1_6_bam_reference_information_name1   == bam_v1_6_bam_reference_information_name2   &&
      bam_v1_6_bam_reference_information_l_ref1  == bam_v1_6_bam_reference_information_l_ref2

instance Show BAM_V1_6_BAM_Reference_Information where
  show (BAM_V1_6_BAM_Reference_Information l_name
                                           name
                                           l_ref
       ) =
    "BAM_V1_6_BAM_Reference_Information { "          ++
    "bam_v1_6_bam_reference_information_l_name = "   ++
    (show l_name)                                    ++
    " , bam_v1_6_bam_reference_information_name = "  ++
    (show name)                                      ++
    " , bam_v1_6_bam_reference_information_l_ref = " ++
    (show l_ref)                                     ++
    " }"
