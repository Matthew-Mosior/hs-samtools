{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.SAM.Version1_6.Alignment.IOPT
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

module Data.SAM.Version1_6.Alignment.IOPT ( -- * SAM version 1.6 alignment optional fields data type
                                            SAM_V1_6_Alignment_IOPT(..)
                                          ) where

import Data.ByteString (ByteString)
import Data.Data
import Generics.Deriving.Base


-- | Custom SAM (version 1.6) @"SAM_V1_6_Alignment_IOPT"@ data type.
--
-- See section 1.5 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Alignment_IOPT = SAM_V1_6_Alignment_IOPT { sam_v1_6_alignment_iopt_tag   :: ByteString 
                                                       , sam_v1_6_alignment_iopt_value :: Integer
                                                       }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Alignment_IOPT where
  SAM_V1_6_Alignment_IOPT sam_v1_6_alignment_iopt_tag1
                          sam_v1_6_alignment_iopt_value1 == SAM_V1_6_Alignment_IOPT sam_v1_6_alignment_iopt_tag2
                                                                                    sam_v1_6_alignment_iopt_value2 = sam_v1_6_alignment_iopt_tag1   == sam_v1_6_alignment_iopt_tag2     &&
                                                                                                                     sam_v1_6_alignment_iopt_value1  == sam_v1_6_alignment_iopt_value2

instance Show SAM_V1_6_Alignment_IOPT where
  show (SAM_V1_6_Alignment_IOPT tag
                                value
       ) =
    "SAM_V1_6_Alignment_IOPT { "          ++
    "sam_v1_6_alignment_iopt_tag = "      ++
    (show tag)                            ++
    " , sam_v1_6_alignment_iopt_value = " ++
    (show value)                          ++
    " }"
