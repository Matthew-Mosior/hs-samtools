{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.SAM.Version1_6.Alignment.OptionalFields.FOPT
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

module Data.SAM.Version1_6.Alignment.OptionalFields.FOPT ( -- * SAM version 1.6 alignment optional fields data type
                                                           SAM_V1_6_Alignment_OptionalFields_FOPT(..)
                                                         ) where

import Data.ByteString (ByteString)
import Data.Data
import Generics.Deriving.Base

-- | Custom SAM (version 1.6) @"SAM_V1_6_Alignment_OptionalFields_FOPT"@ data type.
--
-- See section 1.5 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Alignment_OptionalFields_FOPT = SAM_V1_6_Alignment_OptionalFields_FOPT
  { sam_v1_6_alignment_optionalfields_fopt_tag   :: ByteString 
  , sam_v1_6_alignment_optionalfields_fopt_value :: Float
  } deriving (Generic,Typeable)

instance Eq SAM_V1_6_Alignment_OptionalFields_FOPT where
  SAM_V1_6_Alignment_OptionalFields_FOPT sam_v1_6_alignment_optionalfields_fopt_tag1
                                         sam_v1_6_alignment_optionalfields_fopt_value1 ==
    SAM_V1_6_Alignment_OptionalFields_FOPT sam_v1_6_alignment_optionalfields_fopt_tag2
                                           sam_v1_6_alignment_optionalfields_fopt_value2 =
      sam_v1_6_alignment_optionalfields_fopt_tag1 == sam_v1_6_alignment_optionalfields_fopt_tag2 &&
      sam_v1_6_alignment_optionalfields_fopt_value1 == sam_v1_6_alignment_optionalfields_fopt_value2

instance Show SAM_V1_6_Alignment_OptionalFields_FOPT where
  show (SAM_V1_6_Alignment_OptionalFields_FOPT tag
                                               value
       ) =
    "SAM_V1_6_Alignment_OptionalFields_FOPT { "          ++
    "sam_v1_6_alignment_optionalfields_fopt_tag = "      ++
    (show tag)                                           ++
    " , sam_v1_6_alignment_optionalfields_fopt_value = " ++
    (show value)                                         ++
    " }"
