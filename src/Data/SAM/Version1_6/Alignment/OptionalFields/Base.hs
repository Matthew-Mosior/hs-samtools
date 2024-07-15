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
-- Module      :  Data.SAM.Version1_6.Alignment.OptionalFields.Base
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.SAM.Version1_6.Alignment.OptionalFields.Base ( -- * SAM version 1.6 alignment optional data types
                                                           SAM_V1_6_Alignment_OptionalFields(..)
                                                         ) where

import Data.SAM.Version1_6.Alignment.OptionalFields.AOPT
import Data.SAM.Version1_6.Alignment.OptionalFields.IOPT
import Data.SAM.Version1_6.Alignment.OptionalFields.FOPT
import Data.SAM.Version1_6.Alignment.OptionalFields.ZOPT
import Data.SAM.Version1_6.Alignment.OptionalFields.HOPT
import Data.SAM.Version1_6.Alignment.OptionalFields.BOPT

import Data.Data
import Generics.Deriving.Base

-- | Custom SAM (version 1.6) @"SAM_V1_6_Alignment_OptionalFields"@ data type.
--
-- See section 1.4 and 1.5 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Alignment_OptionalFields = SAM_V1_6_Alignment_OptionalFields
  { sam_v1_6_alignment_optionalfields_aopt  :: Maybe SAM_V1_6_Alignment_OptionalFields_AOPT -- ^ A - [!-~] - Printable characters.
  , sam_v1_6_alignment_optionalfields_iopt  :: Maybe SAM_V1_6_Alignment_OptionalFields_IOPT -- ^ i - [-+]?[0-9]+ - Signed integer.
  , sam_v1_6_alignment_optionalfields_fopt  :: Maybe SAM_V1_6_Alignment_OptionalFields_FOPT -- ^ f - [-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)? - Single-precision floating number.
  , sam_v1_6_alignment_optionalfields_zopt  :: Maybe SAM_V1_6_Alignment_OptionalFields_ZOPT -- ^ Z - [ !-~]* - Printable string, including space.
  , sam_v1_6_alignment_optionalfields_hopt  :: Maybe SAM_V1_6_Alignment_OptionalFields_HOPT -- ^ H - ([0-9A-F][0-9A-F])* - Byte array in the Hex format.
  , sam_v1_6_alignment_optionalfields_bopt  :: Maybe SAM_V1_6_Alignment_OptionalFields_BOPT -- ^ B - [cCsSiIf]&#8203;(,[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)* - Integer or numeric array.
  } deriving (Generic,Typeable)

instance Eq SAM_V1_6_Alignment_OptionalFields where
  SAM_V1_6_Alignment_OptionalFields sam_v1_6_alignment_optionalfields_aopt1
                                    sam_v1_6_alignment_optionalfields_iopt1
                                    sam_v1_6_alignment_optionalfields_fopt1
                                    sam_v1_6_alignment_optionalfields_zopt1
                                    sam_v1_6_alignment_optionalfields_hopt1
                                    sam_v1_6_alignment_optionalfields_bopt1 ==
    SAM_V1_6_Alignment_OptionalFields sam_v1_6_alignment_optionalfields_aopt2
                                      sam_v1_6_alignment_optionalfields_iopt2
                                      sam_v1_6_alignment_optionalfields_fopt2
                                      sam_v1_6_alignment_optionalfields_zopt2
                                      sam_v1_6_alignment_optionalfields_hopt2
                                      sam_v1_6_alignment_optionalfields_bopt2 =
      sam_v1_6_alignment_optionalfields_aopt1  == sam_v1_6_alignment_optionalfields_aopt2  &&
      sam_v1_6_alignment_optionalfields_iopt1  == sam_v1_6_alignment_optionalfields_iopt2  &&
      sam_v1_6_alignment_optionalfields_fopt1  == sam_v1_6_alignment_optionalfields_fopt2  &&
      sam_v1_6_alignment_optionalfields_zopt1  == sam_v1_6_alignment_optionalfields_zopt2  &&
      sam_v1_6_alignment_optionalfields_hopt1  == sam_v1_6_alignment_optionalfields_hopt2  &&
      sam_v1_6_alignment_optionalfields_bopt1  == sam_v1_6_alignment_optionalfields_bopt2

instance Show SAM_V1_6_Alignment_OptionalFields where
  show (SAM_V1_6_Alignment_OptionalFields aopt
                                          iopt
                                          fopt
                                          zopt
                                          hopt
                                          bopt
       ) =
    "SAM_V1_6_Alignment_OptionalFields { "          ++
    "sam_v1_6_alignment_optionalfields_aopt = "     ++
    ( show aopt)                                    ++
    " , sam_v1_6_alignment_optionalfields_iopt = "  ++
    (show iopt)                                     ++
    " , sam_v1_6_alignment_optionalfields_fopt = "  ++
    (show fopt)                                     ++
    " , sam_v1_6_alignment_optionalfields_zopt = "  ++
    (show zopt)                                     ++
    " , sam_v1_6_alignment_optionalfields_hopt = "  ++
    (show hopt)                                     ++
    " , sam_v1_6_alignment_optionalfields_bopt = "  ++
    (show bopt)                                     ++
    " }"
