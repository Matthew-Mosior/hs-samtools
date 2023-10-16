{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# Language QuasiQuotes           #-}

-- |
-- Module      :  Data.SAM.Version1_6.Alignment.BOPT
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

module Data.SAM.Version1_6.Alignment.BOPT ( -- * SAM version 1.6 alignment optional fields data type
                                            SAM_V1_6_Alignment_BOPT(..),
                                            SAM_V1_6_Alignment_BOPT_Int8(..),
                                            SAM_V1_6_Alignment_BOPT_Word8(..),
                                            SAM_V1_6_Alignment_BOPT_Int16(..),
                                            SAM_V1_6_Alignment_BOPT_Word16(..),
                                            SAM_V1_6_Alignment_BOPT_Int32(..),
                                            SAM_V1_6_Alignment_BOPT_Word32(..),
                                            SAM_V1_6_Alignment_BOPT_Float(..)
                                          ) where

import Data.Data
import Data.Int
import Data.Sequence
import Data.Word
import Generics.Deriving.Base


-- | Custom SAM (version 1.6) @"SAM_V1_6_Alignment_BOPT"@ data type.
--
-- See section 1.5 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Alignment_BOPT = SAM_V1_6_Alignment_BOPT { sam_v1_6_alignment_bopt_int8   :: Maybe SAM_V1_6_Alignment_BOPT_Int8
                                                       , sam_v1_6_alignment_bopt_word8  :: Maybe SAM_V1_6_Alignment_BOPT_Word8
                                                       , sam_v1_6_alignment_bopt_int16  :: Maybe SAM_V1_6_Alignment_BOPT_Int16
                                                       , sam_v1_6_alignment_bopt_word16 :: Maybe SAM_V1_6_Alignment_BOPT_Word16
                                                       , sam_v1_6_alignment_bopt_int32  :: Maybe SAM_V1_6_Alignment_BOPT_Int32
                                                       , sam_v1_6_alignment_bopt_word32 :: Maybe SAM_V1_6_Alignment_BOPT_Word32
                                                       , sam_v1_6_alignment_bopt_float  :: Maybe SAM_V1_6_Alignment_BOPT_Float
                                                       }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Alignment_BOPT where
  SAM_V1_6_Alignment_BOPT sam_v1_6_alignment_bopt_int81
                          sam_v1_6_alignment_bopt_word81
                          sam_v1_6_alignment_bopt_int161
                          sam_v1_6_alignment_bopt_word161
                          sam_v1_6_alignment_bopt_int321
                          sam_v1_6_alignment_bopt_word321
                          sam_v1_6_alignment_bopt_float1 == SAM_V1_6_Alignment_BOPT sam_v1_6_alignment_bopt_int82
                                                                                    sam_v1_6_alignment_bopt_word82
                                                                                    sam_v1_6_alignment_bopt_int162
                                                                                    sam_v1_6_alignment_bopt_word162
                                                                                    sam_v1_6_alignment_bopt_int322
                                                                                    sam_v1_6_alignment_bopt_word322
                                                                                    sam_v1_6_alignment_bopt_float2 = sam_v1_6_alignment_bopt_int81   == sam_v1_6_alignment_bopt_int82   &&
                                                                                                                     sam_v1_6_alignment_bopt_word81  == sam_v1_6_alignment_bopt_word82  &&
                                                                                                                     sam_v1_6_alignment_bopt_int161  == sam_v1_6_alignment_bopt_int162  &&
                                                                                                                     sam_v1_6_alignment_bopt_word161 == sam_v1_6_alignment_bopt_word162 &&
                                                                                                                     sam_v1_6_alignment_bopt_int321  == sam_v1_6_alignment_bopt_int322  &&
                                                                                                                     sam_v1_6_alignment_bopt_word321 == sam_v1_6_alignment_bopt_word322 &&
                                                                                                                     sam_v1_6_alignment_bopt_float1  == sam_v1_6_alignment_bopt_float2

instance Show SAM_V1_6_Alignment_BOPT where
  show (SAM_V1_6_Alignment_BOPT int8
                                word8
                                int16
                                word16
                                int32
                                word32
                                float
       ) =
    "SAM_V1_6_Alignment_BOPT { "           ++
    "sam_v1_6_alignment_bopt_int8 = "      ++
    (show int8)                            ++
    " , sam_v1_6_alignment_bopt_word8 = "  ++
    (show word8)                           ++
    " , sam_v1_6_alignment_bopt_int16 = "  ++
    (show int16)                           ++
    " , sam_v1_6_alignment_bopt_word16 = " ++
    (show word16)                          ++
    " , sam_v1_6_alignment_bopt_int32 = "  ++
    (show int32)                           ++
    " , sam_v1_6_alignment_bopt_word32 = " ++
    (show word32)                          ++
    " , sam_v1_6_alignment_bopt_float = "  ++
    (show float)                           ++
    " }"

-- | __c__CsSiIf of the last optional field (type B).
--
-- See section 1.5 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Alignment_BOPT_Int8 = SAM_V1_6_Alignment_BOPT_Int8 { sam_v1_6_alignment_bopt_int8_tag   :: Seq Word8
                                                                 , sam_v1_6_alignment_bopt_int8_type  :: Word8
                                                                 , sam_v1_6_alignment_bopt_int8_value :: Seq Int8
                                                                 }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Alignment_BOPT_Int8 where
  SAM_V1_6_Alignment_BOPT_Int8 sam_v1_6_alignment_bopt_int8_tag1 sam_v1_6_alignment_bopt_int8_type1 sam_v1_6_alignment_bopt_int8_value1 == SAM_V1_6_Alignment_BOPT_Int8 sam_v1_6_alignment_bopt_int8_tag2 sam_v1_6_alignment_bopt_int8_type2 sam_v1_6_alignment_bopt_int8_value2 = sam_v1_6_alignment_bopt_int8_tag1 == sam_v1_6_alignment_bopt_int8_tag2 && sam_v1_6_alignment_bopt_int8_type1 == sam_v1_6_alignment_bopt_int8_type2 && sam_v1_6_alignment_bopt_int8_value1 == sam_v1_6_alignment_bopt_int8_value2

instance Show SAM_V1_6_Alignment_BOPT_Int8 where
  show (SAM_V1_6_Alignment_BOPT_Int8 tag
                                     bopttype
                                     value
       ) =
    "SAM_V1_6_Alignment_BOPT_Int8 { " ++
    "tag  = "                         ++
    (show tag)                        ++
    " , type = "                      ++
    (show bopttype)                   ++
    " , value = "                     ++
    (show value)                      ++
    " }"

-- | c__C__sSiIf of the last optional field (type B).
--
-- See section 1.5 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Alignment_BOPT_Word8 = SAM_V1_6_Alignment_BOPT_Word8 { sam_v1_6_alignment_bopt_word8_tag   :: Seq Word8
                                                                   , sam_v1_6_alignment_bopt_word8_type  :: Word8
                                                                   , sam_v1_6_alignment_bopt_word8_value :: Seq Word8
                                                                   }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Alignment_BOPT_Word8 where
  SAM_V1_6_Alignment_BOPT_Word8 sam_v1_6_alignment_bopt_word8_tag1 sam_v1_6_alignment_bopt_word8_type1 sam_v1_6_alignment_bopt_word8_value1 == SAM_V1_6_Alignment_BOPT_Word8 sam_v1_6_alignment_bopt_word8_tag2 sam_v1_6_alignment_bopt_word8_type2 sam_v1_6_alignment_bopt_word8_value2 = sam_v1_6_alignment_bopt_word8_tag1 == sam_v1_6_alignment_bopt_word8_tag2 && sam_v1_6_alignment_bopt_word8_type1 == sam_v1_6_alignment_bopt_word8_type2 && sam_v1_6_alignment_bopt_word8_value1 == sam_v1_6_alignment_bopt_word8_value2

instance Show SAM_V1_6_Alignment_BOPT_Word8 where
  show (SAM_V1_6_Alignment_BOPT_Word8 tag
                                      bopttype
                                      value
       ) =
    "SAM_V1_6_Alignment_BOPT_Word8 { " ++
    "tag  = "                          ++
    (show tag)                         ++
    " , type = "                       ++
    (show bopttype)                    ++
    " , value = "                      ++
    (show value)                       ++
    " }"

-- | cC__s__SiIf of the last optional field (type B).
--
-- See section 1.5 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Alignment_BOPT_Int16 = SAM_V1_6_Alignment_BOPT_Int16 { sam_v1_6_alignment_bopt_int16_tag   :: Seq Word8
                                                                   , sam_v1_6_alignment_bopt_int16_type  :: Word8
                                                                   , sam_v1_6_alignment_bopt_int16_value :: Seq Int16
                                                                   }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Alignment_BOPT_Int16 where
  SAM_V1_6_Alignment_BOPT_Int16 sam_v1_6_alignment_bopt_int16_tag1 sam_v1_6_alignment_bopt_int16_type1 sam_v1_6_alignment_bopt_int16_value1 == SAM_V1_6_Alignment_BOPT_Int16 sam_v1_6_alignment_bopt_int16_tag2 sam_v1_6_alignment_bopt_int16_type2 sam_v1_6_alignment_bopt_int16_value2 = sam_v1_6_alignment_bopt_int16_tag1 == sam_v1_6_alignment_bopt_int16_tag2 && sam_v1_6_alignment_bopt_int16_type1 == sam_v1_6_alignment_bopt_int16_type2 && sam_v1_6_alignment_bopt_int16_value1 == sam_v1_6_alignment_bopt_int16_value2

instance Show SAM_V1_6_Alignment_BOPT_Int16 where
  show (SAM_V1_6_Alignment_BOPT_Int16 tag
                                      bopttype
                                      value
       ) =
    "SAM_V1_6_Alignment_BOPT_Int16 { " ++
    "tag  = "                          ++
    (show tag)                         ++
    " , type = "                       ++
    (show bopttype)                    ++
    " , value = "                      ++
    (show value)                       ++
    " }"

-- | cCs__S__iIf of the last optional field (type B).
--
-- See section 1.5 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Alignment_BOPT_Word16 = SAM_V1_6_Alignment_BOPT_Word16 { sam_v1_6_alignment_bopt_word16_tag   :: Seq Word8
                                                                     , sam_v1_6_alignment_bopt_word16_type  :: Word8
                                                                     , sam_v1_6_alignment_bopt_word16_value :: Seq Word16
                                                                     }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Alignment_BOPT_Word16 where
  SAM_V1_6_Alignment_BOPT_Word16 sam_v1_6_alignment_bopt_word16_tag1 sam_v1_6_alignment_bopt_word16_type1 sam_v1_6_alignment_bopt_word16_value1 == SAM_V1_6_Alignment_BOPT_Word16 sam_v1_6_alignment_bopt_word16_tag2 sam_v1_6_alignment_bopt_word16_type2 sam_v1_6_alignment_bopt_word16_value2 = sam_v1_6_alignment_bopt_word16_tag1 == sam_v1_6_alignment_bopt_word16_tag2 && sam_v1_6_alignment_bopt_word16_type1 == sam_v1_6_alignment_bopt_word16_type2 && sam_v1_6_alignment_bopt_word16_value1 == sam_v1_6_alignment_bopt_word16_value2

instance Show SAM_V1_6_Alignment_BOPT_Word16 where
  show (SAM_V1_6_Alignment_BOPT_Word16 tag
                                       bopttype
                                       value
       ) =
    "SAM_V1_6_Alignment_BOPT_Word16 { " ++
    "tag  = "                           ++
    (show tag)                          ++
    " , type = "                        ++
    (show bopttype)                     ++
    " , value = "                       ++
    (show value)                        ++
    " }"

-- | cCsS__i__If of the last optional field (type B).
--
-- See section 1.5 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Alignment_BOPT_Int32 = SAM_V1_6_Alignment_BOPT_Int32 { sam_v1_6_alignment_bopt_int32_tag   :: Seq Word8
                                                                   , sam_v1_6_alignment_bopt_int32_type  :: Word8
                                                                   , sam_v1_6_alignment_bopt_int32_value :: Seq Int32
                                                                   }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Alignment_BOPT_Int32 where
  SAM_V1_6_Alignment_BOPT_Int32 sam_v1_6_alignment_bopt_int32_tag1 sam_v1_6_alignment_bopt_int32_type1 sam_v1_6_alignment_bopt_int32_value1 == SAM_V1_6_Alignment_BOPT_Int32 sam_v1_6_alignment_bopt_int32_tag2 sam_v1_6_alignment_bopt_int32_type2 sam_v1_6_alignment_bopt_int32_value2 = sam_v1_6_alignment_bopt_int32_tag1 == sam_v1_6_alignment_bopt_int32_tag2 && sam_v1_6_alignment_bopt_int32_type1 == sam_v1_6_alignment_bopt_int32_type2 && sam_v1_6_alignment_bopt_int32_value1 == sam_v1_6_alignment_bopt_int32_value2

instance Show SAM_V1_6_Alignment_BOPT_Int32 where
  show (SAM_V1_6_Alignment_BOPT_Int32 tag
                                      bopttype
                                      value
       ) =
    "SAM_V1_6_Alignment_BOPT_Int32 { " ++
    "tag  = "                          ++
    (show tag)                         ++
    " , type = "                       ++
    (show bopttype)                    ++
    " , value = "                      ++
    (show value)                       ++
    " }"

-- | cCsSi__I__f of the last optional field (type B).
--
-- See section 1.5 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Alignment_BOPT_Word32 = SAM_V1_6_Alignment_BOPT_Word32 { sam_v1_6_alignment_bopt_word32_tag   :: Seq Word8
                                                                     , sam_v1_6_alignment_bopt_word32_type  :: Word8
                                                                     , sam_v1_6_alignment_bopt_word32_value :: Seq Word32
                                                                     }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Alignment_BOPT_Word32 where
  SAM_V1_6_Alignment_BOPT_Word32 sam_v1_6_alignment_bopt_word32_tag1 sam_v1_6_alignment_bopt_word32_type1 sam_v1_6_alignment_bopt_word32_value1 == SAM_V1_6_Alignment_BOPT_Word32 sam_v1_6_alignment_bopt_word32_tag2 sam_v1_6_alignment_bopt_word32_type2 sam_v1_6_alignment_bopt_word32_value2 = sam_v1_6_alignment_bopt_word32_tag1 == sam_v1_6_alignment_bopt_word32_tag2 && sam_v1_6_alignment_bopt_word32_type1 == sam_v1_6_alignment_bopt_word32_type2 && sam_v1_6_alignment_bopt_word32_value1 == sam_v1_6_alignment_bopt_word32_value2

instance Show SAM_V1_6_Alignment_BOPT_Word32 where
  show (SAM_V1_6_Alignment_BOPT_Word32 tag
                                       bopttype
                                       value
       ) =
    "SAM_V1_6_Alignment_BOPT_Word32 { " ++
    "tag  = "                           ++
    (show tag)                          ++
    " , type = "                        ++
    (show bopttype)                     ++
    " , value = "                       ++
    (show value)                        ++
    " }"

-- | cCsSiI__f__ of the last optional field (type B).
--
-- See section 1.5 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Alignment_BOPT_Float = SAM_V1_6_Alignment_BOPT_Float { sam_v1_6_alignment_bopt_float_tag   :: Seq Word8
                                                                   , sam_v1_6_alignment_bopt_float_type  :: Word8
                                                                   , sam_v1_6_alignment_bopt_float_value :: Seq Float
                                                                   }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Alignment_BOPT_Float where
  SAM_V1_6_Alignment_BOPT_Float sam_v1_6_alignment_bopt_float_tag1 sam_v1_6_alignment_bopt_float_type1 sam_v1_6_alignment_bopt_float_value1 == SAM_V1_6_Alignment_BOPT_Float sam_v1_6_alignment_bopt_float_tag2 sam_v1_6_alignment_bopt_float_type2 sam_v1_6_alignment_bopt_float_value2 = sam_v1_6_alignment_bopt_float_tag1 == sam_v1_6_alignment_bopt_float_tag2 && sam_v1_6_alignment_bopt_float_type1 == sam_v1_6_alignment_bopt_float_type2 && sam_v1_6_alignment_bopt_float_value1 == sam_v1_6_alignment_bopt_float_value2

instance Show SAM_V1_6_Alignment_BOPT_Float where
  show (SAM_V1_6_Alignment_BOPT_Float tag
                                      bopttype
                                      value
       ) =
    "SAM_V1_6_Alignment_BOPT_Float { " ++
    "tag  = "                          ++
    (show tag)                         ++
    " , type = "                       ++
    (show bopttype)                    ++
    " , value = "                      ++
    (show value)                       ++
    " }"
