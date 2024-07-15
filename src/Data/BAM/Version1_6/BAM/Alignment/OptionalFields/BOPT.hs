{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.BAM.Alignment.OptionalFields.BOPT
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

module Data.BAM.Version1_6.BAM.Alignment.OptionalFields.BOPT ( -- * SAM version 1.6 alignment optional fields data type
                                                               BAM_V1_6_BAM_Alignment_OptionalFields_BOPT(..),
                                                               BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int8(..),
                                                               BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word8(..),
                                                               BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int16(..),
                                                               BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word16(..),
                                                               BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int32(..),
                                                               BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word32(..),
                                                               BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Float(..),
                                                               BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_SumType(..)
                                                             ) where

import Data.ByteString hiding (count)
import Data.Data
import Data.Int
import Data.Sequence
import Data.Word
import Generics.Deriving.Base

-- | Custom BAM (version 1.6) @"BAM_V1_6_BAM_Alignment_OptionalFields_BOPT"@ data type.
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BAM_Alignment_OptionalFields_BOPT = BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
  { bam_v1_6_bam_alignment_optionalfields_bopt_int8   :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int8
  , bam_v1_6_bam_alignment_optionalfields_bopt_word8  :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word8
  , bam_v1_6_bam_alignment_optionalfields_bopt_int16  :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int16
  , bam_v1_6_bam_alignment_optionalfields_bopt_word16 :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word16
  , bam_v1_6_bam_alignment_optionalfields_bopt_int32  :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int32
  , bam_v1_6_bam_alignment_optionalfields_bopt_word32 :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word32
  , bam_v1_6_bam_alignment_optionalfields_bopt_float  :: Maybe BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Float
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM_Alignment_OptionalFields_BOPT where
  BAM_V1_6_BAM_Alignment_OptionalFields_BOPT bam_v1_6_bam_alignment_optionalfields_bopt_int81
                                             bam_v1_6_bam_alignment_optionalfields_bopt_word81
                                             bam_v1_6_bam_alignment_optionalfields_bopt_int161
                                             bam_v1_6_bam_alignment_optionalfields_bopt_word161
                                             bam_v1_6_bam_alignment_optionalfields_bopt_int321
                                             bam_v1_6_bam_alignment_optionalfields_bopt_word321
                                             bam_v1_6_bam_alignment_optionalfields_bopt_float1 ==
    BAM_V1_6_BAM_Alignment_OptionalFields_BOPT bam_v1_6_bam_alignment_optionalfields_bopt_int82
                                               bam_v1_6_bam_alignment_optionalfields_bopt_word82
                                               bam_v1_6_bam_alignment_optionalfields_bopt_int162
                                               bam_v1_6_bam_alignment_optionalfields_bopt_word162
                                               bam_v1_6_bam_alignment_optionalfields_bopt_int322
                                               bam_v1_6_bam_alignment_optionalfields_bopt_word322
                                               bam_v1_6_bam_alignment_optionalfields_bopt_float2 =
      bam_v1_6_bam_alignment_optionalfields_bopt_int81   == bam_v1_6_bam_alignment_optionalfields_bopt_int82   &&
      bam_v1_6_bam_alignment_optionalfields_bopt_word81  == bam_v1_6_bam_alignment_optionalfields_bopt_word82  &&
      bam_v1_6_bam_alignment_optionalfields_bopt_int161  == bam_v1_6_bam_alignment_optionalfields_bopt_int162  &&
      bam_v1_6_bam_alignment_optionalfields_bopt_word161 == bam_v1_6_bam_alignment_optionalfields_bopt_word162 &&
      bam_v1_6_bam_alignment_optionalfields_bopt_int321  == bam_v1_6_bam_alignment_optionalfields_bopt_int322  &&
      bam_v1_6_bam_alignment_optionalfields_bopt_word321 == bam_v1_6_bam_alignment_optionalfields_bopt_word322 &&
      bam_v1_6_bam_alignment_optionalfields_bopt_float1  == bam_v1_6_bam_alignment_optionalfields_bopt_float2

instance Show BAM_V1_6_BAM_Alignment_OptionalFields_BOPT where
  show (BAM_V1_6_BAM_Alignment_OptionalFields_BOPT int8
                                                   word8
                                                   int16
                                                   word16
                                                   int32
                                                   word32
                                                   float
       ) =
    "BAM_V1_6_BAM_Alignment_OptionalFields_BOPT { "           ++
    "bam_v1_6_bam_alignment_optionalfields_bopt_int8 = "      ++
    (show int8)                                               ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_word8 = "  ++
    (show word8)                                              ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_int16 = "  ++
    (show int16)                                              ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_word16 = " ++
    (show word16)                                             ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_int32 = "  ++
    (show int32)                                              ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_word32 = " ++
    (show word32)                                             ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_float = "  ++
    (show float)                                              ++
    " }"

-- | __c__CsSiIf of the last optional field (type B).
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int8 = BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int8
  { bam_v1_6_bam_alignment_optionalfields_bopt_int8_tag   :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_bopt_int8_type  :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_bopt_int8_count :: Word32
  , bam_v1_6_bam_alignment_optionalfields_bopt_int8_value :: Seq Int8
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int8 where
  BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int8 bam_v1_6_bam_alignment_optionalfields_bopt_int8_tag1
                                                  bam_v1_6_bam_alignment_optionalfields_bopt_int8_type1
                                                  bam_v1_6_bam_alignment_optionalfields_bopt_int8_count1
                                                  bam_v1_6_bam_alignment_optionalfields_bopt_int8_value1 ==
    BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int8 bam_v1_6_bam_alignment_optionalfields_bopt_int8_tag2
                                                    bam_v1_6_bam_alignment_optionalfields_bopt_int8_type2
                                                    bam_v1_6_bam_alignment_optionalfields_bopt_int8_count2
                                                    bam_v1_6_bam_alignment_optionalfields_bopt_int8_value2 =
      bam_v1_6_bam_alignment_optionalfields_bopt_int8_tag1 == bam_v1_6_bam_alignment_optionalfields_bopt_int8_tag2     &&
      bam_v1_6_bam_alignment_optionalfields_bopt_int8_type1 == bam_v1_6_bam_alignment_optionalfields_bopt_int8_type2   &&
      bam_v1_6_bam_alignment_optionalfields_bopt_int8_count1 == bam_v1_6_bam_alignment_optionalfields_bopt_int8_count2 &&
      bam_v1_6_bam_alignment_optionalfields_bopt_int8_value1 == bam_v1_6_bam_alignment_optionalfields_bopt_int8_value2

instance Show BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int8 where
  show (BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int8 tag
                                                        bopttype
                                                        count
                                                        value
       ) =
    "BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int8 { "          ++
    "bam_v1_6_bam_alignment_optionalfields_bopt_int8_tag  = "     ++
    (show tag)                                                    ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_int8_type = "  ++
    (show bopttype)                                               ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_int8_count = " ++
    (show count)                                                  ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_int8_value = " ++
    (show value)                                                  ++
    " }"

-- | c__C__sSiIf of the last optional field (type B).
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word8 = BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word8
  { bam_v1_6_bam_alignment_optionalfields_bopt_word8_tag   :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_bopt_word8_type  :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_bopt_word8_count :: Word32
  , bam_v1_6_bam_alignment_optionalfields_bopt_word8_value :: Seq Word8
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word8 where
  BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word8 bam_v1_6_bam_alignment_optionalfields_bopt_word8_tag1
                                                   bam_v1_6_bam_alignment_optionalfields_bopt_word8_type1
                                                   bam_v1_6_bam_alignment_optionalfields_bopt_word8_count1
                                                   bam_v1_6_bam_alignment_optionalfields_bopt_word8_value1 ==
    BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word8 bam_v1_6_bam_alignment_optionalfields_bopt_word8_tag2
                                                     bam_v1_6_bam_alignment_optionalfields_bopt_word8_type2
                                                     bam_v1_6_bam_alignment_optionalfields_bopt_word8_count2
                                                     bam_v1_6_bam_alignment_optionalfields_bopt_word8_value2 =
      bam_v1_6_bam_alignment_optionalfields_bopt_word8_tag1 == bam_v1_6_bam_alignment_optionalfields_bopt_word8_tag2     &&
      bam_v1_6_bam_alignment_optionalfields_bopt_word8_type1 == bam_v1_6_bam_alignment_optionalfields_bopt_word8_type2   &&
      bam_v1_6_bam_alignment_optionalfields_bopt_word8_count1 == bam_v1_6_bam_alignment_optionalfields_bopt_word8_count2 &&
      bam_v1_6_bam_alignment_optionalfields_bopt_word8_value1 == bam_v1_6_bam_alignment_optionalfields_bopt_word8_value2

instance Show BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word8 where
  show (BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word8 tag
                                                         bopttype
                                                         count
                                                         value
       ) =
    "BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word8 { "          ++
    "bam_v1_6_bam_alignment_optionalfields_bopt_word8_tag  = "     ++
    (show tag)                                                     ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_word8_type = "  ++
    (show bopttype)                                                ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_word8_count = " ++
    (show count)                                                   ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_word8_value = " ++
    (show value)                                                   ++
    " }"

-- | cC__s__SiIf of the last optional field (type B).
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int16 = BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int16
  { bam_v1_6_bam_alignment_optionalfields_bopt_int16_tag   :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_bopt_int16_type  :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_bopt_int16_count :: Word32
  , bam_v1_6_bam_alignment_optionalfields_bopt_int16_value :: Seq Int16
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int16 where
  BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int16 bam_v1_6_bam_alignment_optionalfields_bopt_int16_tag1
                                                   bam_v1_6_bam_alignment_optionalfields_bopt_int16_type1
                                                   bam_v1_6_bam_alignment_optionalfields_bopt_int16_count1
                                                   bam_v1_6_bam_alignment_optionalfields_bopt_int16_value1 ==
    BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int16 bam_v1_6_bam_alignment_optionalfields_bopt_int16_tag2
                                                     bam_v1_6_bam_alignment_optionalfields_bopt_int16_type2
                                                     bam_v1_6_bam_alignment_optionalfields_bopt_int16_count2
                                                     bam_v1_6_bam_alignment_optionalfields_bopt_int16_value2 =
      bam_v1_6_bam_alignment_optionalfields_bopt_int16_tag1 == bam_v1_6_bam_alignment_optionalfields_bopt_int16_tag2     &&
      bam_v1_6_bam_alignment_optionalfields_bopt_int16_type1 == bam_v1_6_bam_alignment_optionalfields_bopt_int16_type2   &&
      bam_v1_6_bam_alignment_optionalfields_bopt_int16_count1 == bam_v1_6_bam_alignment_optionalfields_bopt_int16_count2 &&
      bam_v1_6_bam_alignment_optionalfields_bopt_int16_value1 == bam_v1_6_bam_alignment_optionalfields_bopt_int16_value2

instance Show BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int16 where
  show (BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int16 tag
                                                         bopttype
                                                         count
                                                         value
       ) =
    "BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int16 { "          ++
    "bam_v1_6_bam_alignment_optionalfields_bopt_int16_tag  = "     ++
    (show tag)                                                     ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_int16_type = "  ++
    (show bopttype)                                                ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_int16_count = " ++
    (show count)                                                   ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_int16_value = " ++
    (show value)                                                   ++
    " }"

-- | cCs__S__iIf of the last optional field (type B).
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word16 = BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word16
  { bam_v1_6_bam_alignment_optionalfields_bopt_word16_tag   :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_bopt_word16_type  :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_bopt_word16_count :: Word32
  , bam_v1_6_bam_alignment_optionalfields_bopt_word16_value :: Seq Word16
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word16 where
  BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word16 bam_v1_6_bam_alignment_optionalfields_bopt_word16_tag1
                                                    bam_v1_6_bam_alignment_optionalfields_bopt_word16_type1
                                                    bam_v1_6_bam_alignment_optionalfields_bopt_word16_count1
                                                    bam_v1_6_bam_alignment_optionalfields_bopt_word16_value1 ==
    BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word16 bam_v1_6_bam_alignment_optionalfields_bopt_word16_tag2
                                                      bam_v1_6_bam_alignment_optionalfields_bopt_word16_type2
                                                      bam_v1_6_bam_alignment_optionalfields_bopt_word16_count2
                                                      bam_v1_6_bam_alignment_optionalfields_bopt_word16_value2 =
      bam_v1_6_bam_alignment_optionalfields_bopt_word16_tag1 == bam_v1_6_bam_alignment_optionalfields_bopt_word16_tag2     &&
      bam_v1_6_bam_alignment_optionalfields_bopt_word16_type1 == bam_v1_6_bam_alignment_optionalfields_bopt_word16_type2   &&
      bam_v1_6_bam_alignment_optionalfields_bopt_word16_count1 == bam_v1_6_bam_alignment_optionalfields_bopt_word16_count2 &&
      bam_v1_6_bam_alignment_optionalfields_bopt_word16_value1 == bam_v1_6_bam_alignment_optionalfields_bopt_word16_value2

instance Show BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word16 where
  show (BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word16 tag
                                                          bopttype
                                                          count
                                                          value
       ) =
    "BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word16 { "          ++
    "bam_v1_6_bam_alignment_optionalfields_bopt_word16_tag  = "     ++
    (show tag)                                                      ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_word16_type = "  ++
    (show bopttype)                                                 ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_word16_count = " ++
    (show count)                                                    ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_word16_value = " ++
    (show value)                                                    ++
    " }"

-- | cCsS__i__If of the last optional field (type B).
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int32 = BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int32
  { bam_v1_6_bam_alignment_optionalfields_bopt_int32_tag   :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_bopt_int32_type  :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_bopt_int32_count :: Word32
  , bam_v1_6_bam_alignment_optionalfields_bopt_int32_value :: Seq Int32
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int32 where
  BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int32 bam_v1_6_bam_alignment_optionalfields_bopt_int32_tag1
                                                   bam_v1_6_bam_alignment_optionalfields_bopt_int32_type1
                                                   bam_v1_6_bam_alignment_optionalfields_bopt_int32_count1
                                                   bam_v1_6_bam_alignment_optionalfields_bopt_int32_value1 ==
    BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int32 bam_v1_6_bam_alignment_optionalfields_bopt_int32_tag2
                                                     bam_v1_6_bam_alignment_optionalfields_bopt_int32_type2
                                                     bam_v1_6_bam_alignment_optionalfields_bopt_int32_count2
                                                     bam_v1_6_bam_alignment_optionalfields_bopt_int32_value2 =
      bam_v1_6_bam_alignment_optionalfields_bopt_int32_tag1 == bam_v1_6_bam_alignment_optionalfields_bopt_int32_tag2     &&
      bam_v1_6_bam_alignment_optionalfields_bopt_int32_type1 == bam_v1_6_bam_alignment_optionalfields_bopt_int32_type2   &&
      bam_v1_6_bam_alignment_optionalfields_bopt_int32_count1 == bam_v1_6_bam_alignment_optionalfields_bopt_int32_count2 &&
      bam_v1_6_bam_alignment_optionalfields_bopt_int32_value1 == bam_v1_6_bam_alignment_optionalfields_bopt_int32_value2

instance Show BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int32 where
  show (BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int32 tag
                                                         bopttype
                                                         count
                                                         value
       ) =
    "BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int32 { "          ++
    "bam_v1_6_bam_alignment_optionalfields_bopt_int32_tag  = "     ++
    (show tag)                                                     ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_int32_type = "  ++
    (show bopttype)                                                ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_int32_count = " ++
    (show count)                                                   ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_int32_value = " ++
    (show value)                                                   ++
    " }"

-- | cCsSi__I__f of the last optional field (type B).
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word32 = BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word32
  { bam_v1_6_bam_alignment_optionalfields_bopt_word32_tag   :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_bopt_word32_type  :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_bopt_word32_count :: Word32
  , bam_v1_6_bam_alignment_optionalfields_bopt_word32_value :: Seq Word32
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word32 where
  BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word32 bam_v1_6_bam_alignment_optionalfields_bopt_word32_tag1
                                                    bam_v1_6_bam_alignment_optionalfields_bopt_word32_type1
                                                    bam_v1_6_bam_alignment_optionalfields_bopt_word32_count1
                                                    bam_v1_6_bam_alignment_optionalfields_bopt_word32_value1 ==
    BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word32 bam_v1_6_bam_alignment_optionalfields_bopt_word32_tag2
                                                      bam_v1_6_bam_alignment_optionalfields_bopt_word32_type2
                                                      bam_v1_6_bam_alignment_optionalfields_bopt_word32_count2
                                                      bam_v1_6_bam_alignment_optionalfields_bopt_word32_value2 =
      bam_v1_6_bam_alignment_optionalfields_bopt_word32_tag1 == bam_v1_6_bam_alignment_optionalfields_bopt_word32_tag2     &&
      bam_v1_6_bam_alignment_optionalfields_bopt_word32_type1 == bam_v1_6_bam_alignment_optionalfields_bopt_word32_type2   &&
      bam_v1_6_bam_alignment_optionalfields_bopt_word32_count1 == bam_v1_6_bam_alignment_optionalfields_bopt_word32_count2 &&
      bam_v1_6_bam_alignment_optionalfields_bopt_word32_value1 == bam_v1_6_bam_alignment_optionalfields_bopt_word32_value2

instance Show BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word32 where
  show (BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word32 tag
                                                          bopttype
                                                          count
                                                          value
       ) =
    "BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word32 { "          ++
    "bam_v1_6_bam_alignment_optionalfields_bopt_word32_tag  = "     ++
    (show tag)                                                      ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_word32_type = "  ++
    (show bopttype)                                                 ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_word32_count = " ++
    (show count)                                                    ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_word32_value = " ++
    (show value)                                                    ++
    " }"

-- | cCsSiI__f__ of the last optional field (type B).
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Float = BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Float
  { bam_v1_6_bam_alignment_optionalfields_bopt_float_tag   :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_bopt_float_type  :: ByteString
  , bam_v1_6_bam_alignment_optionalfields_bopt_float_count :: Word32
  , bam_v1_6_bam_alignment_optionalfields_bopt_float_value :: Seq Float
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Float where
  BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Float bam_v1_6_bam_alignment_optionalfields_bopt_float_tag1
                                                   bam_v1_6_bam_alignment_optionalfields_bopt_float_type1
                                                   bam_v1_6_bam_alignment_optionalfields_bopt_float_count1
                                                   bam_v1_6_bam_alignment_optionalfields_bopt_float_value1 ==
    BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Float bam_v1_6_bam_alignment_optionalfields_bopt_float_tag2
                                                     bam_v1_6_bam_alignment_optionalfields_bopt_float_type2
                                                     bam_v1_6_bam_alignment_optionalfields_bopt_float_count2
                                                     bam_v1_6_bam_alignment_optionalfields_bopt_float_value2 =
      bam_v1_6_bam_alignment_optionalfields_bopt_float_tag1   == bam_v1_6_bam_alignment_optionalfields_bopt_float_tag2   &&
      bam_v1_6_bam_alignment_optionalfields_bopt_float_type1  == bam_v1_6_bam_alignment_optionalfields_bopt_float_type2  &&
      bam_v1_6_bam_alignment_optionalfields_bopt_float_count1 == bam_v1_6_bam_alignment_optionalfields_bopt_float_count2 &&
      bam_v1_6_bam_alignment_optionalfields_bopt_float_value1 == bam_v1_6_bam_alignment_optionalfields_bopt_float_value2

instance Show BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Float where
  show (BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Float tag
                                                         bopttype
                                                         count
                                                         value
       ) =
    "BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Float { "          ++
    "bam_v1_6_bam_alignment_optionalfields_bopt_float_tag  = "     ++
    (show tag)                                                     ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_float_type = "  ++
    (show bopttype)                                                ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_float_count = " ++
    (show count)                                                   ++
    " , bam_v1_6_bam_alignment_optionalfields_bopt_float_value = " ++
    (show value)                                                   ++
    " }"

-- | Sum type that can represent any of the
-- BAM_V1_6_BAM_Alignment_OptionalFields_BOPT types.
data BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_SumType =
    BOPTInt8
  | BOPTWord8
  | BOPTInt16
  | BOPTWord16
  | BOPTInt32
  | BOPTWord32
  | BOPTFloat
  | BOPTEmpty
  deriving (Eq,Generic,Typeable)
