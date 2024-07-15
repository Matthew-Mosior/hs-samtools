{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# Language QuasiQuotes           #-}

-- |
-- Module      :  Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.BOPT
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

module Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.BOPT ( -- * BAM_V1_6_BAM parser - alignment section - bopt field 
                                                                           parse_BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
                                                                         ) where

import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.BOPT
import Data.BAM.Version1_6.Internal
import Data.BAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Lazy as DABL
import qualified Data.ByteString                 as DB (unpack)
import qualified Data.ByteString.Char8           as DBC8
import           Data.Sequence                   as DSeq
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the optional bopt field of alignment section of the BAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_BAM_V1_6_BAM_Alignment_OptionalFields_BOPT :: Parser BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
parse_BAM_V1_6_BAM_Alignment_OptionalFields_BOPT = do
  alignmentboptfieldtag <- do
    alignmentboptfieldtagp <-
      DABL.take 2
    -- Parse BOPT tag of the alignment section.
    case (alignmentboptfieldtagp =~ [re|[A-Za-z][A-Za-z0-9]|]) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_Alignment_BOPT_Tag_Incorrect_Format
      True  ->
        -- BOPT tag is in the accepted format. 
        return alignmentboptfieldtagp
  _ <- do
    alignmentboptfieldtypep <-
      DABL.take 1
    -- Parse BOPT type of the alignment section.
    case (alignmentboptfieldtypep =~ [re|[B]|]) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_Alignment_BOPT_Type_Incorrect_Format
      True  ->
        -- BOPT type is in the accepted format.
        return ()
  alignmentboptfieldvaluetype <- do
    alignmentboptfieldvaluetypep <-
      DABL.take 1
    -- Parse BOPT value type of the alignment section.
    case (alignmentboptfieldvaluetypep =~ [re|[cCsSiIf]|]) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_Alignment_BOPT_Value_Type_Incorrect_Format
      True  ->
        -- BOPT value type is in the accepted format.
        return alignmentboptfieldvaluetypep
  alignmentboptfieldcount <-
    DABL.take 4
  case (DBC8.unpack alignmentboptfieldvaluetype) of
    "c" -> do
      alignmentboptfieldvaluedata <- do
        alignmentboptfieldvaluedatap <-
          DABL.take $
            ( fromIntegral       $
                word8sToWord32LE $
                  DB.unpack alignmentboptfieldcount
            ) * 1
        -- Parse BOPT value data of the alignment section.
        case (alignmentboptfieldvaluedatap =~ [re|([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)*|]) of
          False ->
            fail $
              show BAM_V1_6_Read_Error_Alignment_BOPT_Value_Data_Incorrect_Format
          True  ->
            -- BOPT value data is in the accepted format.
            return alignmentboptfieldvaluedatap 
      return BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
               { bam_v1_6_bam_alignment_optionalfields_bopt_int8   = Just BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int8
                                                                            { bam_v1_6_bam_alignment_optionalfields_bopt_int8_tag   = alignmentboptfieldtag
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_int8_type  = alignmentboptfieldvaluetype
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_int8_count = word8sToWord32LE $
                                                                                                                                        DB.unpack alignmentboptfieldcount
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_int8_value = DSeq.fromList                      $
                                                                                                                                        map (word8sToInt8LE . DB.unpack) $
                                                                                                                                          splitByteString 1
                                                                                                                                                          alignmentboptfieldvaluedata
                                                                            }
               , bam_v1_6_bam_alignment_optionalfields_bopt_word8  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_int16  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word16 = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_int32  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word32 = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_float  = Nothing
               }
    "C" -> do
      alignmentboptfieldvaluedata <- do
        alignmentboptfieldvaluedatap <-
          DABL.take $
            ( fromIntegral       $
                word8sToWord32LE $
                  DB.unpack alignmentboptfieldcount
            ) * 1
        -- Parse BOPT value data of the alignment section.
        case (alignmentboptfieldvaluedatap =~ [re|([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)*|]) of
          False ->
            fail $
              show BAM_V1_6_Read_Error_Alignment_BOPT_Value_Data_Incorrect_Format
          True  ->
            -- BOPT value data is in the accepted format.
            return alignmentboptfieldvaluedatap
      return BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
               { bam_v1_6_bam_alignment_optionalfields_bopt_int8   = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word8  = Just BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word8
                                                                            { bam_v1_6_bam_alignment_optionalfields_bopt_word8_tag   = alignmentboptfieldtag
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_word8_type  = alignmentboptfieldvaluetype
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_word8_count = word8sToWord32LE $
                                                                                                                                         DB.unpack alignmentboptfieldcount
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_word8_value = DSeq.fromList                       $
                                                                                                                                         map (word8sToWord8LE . DB.unpack) $
                                                                                                                                           splitByteString 1
                                                                                                                                                           alignmentboptfieldvaluedata
                                                                            }
               , bam_v1_6_bam_alignment_optionalfields_bopt_int16  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word16 = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_int32  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word32 = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_float  = Nothing
               }
    "s" -> do
      alignmentboptfieldvaluedata <- do
        alignmentboptfieldvaluedatap <-
          DABL.take $
            ( fromIntegral       $
                word8sToWord32LE $
                  DB.unpack alignmentboptfieldcount
            ) * 2
        -- Parse BOPT value data of the alignment section.
        case (alignmentboptfieldvaluedatap =~ [re|([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)*|]) of
          False ->
            fail $
              show BAM_V1_6_Read_Error_Alignment_BOPT_Value_Data_Incorrect_Format
          True  ->
            -- BOPT value data is in the accepted format.
            return alignmentboptfieldvaluedatap
      return BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
               { bam_v1_6_bam_alignment_optionalfields_bopt_int8   = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word8  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_int16  = Just BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int16
                                                                            { bam_v1_6_bam_alignment_optionalfields_bopt_int16_tag   = alignmentboptfieldtag
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_int16_type  = alignmentboptfieldvaluetype
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_int16_count = word8sToWord32LE $
                                                                                                                                         DB.unpack alignmentboptfieldcount
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_int16_value = DSeq.fromList                       $
                                                                                                                                         map (word8sToInt16LE . DB.unpack) $ 
                                                                                                                                           splitByteString 2
                                                                                                                                                           alignmentboptfieldvaluedata
                                                                            }
               , bam_v1_6_bam_alignment_optionalfields_bopt_word16 = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_int32  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word32 = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_float  = Nothing
               }
    "S" -> do
      alignmentboptfieldvaluedata <- do
        alignmentboptfieldvaluedatap <-
          DABL.take $
            ( fromIntegral       $
                word8sToWord32LE $
                  DB.unpack alignmentboptfieldcount
            ) * 2
        -- Parse BOPT value data of the alignment section.
        case (alignmentboptfieldvaluedatap =~ [re|([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)*|]) of
          False ->
            fail $
              show BAM_V1_6_Read_Error_Alignment_BOPT_Value_Data_Incorrect_Format
          True  ->
            -- BOPT value data is in the accepted format.
            return alignmentboptfieldvaluedatap
      return BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
               { bam_v1_6_bam_alignment_optionalfields_bopt_int8   = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word8  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_int16  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word16 = Just BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word16
                                                                            { bam_v1_6_bam_alignment_optionalfields_bopt_word16_tag   = alignmentboptfieldtag
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_word16_type  = alignmentboptfieldvaluetype
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_word16_count = word8sToWord32LE $
                                                                                                                                          DB.unpack alignmentboptfieldcount
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_word16_value = DSeq.fromList                        $
                                                                                                                                          map (word8sToWord16LE . DB.unpack) $
                                                                                                                                            splitByteString 2
                                                                                                                                                            alignmentboptfieldvaluedata
                                                                            }
               , bam_v1_6_bam_alignment_optionalfields_bopt_int32  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word32 = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_float  = Nothing
               }
    "i" -> do
      alignmentboptfieldvaluedata <- do
        alignmentboptfieldvaluedatap <-
          DABL.take $
            ( fromIntegral       $
                word8sToWord32LE $
                  DB.unpack alignmentboptfieldcount
            ) * 4
        -- Parse BOPT value data of the alignment section.
        case (alignmentboptfieldvaluedatap =~ [re|([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)*|]) of
          False ->
            fail $
              show BAM_V1_6_Read_Error_Alignment_BOPT_Value_Data_Incorrect_Format
          True  ->
            -- BOPT value data is in the accepted format.
            return alignmentboptfieldvaluedatap
      return BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
               { bam_v1_6_bam_alignment_optionalfields_bopt_int8   = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word8  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_int16  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word16 = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_int32  = Just BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Int32
                                                                            { bam_v1_6_bam_alignment_optionalfields_bopt_int32_tag   = alignmentboptfieldtag
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_int32_type  = alignmentboptfieldvaluetype
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_int32_count = word8sToWord32LE $
                                                                                                                                         DB.unpack alignmentboptfieldcount
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_int32_value = DSeq.fromList                       $
                                                                                                                                         map (word8sToInt32LE . DB.unpack) $
                                                                                                                                           splitByteString 4
                                                                                                                                                           alignmentboptfieldvaluedata
                                                                            }
               , bam_v1_6_bam_alignment_optionalfields_bopt_word32 = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_float  = Nothing
               }
    "I" -> do
      alignmentboptfieldvaluedata <- do
        alignmentboptfieldvaluedatap <-
          DABL.take $
            ( fromIntegral       $
                word8sToWord32LE $
                  DB.unpack alignmentboptfieldcount
            ) * 4
        -- Parse BOPT value data of the alignment section.
        case (alignmentboptfieldvaluedatap =~ [re|([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)*|]) of
          False ->
            fail $
              show BAM_V1_6_Read_Error_Alignment_BOPT_Value_Data_Incorrect_Format
          True  ->
            -- BOPT value data is in the accepted format.
            return alignmentboptfieldvaluedatap
      return BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
               { bam_v1_6_bam_alignment_optionalfields_bopt_int8   = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word8  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_int16  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word16 = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_int32  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word32 = Just BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Word32
                                                                            { bam_v1_6_bam_alignment_optionalfields_bopt_word32_tag   = alignmentboptfieldtag
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_word32_type  = alignmentboptfieldvaluetype
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_word32_count = word8sToWord32LE $
                                                                                                                                          DB.unpack alignmentboptfieldcount
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_word32_value = DSeq.fromList                        $
                                                                                                                                          map (word8sToWord32LE . DB.unpack) $
                                                                                                                                            splitByteString 4
                                                                                                                                                            alignmentboptfieldvaluedata
                                                                            }
               , bam_v1_6_bam_alignment_optionalfields_bopt_float  = Nothing
               }
    "f" -> do
      alignmentboptfieldvaluedata <- do
        alignmentboptfieldvaluedatap <-
          DABL.take $
            ( fromIntegral       $
                word8sToWord32LE $
                  DB.unpack alignmentboptfieldcount
            ) * 4
        -- Parse BOPT value data of the alignment section.
        case (alignmentboptfieldvaluedatap =~ [re|([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)*|]) of
          False ->
            fail $
              show BAM_V1_6_Read_Error_Alignment_BOPT_Value_Data_Incorrect_Format
          True  ->
            -- BOPT value data is in the accepted format.
            return alignmentboptfieldvaluedatap
      return BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
               { bam_v1_6_bam_alignment_optionalfields_bopt_int8   = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word8  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_int16  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word16 = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_int32  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word32 = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_float  = Just BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_Float
                                                                            { bam_v1_6_bam_alignment_optionalfields_bopt_float_tag   = alignmentboptfieldtag
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_float_type  = alignmentboptfieldvaluetype
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_float_count = word8sToWord32LE $
                                                                                                                                         DB.unpack alignmentboptfieldcount
                                                                            , bam_v1_6_bam_alignment_optionalfields_bopt_float_value = DSeq.fromList             $
                                                                                                                                         map bytestringToFloatLE $
                                                                                                                                           splitByteString 4
                                                                                                                                                           alignmentboptfieldvaluedata
                                                                            }
               }
    _   ->
      return BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
               { bam_v1_6_bam_alignment_optionalfields_bopt_int8   = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word8  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_int16  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word16 = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_int32  = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_word32 = Nothing
               , bam_v1_6_bam_alignment_optionalfields_bopt_float  = Nothing
               }
