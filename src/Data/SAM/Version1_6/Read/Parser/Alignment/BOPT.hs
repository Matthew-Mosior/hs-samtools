{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# Language QuasiQuotes           #-}

-- |
-- Module      :  Data.SAM.Version1_6.Read.Parser.Alignment.BOPT
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

module Data.SAM.Version1_6.Read.Parser.Alignment.BOPT ( -- * SAM_V1_6 parser - alignment section - bopt field 
                                                        parse_SAM_V1_6_Alignment_BOPT
                                                      ) where

import Data.SAM.Version1_6.Alignment.BOPT
import Data.SAM.Version1_6.Read.Error

import           Data.Attoparsec.ByteString.Char8  as DABC8 (isEndOfLine)
import           Data.Attoparsec.ByteString.Lazy   as DABL
import qualified Data.ByteString                   as DB (head,unpack)
import qualified Data.ByteString.Char8             as DBC8
import           Data.Sequence                     as DSeq
import           Text.Regex.PCRE.Heavy

-- | Defines a parser for the optional bopt field of alignment section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Alignment_BOPT :: Parser SAM_V1_6_Alignment_BOPT
parse_SAM_V1_6_Alignment_BOPT = do
  alignmentboptfieldtag <- do alignmentboptfieldtagp <- DABL.takeTill (== 58)
                              -- Parse BOPT tag of the alignment section.
                              case (alignmentboptfieldtagp =~ [re|[A-Za-z][A-Za-z0-9]|]) of
                                False -> fail $ show SAM_V1_6_Error_Alignment_BOPT_Tag_Incorrect_Format
                                True  -> -- BOPT tag is in the accepted format. 
                                         return alignmentboptfieldtagp
  _ <- word8 58
  _ <- do alignmentboptfieldtypep <- DABL.takeTill (== 58)
          -- Parse BOPT type of the alignment section.
          case (alignmentboptfieldtypep =~ [re|[B]|]) of
            False -> fail $ show SAM_V1_6_Error_Alignment_BOPT_Type_Incorrect_Format
            True  -> -- BOPT type is in the accepted format.
                     return ()
  _ <- word8 58
  alignmentboptfieldvaluetype <- do alignmentboptfieldvaluetypep <- DABL.take 1
                                    -- Parse BOPT value type of the alignment section.
                                    case (alignmentboptfieldvaluetypep =~ [re|[cCsSiIf]|]) of
                                      False -> fail $ show SAM_V1_6_Error_Alignment_BOPT_Value_Type_Incorrect_Format
                                      True  -> -- BOPT value type is in the accepted format.
                                               return alignmentboptfieldvaluetypep
  _ <- word8 44
  alignmentboptfieldvaluedata <- do alignmentboptfieldvaluedatap <- DABL.takeTill (\x -> x == 09 || isEndOfLine x)
                                    -- Parse BOPT value data of the alignment section.
                                    case (alignmentboptfieldvaluedatap =~ [re|([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)*|]) of
                                      False -> fail $ show SAM_V1_6_Error_Alignment_BOPT_Value_Data_Incorrect_Format
                                      True  -> -- BOPT value data is in the accepted format.
                                               return alignmentboptfieldvaluedatap
  case (DBC8.unpack alignmentboptfieldvaluetype) of
    "c" -> return SAM_V1_6_Alignment_BOPT { sam_v1_6_alignment_bopt_int8   = Just SAM_V1_6_Alignment_BOPT_Int8 { sam_v1_6_alignment_bopt_int8_tag   = DSeq.fromList $ DB.unpack alignmentboptfieldtag
                                                                                                               , sam_v1_6_alignment_bopt_int8_type  = DB.head alignmentboptfieldvaluetype
                                                                                                               , sam_v1_6_alignment_bopt_int8_value = fmap fromIntegral (DSeq.fromList $ DB.unpack alignmentboptfieldvaluedata)
                                                                                                               }
                                          , sam_v1_6_alignment_bopt_word8  = Nothing
                                          , sam_v1_6_alignment_bopt_int16  = Nothing
                                          , sam_v1_6_alignment_bopt_word16 = Nothing
                                          , sam_v1_6_alignment_bopt_int32  = Nothing
                                          , sam_v1_6_alignment_bopt_word32 = Nothing
                                          , sam_v1_6_alignment_bopt_float  = Nothing
                                          }
    "C" -> return SAM_V1_6_Alignment_BOPT { sam_v1_6_alignment_bopt_int8   = Nothing
                                          , sam_v1_6_alignment_bopt_word8  = Just SAM_V1_6_Alignment_BOPT_Word8 { sam_v1_6_alignment_bopt_word8_tag   = DSeq.fromList $ DB.unpack alignmentboptfieldtag
                                                                                                                , sam_v1_6_alignment_bopt_word8_type  = DB.head alignmentboptfieldvaluetype
                                                                                                                , sam_v1_6_alignment_bopt_word8_value = DSeq.fromList $ DB.unpack alignmentboptfieldvaluedata
                                                                                                                } 
                                          , sam_v1_6_alignment_bopt_int16  = Nothing
                                          , sam_v1_6_alignment_bopt_word16 = Nothing
                                          , sam_v1_6_alignment_bopt_int32  = Nothing
                                          , sam_v1_6_alignment_bopt_word32 = Nothing
                                          , sam_v1_6_alignment_bopt_float  = Nothing
                                          }
    "s" -> return SAM_V1_6_Alignment_BOPT { sam_v1_6_alignment_bopt_int8   = Nothing
                                          , sam_v1_6_alignment_bopt_word8  = Nothing
                                          , sam_v1_6_alignment_bopt_int16  = Just SAM_V1_6_Alignment_BOPT_Int16 { sam_v1_6_alignment_bopt_int16_tag   = DSeq.fromList $ DB.unpack alignmentboptfieldtag
                                                                                                                , sam_v1_6_alignment_bopt_int16_type  = DB.head alignmentboptfieldvaluetype
                                                                                                                , sam_v1_6_alignment_bopt_int16_value = fmap fromIntegral (DSeq.fromList $ DB.unpack alignmentboptfieldvaluedata)
                                                                                                                }
                                          , sam_v1_6_alignment_bopt_word16 = Nothing
                                          , sam_v1_6_alignment_bopt_int32  = Nothing
                                          , sam_v1_6_alignment_bopt_word32 = Nothing
                                          , sam_v1_6_alignment_bopt_float  = Nothing
                                          }
    "S" -> return SAM_V1_6_Alignment_BOPT { sam_v1_6_alignment_bopt_int8   = Nothing
                                          , sam_v1_6_alignment_bopt_word8  = Nothing
                                          , sam_v1_6_alignment_bopt_int16  = Nothing
                                          , sam_v1_6_alignment_bopt_word16 = Just SAM_V1_6_Alignment_BOPT_Word16 { sam_v1_6_alignment_bopt_word16_tag   = DSeq.fromList $ DB.unpack alignmentboptfieldtag
                                                                                                                 , sam_v1_6_alignment_bopt_word16_type  = DB.head alignmentboptfieldvaluetype
                                                                                                                 , sam_v1_6_alignment_bopt_word16_value = fmap fromIntegral (DSeq.fromList $ DB.unpack alignmentboptfieldvaluedata)
                                                                                                                 }
                                          , sam_v1_6_alignment_bopt_int32  = Nothing
                                          , sam_v1_6_alignment_bopt_word32 = Nothing
                                          , sam_v1_6_alignment_bopt_float  = Nothing
                                          }
    "i" -> return SAM_V1_6_Alignment_BOPT { sam_v1_6_alignment_bopt_int8   = Nothing
                                          , sam_v1_6_alignment_bopt_word8  = Nothing
                                          , sam_v1_6_alignment_bopt_int16  = Nothing
                                          , sam_v1_6_alignment_bopt_word16 = Nothing
                                          , sam_v1_6_alignment_bopt_int32  = Just SAM_V1_6_Alignment_BOPT_Int32 { sam_v1_6_alignment_bopt_int32_tag   = DSeq.fromList $ DB.unpack alignmentboptfieldtag
                                                                                                                , sam_v1_6_alignment_bopt_int32_type  = DB.head alignmentboptfieldvaluetype
                                                                                                                , sam_v1_6_alignment_bopt_int32_value = fmap fromIntegral (DSeq.fromList $ DB.unpack alignmentboptfieldvaluedata)
                                                                                                                }
                                          , sam_v1_6_alignment_bopt_word32 = Nothing
                                          , sam_v1_6_alignment_bopt_float  = Nothing
                                          }
    "I" -> return SAM_V1_6_Alignment_BOPT { sam_v1_6_alignment_bopt_int8   = Nothing
                                          , sam_v1_6_alignment_bopt_word8  = Nothing
                                          , sam_v1_6_alignment_bopt_int16  = Nothing
                                          , sam_v1_6_alignment_bopt_word16 = Nothing
                                          , sam_v1_6_alignment_bopt_int32  = Nothing
                                          , sam_v1_6_alignment_bopt_word32 = Just SAM_V1_6_Alignment_BOPT_Word32 { sam_v1_6_alignment_bopt_word32_tag   = DSeq.fromList $ DB.unpack alignmentboptfieldtag
                                                                                                                 , sam_v1_6_alignment_bopt_word32_type  = DB.head alignmentboptfieldvaluetype
                                                                                                                 , sam_v1_6_alignment_bopt_word32_value = fmap fromIntegral (DSeq.fromList $ DB.unpack alignmentboptfieldvaluedata)
                                                                                                                 }
                                          , sam_v1_6_alignment_bopt_float  = Nothing
                                          }
    "f" -> return SAM_V1_6_Alignment_BOPT { sam_v1_6_alignment_bopt_int8   = Nothing
                                          , sam_v1_6_alignment_bopt_word8  = Nothing
                                          , sam_v1_6_alignment_bopt_int16  = Nothing
                                          , sam_v1_6_alignment_bopt_word16 = Nothing
                                          , sam_v1_6_alignment_bopt_int32  = Nothing
                                          , sam_v1_6_alignment_bopt_word32 = Nothing
                                          , sam_v1_6_alignment_bopt_float  = Just SAM_V1_6_Alignment_BOPT_Float { sam_v1_6_alignment_bopt_float_tag   = DSeq.fromList $ DB.unpack alignmentboptfieldtag
                                                                                                                , sam_v1_6_alignment_bopt_float_type  = DB.head alignmentboptfieldvaluetype
                                                                                                                , sam_v1_6_alignment_bopt_float_value = fmap fromIntegral (DSeq.fromList $ DB.unpack alignmentboptfieldvaluedata)
                                                                                                                }
                                          }
    _   -> return SAM_V1_6_Alignment_BOPT { sam_v1_6_alignment_bopt_int8   = Nothing
                                          , sam_v1_6_alignment_bopt_word8  = Nothing
                                          , sam_v1_6_alignment_bopt_int16  = Nothing
                                          , sam_v1_6_alignment_bopt_word16 = Nothing
                                          , sam_v1_6_alignment_bopt_int32  = Nothing
                                          , sam_v1_6_alignment_bopt_word32 = Nothing
                                          , sam_v1_6_alignment_bopt_float  = Nothing
                                          }
