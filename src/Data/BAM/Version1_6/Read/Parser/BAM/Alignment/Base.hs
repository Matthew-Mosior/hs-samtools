{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedLists             #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE MultiWayIf                  #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}
{-# Language QuasiQuotes                 #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Module      :  Data.BAM.Version1_6.Read.Parser.BAM.Alignment.Base
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

module Data.BAM.Version1_6.Read.Parser.BAM.Alignment.Base ( -- * BAM_V1_6_BAM parser - alignment section
                                                            parse_BAM_V1_6_BAM_Alignment
                                                          ) where

import Data.BAM.Version1_6.BAM.Alignment
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.Base
import Data.BAM.Version1_6.Read.Error 
import Data.BAM.Version1_6.Internal
import Data.BAM.Version1_6.Read.Parser.BAM.Alignment.Internal
import Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.AOPT
import Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.BigCOPT
import Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.BigIOPT
import Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.BigSOPT
import Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.SmallCOPT
import Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.SmallIOPT
import Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.SmallSOPT
import Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.FOPT
import Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.ZOPT
import Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.HOPT
import Data.BAM.Version1_6.Read.Parser.BAM.Alignment.OptionalFields.BOPT

import Data.Attoparsec.ByteString.Lazy  as DABL
import Data.ByteString                  as DB hiding (intercalate,map)
import Data.List                                     (intercalate)
import Data.Sequence                    as DSeq      (empty,fromList,singleton,Seq(..))

-- | @"BAM_V1_6_BAM_Alignment"@ parser.
--
-- Defines a parser for the alignment section of the BAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_BAM_V1_6_BAM_Alignment :: Parser BAM_V1_6_BAM_Alignment
parse_BAM_V1_6_BAM_Alignment = do
  block_size  <-
    DABL.take 4
  refID       <-
    DABL.take 4
  pos         <-
    DABL.take 4
  l_read_name <-
    DABL.take 1
  mapq        <-
    DABL.take 1
  bin         <-
    DABL.take 2
  n_cigar_op  <-
    DABL.take 2
  flag        <-
    DABL.take 2
  l_seq       <-
    DABL.take 4
  next_refID  <-
    DABL.take 4 
  next_pos    <-
    DABL.take 4
  tlen        <-
    DABL.take 4
  read_name   <-
    DABL.take
      ( fromIntegral      $
          word8sToWord8LE $
            DB.unpack l_read_name :: Int
      )
  cigar       <-
    DABL.take
      ( ( ( fromIntegral       $
              word8sToWord16LE $
                DB.unpack n_cigar_op
          ) * 4
        ) :: Int
      )
  seq         <-
    DABL.take
      ( ( ( ( fromIntegral       $
                word8sToWord32LE $
                  DB.unpack l_seq
            ) + 1
          ) `div` 2
        ) :: Int
      )
  qual        <-
    DABL.take
      ( fromIntegral       $
          word8sToWord32LE $
            DB.unpack l_seq :: Int
      )
  let bytesuptooptionalfields = 4                                             + 
                                4                                             +
                                1                                             +
                                1                                             + 
                                2                                             +
                                2                                             +
                                2                                             +
                                4                                             +
                                4                                             +
                                4                                             +
                                4                                             +
                                ( fromIntegral      $
                                    word8sToWord8LE $
                                      DB.unpack l_read_name :: Int
                                )                                             +
                                ( ( ( fromIntegral       $
                                        word8sToWord16LE $
                                          DB.unpack n_cigar_op
                                    ) * 4
                                  ) :: Int
                                )                                             +
                                ( ( ( ( fromIntegral       $
                                          word8sToWord32LE $
                                            DB.unpack l_seq
                                      ) + 1
                                    ) `div` 2
                                  ) :: Int
                                )                                             +
                                ( fromIntegral       $
                                    word8sToWord32LE $
                                      DB.unpack l_seq
                                ) :: Int
  let block_size_int = fromIntegral       $
                         word8sToWord32LE $
                           DB.unpack block_size :: Int
  case (bytesuptooptionalfields == block_size_int) of
    True ->
      return BAM_V1_6_BAM_Alignment
               { bam_v1_6_bam_alignment_block_size     = word8sToWord32LE $
                                                           DB.unpack block_size
               , bam_v1_6_bam_alignment_refID          = word8sToInt32LE $
                                                           DB.unpack refID
               , bam_v1_6_bam_alignment_pos            = word8sToInt32LE $
                                                           DB.unpack pos
               , bam_v1_6_bam_alignment_l_read_name    = word8sToWord8LE $
                                                           DB.unpack l_read_name
               , bam_v1_6_bam_alignment_mapq           = word8sToWord8LE $
                                                           DB.unpack mapq
               , bam_v1_6_bam_alignment_bin            = word8sToWord16LE $
                                                           DB.unpack bin
               , bam_v1_6_bam_alignment_n_cigar_op     = word8sToWord16LE $
                                                           DB.unpack n_cigar_op
               , bam_v1_6_bam_alignment_flag           = word8sToWord16LE $
                                                           DB.unpack flag
               , bam_v1_6_bam_alignment_l_seq          = word8sToWord32LE $
                                                           DB.unpack l_seq
               , bam_v1_6_bam_alignment_next_refID     = word8sToInt32LE $
                                                           DB.unpack next_refID
               , bam_v1_6_bam_alignment_next_pos       = word8sToInt32LE $
                                                           DB.unpack next_pos
               , bam_v1_6_bam_alignment_tlen           = word8sToInt32LE $
                                                           DB.unpack tlen
               , bam_v1_6_bam_alignment_read_name      = read_name
               , bam_v1_6_bam_alignment_cigar          = DSeq.fromList                        $
                                                           map (word8sToWord32LE . DB.unpack) $
                                                             splitByteString 4
                                                                             cigar
               , bam_v1_6_bam_alignment_seq            = DSeq.fromList $
                                                           decodeSeqField seq
               , bam_v1_6_bam_alignment_qual           = qual
               , bam_v1_6_bam_alignment_optionalfields = DSeq.singleton
                                                           BAM_V1_6_BAM_Alignment_OptionalFields
                                                             { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                                                             , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                                                             , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                                                             , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                                                             , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                                                             , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                                                             , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                                                             , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                                                             , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                                                             , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                                                             , bam_v1_6_bam_alignment_optionalfields_bopt        = Nothing
                                                             }
               }
    False -> do
      -- This parser assumes that
      -- the optional alignment
      -- tags can appear in any order.
      optionalfieldbytes <-
        parse_BAM_V1_6_BAM_Alignment_OptionalFields block_size_int
                                                    bytesuptooptionalfields
      return BAM_V1_6_BAM_Alignment
               { bam_v1_6_bam_alignment_block_size     = word8sToWord32LE $
                                                           DB.unpack block_size
               , bam_v1_6_bam_alignment_refID          = word8sToInt32LE $
                                                           DB.unpack refID
               , bam_v1_6_bam_alignment_pos            = word8sToInt32LE $
                                                           DB.unpack pos
               , bam_v1_6_bam_alignment_l_read_name    = word8sToWord8LE $
                                                           DB.unpack l_read_name
               , bam_v1_6_bam_alignment_mapq           = word8sToWord8LE $
                                                           DB.unpack mapq
               , bam_v1_6_bam_alignment_bin            = word8sToWord16LE $
                                                           DB.unpack bin
               , bam_v1_6_bam_alignment_n_cigar_op     = word8sToWord16LE $
                                                           DB.unpack n_cigar_op
               , bam_v1_6_bam_alignment_flag           = word8sToWord16LE $
                                                           DB.unpack flag
               , bam_v1_6_bam_alignment_l_seq          = word8sToWord32LE $
                                                           DB.unpack l_seq
               , bam_v1_6_bam_alignment_next_refID     = word8sToInt32LE $
                                                           DB.unpack next_refID
               , bam_v1_6_bam_alignment_next_pos       = word8sToInt32LE $
                                                           DB.unpack next_pos
               , bam_v1_6_bam_alignment_tlen           = word8sToInt32LE $
                                                           DB.unpack tlen
               , bam_v1_6_bam_alignment_read_name      = read_name
               , bam_v1_6_bam_alignment_cigar          = DSeq.fromList                        $
                                                           map (word8sToWord32LE . DB.unpack) $
                                                             splitByteString 4
                                                                             cigar
               , bam_v1_6_bam_alignment_seq            = DSeq.fromList $
                                                           decodeSeqField seq
               , bam_v1_6_bam_alignment_qual           = qual
               , bam_v1_6_bam_alignment_optionalfields = optionalfieldbytes
               }
  where
    parse_BAM_V1_6_BAM_Alignment_OptionalFields :: Int
                                                -> Int
                                                -> Parser (Seq BAM_V1_6_BAM_Alignment_OptionalFields)
    parse_BAM_V1_6_BAM_Alignment_OptionalFields block_size_int
                                                bytesuptooptionalfields = do
      optionalfieldbytes' <-
        DABL.take $
          block_size_int -
          bytesuptooptionalfields
      return $
        parse_OptionalFields optionalfieldbytes'
      where
        parse_OptionalFields :: DB.ByteString
                             -> Seq BAM_V1_6_BAM_Alignment_OptionalFields 
        parse_OptionalFields bs =
          case DB.null bs of
            True  ->
              DSeq.empty
            False ->
              case DB.indexMaybe bs 2 of
                Nothing                      ->
                  error $
                    show BAM_V1_6_Read_Error_Alignment_OptionalFields_Index_Missing
                -- AOPT <-> 'A'
                Just 0x41 ->
                  case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_AOPT
                                    ( DB.fromStrict $
                                        DB.take 4
                                                bs
                                    )
                       ) of
                    Fail _
                         ctxs
                         err ->
                      error  $
                        show $ "Error while parsing AOPT field." ++
                               "\n"                              ++
                               "Parsing contexts: "              ++
                               (intercalate ", " ctxs)           ++
                               "\n"                              ++
                               "Error message: "                 ++
                               err
                    Done remainingbytes
                         parsedbs -> do
                      let strictremainingbytes = DB.toStrict remainingbytes
                      case DB.null strictremainingbytes of
                        True  ->
                          BAM_V1_6_BAM_Alignment_OptionalFields
                            { bam_v1_6_bam_alignment_optionalfields_aopt        = Just parsedbs
                            , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bopt        = Nothing
                            }
                          DSeq.:<|
                          parse_OptionalFields ( DB.drop 4
                                                         bs
                                               )
                        False ->
                          error $
                            show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                -- SmallCOPT <-> 'c'
                Just 0x63 ->
                  case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_SmallCOPT
                                    ( DB.fromStrict $
                                        DB.take 4
                                                bs
                                    )
                       ) of
                    Fail _
                         ctxs
                         err ->
                      error  $
                        show $ "Error while parsing SmallCOPT field." ++
                               "\n"                                   ++
                               "Parsing contexts: "                   ++
                               (intercalate ", " ctxs)                ++
                               "\n"                                   ++
                               "Error message: "                      ++
                               err
                    Done remainingbytes
                         parsedbs -> do
                      let strictremainingbytes = DB.toStrict remainingbytes
                      case DB.null strictremainingbytes of
                        True  ->
                          BAM_V1_6_BAM_Alignment_OptionalFields
                            { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Just parsedbs
                            , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bopt        = Nothing
                            }
                          DSeq.:<|
                          parse_OptionalFields ( DB.drop 4
                                                         bs
                                               )
                        False ->
                          error $
                            show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                -- BigCOPT <-> 'C'
                Just 0x43 ->
                  case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_BigCOPT
                                    ( DB.fromStrict $
                                        DB.take 4
                                                bs
                                    )
                       ) of
                    Fail _
                         ctxs
                         err ->
                      error  $
                        show $ "Error while parsing BigCOPT field." ++
                               "\n"                                 ++
                               "Parsing contexts: "                 ++
                               (intercalate ", " ctxs)              ++
                               "\n"                                 ++
                               "Error message: "                    ++
                               err
                    Done remainingbytes
                         parsedbs -> do
                      let strictremainingbytes = DB.toStrict remainingbytes
                      case DB.null strictremainingbytes of
                        True  ->
                          BAM_V1_6_BAM_Alignment_OptionalFields
                            { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Just parsedbs
                            , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bopt        = Nothing
                            }
                          DSeq.:<|
                          parse_OptionalFields ( DB.drop 4
                                                         bs
                                               )
                        False ->
                          error $
                            show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                -- SmallIOPT <-> 'i'
                Just 0x69 ->
                  case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_SmallIOPT
                                    ( DB.fromStrict $
                                        DB.take 7
                                                bs
                                    )
                       ) of
                    Fail _
                         ctxs
                         err ->
                      error  $
                        show $ "Error while parsing SmallIOPT field." ++
                               "\n"                                   ++
                               "Parsing contexts: "                   ++
                               (intercalate ", " ctxs)                ++
                               "\n"                                   ++
                               "Error message: "                      ++
                               err
                    Done remainingbytes
                         parsedbs -> do
                      let strictremainingbytes = DB.toStrict remainingbytes
                      case DB.null strictremainingbytes of
                        True  ->
                          BAM_V1_6_BAM_Alignment_OptionalFields
                            { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Just parsedbs
                            , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bopt        = Nothing
                            }
                          DSeq.:<|
                          parse_OptionalFields ( DB.drop 7
                                                         bs
                                               ) 
                        False ->
                          error $
                            show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                -- BigIOPT <-> 'I'
                Just 0x49 ->
                  case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_BigIOPT
                                    ( DB.fromStrict $
                                        DB.take 7
                                                bs
                                    )
                       ) of
                    Fail _
                         ctxs
                         err ->
                      error  $
                        show $ "Error while parsing BigIOPT field." ++
                               "\n"                                 ++
                               "Parsing contexts: "                 ++
                               (intercalate ", " ctxs)              ++
                               "\n"                                 ++
                               "Error message: "                    ++
                               err
                    Done remainingbytes
                         parsedbs -> do
                      let strictremainingbytes = DB.toStrict remainingbytes
                      case DB.null strictremainingbytes of
                        True  ->
                          BAM_V1_6_BAM_Alignment_OptionalFields
                            { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Just parsedbs
                            , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bopt        = Nothing
                            }
                          DSeq.:<|
                          parse_OptionalFields ( DB.drop 7
                                                         bs
                                               )
                        False ->
                          error $
                            show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                -- SmallSOPT <-> 's'
                Just 0x73 ->
                  case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_SmallSOPT
                                    ( DB.fromStrict $
                                        DB.take 5
                                                bs
                                    )
                       ) of
                    Fail _
                         ctxs
                         err ->
                      error  $
                        show $ "Error while parsing SmallSOPT field." ++
                               "\n"                                   ++
                               "Parsing contexts: "                   ++
                               (intercalate ", " ctxs)                ++
                               "\n"                                   ++
                               "Error message: "                      ++
                               err
                    Done remainingbytes
                         parsedbs -> do
                      let strictremainingbytes = DB.toStrict remainingbytes
                      case DB.null strictremainingbytes of
                        True  ->
                          BAM_V1_6_BAM_Alignment_OptionalFields
                            { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Just parsedbs
                            , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bopt        = Nothing
                            }
                          DSeq.:<|
                          parse_OptionalFields ( DB.drop 5
                                                         bs
                                               )
                        False ->
                          error $
                            show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                -- BigSOPT <-> 'S'
                Just 0x53 ->
                  case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_BigSOPT
                                    ( DB.fromStrict $
                                        DB.take 5
                                                bs
                                    )
                       ) of
                    Fail _
                         ctxs
                         err ->
                      error  $
                        show $ "Error while parsing BigSOPT field." ++
                               "\n"                                 ++
                               "Parsing contexts: "                 ++
                               (intercalate ", " ctxs)              ++
                               "\n"                                 ++
                               "Error message: "                    ++
                               err
                    Done remainingbytes
                         parsedbs -> do
                      let strictremainingbytes = DB.toStrict remainingbytes
                      case DB.null strictremainingbytes of
                        True  ->
                          BAM_V1_6_BAM_Alignment_OptionalFields
                            { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Just parsedbs
                            , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bopt        = Nothing
                            }
                          DSeq.:<|
                          parse_OptionalFields ( DB.drop 5
                                                         bs
                                               )
                        False ->
                          error $
                            show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                -- FOPT <-> 'f'
                Just 0x66 ->
                  case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_FOPT
                                    ( DB.fromStrict $
                                        DB.take 7
                                                bs
                                    )
                       ) of
                    Fail _
                         ctxs
                         err ->
                      error  $
                        show $ "Error while parsing FOPT field." ++
                               "\n"                              ++
                               "Parsing contexts: "              ++
                               (intercalate ", " ctxs)           ++
                               "\n"                              ++
                               "Error message: "                 ++
                               err
                    Done remainingbytes
                         parsedbs -> do
                      let strictremainingbytes = DB.toStrict remainingbytes
                      case DB.null strictremainingbytes of
                        True  ->
                          BAM_V1_6_BAM_Alignment_OptionalFields
                            { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_fopt        = Just parsedbs
                            , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bopt        = Nothing
                            }
                          DSeq.:<|
                          parse_OptionalFields ( DB.drop 7
                                                         bs
                                               )
                        False ->
                          error $
                            show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                -- ZOPT <-> 'Z'
                Just 0x5A ->
                  case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_ZOPT
                                    ( DB.fromStrict $
                                        takeUpTo 0x00
                                                 bs
                                    )
                       ) of
                    Fail _
                         ctxs
                         err ->
                      error  $
                        show $ "Error while parsing ZOPT field." ++
                               "\n"                              ++
                               "Parsing contexts: "              ++
                               (intercalate ", " ctxs)           ++
                               "\n"                              ++
                               "Error message: "                 ++
                               err
                    Done remainingbytes
                         parsedbs -> do
                      let strictremainingbytes = DB.toStrict remainingbytes
                      case DB.null strictremainingbytes of
                        True  ->
                          BAM_V1_6_BAM_Alignment_OptionalFields
                            { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_zopt        = Just parsedbs
                            , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bopt        = Nothing
                            }
                          DSeq.:<|
                          parse_OptionalFields ( dropUpTo 0x00
                                                          bs
                                               )
                        False ->
                          error $
                            show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                -- HOPT <-> 'H'
                Just 0x48 ->
                  case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_HOPT
                                    ( DB.fromStrict $
                                        takeUpTo 0x00
                                                 bs
                                    )
                       ) of
                    Fail _
                         ctxs
                         err ->
                      error  $
                        show $ "Error while parsing HOPT field." ++
                               "\n"                              ++
                               "Parsing contexts: "              ++
                               (intercalate ", " ctxs)           ++
                               "\n"                              ++
                               "Error message: "                 ++
                               err
                    Done remainingbytes
                         parsedbs -> do
                      let strictremainingbytes = DB.toStrict remainingbytes
                      case DB.null strictremainingbytes of
                        True  ->
                          BAM_V1_6_BAM_Alignment_OptionalFields
                            { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                            , bam_v1_6_bam_alignment_optionalfields_hopt        = Just parsedbs
                            , bam_v1_6_bam_alignment_optionalfields_bopt        = Nothing
                            }
                          DSeq.:<|
                          parse_OptionalFields ( dropUpTo 0x00
                                                          bs
                                               )
                        False ->
                          error $
                            show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                -- BOPT <-> 'B'
                Just 0x42 ->
                  case DB.indexMaybe bs 3 of
                    Nothing            ->
                      error $
                        show BAM_V1_6_Read_Error_Alignment_OptionalFields_Missing_Type
                    Just boptcharacter ->
                      case boptcharacter of
                        -- 'c'
                        0x63 -> do
                          let boptcount = fromIntegral       $
                                            word8sToWord32LE $
                                              DB.unpack      $
                                                DB.take 4    $
                                                  DB.drop 4
                                                          bs
                          case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
                                            ( DB.fromStrict $
                                                DB.take
                                                  ( ( boptcount * 1
                                                    ) + 8
                                                  )
                                                  bs
                                            )
                               ) of
                            Fail _
                                 ctxs
                                 err ->
                              error  $
                                show $ "Error while parsing BOPT field." ++
                                       "\n"                              ++
                                       "Parsing contexts: "              ++
                                       (intercalate ", " ctxs)           ++
                                       "\n"                              ++
                                       "Error message: "                 ++
                                       err
                            Done remainingbytes
                                 parsedbs -> do
                              let strictremainingbytes = DB.toStrict remainingbytes
                              case DB.null strictremainingbytes of
                                True  ->
                                  BAM_V1_6_BAM_Alignment_OptionalFields
                                    { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bopt        = Just parsedbs
                                    }
                                  DSeq.:<|
                                  parse_OptionalFields ( DB.drop ( ( boptcount * 1
                                                                   ) + 8
                                                                 )
                                                                 bs
                                                       )
                                False ->
                                  error $
                                    show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                        -- 'C'
                        0x43 -> do
                          let boptcount = fromIntegral       $
                                            word8sToWord32LE $
                                              DB.unpack      $
                                                DB.take 4    $
                                                  DB.drop 4
                                                          bs
                          case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
                                            ( DB.fromStrict $
                                                DB.take
                                                  ( ( boptcount * 1
                                                    ) + 8
                                                  )
                                                  bs
                                            )
                               ) of
                            Fail _
                                 ctxs
                                 err ->
                              error  $
                                show $ "Error while parsing BOPT field." ++
                                       "\n"                              ++
                                       "Parsing contexts: "              ++
                                       (intercalate ", " ctxs)           ++
                                       "\n"                              ++
                                       "Error message: "                 ++
                                       err
                            Done remainingbytes
                                 parsedbs -> do
                              let strictremainingbytes = DB.toStrict remainingbytes
                              case DB.null strictremainingbytes of
                                True  ->
                                  BAM_V1_6_BAM_Alignment_OptionalFields
                                    { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bopt        = Just parsedbs
                                    }
                                  DSeq.:<|
                                  parse_OptionalFields ( DB.drop ( ( boptcount * 1
                                                                   ) + 8
                                                                 )
                                                                 bs
                                                       )
                                False ->
                                  error $
                                    show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                        -- 'i'
                        0x69 -> do
                          let boptcount = fromIntegral       $
                                            word8sToWord32LE $
                                              DB.unpack      $
                                                DB.take 4    $
                                                  DB.drop 4
                                                          bs
                          case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
                                            ( DB.fromStrict $
                                                DB.take
                                                  ( ( boptcount * 4
                                                    ) + 8
                                                  )
                                                  bs
                                            )
                               ) of
                            Fail _
                                 ctxs
                                 err ->
                              error  $
                                show $ "Error while parsing BOPT field." ++
                                       "\n"                              ++
                                       "Parsing contexts: "              ++
                                       (intercalate ", " ctxs)           ++
                                       "\n"                              ++
                                       "Error message: "                 ++
                                       err
                            Done remainingbytes
                                 parsedbs -> do
                              let strictremainingbytes = DB.toStrict remainingbytes
                              case DB.null strictremainingbytes of
                                True  ->
                                  BAM_V1_6_BAM_Alignment_OptionalFields
                                    { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bopt        = Just parsedbs
                                    }
                                  DSeq.:<|
                                  parse_OptionalFields ( DB.drop ( ( boptcount * 4
                                                                   ) + 8
                                                                 )
                                                                 bs
                                                       )
                                False ->
                                  error $
                                    show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                        -- 'I'
                        0x49 -> do
                          let boptcount = fromIntegral       $
                                            word8sToWord32LE $
                                              DB.unpack      $
                                                DB.take 4    $
                                                  DB.drop 4
                                                          bs
                          case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
                                            ( DB.fromStrict $
                                                DB.take
                                                  ( ( boptcount * 4
                                                    ) + 8
                                                  )
                                                  bs
                                            )
                               ) of
                            Fail _
                                 ctxs
                                 err ->
                              error  $
                                show $ "Error while parsing BOPT field." ++
                                       "\n"                              ++
                                       "Parsing contexts: "              ++
                                       (intercalate ", " ctxs)           ++
                                       "\n"                              ++
                                       "Error message: "                 ++
                                       err
                            Done remainingbytes
                                 parsedbs -> do
                              let strictremainingbytes = DB.toStrict remainingbytes
                              case DB.null strictremainingbytes of
                                True  ->
                                  BAM_V1_6_BAM_Alignment_OptionalFields
                                    { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bopt        = Just parsedbs
                                    }
                                  DSeq.:<|
                                  parse_OptionalFields ( DB.drop ( ( boptcount * 4
                                                                   ) + 8
                                                                 )
                                                                 bs
                                                       )
                                False ->
                                  error $
                                    show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                        -- 's'
                        0x73 -> do
                          let boptcount = fromIntegral       $
                                            word8sToWord32LE $
                                              DB.unpack      $
                                                DB.take 4    $
                                                  DB.drop 4
                                                          bs
                          case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
                                            ( DB.fromStrict $
                                                DB.take
                                                  ( ( boptcount * 2
                                                    ) + 8
                                                  )
                                                  bs
                                            )
                               ) of
                            Fail _
                                 ctxs
                                 err ->
                              error  $
                                show $ "Error while parsing BOPT field." ++
                                       "\n"                              ++
                                       "Parsing contexts: "              ++
                                       (intercalate ", " ctxs)           ++
                                       "\n"                              ++
                                       "Error message: "                 ++
                                       err
                            Done remainingbytes
                                 parsedbs -> do
                              let strictremainingbytes = DB.toStrict remainingbytes
                              case DB.null strictremainingbytes of
                                True  ->
                                  BAM_V1_6_BAM_Alignment_OptionalFields
                                    { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bopt        = Just parsedbs
                                    }
                                  DSeq.:<|
                                  parse_OptionalFields ( DB.drop ( ( boptcount * 2
                                                                   ) + 8
                                                                 )
                                                                 bs
                                                       )
                                False ->
                                  error $
                                    show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                        -- 'S'
                        0x53 -> do
                          let boptcount = fromIntegral       $
                                            word8sToWord32LE $
                                              DB.unpack      $
                                                DB.take 4    $
                                                  DB.drop 4
                                                          bs
                          case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
                                            ( DB.fromStrict $
                                                DB.take
                                                  ( ( boptcount * 2
                                                    ) + 8
                                                  )
                                                  bs
                                            )
                               ) of
                            Fail _
                                 ctxs
                                 err ->
                              error  $
                                show $ "Error while parsing BOPT field." ++
                                       "\n"                              ++
                                       "Parsing contexts: "              ++
                                       (intercalate ", " ctxs)           ++
                                       "\n"                              ++
                                       "Error message: "                 ++
                                       err
                            Done remainingbytes
                                 parsedbs -> do
                              let strictremainingbytes = DB.toStrict remainingbytes
                              case DB.null strictremainingbytes of
                                True  ->
                                  BAM_V1_6_BAM_Alignment_OptionalFields
                                    { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bopt        = Just parsedbs
                                    }
                                  DSeq.:<|
                                  parse_OptionalFields ( DB.drop ( ( boptcount * 2
                                                                   ) + 8
                                                                 )
                                                                 bs
                                                       )
                                False ->
                                  error $
                                    show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                        -- 'f'
                        0x66 -> do
                          let boptcount = fromIntegral       $
                                            word8sToWord32LE $
                                              DB.unpack      $
                                                DB.take 4    $
                                                  DB.drop 4
                                                          bs
                          case ( DABL.parse parse_BAM_V1_6_BAM_Alignment_OptionalFields_BOPT
                                            ( DB.fromStrict $
                                                DB.take
                                                  ( ( boptcount * 4
                                                    ) + 8
                                                  )
                                                  bs
                                            )
                               ) of
                            Fail _
                                 ctxs
                                 err ->
                              error  $
                                show $ "Error while parsing BOPT field." ++
                                       "\n"                              ++
                                       "Parsing contexts: "              ++
                                       (intercalate ", " ctxs)           ++
                                       "\n"                              ++
                                       "Error message: "                 ++
                                       err
                            Done remainingbytes
                                 parsedbs -> do
                              let strictremainingbytes = DB.toStrict remainingbytes
                              case DB.null strictremainingbytes of
                                True  ->
                                  BAM_V1_6_BAM_Alignment_OptionalFields
                                    { bam_v1_6_bam_alignment_optionalfields_aopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smallcopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigcopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smalliopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigiopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_smallsopt   = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bigsopt     = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_fopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_zopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_hopt        = Nothing
                                    , bam_v1_6_bam_alignment_optionalfields_bopt        = Just parsedbs
                                    }
                                  DSeq.:<|
                                  parse_OptionalFields ( DB.drop ( ( boptcount * 4
                                                                   ) + 8
                                                                 )
                                                                 bs
                                                       )
                                False ->
                                  error $
                                    show BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                        _    ->
                          error $
                            show BAM_V1_6_Read_Error_Alignment_OptionalFields_BOPT_Missing_Type
                _         ->
                  error $
                    show BAM_V1_6_Read_Error_Alignment_OptionalFields_Missing_Type
