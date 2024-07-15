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
{-# Language QuasiQuotes                 #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Module      :  Data.SAM.Version1_6.Read.Parser.Alignment.Base
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

module Data.SAM.Version1_6.Read.Parser.Alignment.Base ( -- * SAM_V1_6 parser - alignment section
                                                        parse_SAM_V1_6_Alignment
                                                      ) where

import Data.SAM.Version1_6.Alignment
import Data.SAM.Version1_6.Read.Error
import Data.SAM.Version1_6.Alignment.OptionalFields.Base
import Data.SAM.Version1_6.Read.Parser.Alignment.OptionalFields.AOPT
import Data.SAM.Version1_6.Read.Parser.Alignment.OptionalFields.IOPT
import Data.SAM.Version1_6.Read.Parser.Alignment.OptionalFields.FOPT
import Data.SAM.Version1_6.Read.Parser.Alignment.OptionalFields.ZOPT
import Data.SAM.Version1_6.Read.Parser.Alignment.OptionalFields.HOPT
import Data.SAM.Version1_6.Read.Parser.Alignment.OptionalFields.BOPT

import           Data.Attoparsec.ByteString.Char8  as DABC8 (endOfLine,isEndOfLine)
import           Data.Attoparsec.ByteString.Lazy   as DABL
import qualified Data.ByteString                   as DB
import qualified Data.ByteString.Char8             as DBC8
import           Data.List                         as DL    (intercalate)
import           Data.Sequence                     as DSeq  (empty,Seq(..))
import           Text.Regex.PCRE.Heavy

-- | @"SAM_V1_6_Alignment"@ parser.
--
-- Defines a parser for the alignment section of the SAM v1.6 file format.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_SAM_V1_6_Alignment :: Parser SAM_V1_6_Alignment
parse_SAM_V1_6_Alignment = do
  qname <- do
    qnamep <-
      DABL.takeTill (== 09)
    -- Parse QNAME field of alignment section.
    case (qnamep =~ [re|[!-?A-~]{1,254}|\*|]) of
      False ->
        fail $
          show SAM_V1_6_Error_Alignment_QNAME_Incorrect_Format
      True  ->
        -- QNAME is in the accepted format.
        return qnamep
  _ <-
    word8 09
  flag <- do
    flagp <-
      DABL.takeTill (== 09)
    -- Parse FLAG field of alignment section.
    case (flagp =~ [re|[0-9]+|]) of
      False ->
        fail $
          show SAM_V1_6_Error_Alignment_FLAG_Incorrect_Format
      True  ->
        -- FLAG is in the accepted format.
        return flagp
  _ <-
    word8 09
  rname <- do
    rnamep <-
      DABL.takeTill (== 09)
    -- Parse RNAME field of alignment section.
    case (rnamep =~ [re|\*|[0-9A-Za-z!#$%&+.:;?@^_|~-][0-9A-Za-z!#$%&*+.:;=?@^_|~-]*|]) of
      False ->
        fail $
          show SAM_V1_6_Error_Alignment_RNAME_Incorrect_Format 
      True  ->
        -- RNAME is in the accepted format.
        return rnamep
  _ <-
    word8 09
  pos <- do
    posp <-
      DABL.takeTill (== 09)
    -- Parse POS field of the alignment section.
    case (posp =~ [re|[0-9]+|]) of
      False ->
        fail $
          show SAM_V1_6_Error_Alignment_POS_Incorrect_Format
      True  ->
        -- POS is in the accepted format.
        return posp
  _ <-
    word8 09
  mapq <- do
    mapqp <-
      DABL.takeTill (== 09)
    -- Parse MAPQ field of the alignment section.
    case (mapqp =~ [re|[0-9]+|]) of
      False ->
        fail $
          show SAM_V1_6_Error_Alignment_MAPQ_Incorrect_Format
      True  ->
        -- MAPQ is in the accepted format.
        return mapqp
  _ <-
    word8 09
  cigar <- do
    cigarp <-
      DABL.takeTill (== 09)
    -- Parse CIGAR field of alignment section.
    case (cigarp =~ [re|\*|([0-9]+[MIDNSHPX=])+|]) of
      False ->
        fail $
          show SAM_V1_6_Error_Alignment_CIGAR_Incorrect_Format
      True  ->
        -- CIGAR is in the accepted format.
        return cigarp
  _ <-
    word8 09
  rnext <- do
    rnextp <-
      DABL.takeTill (== 09)
    -- Parse RNEXT field of the alignment section.
    case (rnextp =~ [re|\*|=|[0-9A-Za-z!#$%&+.:;?@^_|~-][0-9A-Za-z!#$%&*+.:;=?@^_|~-]*|]) of
      False ->
        fail $
          show SAM_V1_6_Error_Alignment_RNEXT_Incorrect_Format
      True  ->
        -- RNEXT is in the accepted format.
        return rnextp
  _ <-
    word8 09
  pnext <- do
    pnextp <-
      DABL.takeTill (== 09)
    -- Parse PNEXT field of the alignment section.
    case (pnextp =~ [re|[0-9]+|]) of
      False ->
        fail $
          show SAM_V1_6_Error_Alignment_PNEXT_Incorrect_Format
      True  ->
        -- PNEXT is in the accepted format.
        return pnextp
  _ <-
    word8 09
  tlen <- do
    tlenp <-
      DABL.takeTill (== 09)
    -- Parse TLEN field of the alignment section.
    case (tlenp =~ [re|[-]?[0-9]+|]) of
      False ->
        fail $
          show SAM_V1_6_Error_Alignment_TLEN_Incorrect_Format 
      True  ->
        -- TLEN is in the accepted format.
        return tlenp
  _ <-
    word8 09
  seq <- do
    seqp <-
      DABL.takeTill (== 09)
    -- Parse SEQ field of the alignment section.
    case (seqp =~ [re|\*|[A-Za-z=.]+|]) of
      False ->
        fail $
          show SAM_V1_6_Error_Alignment_SEQ_Incorrect_Format 
      True  ->
        -- SEQ is in the accepted format.
        return seqp
  _ <-
    word8 09
  qual <- do
    qualp <-
      DABL.takeTill (\x -> x == 09 || isEndOfLine x)
    -- Parse QUAL field of the alignment section.
    case (qualp =~ [re|[!-~?]+|\*|]) of
      False ->
        fail $
          show SAM_V1_6_Error_Alignment_QUAL_Incorrect_Format
      True  ->
        -- QUAL is in the accepted format.
        return qualp 
  optfields <-
    peekWord8
  case optfields of
    Just 10 -> do
      -- Return the parsed SAM_V1_6.
      _ <-
        endOfLine
      return SAM_V1_6_Alignment { sam_v1_6_alignment_qname          = qname
                                , sam_v1_6_alignment_flag           = case (DBC8.readInt flag) of
                                                                        Nothing          ->
                                                                          (-1)
                                                                        Just (flagint,_) ->
                                                                          flagint
                                , sam_v1_6_alignment_rname          = rname
                                , sam_v1_6_alignment_pos            = case (DBC8.readInteger pos) of
                                                                        Nothing             ->
                                                                          0
                                                                        Just (posinteger,_) ->
                                                                          posinteger
                                , sam_v1_6_alignment_mapq           = case (DBC8.readInt mapq) of
                                                                        Nothing          ->
                                                                          255
                                                                        Just (mapqint,_) ->
                                                                          mapqint
                                , sam_v1_6_alignment_cigar          = cigar
                                , sam_v1_6_alignment_rnext          = rnext
                                , sam_v1_6_alignment_pnext          = case (DBC8.readInteger pnext) of
                                                                        Nothing               ->
                                                                          0
                                                                        Just (pnextinteger,_) ->
                                                                          pnextinteger
                                , sam_v1_6_alignment_tlen           = case (DBC8.readInteger tlen) of
                                                                        Nothing              ->
                                                                          0
                                                                        Just (tleninteger,_) ->
                                                                          tleninteger
                                , sam_v1_6_alignment_seq            = seq
                                , sam_v1_6_alignment_qual           = qual
                                , sam_v1_6_alignment_optionalfields = Nothing
                                }
    _       -> do
      -- This parser assumes that
      -- the AOPT, IOPT, FOPT, ZOPT, HOPT, and BOPT
      -- tags can appear in any order.
      _                   <-
        word8 09
      optionalfieldsbytes <-
        DABL.takeTill isEndOfLine
      optionalfields      <-
        return $
          parse_OptionalFields optionalfieldsbytes
      _                   <-
        endOfLine
      -- Return the parsed SAM_V1_6.
      return SAM_V1_6_Alignment { sam_v1_6_alignment_qname          = qname
                                , sam_v1_6_alignment_flag           = case (DBC8.readInt flag) of
                                                                        Nothing          ->
                                                                          (-1)
                                                                        Just (flagint,_) ->
                                                                          flagint
                                , sam_v1_6_alignment_rname          = rname
                                , sam_v1_6_alignment_pos            = case (DBC8.readInteger pos) of
                                                                        Nothing             ->
                                                                          0
                                                                        Just (posinteger,_) ->
                                                                          posinteger
                                , sam_v1_6_alignment_mapq           = case (DBC8.readInt mapq) of
                                                                        Nothing          ->
                                                                          255
                                                                        Just (mapqint,_) ->
                                                                          mapqint
                                , sam_v1_6_alignment_cigar          = cigar
                                , sam_v1_6_alignment_rnext          = rnext
                                , sam_v1_6_alignment_pnext          = case (DBC8.readInteger pnext) of
                                                                        Nothing               ->
                                                                          0
                                                                        Just (pnextinteger,_) ->
                                                                          pnextinteger
                                , sam_v1_6_alignment_tlen           = case (DBC8.readInteger tlen) of
                                                                        Nothing              ->
                                                                          0
                                                                        Just (tleninteger,_) ->
                                                                          tleninteger
                                , sam_v1_6_alignment_seq            = seq
                                , sam_v1_6_alignment_qual           = qual
                                , sam_v1_6_alignment_optionalfields = Just optionalfields
                                }
  where
    parse_OptionalFields :: DB.ByteString
                         -> Seq SAM_V1_6_Alignment_OptionalFields
    parse_OptionalFields bs = do
      case DB.null bs of
        True  ->
          DSeq.empty
        False ->
          case DB.indexMaybe bs 3 of
            Nothing   ->
              error $
                show SAM_V1_6_Read_Error_Alignment_OptionalFields_Index_Missing
            -- AOPT <-> 'A'
            Just 0x41 ->
              case ( DABL.parse parse_SAM_V1_6_Alignment_OptionalFields_AOPT
                                ( DB.fromStrict $
                                    DB.takeWhile (/= 0x09)
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
                      SAM_V1_6_Alignment_OptionalFields
                        { sam_v1_6_alignment_optionalfields_aopt  = Just parsedbs
                        , sam_v1_6_alignment_optionalfields_iopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_fopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_zopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_hopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_bopt  = Nothing
                        }
                      DSeq.:<|
                        parse_OptionalFields ( DB.drop 1 $
                                                       DB.dropWhile (/= 0x09)
                                                                    bs
                                             )
                    False ->
                      error $
                        show SAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
            -- IOPT <-> 'i'
            Just 0x69 ->
              case ( DABL.parse parse_SAM_V1_6_Alignment_OptionalFields_IOPT
                                ( DB.fromStrict $
                                    DB.takeWhile (/= 0x09)
                                                 bs
                                )
                   ) of
                Fail _
                     ctxs
                     err ->
                  error  $
                    show $ "Error while parsing IOPT field." ++
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
                      SAM_V1_6_Alignment_OptionalFields
                        { sam_v1_6_alignment_optionalfields_aopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_iopt  = Just parsedbs
                        , sam_v1_6_alignment_optionalfields_fopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_zopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_hopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_bopt  = Nothing
                        }
                      DSeq.:<|
                      parse_OptionalFields ( DB.drop 1 $
                                               DB.dropWhile (/= 0x09)
                                                            bs
                                           )
                    False ->
                      error $
                        show SAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
            -- FOPT <-> 'f'
            Just 0x66 ->
              case ( DABL.parse parse_SAM_V1_6_Alignment_OptionalFields_FOPT
                                ( DB.fromStrict $
                                    DB.takeWhile (/= 0x09)
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
                      SAM_V1_6_Alignment_OptionalFields
                        { sam_v1_6_alignment_optionalfields_aopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_iopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_fopt  = Just parsedbs
                        , sam_v1_6_alignment_optionalfields_zopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_hopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_bopt  = Nothing
                        }                      
                      DSeq.:<|
                      parse_OptionalFields ( DB.drop 1 $
                                               DB.dropWhile (/= 0x09)
                                                            bs
                                           )
                    False ->
                      error $
                        show SAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
            -- ZOPT <-> 'Z'
            Just 0x5A ->
              case ( DABL.parse parse_SAM_V1_6_Alignment_OptionalFields_ZOPT
                                ( DB.fromStrict $
                                    DB.takeWhile (/= 0x09)
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
                      SAM_V1_6_Alignment_OptionalFields
                        { sam_v1_6_alignment_optionalfields_aopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_iopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_fopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_zopt  = Just parsedbs
                        , sam_v1_6_alignment_optionalfields_hopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_bopt  = Nothing
                        }
                      DSeq.:<|
                      parse_OptionalFields ( DB.drop 1 $
                                               DB.dropWhile (/= 0x09)
                                                            bs
                                           )
                    False ->
                      error $
                        show SAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
            -- HOPT <-> 'H'
            Just 0x48 ->
              case ( DABL.parse parse_SAM_V1_6_Alignment_OptionalFields_HOPT
                                ( DB.fromStrict $
                                    DB.takeWhile (/= 0x09)
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
                      SAM_V1_6_Alignment_OptionalFields
                        { sam_v1_6_alignment_optionalfields_aopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_iopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_fopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_zopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_hopt  = Just parsedbs
                        , sam_v1_6_alignment_optionalfields_bopt  = Nothing
                        }
                      DSeq.:<|
                      parse_OptionalFields ( DB.drop 1 $
                                               DB.dropWhile (/= 0x09)
                                                            bs
                                           )
                    False ->
                      error $
                        show SAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
            -- BOPT <-> 'B'
            Just 0x42 ->
              case ( DABL.parse parse_SAM_V1_6_Alignment_OptionalFields_BOPT
                                ( DB.fromStrict $
                                    DB.takeWhile (/= 0x09)
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
                      SAM_V1_6_Alignment_OptionalFields
                        { sam_v1_6_alignment_optionalfields_aopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_iopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_fopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_zopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_hopt  = Nothing
                        , sam_v1_6_alignment_optionalfields_bopt  = Just parsedbs
                        }
                      DSeq.:<|
                      parse_OptionalFields ( DB.drop 1 $
                                               DB.dropWhile (/= 0x09)
                                                            bs
                                           )
                    False ->
                      error $
                        show SAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
            _    ->
              error $
                show SAM_V1_6_Read_Error_Alignment_OptionalFields_BOPT_Missing_Type
