{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.Write.Base
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.Write.Base ( -- * Writing
                                        writeBAM_V1_6
                                      ) where

import Data.BAM.Version1_6.Base
import Data.BAM.Version1_6.BAM
import Data.BAM.Version1_6.BAM.BAMAlignments
import Data.BAM.Version1_6.BAM.BAMHeader
import Data.BAM.Version1_6.BAM.Alignment.Base
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.Base
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.AOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.BigCOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.BigIOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.BigSOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.SmallCOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.SmallIOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.SmallSOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.FOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.ZOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.HOPT
import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.BOPT
import Data.BAM.Version1_6.BAM.ReferenceInformation
import Data.BAM.Version1_6.BGZFBlock
import Data.BAM.Version1_6.GZipHeader
import Data.BAM.Version1_6.Internal
import Data.BAM.Version1_6.Write.Error
import Data.BAM.Version1_6.Write.Internal

import Codec.Compression.Zlib.Raw as CCZlibR (compress)
import Data.ByteString            as DB      (append,concat,empty,fromStrict,hPutStr,length,pack,singleton,toStrict)
import Data.ByteString.Base16     as DBB16
import Data.Base16.Types          as DBB16T
import Data.Digest.CRC32          as DDCRC32 (crc32)
import Data.Foldable                         (forM_,toList)
import Data.List                  as DL      (elem)
import Data.Maybe                            (fromJust)
import System.IO                             (hFlush,hClose,IOMode(..),openBinaryFile,Handle)

-- | Deconstruct a @"BAM_V1_6"@ and
-- write to a file.
deconstructBAM_V1_6 :: Handle
                    -> BAM_V1_6
                    -> IO ()
deconstructBAM_V1_6 h
                    bamv16 =
  forM_ (bam_v1_6 bamv16) $ \currentbgzfblock ->
    case ( bam_v1_6_bam_endoffilemarker $
             bam_v1_6_bgzfblock_cdata currentbgzfblock
         ) of
      True  -> do
        -- Write to file.
        -- GZip header.
        _ <-
          DB.hPutStr h                                        $
                     DB.singleton                             $
                       bam_v1_6_gzip_header_gzip_identifier_1 $
                         bam_v1_6_bgzfblock_gzip_header currentbgzfblock
        _ <-
          DB.hPutStr h                                        $
                     DB.singleton                             $
                       bam_v1_6_gzip_header_gzip_identifier_2 $
                         bam_v1_6_bgzfblock_gzip_header currentbgzfblock
        _ <-
          DB.hPutStr h                                         $
                     DB.singleton                              $
                       bam_v1_6_gzip_header_compression_method $
                         bam_v1_6_bgzfblock_gzip_header currentbgzfblock
        _ <-
          DB.hPutStr h                                   $
                     DB.singleton                        $
                       bam_v1_6_gzip_header_header_flags $
                         bam_v1_6_bgzfblock_gzip_header currentbgzfblock
        _ <-
          DB.hPutStr h                                        $
                     word32ToByteStringLE                     $
                       bam_v1_6_gzip_header_modification_time $
                         bam_v1_6_bgzfblock_gzip_header currentbgzfblock
        _ <-
          DB.hPutStr h                                  $
                     DB.singleton                       $
                       bam_v1_6_gzip_header_extra_flags $
                         bam_v1_6_bgzfblock_gzip_header currentbgzfblock
        _ <-
          DB.hPutStr h                                       $
                     DB.singleton                            $
                       bam_v1_6_gzip_header_operating_system $
                         bam_v1_6_bgzfblock_gzip_header currentbgzfblock   
        _ <-
          DB.hPutStr h                                   $
                     word16ToByteStringLE                $
                       bam_v1_6_gzip_header_extra_length $
                         bam_v1_6_bgzfblock_gzip_header currentbgzfblock
        -- Subfield Identifier 1.
        _ <-
          DB.hPutStr h            $
                     DB.singleton $
                       bam_v1_6_bgzfblock_subfield_identifier_one currentbgzfblock
        -- Subfield Identifier 2.
        _ <-
          DB.hPutStr h            $
                     DB.singleton $
                       bam_v1_6_bgzfblock_subfield_identifier_two currentbgzfblock
        -- Subfield LENgth.
        _ <-
          DB.hPutStr h                    $
                     word16ToByteStringLE $
                       bam_v1_6_bgzfblock_subfield_length currentbgzfblock
        -- total Block SIZE minus 1.
        _ <-
          DB.hPutStr h                    $
                     word16ToByteStringLE $
                       bam_v1_6_bgzfblock_total_block_size_minus_one currentbgzfblock
        -- Write the empty CDATA block.
        _ <-
          DB.hPutStr h $
                     DB.pack
                       [ 0x03
                       , 0x00
                       ]
        -- CRC-32.
        _ <-
          DB.hPutStr h                    $
                     word32ToByteStringLE $
                         bam_v1_6_bgzfblock_crc32 currentbgzfblock
        -- Input SIZE (length of uncompressed data).
        DB.hPutStr h                    $
                   word32ToByteStringLE $
                     bam_v1_6_bgzfblock_isize currentbgzfblock
      False ->
        case ( bam_v1_6_bam_bamheader $
                 bam_v1_6_bgzfblock_cdata currentbgzfblock
             ) of
          Nothing                      ->
            case ( bam_v1_6_bam_bamalignments $
                     bam_v1_6_bgzfblock_cdata currentbgzfblock
                 ) of
              Nothing ->
                error $
                  show BAM_V1_6_Write_Error_No_Data
              Just bam_v1_6_bam_bamalignments' -> do
                -- Write to file.
                -- GZip header.
                _ <-
                  DB.hPutStr h                                        $
                             DB.singleton                             $
                               bam_v1_6_gzip_header_gzip_identifier_1 $
                                 bam_v1_6_bgzfblock_gzip_header currentbgzfblock
                _ <-
                  DB.hPutStr h                                        $
                             DB.singleton                             $
                               bam_v1_6_gzip_header_gzip_identifier_2 $
                                 bam_v1_6_bgzfblock_gzip_header currentbgzfblock
                _ <-
                  DB.hPutStr h                                         $
                             DB.singleton                              $
                               bam_v1_6_gzip_header_compression_method $
                                 bam_v1_6_bgzfblock_gzip_header currentbgzfblock
                _ <-
                  DB.hPutStr h                                   $
                             DB.singleton                        $
                               bam_v1_6_gzip_header_header_flags $
                                 bam_v1_6_bgzfblock_gzip_header currentbgzfblock
                _ <-
                  DB.hPutStr h                                        $
                             word32ToByteStringLE                     $
                               bam_v1_6_gzip_header_modification_time $
                                 bam_v1_6_bgzfblock_gzip_header currentbgzfblock
                _ <-
                  DB.hPutStr h                                  $
                             DB.singleton                       $
                               bam_v1_6_gzip_header_extra_flags $
                                 bam_v1_6_bgzfblock_gzip_header currentbgzfblock
                _ <-
                  DB.hPutStr h                                       $
                             DB.singleton                            $
                               bam_v1_6_gzip_header_operating_system $
                                bam_v1_6_bgzfblock_gzip_header currentbgzfblock   
                _ <-
                  DB.hPutStr h                                   $
                             word16ToByteStringLE                $
                               bam_v1_6_gzip_header_extra_length $
                                 bam_v1_6_bgzfblock_gzip_header currentbgzfblock
                -- Subfield Identifier 1.
                _ <-
                  DB.hPutStr h            $
                             DB.singleton $
                               bam_v1_6_bgzfblock_subfield_identifier_one currentbgzfblock
                -- Subfield Identifier 2.
                _ <-
                  DB.hPutStr h            $
                             DB.singleton $
                               bam_v1_6_bgzfblock_subfield_identifier_two currentbgzfblock
                -- Subfield LENgth.
                _ <-
                  DB.hPutStr h                    $
                             word16ToByteStringLE $
                               bam_v1_6_bgzfblock_subfield_length currentbgzfblock
                let bam_v1_6_bam_bamalignments'' = fmap (\currentbamalignmentrecord -> do
                                                               let preoptionalfields = ( word32ToByteStringLE $
                                                                                           bam_v1_6_bam_alignment_block_size currentbamalignmentrecord
                                                                                       )
                                                                                       `DB.append`
                                                                                       ( int32ToByteStringLE $
                                                                                           bam_v1_6_bam_alignment_refID currentbamalignmentrecord
                                                                                       )
                                                                                       `DB.append`
                                                                                       ( int32ToByteStringLE $
                                                                                           bam_v1_6_bam_alignment_pos currentbamalignmentrecord
                                                                                       )
                                                                                       `DB.append`
                                                                                       ( DB.singleton $
                                                                                           bam_v1_6_bam_alignment_l_read_name currentbamalignmentrecord
                                                                                       )
                                                                                       `DB.append`
                                                                                       ( DB.singleton $
                                                                                           bam_v1_6_bam_alignment_mapq currentbamalignmentrecord
                                                                                       )
                                                                                       `DB.append`
                                                                                       ( word16ToByteStringLE $
                                                                                           bam_v1_6_bam_alignment_bin currentbamalignmentrecord
                                                                                       )
                                                                                       `DB.append`
                                                                                       ( word16ToByteStringLE $
                                                                                           bam_v1_6_bam_alignment_n_cigar_op currentbamalignmentrecord
                                                                                       )
                                                                                       `DB.append`
                                                                                       ( word16ToByteStringLE $
                                                                                           bam_v1_6_bam_alignment_flag currentbamalignmentrecord
                                                                                       )
                                                                                       `DB.append`
                                                                                       ( word32ToByteStringLE $
                                                                                           bam_v1_6_bam_alignment_l_seq currentbamalignmentrecord
                                                                                       )
                                                                                       `DB.append`
                                                                                       ( int32ToByteStringLE $
                                                                                           bam_v1_6_bam_alignment_next_refID currentbamalignmentrecord
                                                                                       )
                                                                                       `DB.append`
                                                                                       ( int32ToByteStringLE $
                                                                                           bam_v1_6_bam_alignment_next_pos currentbamalignmentrecord
                                                                                       )
                                                                                       `DB.append`
                                                                                       ( int32ToByteStringLE $
                                                                                           bam_v1_6_bam_alignment_tlen currentbamalignmentrecord
                                                                                       )
                                                                                       `DB.append`
                                                                                       bam_v1_6_bam_alignment_read_name currentbamalignmentrecord
                                                                                       `DB.append`
                                                                                       ( DB.concat $
                                                                                           toList  $
                                                                                             fmap (\currentcigar ->
                                                                                                     word32ToByteStringLE currentcigar
                                                                                                  )
                                                                                             (bam_v1_6_bam_alignment_cigar currentbamalignmentrecord)
                                                                                       )
                                                                                       `DB.append`
                                                                                       ( encodeSeqField $
                                                                                           toList       $
                                                                                             bam_v1_6_bam_alignment_seq currentbamalignmentrecord
                                                                                       )
                                                                                       `DB.append`
                                                                                       ( bam_v1_6_bam_alignment_qual currentbamalignmentrecord
                                                                                       )
                                                               let optionalfields = DB.concat $
                                                                                      toList  $
                                                                                        fmap (\currentbamalignmentoptionalfield -> do
                                                                                                -- Walk through optional fields.
                                                                                                let optionalfieldsl = [ case ( bam_v1_6_bam_alignment_optionalfields_aopt
                                                                                                                                 currentbamalignmentoptionalfield 
                                                                                                                             ) of
                                                                                                                          Nothing ->
                                                                                                                            Empty
                                                                                                                          Just _  ->
                                                                                                                            AOPT
                                                                                                                      , case ( bam_v1_6_bam_alignment_optionalfields_smallcopt
                                                                                                                                 currentbamalignmentoptionalfield
                                                                                                                             ) of
                                                                                                                          Nothing ->
                                                                                                                            Empty
                                                                                                                          Just _  ->
                                                                                                                            SmallCOPT
                                                                                                                      , case ( bam_v1_6_bam_alignment_optionalfields_bigcopt
                                                                                                                                 currentbamalignmentoptionalfield
                                                                                                                             ) of
                                                                                                                          Nothing ->
                                                                                                                            Empty
                                                                                                                          Just _  ->
                                                                                                                            BigCOPT
                                                                                                                      , case ( bam_v1_6_bam_alignment_optionalfields_smalliopt
                                                                                                                                 currentbamalignmentoptionalfield
                                                                                                                             ) of
                                                                                                                          Nothing ->
                                                                                                                            Empty
                                                                                                                          Just _  ->
                                                                                                                            SmallIOPT
                                                                                                                      , case ( bam_v1_6_bam_alignment_optionalfields_bigiopt
                                                                                                                                 currentbamalignmentoptionalfield
                                                                                                                             ) of
                                                                                                                          Nothing ->
                                                                                                                            Empty
                                                                                                                          Just _  ->
                                                                                                                            BigIOPT
                                                                                                                      , case ( bam_v1_6_bam_alignment_optionalfields_smallsopt
                                                                                                                                 currentbamalignmentoptionalfield
                                                                                                                             ) of
                                                                                                                          Nothing ->
                                                                                                                            Empty
                                                                                                                          Just _  ->
                                                                                                                            SmallSOPT
                                                                                                                      , case ( bam_v1_6_bam_alignment_optionalfields_bigsopt
                                                                                                                                 currentbamalignmentoptionalfield
                                                                                                                             ) of
                                                                                                                          Nothing ->
                                                                                                                            Empty
                                                                                                                          Just _  ->
                                                                                                                            BigSOPT
                                                                                                                      , case ( bam_v1_6_bam_alignment_optionalfields_fopt
                                                                                                                                 currentbamalignmentoptionalfield
                                                                                                                             ) of
                                                                                                                          Nothing ->
                                                                                                                            Empty
                                                                                                                          Just _  ->
                                                                                                                            FOPT
                                                                                                                      , case ( bam_v1_6_bam_alignment_optionalfields_zopt
                                                                                                                                 currentbamalignmentoptionalfield
                                                                                                                             ) of
                                                                                                                          Nothing ->
                                                                                                                            Empty
                                                                                                                          Just _  ->
                                                                                                                            ZOPT
                                                                                                                      , case ( bam_v1_6_bam_alignment_optionalfields_hopt
                                                                                                                                 currentbamalignmentoptionalfield
                                                                                                                             ) of
                                                                                                                          Nothing ->
                                                                                                                            Empty
                                                                                                                          Just _  ->
                                                                                                                            HOPT
                                                                                                                      , case ( bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                                 currentbamalignmentoptionalfield
                                                                                                                             ) of
                                                                                                                          Nothing ->
                                                                                                                            Empty
                                                                                                                          Just _  ->
                                                                                                                            BOPT
                                                                                                                      ] :: [BAM_V1_6_BAM_Alignment_OptionalFields_SumType]
                                                                                                if | AOPT `DL.elem` optionalfieldsl
                                                                                                   -> ( bam_v1_6_bam_alignment_optionalfields_aopt_tag $
                                                                                                          fromJust                                     $
                                                                                                            bam_v1_6_bam_alignment_optionalfields_aopt
                                                                                                              currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.singleton 0x41 --'A'
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.singleton                                       $
                                                                                                          bam_v1_6_bam_alignment_optionalfields_aopt_value $
                                                                                                            fromJust                                       $
                                                                                                              bam_v1_6_bam_alignment_optionalfields_aopt
                                                                                                                currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                   | SmallCOPT `DL.elem` optionalfieldsl
                                                                                                   -> ( bam_v1_6_bam_alignment_optionalfields_smallcopt_tag $
                                                                                                          fromJust                                          $
                                                                                                            bam_v1_6_bam_alignment_optionalfields_smallcopt
                                                                                                              currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.singleton 0x63 --'c'
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( int8ToByteString                                        $
                                                                                                          bam_v1_6_bam_alignment_optionalfields_smallcopt_value $
                                                                                                            fromJust                                            $
                                                                                                              bam_v1_6_bam_alignment_optionalfields_smallcopt
                                                                                                                currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                   | BigCOPT `DL.elem` optionalfieldsl
                                                                                                   -> ( bam_v1_6_bam_alignment_optionalfields_bigcopt_tag   $
                                                                                                          fromJust                                          $
                                                                                                            bam_v1_6_bam_alignment_optionalfields_bigcopt
                                                                                                              currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.singleton 0x43 --'C'
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.singleton                                          $
                                                                                                          bam_v1_6_bam_alignment_optionalfields_bigcopt_value $
                                                                                                            fromJust                                          $
                                                                                                              bam_v1_6_bam_alignment_optionalfields_bigcopt
                                                                                                                currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                   | SmallIOPT `DL.elem` optionalfieldsl
                                                                                                   -> ( bam_v1_6_bam_alignment_optionalfields_smalliopt_tag $
                                                                                                          fromJust                                          $
                                                                                                            bam_v1_6_bam_alignment_optionalfields_smalliopt
                                                                                                              currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.singleton 0x69 --'i'
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( int32ToByteStringLE                                     $
                                                                                                          bam_v1_6_bam_alignment_optionalfields_smalliopt_value $
                                                                                                            fromJust                                            $
                                                                                                              bam_v1_6_bam_alignment_optionalfields_smalliopt
                                                                                                                currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                   | BigIOPT `DL.elem` optionalfieldsl
                                                                                                   -> ( bam_v1_6_bam_alignment_optionalfields_bigiopt_tag $
                                                                                                          fromJust                                        $
                                                                                                            bam_v1_6_bam_alignment_optionalfields_bigiopt
                                                                                                              currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.singleton 0x49 --'I'
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( word32ToByteStringLE                                  $
                                                                                                          bam_v1_6_bam_alignment_optionalfields_bigiopt_value $
                                                                                                            fromJust                                          $
                                                                                                              bam_v1_6_bam_alignment_optionalfields_bigiopt
                                                                                                                currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                   | SmallSOPT `DL.elem` optionalfieldsl
                                                                                                   -> ( bam_v1_6_bam_alignment_optionalfields_smallsopt_tag $
                                                                                                          fromJust                                          $
                                                                                                            bam_v1_6_bam_alignment_optionalfields_smallsopt
                                                                                                              currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.singleton 0x73 --'s'
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( int16ToByteStringLE                                     $
                                                                                                          bam_v1_6_bam_alignment_optionalfields_smallsopt_value $
                                                                                                            fromJust                                            $
                                                                                                              bam_v1_6_bam_alignment_optionalfields_smallsopt
                                                                                                                currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                   | BigSOPT `DL.elem` optionalfieldsl
                                                                                                   -> ( bam_v1_6_bam_alignment_optionalfields_bigsopt_tag $
                                                                                                          fromJust                                        $
                                                                                                            bam_v1_6_bam_alignment_optionalfields_bigsopt
                                                                                                              currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.singleton 0x53 --'S'
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( word16ToByteStringLE                                  $
                                                                                                          bam_v1_6_bam_alignment_optionalfields_bigsopt_value $
                                                                                                            fromJust                                          $
                                                                                                              bam_v1_6_bam_alignment_optionalfields_bigsopt
                                                                                                                currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                   | FOPT `DL.elem` optionalfieldsl
                                                                                                   -> ( bam_v1_6_bam_alignment_optionalfields_fopt_tag $
                                                                                                          fromJust                                     $
                                                                                                            bam_v1_6_bam_alignment_optionalfields_fopt
                                                                                                              currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.singleton 0x66 --'f'
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( word32ToByteStringLE                               $
                                                                                                          bam_v1_6_bam_alignment_optionalfields_fopt_value $
                                                                                                            fromJust                                       $
                                                                                                              bam_v1_6_bam_alignment_optionalfields_fopt
                                                                                                                currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                   | ZOPT `DL.elem` optionalfieldsl
                                                                                                   -> ( bam_v1_6_bam_alignment_optionalfields_zopt_tag $
                                                                                                          fromJust                                     $
                                                                                                            bam_v1_6_bam_alignment_optionalfields_zopt
                                                                                                              currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.singleton 0x5A --'Z'
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.pack                                              $
                                                                                                          toList                                             $
                                                                                                            bam_v1_6_bam_alignment_optionalfields_zopt_value $
                                                                                                              fromJust                                       $
                                                                                                                bam_v1_6_bam_alignment_optionalfields_zopt
                                                                                                                  currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.singleton 0x00
                                                                                                      )
                                                                                                   | HOPT `DL.elem` optionalfieldsl
                                                                                                   -> ( bam_v1_6_bam_alignment_optionalfields_hopt_tag $
                                                                                                          fromJust                                     $
                                                                                                            bam_v1_6_bam_alignment_optionalfields_hopt
                                                                                                              currentbamalignmentoptionalfield
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.singleton 0x48 --'H'
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.concat $
                                                                                                          toList
                                                                                                            ( fmap (\currenthexvalue ->
                                                                                                                      case ( DBB16.isBase16 ( DBB16T.extractBase16 $
                                                                                                                                                DBB16.encodeBase16' currenthexvalue
                                                                                                                                            )
                                                                                                                           ) of
                                                                                                                        False ->
                                                                                                                          error $
                                                                                                                            show BAM_V1_6_Write_Error_HOPT_Incorrect_Format
                                                                                                                        True  ->
                                                                                                                          DBB16T.extractBase16 $
                                                                                                                            DBB16.encodeBase16' currenthexvalue
                                                                                                                   )
                                                                                                              ( bam_v1_6_bam_alignment_optionalfields_hopt_value $
                                                                                                                  fromJust                                       $
                                                                                                                    bam_v1_6_bam_alignment_optionalfields_hopt
                                                                                                                      currentbamalignmentoptionalfield
                                                                                                              )
                                                                                                            )
                                                                                                      )
                                                                                                      `DB.append`
                                                                                                      ( DB.singleton 0x00
                                                                                                      )
                                                                                                   | BOPT `DL.elem` optionalfieldsl
                                                                                                   -> do
                                                                                                     let optionalfieldsboptl = [ case ( bam_v1_6_bam_alignment_optionalfields_bopt_int8 $
                                                                                                                                          fromJust                                      $
                                                                                                                                            bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                                              currentbamalignmentoptionalfield
                                                                                                                                      ) of
                                                                                                                                   Nothing ->
                                                                                                                                     BOPTEmpty
                                                                                                                                   Just _  ->
                                                                                                                                     BOPTInt8
                                                                                                                               , case ( bam_v1_6_bam_alignment_optionalfields_bopt_word8 $
                                                                                                                                          fromJust                                       $
                                                                                                                                            bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                                              currentbamalignmentoptionalfield
                                                                                                                                      ) of
                                                                                                                                   Nothing ->
                                                                                                                                     BOPTEmpty
                                                                                                                                   Just _  ->
                                                                                                                                     BOPTWord8
                                                                                                                               , case ( bam_v1_6_bam_alignment_optionalfields_bopt_int16 $
                                                                                                                                          fromJust                                       $
                                                                                                                                            bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                                              currentbamalignmentoptionalfield
                                                                                                                                      ) of
                                                                                                                                   Nothing ->
                                                                                                                                     BOPTEmpty
                                                                                                                                   Just _  ->
                                                                                                                                     BOPTInt16
                                                                                                                               , case ( bam_v1_6_bam_alignment_optionalfields_bopt_word16 $
                                                                                                                                          fromJust                                        $
                                                                                                                                            bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                                              currentbamalignmentoptionalfield
                                                                                                                                      ) of
                                                                                                                                   Nothing ->
                                                                                                                                     BOPTEmpty
                                                                                                                                   Just _  ->
                                                                                                                                     BOPTWord16
                                                                                                                               , case ( bam_v1_6_bam_alignment_optionalfields_bopt_int32 $
                                                                                                                                          fromJust                                       $
                                                                                                                                            bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                                              currentbamalignmentoptionalfield
                                                                                                                                      ) of
                                                                                                                                   Nothing ->
                                                                                                                                     BOPTEmpty
                                                                                                                                   Just _  ->
                                                                                                                                     BOPTInt32
                                                                                                                               , case ( bam_v1_6_bam_alignment_optionalfields_bopt_word32 $
                                                                                                                                          fromJust                                        $
                                                                                                                                            bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                                              currentbamalignmentoptionalfield
                                                                                                                                      ) of
                                                                                                                                   Nothing ->
                                                                                                                                     BOPTEmpty
                                                                                                                                   Just _  ->
                                                                                                                                     BOPTWord32
                                                                                                                               , case ( bam_v1_6_bam_alignment_optionalfields_bopt_float $
                                                                                                                                          fromJust                                       $
                                                                                                                                            bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                                              currentbamalignmentoptionalfield
                                                                                                                                      ) of
                                                                                                                                   Nothing ->
                                                                                                                                     BOPTEmpty
                                                                                                                                   Just _  ->
                                                                                                                                     BOPTFloat
                                                                                                                               ] :: [BAM_V1_6_BAM_Alignment_OptionalFields_BOPT_SumType]
                                                                                                     if | BOPTInt8 `DL.elem` optionalfieldsboptl
                                                                                                        -> ( bam_v1_6_bam_alignment_optionalfields_bopt_int8_tag $
                                                                                                               fromJust                                          $
                                                                                                                 bam_v1_6_bam_alignment_optionalfields_bopt_int8 $
                                                                                                                   fromJust                                      $
                                                                                                                     bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                       currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( DB.singleton 0x42 --'B'
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( bam_v1_6_bam_alignment_optionalfields_bopt_int8_type $
                                                                                                               fromJust                                           $
                                                                                                                 bam_v1_6_bam_alignment_optionalfields_bopt_int8  $
                                                                                                                   fromJust                                       $
                                                                                                                     bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                       currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( word32ToByteStringLE                                    $
                                                                                                               bam_v1_6_bam_alignment_optionalfields_bopt_int8_count $
                                                                                                                 fromJust                                            $
                                                                                                                   bam_v1_6_bam_alignment_optionalfields_bopt_int8   $
                                                                                                                     fromJust                                        $
                                                                                                                       bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                         currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( DB.concat $
                                                                                                               toList
                                                                                                                 ( fmap (\currentint8value ->
                                                                                                                           int8ToByteString currentint8value
                                                                                                                        )
                                                                                                                   ( bam_v1_6_bam_alignment_optionalfields_bopt_int8_value $
                                                                                                                       fromJust                                            $
                                                                                                                         bam_v1_6_bam_alignment_optionalfields_bopt_int8   $
                                                                                                                           fromJust                                        $
                                                                                                                             bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                                currentbamalignmentoptionalfield 
                                                                                                                   )
                                                                                                                 )
                                                                                                           )
                                                                                                        | BOPTWord8 `DL.elem` optionalfieldsboptl
                                                                                                        -> ( bam_v1_6_bam_alignment_optionalfields_bopt_word8_tag $
                                                                                                               fromJust                                           $
                                                                                                                 bam_v1_6_bam_alignment_optionalfields_bopt_word8 $
                                                                                                                   fromJust                                       $
                                                                                                                     bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                       currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( DB.singleton 0x42 --'B'
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( bam_v1_6_bam_alignment_optionalfields_bopt_word8_type $
                                                                                                               fromJust                                            $
                                                                                                                 bam_v1_6_bam_alignment_optionalfields_bopt_word8  $
                                                                                                                   fromJust                                        $
                                                                                                                     bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                       currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( word32ToByteStringLE                                     $
                                                                                                               bam_v1_6_bam_alignment_optionalfields_bopt_word8_count $
                                                                                                                 fromJust                                             $
                                                                                                                   bam_v1_6_bam_alignment_optionalfields_bopt_word8   $
                                                                                                                     fromJust                                         $
                                                                                                                       bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                         currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( DB.concat $
                                                                                                               toList
                                                                                                                 ( fmap (\currentword8value ->
                                                                                                                           DB.singleton currentword8value
                                                                                                                        )
                                                                                                                   ( bam_v1_6_bam_alignment_optionalfields_bopt_word8_value $
                                                                                                                       fromJust                                             $
                                                                                                                         bam_v1_6_bam_alignment_optionalfields_bopt_word8   $
                                                                                                                           fromJust                                         $
                                                                                                                             bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                               currentbamalignmentoptionalfield
                                                                                                                   )
                                                                                                                 )
                                                                                                           )
                                                                                                        | BOPTInt16 `DL.elem` optionalfieldsboptl
                                                                                                        -> ( bam_v1_6_bam_alignment_optionalfields_bopt_int16_tag $
                                                                                                               fromJust                                           $
                                                                                                                 bam_v1_6_bam_alignment_optionalfields_bopt_int16 $
                                                                                                                   fromJust                                       $
                                                                                                                     bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                       currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( DB.singleton 0x42 --'B'
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( bam_v1_6_bam_alignment_optionalfields_bopt_int16_type $
                                                                                                               fromJust                                            $
                                                                                                                 bam_v1_6_bam_alignment_optionalfields_bopt_int16  $
                                                                                                                   fromJust                                        $
                                                                                                                     bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                       currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( word32ToByteStringLE                                     $
                                                                                                               bam_v1_6_bam_alignment_optionalfields_bopt_int16_count $
                                                                                                                 fromJust                                             $
                                                                                                                   bam_v1_6_bam_alignment_optionalfields_bopt_int16   $
                                                                                                                     fromJust                                         $
                                                                                                                       bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                         currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( DB.concat $
                                                                                                               toList
                                                                                                                 ( fmap (\currentint16value ->
                                                                                                                          int16ToByteStringLE currentint16value
                                                                                                                       )
                                                                                                                   ( bam_v1_6_bam_alignment_optionalfields_bopt_int16_value $
                                                                                                                       fromJust                                             $
                                                                                                                         bam_v1_6_bam_alignment_optionalfields_bopt_int16   $
                                                                                                                           fromJust                                         $
                                                                                                                             bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                               currentbamalignmentoptionalfield
                                                                                                                   )
                                                                                                                 )
                                                                                                           )
                                                                                                        | BOPTWord16 `DL.elem` optionalfieldsboptl
                                                                                                        -> ( bam_v1_6_bam_alignment_optionalfields_bopt_word16_tag $
                                                                                                               fromJust                                            $
                                                                                                                 bam_v1_6_bam_alignment_optionalfields_bopt_word16 $
                                                                                                                   fromJust                                        $
                                                                                                                     bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                       currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( DB.singleton 0x42 --'B'
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( bam_v1_6_bam_alignment_optionalfields_bopt_word16_type $
                                                                                                               fromJust                                             $
                                                                                                                 bam_v1_6_bam_alignment_optionalfields_bopt_word16  $
                                                                                                                   fromJust                                         $
                                                                                                                     bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                       currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( word32ToByteStringLE                                      $
                                                                                                               bam_v1_6_bam_alignment_optionalfields_bopt_word16_count $
                                                                                                                 fromJust                                              $
                                                                                                                   bam_v1_6_bam_alignment_optionalfields_bopt_word16   $
                                                                                                                     fromJust                                          $
                                                                                                                       bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                         currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( DB.concat $
                                                                                                               toList
                                                                                                                 ( fmap (\currentword16value ->
                                                                                                                           word16ToByteStringLE currentword16value
                                                                                                                        )
                                                                                                                   ( bam_v1_6_bam_alignment_optionalfields_bopt_word16_value $
                                                                                                                       fromJust                                              $
                                                                                                                         bam_v1_6_bam_alignment_optionalfields_bopt_word16   $
                                                                                                                           fromJust                                          $
                                                                                                                             bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                                currentbamalignmentoptionalfield
                                                                                                                   )
                                                                                                                 )
                                                                                                           )
                                                                                                        | BOPTInt32 `DL.elem` optionalfieldsboptl
                                                                                                        -> ( bam_v1_6_bam_alignment_optionalfields_bopt_int32_tag $
                                                                                                               fromJust                                           $
                                                                                                                 bam_v1_6_bam_alignment_optionalfields_bopt_int32 $
                                                                                                                   fromJust                                       $
                                                                                                                     bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                       currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( DB.singleton 0x42 --'B'
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( bam_v1_6_bam_alignment_optionalfields_bopt_int32_type $
                                                                                                               fromJust                                            $
                                                                                                                 bam_v1_6_bam_alignment_optionalfields_bopt_int32  $
                                                                                                                   fromJust                                        $
                                                                                                                     bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                       currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( word32ToByteStringLE                                     $
                                                                                                               bam_v1_6_bam_alignment_optionalfields_bopt_int32_count $
                                                                                                                 fromJust                                             $
                                                                                                                   bam_v1_6_bam_alignment_optionalfields_bopt_int32   $
                                                                                                                     fromJust                                         $
                                                                                                                       bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                         currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( DB.concat $
                                                                                                               toList
                                                                                                                 ( fmap (\currentint32value ->
                                                                                                                          int32ToByteStringLE currentint32value
                                                                                                                       )
                                                                                                                   ( bam_v1_6_bam_alignment_optionalfields_bopt_int32_value $
                                                                                                                       fromJust                                             $
                                                                                                                         bam_v1_6_bam_alignment_optionalfields_bopt_int32   $
                                                                                                                           fromJust                                         $
                                                                                                                             bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                               currentbamalignmentoptionalfield
                                                                                                                   )
                                                                                                                 )
                                                                                                           )
                                                                                                        | BOPTWord32 `DL.elem` optionalfieldsboptl
                                                                                                        -> ( bam_v1_6_bam_alignment_optionalfields_bopt_word32_tag $
                                                                                                               fromJust                                            $
                                                                                                                 bam_v1_6_bam_alignment_optionalfields_bopt_word32 $
                                                                                                                   fromJust                                        $
                                                                                                                     bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                       currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( DB.singleton 0x42 --'B'
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( bam_v1_6_bam_alignment_optionalfields_bopt_word32_type $
                                                                                                               fromJust                                             $
                                                                                                                 bam_v1_6_bam_alignment_optionalfields_bopt_word32  $
                                                                                                                   fromJust                                         $
                                                                                                                     bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                       currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( word32ToByteStringLE                                      $
                                                                                                               bam_v1_6_bam_alignment_optionalfields_bopt_word32_count $
                                                                                                                 fromJust                                              $
                                                                                                                   bam_v1_6_bam_alignment_optionalfields_bopt_word32   $
                                                                                                                     fromJust                                          $
                                                                                                                       bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                         currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( DB.concat $
                                                                                                               toList
                                                                                                                 ( fmap (\currentword32value ->
                                                                                                                          word32ToByteStringLE currentword32value
                                                                                                                       )
                                                                                                                   ( bam_v1_6_bam_alignment_optionalfields_bopt_word32_value $
                                                                                                                       fromJust                                              $
                                                                                                                         bam_v1_6_bam_alignment_optionalfields_bopt_word32   $
                                                                                                                           fromJust                                          $
                                                                                                                             bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                               currentbamalignmentoptionalfield
                                                                                                                   )
                                                                                                                 )
                                                                                                           )
                                                                                                        | BOPTFloat `DL.elem` optionalfieldsboptl
                                                                                                        -> ( bam_v1_6_bam_alignment_optionalfields_bopt_float_tag $
                                                                                                               fromJust                                           $
                                                                                                                 bam_v1_6_bam_alignment_optionalfields_bopt_float $
                                                                                                                   fromJust                                       $
                                                                                                                     bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                       currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( DB.singleton 0x42 --'B'
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( bam_v1_6_bam_alignment_optionalfields_bopt_float_type $
                                                                                                               fromJust                                            $
                                                                                                                 bam_v1_6_bam_alignment_optionalfields_bopt_float  $
                                                                                                                   fromJust                                        $
                                                                                                                     bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                       currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( word32ToByteStringLE                                     $
                                                                                                               bam_v1_6_bam_alignment_optionalfields_bopt_float_count $
                                                                                                                 fromJust                                             $
                                                                                                                   bam_v1_6_bam_alignment_optionalfields_bopt_float   $
                                                                                                                     fromJust                                         $
                                                                                                                       bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                         currentbamalignmentoptionalfield
                                                                                                           )
                                                                                                           `DB.append`
                                                                                                           ( DB.concat $
                                                                                                               toList
                                                                                                                 ( fmap (\currentfloatvalue ->
                                                                                                                          floatToByteStringLE currentfloatvalue
                                                                                                                       )
                                                                                                                   ( bam_v1_6_bam_alignment_optionalfields_bopt_float_value $
                                                                                                                       fromJust                                             $
                                                                                                                         bam_v1_6_bam_alignment_optionalfields_bopt_float   $
                                                                                                                           fromJust                                         $
                                                                                                                             bam_v1_6_bam_alignment_optionalfields_bopt
                                                                                                                               currentbamalignmentoptionalfield
                                                                                                                   )
                                                                                                                 )
                                                                                                           )
                                                                                                        | BOPTEmpty `DL.elem` optionalfieldsboptl
                                                                                                        -> DB.empty
                                                                                                        | otherwise
                                                                                                        -> DB.empty
                                                                                                   | Empty `DL.elem` optionalfieldsl
                                                                                                   -> DB.empty
                                                                                                   | otherwise
                                                                                                   -> DB.empty 
                                                                                             )
                                                                                        ( bam_v1_6_bam_alignment_optionalfields currentbamalignmentrecord
                                                                                        )
                                                               preoptionalfields `DB.append` optionalfields
                                                            )
                                                       ( bam_v1_6_bam_bamalignments_alignments bam_v1_6_bam_bamalignments'
                                                       )
                let bam_v1_6_bam_bamalignments''' = DB.toStrict        $
                                                      CCZlibR.compress $
                                                        DB.fromStrict  $
                                                          DB.concat    $
                                                            toList bam_v1_6_bam_bamalignments''
                -- total Block SIZE minus 1.
                _ <-
                  DB.hPutStr h                    $
                             word16ToByteStringLE $
                             intToWord16LE ((DB.length bam_v1_6_bam_bamalignments''' + 26) - 1)
                -- Write the BAM alignment(s).
                _ <-
                  DB.hPutStr h
                             bam_v1_6_bam_bamalignments'''
                -- CRC-32.
                _ <-
                  DB.hPutStr h           $
                    word32ToByteStringLE $
                      DDCRC32.crc32      $
                        DB.concat        $
                          toList bam_v1_6_bam_bamalignments''
                -- Input SIZE (length of uncompressed data).
                DB.hPutStr h                    $
                           word32ToByteStringLE $
                             intToWord32LE      $
                               DB.length        $
                                 DB.concat      $
                                   toList bam_v1_6_bam_bamalignments''
          Just bam_v1_6_bam_bamheader' -> do
            -- Write to file.
            -- GZip header.
            _ <-
              DB.hPutStr h                                        $
                         DB.singleton                             $
                           bam_v1_6_gzip_header_gzip_identifier_1 $
                             bam_v1_6_bgzfblock_gzip_header currentbgzfblock
            _ <-
              DB.hPutStr h                                        $
                         DB.singleton                             $
                           bam_v1_6_gzip_header_gzip_identifier_2 $
                             bam_v1_6_bgzfblock_gzip_header currentbgzfblock
            _ <-
              DB.hPutStr h                                         $
                         DB.singleton                              $
                           bam_v1_6_gzip_header_compression_method $
                             bam_v1_6_bgzfblock_gzip_header currentbgzfblock
            _ <-
              DB.hPutStr h                                   $
                         DB.singleton                        $
                           bam_v1_6_gzip_header_header_flags $
                             bam_v1_6_bgzfblock_gzip_header currentbgzfblock
            _ <-
              DB.hPutStr h                                        $
                         word32ToByteStringLE                     $
                           bam_v1_6_gzip_header_modification_time $
                             bam_v1_6_bgzfblock_gzip_header currentbgzfblock
            _ <-
              DB.hPutStr h                                  $
                         DB.singleton                       $
                           bam_v1_6_gzip_header_extra_flags $
                             bam_v1_6_bgzfblock_gzip_header currentbgzfblock
            _ <-
              DB.hPutStr h                                       $
                         DB.singleton                            $
                           bam_v1_6_gzip_header_operating_system $
                            bam_v1_6_bgzfblock_gzip_header currentbgzfblock   
            _ <-
              DB.hPutStr h                                   $
                         word16ToByteStringLE                $
                           bam_v1_6_gzip_header_extra_length $
                             bam_v1_6_bgzfblock_gzip_header currentbgzfblock
            -- Subfield Identifier 1.
            _ <-
              DB.hPutStr h            $
                         DB.singleton $
                           bam_v1_6_bgzfblock_subfield_identifier_one currentbgzfblock
            -- Subfield Identifier 2.
            _ <-
              DB.hPutStr h            $
                         DB.singleton $
                           bam_v1_6_bgzfblock_subfield_identifier_two currentbgzfblock
            -- Subfield LENgth.
            _ <-
              DB.hPutStr h                    $
                         word16ToByteStringLE $
                           bam_v1_6_bgzfblock_subfield_length currentbgzfblock
            let bam_v1_6_bam_bamheader'' = bam_v1_6_bam_bamheader_magic bam_v1_6_bam_bamheader'
                                           `DB.append`
                                           ( word32ToByteStringLE $
                                               bam_v1_6_bam_bamheader_l_text bam_v1_6_bam_bamheader'
                                           )
                                           `DB.append`
                                           bam_v1_6_bam_bamheader_text bam_v1_6_bam_bamheader'
                                           `DB.append`
                                           ( word32ToByteStringLE $
                                               bam_v1_6_bam_bamheader_n_ref bam_v1_6_bam_bamheader'
                                           )
            let bam_v1_6_bam_bamheader_reference_information' = DB.concat $
                                                                  toList  $
                                                                    fmap (\currentreferenceinformation ->
                                                                             ( word32ToByteStringLE $
                                                                                 bam_v1_6_bam_reference_information_l_name currentreferenceinformation
                                                                             )
                                                                             `DB.append`
                                                                             bam_v1_6_bam_reference_information_name currentreferenceinformation
                                                                             `DB.append`
                                                                             ( word32ToByteStringLE $
                                                                                 bam_v1_6_bam_reference_information_l_ref currentreferenceinformation
                                                                             )
                                                                         )
                                                                    (bam_v1_6_bam_bamheader_reference_information bam_v1_6_bam_bamheader')
            let bam_v1_6_bam_bamheader''' = DB.toStrict        $
                                              CCZlibR.compress $
                                                DB.fromStrict  $
                                                  bam_v1_6_bam_bamheader''
                                                  `DB.append`
                                                  bam_v1_6_bam_bamheader_reference_information'
            -- total Block SIZE minus 1.
            _ <-
              DB.hPutStr h                    $
                         word16ToByteStringLE $
                         intToWord16LE ((DB.length bam_v1_6_bam_bamheader''' + 26) - 1)
            -- Write the BAM header.
            _ <-
              DB.hPutStr h
                         bam_v1_6_bam_bamheader'''
            -- CRC-32.
            _ <-
              DB.hPutStr h           $
                word32ToByteStringLE $
                  DDCRC32.crc32      $
                    bam_v1_6_bam_bamheader''
                    `DB.append`
                    bam_v1_6_bam_bamheader_reference_information'
            -- Input SIZE (length of uncompressed data).
            DB.hPutStr h           $
              word32ToByteStringLE $
                intToWord32LE      $
                  DB.length        $
                    bam_v1_6_bam_bamheader''
                    `DB.append`
                    bam_v1_6_bam_bamheader_reference_information'

-- | Write a @"BAM_V1_6"@ to a file.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
writeBAM_V1_6 :: FilePath -- ^ Output path to BAM file.
              -> BAM_V1_6
              -> IO ()
writeBAM_V1_6 fp bamv16 = do
  h <- openBinaryFile fp
                WriteMode
  deconstructBAM_V1_6 h
                      bamv16
  hFlush h
  hClose h
