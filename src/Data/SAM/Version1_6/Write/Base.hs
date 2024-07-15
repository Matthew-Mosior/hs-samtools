{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.SAM.Version1_6.Write.Base
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.SAM.Version1_6.Write.Base ( -- * Writing
                                        writeSAM_V1_6
                                      ) where

import Data.SAM.Version1_6.Base
import Data.SAM.Version1_6.Header.HD
import Data.SAM.Version1_6.Header.SQ
import Data.SAM.Version1_6.Header.RG
import Data.SAM.Version1_6.Header.PG
import Data.SAM.Version1_6.Header.CO
import Data.SAM.Version1_6.Alignment.Base
import Data.SAM.Version1_6.Alignment.OptionalFields.Base
import Data.SAM.Version1_6.Alignment.OptionalFields.AOPT
import Data.SAM.Version1_6.Alignment.OptionalFields.IOPT
import Data.SAM.Version1_6.Alignment.OptionalFields.FOPT
import Data.SAM.Version1_6.Alignment.OptionalFields.ZOPT
import Data.SAM.Version1_6.Alignment.OptionalFields.HOPT
import Data.SAM.Version1_6.Alignment.OptionalFields.BOPT

import Control.Monad                       (forM)
import Data.ByteString            as DB    (pack,singleton)
import Data.ByteString.Lazy       as DBL   (filter)
import Data.ByteString.Lazy.Char8 as DBLC8 (fromStrict,unpack)
import Data.Foldable                       (toList)
import Data.List                           (intercalate)
import Data.Word
import Data.ByteString.Builder             (toLazyByteString,word16LE,word32LE)
import System.IO                           (hFlush,hClose,hPutStr,IOMode(..),openFile,Handle)


-- | Deconstruct a @"SAM_V1_6"@ to a `String`.
deconstructSAM_V1_6 :: SAM_V1_6
                    -> String
deconstructSAM_V1_6 samv16 =
  ( intercalate "\n" $
      Prelude.filter (not . Prelude.null)
                     [ sam_v1_6_file_level_metadata_tos
                     , sam_v1_6_reference_sequence_dictionary_tos
                     , sam_v1_6_read_group_tos
                     , sam_v1_6_program_tos
                     , sam_v1_6_one_line_comment_tos
                     , sam_v1_6_alignment_tos
                     ]
  )
  ++ "\n"
  where
    sam_v1_6_file_level_metadata_format_version_tos x = "VN:" ++
                                                        ( unpack     $
                                                          fromStrict $
                                                          sam_v1_6_file_level_metadata_format_version_value $
                                                          sam_v1_6_file_level_metadata_format_version x
                                                        )
    sam_v1_6_file_level_metadata_sorting_order_tos x = case (sam_v1_6_file_level_metadata_sorting_order x) of
                                                         Nothing  ->
                                                           ""
                                                         Just rgf ->
                                                           "SO:" ++
                                                           ( unpack     $
                                                             fromStrict $
                                                             sam_v1_6_file_level_metadata_sorting_order_value rgf
                                                           )
    sam_v1_6_file_level_metadata_alignment_grouping_tos x = case (sam_v1_6_file_level_metadata_alignment_grouping x) of
                                                              Nothing  ->
                                                                ""
                                                              Just rgf ->
                                                                "GO:" ++
                                                                ( unpack     $
                                                                  fromStrict $
                                                                  sam_v1_6_file_level_metadata_alignment_grouping_value rgf
                                                                )
    sam_v1_6_file_level_metadata_subsorting_order_tos x = case (sam_v1_6_file_level_metadata_subsorting_order x) of
                                                            Nothing  ->
                                                              ""
                                                            Just rgf ->
                                                              "SS:" ++
                                                              ( unpack     $
                                                                fromStrict $
                                                                sam_v1_6_file_level_metadata_subsorting_order_value rgf
                                                              )
    sam_v1_6_file_level_metadata_tos = case (sam_v1_6_file_level_metadata samv16) of
                                         Nothing  ->
                                           ""
                                         Just hdf ->
                                           intercalate "\t" $
                                             Prelude.filter (not . Prelude.null)
                                                            [ "@HD"
                                                            , sam_v1_6_file_level_metadata_format_version_tos hdf
                                                            , sam_v1_6_file_level_metadata_sorting_order_tos hdf
                                                            , sam_v1_6_file_level_metadata_alignment_grouping_tos hdf
                                                            , sam_v1_6_file_level_metadata_subsorting_order_tos hdf 
                                                            ]
    sam_v1_6_reference_sequence_dictionary_reference_sequence_name_tos x = "SN:" ++
                                                                           ( unpack                                                               $
                                                                             fromStrict                                                           $
                                                                             sam_v1_6_reference_sequence_dictionary_reference_sequence_name_value $
                                                                             sam_v1_6_reference_sequence_dictionary_reference_sequence_name x
                                                                           )
    sam_v1_6_reference_sequence_dictionary_reference_sequence_length_tos x = "LN:" ++
                                                                             ( unpack                                                                 $
                                                                               fromStrict                                                             $
                                                                               sam_v1_6_reference_sequence_dictionary_reference_sequence_length_value $
                                                                               sam_v1_6_reference_sequence_dictionary_reference_sequence_length x
                                                                             )
    sam_v1_6_reference_sequence_dictionary_alternative_locus_tos x = case (sam_v1_6_reference_sequence_dictionary_alternative_locus x) of
                                                                       Nothing  ->
                                                                         ""
                                                                       Just sqf ->
                                                                         "AH:" ++
                                                                         ( unpack     $
                                                                           fromStrict $
                                                                           sam_v1_6_reference_sequence_dictionary_alternative_locus_value sqf
                                                                         )
    sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_tos x = case (sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names x) of
                                                                                          Nothing  ->
                                                                                            "" 
                                                                                          Just sqf ->
                                                                                            "AN:" ++
                                                                                            ( unpack     $
                                                                                              fromStrict $
                                                                                              sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value sqf
                                                                                            )
    sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_tos x = case (sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier x) of
                                                                                Nothing  ->
                                                                                  ""
                                                                                Just sqf ->
                                                                                  "AS:" ++
                                                                                  ( unpack     $
                                                                                    fromStrict $
                                                                                    sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value sqf
                                                                                  )
    sam_v1_6_reference_sequence_dictionary_description_tos x = case (sam_v1_6_reference_sequence_dictionary_description x) of
                                                                 Nothing  ->
                                                                   ""
                                                                 Just sqf ->
                                                                   "DS:" ++
                                                                   ( unpack     $
                                                                     fromStrict $
                                                                     sam_v1_6_reference_sequence_dictionary_description_value sqf
                                                                   )
    sam_v1_6_reference_sequence_dictionary_md5_checksum_tos x = case (sam_v1_6_reference_sequence_dictionary_md5_checksum x) of
                                                                  Nothing  ->
                                                                    ""
                                                                  Just sqf ->
                                                                    "M5:" ++
                                                                    ( unpack     $
                                                                      fromStrict $
                                                                      sam_v1_6_reference_sequence_dictionary_md5_checksum_value sqf
                                                                    )
    sam_v1_6_reference_sequence_dictionary_species_tos x = case (sam_v1_6_reference_sequence_dictionary_species x) of
                                                             Nothing  ->
                                                               ""
                                                             Just sqf ->
                                                               "SP:" ++
                                                               ( unpack     $
                                                                 fromStrict $
                                                                 sam_v1_6_reference_sequence_dictionary_species_value sqf
                                                               )
    sam_v1_6_reference_sequence_dictionary_molecule_topology_tos x = case (sam_v1_6_reference_sequence_dictionary_molecule_topology x) of
                                                                       Nothing  ->
                                                                         ""
                                                                       Just sqf ->
                                                                         "TP:" ++
                                                                         ( unpack     $
                                                                           fromStrict $
                                                                           sam_v1_6_reference_sequence_dictionary_molecule_topology_value sqf
                                                                         )
    sam_v1_6_reference_sequence_dictionary_uri_tos x = case (sam_v1_6_reference_sequence_dictionary_uri x) of
                                                         Nothing  ->
                                                           ""
                                                         Just sqf ->
                                                           "UR:" ++
                                                           ( unpack     $
                                                             fromStrict $
                                                             sam_v1_6_reference_sequence_dictionary_uri_value sqf
                                                           )
    sam_v1_6_reference_sequence_dictionary_tos = case (sam_v1_6_reference_sequence_dictionary samv16) of
                                                   Nothing  ->
                                                     ""
                                                   Just sqf ->
                                                     intercalate "\n" $
                                                       map (\x -> intercalate "\t" $
                                                                    Prelude.filter (not . Prelude.null)
                                                                                   [ "@SQ"
                                                                                   , sam_v1_6_reference_sequence_dictionary_reference_sequence_name_tos x
                                                                                   , sam_v1_6_reference_sequence_dictionary_reference_sequence_length_tos x
                                                                                   , sam_v1_6_reference_sequence_dictionary_alternative_locus_tos x
                                                                                   , sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_tos x
                                                                                   , sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_tos x
                                                                                   , sam_v1_6_reference_sequence_dictionary_description_tos x
                                                                                   , sam_v1_6_reference_sequence_dictionary_md5_checksum_tos x
                                                                                   , sam_v1_6_reference_sequence_dictionary_species_tos x
                                                                                   , sam_v1_6_reference_sequence_dictionary_molecule_topology_tos x
                                                                                   , sam_v1_6_reference_sequence_dictionary_uri_tos x
                                                                                   ]
                                                           ) (toList sqf)
    sam_v1_6_read_group_identifer_tos x = "ID:" ++
                                          ( unpack                               $
                                            fromStrict                           $
                                            sam_v1_6_read_group_identifier_value $
                                            sam_v1_6_read_group_identifier x
                                          )
    sam_v1_6_read_group_barcode_sequence_tos x = case (sam_v1_6_read_group_barcode_sequence x) of
                                                   Nothing  ->
                                                     ""
                                                   Just rgf ->
                                                     "BC:" ++
                                                     ( unpack     $
                                                       fromStrict $
                                                       sam_v1_6_read_group_barcode_sequence_value rgf
                                                     )
    sam_v1_6_read_group_sequencing_center_tos x = case (sam_v1_6_read_group_sequencing_center x) of
                                                    Nothing  ->
                                                      ""
                                                    Just rgf ->
                                                      "CN:" ++
                                                      ( unpack     $
                                                        fromStrict $
                                                        sam_v1_6_read_group_sequencing_center_value rgf
                                                      )
    sam_v1_6_read_group_description_tos x = case (sam_v1_6_read_group_description x) of
                                              Nothing  ->
                                                ""
                                              Just rgf ->
                                                "DS:" ++
                                                ( unpack     $
                                                  fromStrict $
                                                  sam_v1_6_read_group_description_value rgf
                                                )
    sam_v1_6_read_group_run_date_tos x = case (sam_v1_6_read_group_run_date x) of
                                           Nothing  ->
                                             ""
                                           Just rgf ->
                                             "DT:" ++
                                             ( unpack     $
                                               fromStrict $
                                               sam_v1_6_read_group_run_date_value rgf
                                             )
    sam_v1_6_read_group_flow_order_tos x = case (sam_v1_6_read_group_flow_order x) of
                                             Nothing  ->
                                               ""
                                             Just rgf ->
                                               "FO:" ++
                                               ( unpack     $
                                                 fromStrict $
                                                 sam_v1_6_read_group_flow_order_value rgf
                                               )
    sam_v1_6_read_group_key_sequence_tos x = case (sam_v1_6_read_group_key_sequence x) of
                                               Nothing  ->
                                                 ""
                                               Just rgf ->
                                                 "KS:" ++
                                                 ( unpack     $
                                                   fromStrict $
                                                   sam_v1_6_read_group_key_sequence_value rgf
                                                 )
    sam_v1_6_read_group_library_tos x = case (sam_v1_6_read_group_library x) of
                                          Nothing  ->
                                            ""
                                          Just rgf ->
                                            "LB:" ++
                                            ( unpack     $
                                              fromStrict $
                                              sam_v1_6_read_group_library_value rgf
                                            )
    sam_v1_6_read_group_programs_tos x = case (sam_v1_6_read_group_programs x) of
                                           Nothing  ->
                                             ""
                                           Just rgf ->
                                             "PG:" ++
                                             ( unpack     $
                                               fromStrict $
                                               sam_v1_6_read_group_programs_value rgf
                                             )
    sam_v1_6_read_group_predicted_median_insert_size_tos x = case (sam_v1_6_read_group_predicted_median_insert_size x) of
                                                               Nothing  ->
                                                                 ""
                                                               Just rgf ->
                                                                 "PI:" ++
                                                                 ( unpack     $
                                                                   fromStrict $
                                                                   sam_v1_6_read_group_predicted_median_insert_size_value rgf
                                                                 )
    sam_v1_6_read_group_platform_tos x = case (sam_v1_6_read_group_platform x) of
                                           Nothing  ->
                                             ""
                                           Just rgf ->
                                             "PL:" ++
                                             ( unpack     $
                                               fromStrict $
                                               sam_v1_6_read_group_platform_value rgf
                                             )
    sam_v1_6_read_group_platform_model_tos x = case (sam_v1_6_read_group_platform_model x) of
                                                 Nothing  ->
                                                   ""
                                                 Just rgf ->
                                                   "PM:" ++
                                                   ( unpack     $
                                                     fromStrict $
                                                     sam_v1_6_read_group_platform_model_value rgf
                                                   )
    sam_v1_6_read_group_platform_unit_tos x = case (sam_v1_6_read_group_platform_unit x) of
                                                Nothing  ->
                                                  ""
                                                Just rgf ->
                                                  "PU:" ++
                                                  ( unpack     $
                                                    fromStrict $
                                                    sam_v1_6_read_group_platform_unit_value rgf
                                                  )
    sam_v1_6_read_group_sample_tos x = case (sam_v1_6_read_group_sample x) of
                                         Nothing  ->
                                           ""
                                         Just rgf ->
                                           "SM:" ++
                                           ( unpack     $
                                             fromStrict $
                                             sam_v1_6_read_group_sample_value rgf
                                           )
    sam_v1_6_read_group_tos = case (sam_v1_6_read_group samv16) of
                                Nothing  ->
                                  ""
                                Just rgf ->
                                  intercalate "\n" $
                                    map (\x -> intercalate "\t" $
                                                 Prelude.filter (not . Prelude.null)
                                                                [ "@RG"
                                                                , sam_v1_6_read_group_identifer_tos x
                                                                , sam_v1_6_read_group_barcode_sequence_tos x
                                                                , sam_v1_6_read_group_sequencing_center_tos x
                                                                , sam_v1_6_read_group_description_tos x
                                                                , sam_v1_6_read_group_run_date_tos x
                                                                , sam_v1_6_read_group_flow_order_tos x
                                                                , sam_v1_6_read_group_key_sequence_tos x
                                                                , sam_v1_6_read_group_library_tos x
                                                                , sam_v1_6_read_group_programs_tos x
                                                                , sam_v1_6_read_group_predicted_median_insert_size_tos x
                                                                , sam_v1_6_read_group_platform_tos x
                                                                , sam_v1_6_read_group_platform_model_tos x
                                                                , sam_v1_6_read_group_platform_unit_tos x
                                                                , sam_v1_6_read_group_sample_tos x
                                                                ]
                                        ) (toList rgf)
    sam_v1_6_program_record_identifier_tos x = "ID:" ++
                                               ( unpack                                   $
                                                 fromStrict                               $
                                                 sam_v1_6_program_record_identifier_value $
                                                 sam_v1_6_program_record_identifier x
                                               )
    sam_v1_6_program_name_tos x = case (sam_v1_6_program_name x) of
                                    Nothing  ->
                                      ""
                                    Just rgf ->
                                      "PN:" ++
                                      ( unpack     $
                                        fromStrict $
                                        sam_v1_6_program_name_value rgf
                                      )
    sam_v1_6_program_command_line_tos x = case (sam_v1_6_program_command_line x) of
                                            Nothing  ->
                                              ""
                                            Just rgf ->
                                              "CL:" ++
                                              ( unpack     $
                                                fromStrict $
                                                sam_v1_6_program_command_line_value rgf
                                              )
    sam_v1_6_program_previous_pg_id_tos x = case (sam_v1_6_program_previous_pg_id x) of
                                              Nothing  ->
                                                ""
                                              Just rgf ->
                                                "PP:" ++
                                                ( unpack     $
                                                  fromStrict $
                                                  sam_v1_6_program_previous_pg_id_value rgf
                                                )
    sam_v1_6_program_description_tos x = case (sam_v1_6_program_description x) of
                                           Nothing  ->
                                             ""
                                           Just rgf ->
                                             "DS:" ++
                                             ( unpack     $
                                               fromStrict $
                                               sam_v1_6_program_description_value rgf
                                             )
    sam_v1_6_program_version_tos x = case (sam_v1_6_program_version x) of
                                       Nothing  ->
                                         ""
                                       Just rgf ->
                                         "VN:" ++
                                         ( unpack     $
                                           fromStrict $
                                           sam_v1_6_program_version_value rgf
                                         )
    sam_v1_6_program_tos = case (sam_v1_6_program samv16) of
                             Nothing  ->
                               ""
                             Just pgf ->
                               intercalate "\t" $
                                 Prelude.filter (not . Prelude.null)
                                                [ "@PG"
                                                , sam_v1_6_program_record_identifier_tos pgf
                                                , sam_v1_6_program_name_tos pgf
                                                , sam_v1_6_program_command_line_tos pgf
                                                , sam_v1_6_program_previous_pg_id_tos pgf
                                                , sam_v1_6_program_description_tos pgf
                                                , sam_v1_6_program_version_tos pgf
                                                ]
    sam_v1_6_one_line_comment_tos = case (sam_v1_6_one_line_comment samv16) of
                                      Nothing  ->
                                        ""
                                      Just cof ->
                                        intercalate "\n" $
                                          map (\x -> intercalate "\t" $
                                                       Prelude.filter (not . Prelude.null) 
                                                                      [ "@CO"
                                                                      , unpack       $
                                                                          fromStrict $
                                                                            sam_v1_6_one_line_comment_value x
                                                                      ]
                                              ) (toList cof)
    sam_v1_6_alignment_tos            = intercalate "\n" $
                                          map (\x -> case (Prelude.null $ sam_v1_6_alignment_opts x) of
                                                       True  ->
                                                         sam_v1_6_alignment_mand x 
                                                       False ->
                                                         intercalate "\t"
                                                                     [ sam_v1_6_alignment_mand x
                                                                     , sam_v1_6_alignment_opts x
                                                                     ]
                                              ) ( toList $
                                                    sam_v1_6_alignment samv16
                                                )
    sam_v1_6_alignment_mand x           = intercalate "\t" $
                                            Prelude.filter (not . Prelude.null)
                                                           [ unpack $ fromStrict $ sam_v1_6_alignment_qname x 
                                                           , show $ sam_v1_6_alignment_flag x 
                                                           , unpack $ fromStrict $ sam_v1_6_alignment_rname x 
                                                           , show $ sam_v1_6_alignment_pos x 
                                                           , show $ sam_v1_6_alignment_mapq x 
                                                           , unpack $ fromStrict $ sam_v1_6_alignment_cigar x 
                                                           , unpack $ fromStrict $ sam_v1_6_alignment_rnext x 
                                                           , show $ sam_v1_6_alignment_pnext x 
                                                           , show $ sam_v1_6_alignment_tlen x 
                                                           , unpack $ fromStrict $ sam_v1_6_alignment_seq x 
                                                           , unpack $ fromStrict $ sam_v1_6_alignment_qual x
                                                           ]
    --sam_v1_6_alignment_opts x           = intercalate "\t" $
    --                                        Prelude.filter (not . Prelude.null)
    --                                                       [ sam_v1_6_alignment_aopt_d x 
    --                                                       , sam_v1_6_alignment_iopt_d x 
    --                                                       , sam_v1_6_alignment_fopt_d x 
    --                                                       , sam_v1_6_alignment_zopt_d x
    --                                                       , sam_v1_6_alignment_hopt_d x 
    --                                                       , sam_v1_6_alignment_bopt_d x
    --                                                       ]
    sam_v1_6_alignment_opts x         = case (sam_v1_6_alignment_optionalfields x) of
                                            Nothing             ->
                                              ""
                                            Just optionalfields ->
                                              concat $ forM (toList optionalfields) $ \currentoptionalfield -> 
                                                intercalate "\t" $
                                                  Prelude.filter (not . Prelude.null)
                                                                 [ case (sam_v1_6_alignment_optionalfields_aopt currentoptionalfield) of
                                                                     Nothing   ->
                                                                       ""
                                                                     Just aopt ->
                                                                       ( unpack       $
                                                                           fromStrict $
                                                                             sam_v1_6_alignment_optionalfields_aopt_tag aopt
                                                                       )
                                                                       ++
                                                                       ":A:"
                                                                       ++
                                                                       ( unpack       $
                                                                           fromStrict $
                                                                             sam_v1_6_alignment_optionalfields_aopt_value aopt
                                                                       )
                                                                 , case (sam_v1_6_alignment_optionalfields_iopt currentoptionalfield) of
                                                                     Nothing   ->
                                                                       ""
                                                                     Just iopt ->
                                                                       ( unpack       $
                                                                           fromStrict $
                                                                             sam_v1_6_alignment_optionalfields_iopt_tag iopt
                                                                       )
                                                                       ++
                                                                       ":i:"
                                                                       ++
                                                                       ( show $
                                                                           sam_v1_6_alignment_optionalfields_iopt_value iopt
                                                                       )
                                                                 , case (sam_v1_6_alignment_optionalfields_fopt currentoptionalfield) of
                                                                     Nothing   ->
                                                                       ""
                                                                     Just fopt ->
                                                                       ( unpack       $
                                                                           fromStrict $
                                                                             sam_v1_6_alignment_optionalfields_fopt_tag fopt
                                                                       )
                                                                       ++
                                                                       ":f:"
                                                                       ++
                                                                       ( show $
                                                                           sam_v1_6_alignment_optionalfields_fopt_value fopt
                                                                       )
                                                                 , case (sam_v1_6_alignment_optionalfields_zopt currentoptionalfield) of
                                                                     Nothing   ->
                                                                       ""
                                                                     Just zopt ->
                                                                       ( unpack       $
                                                                           fromStrict $
                                                                             sam_v1_6_alignment_optionalfields_zopt_tag zopt
                                                                       )
                                                                       ++
                                                                       ":Z:"
                                                                       ++
                                                                       ( unpack       $
                                                                           fromStrict $
                                                                             sam_v1_6_alignment_optionalfields_zopt_value zopt
                                                                       )
                                                                 , case (sam_v1_6_alignment_optionalfields_hopt currentoptionalfield) of
                                                                     Nothing   ->
                                                                       ""
                                                                     Just hopt ->
                                                                       ( unpack       $
                                                                           fromStrict $
                                                                             sam_v1_6_alignment_optionalfields_hopt_tag hopt
                                                                       )
                                                                       ++
                                                                       ":H:"
                                                                       ++
                                                                       ( unpack       $
                                                                           fromStrict $
                                                                             sam_v1_6_alignment_optionalfields_hopt_value hopt
                                                                       )
                                                                 , case (sam_v1_6_alignment_optionalfields_bopt currentoptionalfield) of
                                                                     Nothing   ->
                                                                       ""
                                                                     Just bopt ->
                                                                       concat $
                                                                         Prelude.filter (not . Prelude.null)
                                                                                        [ case (sam_v1_6_alignment_optionalfields_bopt_int8 bopt) of
                                                                                            Nothing        ->
                                                                                              ""
                                                                                            Just bopt_int8 ->
                                                                                              ( unpack       $
                                                                                                  fromStrict $
                                                                                                    pack     $
                                                                                                      toList $
                                                                                                        sam_v1_6_alignment_optionalfields_bopt_int8_tag bopt_int8
                                                                                              )   ++
                                                                                              ":" ++
                                                                                              "B" ++
                                                                                              ":" ++
                                                                                              ( unpack           $
                                                                                                  fromStrict     $
                                                                                                    DB.singleton $
                                                                                                      sam_v1_6_alignment_optionalfields_bopt_int8_type bopt_int8
                                                                                              )   ++
                                                                                              "," ++
                                                                                              ( concat     $
                                                                                                  map show $
                                                                                                    toList $
                                                                                                      sam_v1_6_alignment_optionalfields_bopt_int8_value bopt_int8
                                                                                              )
                                                                                        , case (sam_v1_6_alignment_optionalfields_bopt_word8 bopt) of 
                                                                                            Nothing         ->
                                                                                              ""
                                                                                            Just bopt_word8 ->
                                                                                              ( unpack       $
                                                                                                  fromStrict $
                                                                                                    pack     $
                                                                                                      toList $
                                                                                                        sam_v1_6_alignment_optionalfields_bopt_word8_tag bopt_word8
                                                                                              )   ++
                                                                                              ":" ++
                                                                                              "B" ++
                                                                                              ":" ++
                                                                                              ( unpack           $
                                                                                                  fromStrict     $
                                                                                                    DB.singleton $
                                                                                                      sam_v1_6_alignment_optionalfields_bopt_word8_type bopt_word8
                                                                                              )   ++
                                                                                              "," ++
                                                                                              ( unpack       $
                                                                                                  fromStrict $
                                                                                                    pack     $
                                                                                                      toList $
                                                                                                        sam_v1_6_alignment_optionalfields_bopt_word8_value bopt_word8
                                                                                              )
                                                                                        , case (sam_v1_6_alignment_optionalfields_bopt_int16 bopt) of 
                                                                                            Nothing         ->
                                                                                              ""
                                                                                            Just bopt_int16 ->
                                                                                              ( unpack       $
                                                                                                  fromStrict $
                                                                                                    pack     $
                                                                                                      toList $
                                                                                                        sam_v1_6_alignment_optionalfields_bopt_int16_tag bopt_int16
                                                                                              )   ++
                                                                                              ":" ++
                                                                                              "B" ++
                                                                                              ":" ++
                                                                                              ( unpack           $
                                                                                                  fromStrict     $
                                                                                                    DB.singleton $
                                                                                                      sam_v1_6_alignment_optionalfields_bopt_int16_type bopt_int16
                                                                                              )   ++
                                                                                              "," ++
                                                                                              ( concat     $
                                                                                                  map show $
                                                                                                    toList $
                                                                                                      sam_v1_6_alignment_optionalfields_bopt_int16_value bopt_int16
                                                                                              )
                                                                                        , case (sam_v1_6_alignment_optionalfields_bopt_word16 bopt) of
                                                                                            Nothing          ->
                                                                                              ""
                                                                                            Just bopt_word16 ->
                                                                                              ( unpack       $
                                                                                                  fromStrict $
                                                                                                    pack     $
                                                                                                      toList $
                                                                                                        sam_v1_6_alignment_optionalfields_bopt_word16_tag bopt_word16
                                                                                              )   ++
                                                                                              ":" ++
                                                                                              "B" ++
                                                                                              ":" ++
                                                                                              ( unpack           $
                                                                                                  fromStrict     $
                                                                                                    DB.singleton $
                                                                                                      sam_v1_6_alignment_optionalfields_bopt_word16_type bopt_word16
                                                                                              )   ++
                                                                                              "," ++
                                                                                              ( concat             $
                                                                                                  map encodeWord16 $
                                                                                                    toList         $
                                                                                                      sam_v1_6_alignment_optionalfields_bopt_word16_value bopt_word16
                                                                                              )
                                                                                        , case (sam_v1_6_alignment_optionalfields_bopt_int32 bopt) of
                                                                                            Nothing         ->
                                                                                              ""
                                                                                            Just bopt_int32 ->
                                                                                              ( unpack       $
                                                                                                  fromStrict $
                                                                                                    pack     $
                                                                                                      toList $
                                                                                                        sam_v1_6_alignment_optionalfields_bopt_int32_tag bopt_int32
                                                                                              )   ++
                                                                                              ":" ++
                                                                                              "B" ++
                                                                                              ":" ++
                                                                                              ( unpack           $
                                                                                                  fromStrict     $
                                                                                                    DB.singleton $
                                                                                                      sam_v1_6_alignment_optionalfields_bopt_int32_type bopt_int32
                                                                                              )   ++
                                                                                              "," ++
                                                                                              ( concat     $
                                                                                                  map show $
                                                                                                    toList $
                                                                                                      sam_v1_6_alignment_optionalfields_bopt_int32_value bopt_int32
                                                                                              )
                                                                                        , case (sam_v1_6_alignment_optionalfields_bopt_word32 bopt) of 
                                                                                            Nothing          ->
                                                                                              ""
                                                                                            Just bopt_word32 ->
                                                                                              ( unpack       $
                                                                                                  fromStrict $
                                                                                                    pack     $
                                                                                                      toList $
                                                                                                        sam_v1_6_alignment_optionalfields_bopt_word32_tag bopt_word32
                                                                                              )   ++
                                                                                              ":" ++
                                                                                              "B" ++
                                                                                              ":" ++
                                                                                              ( unpack           $
                                                                                                  fromStrict     $
                                                                                                    DB.singleton $
                                                                                                      sam_v1_6_alignment_optionalfields_bopt_word32_type bopt_word32
                                                                                              )   ++
                                                                                              "," ++
                                                                                              ( concat             $
                                                                                                  map encodeWord32 $
                                                                                                    toList         $
                                                                                                      sam_v1_6_alignment_optionalfields_bopt_word32_value bopt_word32
                                                                                              )
                                                                                        , case (sam_v1_6_alignment_optionalfields_bopt_float bopt) of 
                                                                                            Nothing         ->
                                                                                              ""
                                                                                            Just bopt_float ->
                                                                                              ( unpack       $
                                                                                                  fromStrict $
                                                                                                    pack     $
                                                                                                      toList $
                                                                                                        sam_v1_6_alignment_optionalfields_bopt_float_tag bopt_float
                                                                                              )   ++
                                                                                              ":" ++
                                                                                              "B" ++
                                                                                              ":" ++
                                                                                              ( unpack           $
                                                                                                  fromStrict     $
                                                                                                    DB.singleton $
                                                                                                      sam_v1_6_alignment_optionalfields_bopt_float_type bopt_float
                                                                                              )   ++
                                                                                              "," ++
                                                                                              ( concat     $
                                                                                                  map show $
                                                                                                    toList $
                                                                                                      sam_v1_6_alignment_optionalfields_bopt_float_value bopt_float
                                                                                              )
                                                                                        ]
                                                                 ]

{-
    sam_v1_6_alignment_aopt_d x =             ( unpack       $
                                                  fromStrict $
                                                    sam_v1_6_alignment_aopt_tag aopt
                                              )
                                              ++
                                              ":A:"
                                              ++
                                              ( unpack       $
                                                  fromStrict $
                                                    sam_v1_6_alignment_aopt_value aopt
                                              )
    sam_v1_6_alignment_iopt_d x         = case (sam_v1_6_alignment_iopt x) of
                                            Nothing   ->
                                              ""
                                            Just iopt ->
                                              ( unpack       $
                                                  fromStrict $
                                                    sam_v1_6_alignment_iopt_tag iopt
                                              )
                                              ++
                                              ":i:"
                                              ++
                                              ( show $
                                                  sam_v1_6_alignment_iopt_value iopt
                                              )
    sam_v1_6_alignment_fopt_d x         = case (sam_v1_6_alignment_fopt x) of
                                            Nothing   ->
                                              ""
                                            Just fopt ->
                                              ( unpack       $
                                                  fromStrict $
                                                    sam_v1_6_alignment_fopt_tag fopt
                                              )
                                              ++
                                              ":f:"
                                              ++
                                              ( show $
                                                  sam_v1_6_alignment_fopt_value fopt
                                              )
    sam_v1_6_alignment_zopt_d x         = case (sam_v1_6_alignment_zopt x) of
                                            Nothing   ->
                                              ""
                                            Just zopt ->
                                              ( unpack       $
                                                  fromStrict $
                                                    sam_v1_6_alignment_zopt_tag zopt
                                              )
                                              ++
                                              ":Z:"
                                              ++
                                              ( unpack       $
                                                  fromStrict $
                                                    sam_v1_6_alignment_zopt_value zopt
                                              )
    sam_v1_6_alignment_hopt_d x         = case (sam_v1_6_alignment_hopt x) of
                                            Nothing   ->
                                              ""
                                            Just hopt ->
                                              ( unpack       $
                                                  fromStrict $
                                                    sam_v1_6_alignment_hopt_tag hopt
                                              )
                                              ++
                                              ":H:"
                                              ++
                                              ( unpack       $
                                                  fromStrict $
                                                    sam_v1_6_alignment_hopt_value hopt
                                              )
    sam_v1_6_alignment_bopt_d x         = case (sam_v1_6_alignment_bopt x) of
                                            Nothing   ->
                                              ""
                                            Just bopt ->
                                              concat $
                                                Prelude.filter (not . Prelude.null)
                                                               [ sam_v1_6_alignment_bopt_int8_d bopt
                                                               , sam_v1_6_alignment_bopt_word8_d bopt
                                                               , sam_v1_6_alignment_bopt_int16_d bopt
                                                               , sam_v1_6_alignment_bopt_word16_d bopt
                                                               , sam_v1_6_alignment_bopt_int32_d bopt
                                                               , sam_v1_6_alignment_bopt_word32_d bopt
                                                               , sam_v1_6_alignment_bopt_float_d bopt
                                                               ]
    sam_v1_6_alignment_bopt_int8_d x = case (sam_v1_6_alignment_bopt_int8 x) of
                                         Nothing        ->
                                           ""
                                         Just bopt_int8 ->
                                           ( unpack       $
                                               fromStrict $
                                                 pack     $
                                                   toList $
                                                     sam_v1_6_alignment_bopt_int8_tag bopt_int8
                                           )   ++
                                           ":" ++
                                           "B" ++
                                           ":" ++
                                           ( unpack           $
                                               fromStrict     $
                                                 DB.singleton $
                                                   sam_v1_6_alignment_bopt_int8_type bopt_int8
                                           )   ++
                                           "," ++
                                           ( concat     $
                                               map show $
                                                 toList $
                                                   sam_v1_6_alignment_bopt_int8_value bopt_int8
                                           )
    sam_v1_6_alignment_bopt_word8_d x = case (sam_v1_6_alignment_bopt_word8 x) of 
                                          Nothing         ->
                                            ""
                                          Just bopt_word8 ->
                                            ( unpack       $
                                                fromStrict $
                                                  pack     $
                                                    toList $
                                                      sam_v1_6_alignment_bopt_word8_tag bopt_word8
                                            )   ++
                                            ":" ++
                                            "B" ++
                                            ":" ++
                                            ( unpack           $
                                                fromStrict     $
                                                  DB.singleton $
                                                    sam_v1_6_alignment_bopt_word8_type bopt_word8
                                            )   ++
                                            "," ++
                                            ( unpack       $
                                                fromStrict $
                                                  pack     $
                                                    toList $
                                                      sam_v1_6_alignment_bopt_word8_value bopt_word8
                                            )
    sam_v1_6_alignment_bopt_int16_d x = case (sam_v1_6_alignment_bopt_int16 x) of 
                                          Nothing         ->
                                            ""
                                          Just bopt_int16 ->
                                            ( unpack       $
                                                fromStrict $
                                                  pack     $
                                                    toList $
                                                      sam_v1_6_alignment_bopt_int16_tag bopt_int16
                                            )   ++
                                            ":" ++
                                            "B" ++
                                            ":" ++
                                            ( unpack           $
                                                fromStrict     $
                                                  DB.singleton $
                                                    sam_v1_6_alignment_bopt_int16_type bopt_int16
                                            )   ++
                                            "," ++
                                            ( concat     $
                                                map show $
                                                  toList $
                                                    sam_v1_6_alignment_bopt_int16_value bopt_int16
                                            )
    sam_v1_6_alignment_bopt_word16_d x = case (sam_v1_6_alignment_bopt_word16 x) of
                                           Nothing          ->
                                             ""
                                           Just bopt_word16 ->
                                             ( unpack       $
                                                 fromStrict $
                                                   pack     $
                                                     toList $
                                                       sam_v1_6_alignment_bopt_word16_tag bopt_word16
                                             )   ++
                                             ":" ++
                                             "B" ++
                                             ":" ++
                                             ( unpack           $
                                                 fromStrict     $
                                                   DB.singleton $
                                                     sam_v1_6_alignment_bopt_word16_type bopt_word16
                                             )   ++
                                             "," ++
                                             ( concat             $
                                                 map encodeWord16 $
                                                   toList         $
                                                     sam_v1_6_alignment_bopt_word16_value bopt_word16
                                             )
    sam_v1_6_alignment_bopt_int32_d x = case (sam_v1_6_alignment_bopt_int32 x) of
                                          Nothing         ->
                                            ""
                                          Just bopt_int32 ->
                                            ( unpack       $
                                                fromStrict $
                                                  pack     $
                                                    toList $
                                                      sam_v1_6_alignment_bopt_int32_tag bopt_int32
                                            )   ++
                                            ":" ++
                                            "B" ++
                                            ":" ++
                                            ( unpack           $
                                                fromStrict     $
                                                  DB.singleton $
                                                    sam_v1_6_alignment_bopt_int32_type bopt_int32
                                            )   ++
                                            "," ++
                                            ( concat     $
                                                map show $
                                                  toList $
                                                    sam_v1_6_alignment_bopt_int32_value bopt_int32
                                            )
    sam_v1_6_alignment_bopt_word32_d x = case (sam_v1_6_alignment_bopt_word32 x) of 
                                           Nothing          ->
                                             ""
                                           Just bopt_word32 ->
                                             ( unpack       $
                                                 fromStrict $
                                                   pack     $
                                                     toList $
                                                       sam_v1_6_alignment_bopt_word32_tag bopt_word32
                                             )   ++
                                             ":" ++
                                             "B" ++
                                             ":" ++
                                             ( unpack           $
                                                 fromStrict     $
                                                   DB.singleton $
                                                     sam_v1_6_alignment_bopt_word32_type bopt_word32
                                             )   ++
                                             "," ++
                                             ( concat             $
                                                 map encodeWord32 $
                                                   toList         $
                                                     sam_v1_6_alignment_bopt_word32_value bopt_word32
                                             )
    sam_v1_6_alignment_bopt_float_d x = case (sam_v1_6_alignment_bopt_float x) of 
                                          Nothing         ->
                                            ""
                                          Just bopt_float ->
                                            ( unpack       $
                                                fromStrict $
                                                  pack     $
                                                    toList $
                                                      sam_v1_6_alignment_bopt_float_tag bopt_float
                                            )   ++
                                            ":" ++
                                            "B" ++
                                            ":" ++
                                            ( unpack           $
                                                fromStrict     $
                                                  DB.singleton $
                                                    sam_v1_6_alignment_bopt_float_type bopt_float
                                            )   ++
                                            "," ++
                                            ( concat     $
                                                map show $
                                                  toList $
                                                    sam_v1_6_alignment_bopt_float_value bopt_float
                                            )
-}
    encodeWord16 :: Word16
                 -> [Char]
    encodeWord16 = unpack                      .
                     DBL.filter (\x -> x /= 0) .
                       toLazyByteString        .
                         word16LE
    encodeWord32 :: Word32
                 -> [Char]
    encodeWord32 = unpack                      .
                     DBL.filter (\x -> x /= 0) .
                       toLazyByteString        .
                         word32LE

-- | Write @"SAM_V1_6"@ to a file.
-- Calls deconstructSAM_V1_6.
hPutSAM_V1_6 :: Handle
             -> SAM_V1_6
             -> IO ()
hPutSAM_V1_6 h samv16 = System.IO.hPutStr h
                                          (deconstructSAM_V1_6 samv16)

-- | Write a @"SAM_V1_6"@ to a file.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
writeSAM_V1_6 :: FilePath -- ^ Output path to SAM file.
              -> SAM_V1_6
              -> IO ()
writeSAM_V1_6 fp samv16 = do
  h <- openFile fp
                WriteMode
  hPutSAM_V1_6 h
               samv16
  hFlush h
  hClose h
