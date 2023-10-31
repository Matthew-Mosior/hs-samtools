module Main (main) where

import Data.SAM.Version1_6.Base
import Data.SAM.Version1_6.Alignment
import Data.SAM.Version1_6.Alignment.IOPT
import Data.SAM.Version1_6.Alignment.ZOPT
import Data.SAM.Version1_6.Alignment.BOPT
import Data.SAM.Version1_6.Header
import Data.SAM.Version1_6.Read.Base

import Data.Sequence (fromList)
import Data.String   (fromString)
import Test.Hspec
import Data.SAM.Version1_6.Write.Base 

main :: IO ()
main = hspec $ do
  describe "Data.SAM.Version1_6.Read.Base" $ do
    describe "readSAM_V1_6" $ do
      describe "toy5.sam" $ do
        it "Ensures that readSAM_V1_6 can read and parse a SAM file with only alignment fields." $ do
          readSAM_V1_6 "test/examples/toy5.sam" `shouldReturn` toy5sam
      describe "toy4.sam" $ do
        it "Ensures that readSAM_V1_6 can read and parse a SAM file with an optional alignment field." $ do
          readSAM_V1_6 "test/examples/toy4.sam" `shouldReturn` toy4sam
      describe "toy2.sam" $ do
        it "Ensures that readSAM_V1_6 can read and parse a SAM file with file-level metadata (@HD) and reference sequence dictionary (@SQ) optional header fields." $ do
          readSAM_V1_6 "test/examples/toy2.sam" `shouldReturn` toy2sam
      describe "toy1.sam" $ do
        it "Ensures that readSAM_V1_6 can read and parse a SAM file with multiple reference sequence dictionary (@SQ) optional header fields." $ do
          readSAM_V1_6 "test/examples/toy1.sam" `shouldReturn` toy1sam
  describe "Data.SAM.Version1_6.Write.Base" $ do
    describe "writeSAM_V1_6" $ do
      describe "toy2.sam" $ do
        it "Ensures writeSAM_V1_6 produces the a sam file equivalent to what was parsed via readSAM_V1_6." $ do
          toy2 <- readSAM_V1_6 "test/examples/toy2.sam"
          writeSAM_V1_6 "test/examples/toy2f.sam" toy2
          toy2f <- readSAM_V1_6 "test/examples/toy2f.sam"
          toy2 `shouldBe` toy2f  
  where
    toy1sam = SAM_V1_6 { sam_v1_6_file_level_metadata = Nothing , sam_v1_6_reference_sequence_dictionary = Just (fromList [SAM_V1_6_Reference_Sequence_Dictionary { sam_v1_6_reference_sequence_dictionary_reference_sequence_name = SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name { sam_v1_6_reference_sequence_dictionary_reference_sequence_name_value = fromString  "ref" } , sam_v1_6_reference_sequence_dictionary_reference_sequence_length = SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length { sam_v1_6_reference_sequence_dictionary_reference_sequence_length_value = fromString  "45" } , sam_v1_6_reference_sequence_dictionary_alternative_locus = Nothing , sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names = Nothing , sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier = Nothing , sam_v1_6_reference_sequence_dictionary_description = Nothing , sam_v1_6_reference_sequence_dictionary_md5_checksum = Nothing , sam_v1_6_reference_sequence_dictionary_species = Nothing , sam_v1_6_reference_sequence_dictionary_molecule_topology = Nothing , sam_v1_6_reference_sequence_dictionary_uri = Nothing },SAM_V1_6_Reference_Sequence_Dictionary { sam_v1_6_reference_sequence_dictionary_reference_sequence_name = SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name { sam_v1_6_reference_sequence_dictionary_reference_sequence_name_value = fromString  "ref2" } , sam_v1_6_reference_sequence_dictionary_reference_sequence_length = SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length { sam_v1_6_reference_sequence_dictionary_reference_sequence_length_value = fromString  "40" } , sam_v1_6_reference_sequence_dictionary_alternative_locus = Nothing , sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names = Nothing , sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier = Nothing , sam_v1_6_reference_sequence_dictionary_description = Nothing , sam_v1_6_reference_sequence_dictionary_md5_checksum = Nothing , sam_v1_6_reference_sequence_dictionary_species = Nothing , sam_v1_6_reference_sequence_dictionary_molecule_topology = Nothing , sam_v1_6_reference_sequence_dictionary_uri = Nothing }]) , sam_v1_6_read_group = Nothing , sam_v1_6_program = Nothing , sam_v1_6_one_line_comment = Nothing , sam_v1_6_alignment = fromList [SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r001" , sam_v1_6_alignment_flag = 163 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 7 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "8M4I4M1D3M" , sam_v1_6_alignment_rnext = fromString  "=" , sam_v1_6_alignment_pnext = 37 , sam_v1_6_alignment_tlen = 39 , sam_v1_6_alignment_seq = fromString  "TTAGATAAAGAGGATACTG" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Just SAM_V1_6_Alignment_BOPT { sam_v1_6_alignment_bopt_int8 = Nothing , sam_v1_6_alignment_bopt_word8 = Nothing , sam_v1_6_alignment_bopt_int16 = Nothing , sam_v1_6_alignment_bopt_word16 = Just SAM_V1_6_Alignment_BOPT_Word16 { sam_v1_6_alignment_bopt_word16_tag  = fromList [88,88] , sam_v1_6_alignment_bopt_word16_type = 83 , sam_v1_6_alignment_bopt_word16_value = fromList [49,50,53,54,49,44,50,44,50,48,44,49,49,50] } , sam_v1_6_alignment_bopt_int32 = Nothing , sam_v1_6_alignment_bopt_word32 = Nothing , sam_v1_6_alignment_bopt_float = Nothing } },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r002" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 9 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "1S2I6M1P1I1P1I4M2I" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "AAAAGATAAGGGATAAA" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r003" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 9 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "5H6M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "AGCTAA" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r004" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 16 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "6M14N1I5M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "ATAGCTCTCAGC" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r003" , sam_v1_6_alignment_flag = 16 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 29 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "6H5M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "TAGGC" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r001" , sam_v1_6_alignment_flag = 83 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 37 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "9M" , sam_v1_6_alignment_rnext = fromString  "=" , sam_v1_6_alignment_pnext = 7 , sam_v1_6_alignment_tlen = -39 , sam_v1_6_alignment_seq = fromString  "CAGCGCCAT" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x1" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 1 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "20M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "aggttttataaaacaaataa" , sam_v1_6_alignment_qual = fromString  "????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x2" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 2 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "21M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "ggttttataaaacaaataatt" , sam_v1_6_alignment_qual = fromString  "?????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x3" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 6 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "9M4I13M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "ttataaaacAAATaattaagtctaca" , sam_v1_6_alignment_qual = fromString  "??????????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x4" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 10 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "25M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "CaaaTaattaagtctacagagcaac" , sam_v1_6_alignment_qual = fromString  "?????????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x5" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 12 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "24M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "aaTaattaagtctacagagcaact" , sam_v1_6_alignment_qual = fromString  "????????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x6" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 14 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "23M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "Taattaagtctacagagcaacta" , sam_v1_6_alignment_qual = fromString  "???????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing }] }
    toy2sam = SAM_V1_6 { sam_v1_6_file_level_metadata = Just SAM_V1_6_File_Level_Metadata { sam_v1_6_file_level_metadata_format_version = SAM_V1_6_File_Level_Metadata_Format_Version { sam_v1_6_file_level_metadata_format_version_value = fromString  "1.6" } , sam_v1_6_file_level_metadata_sorting_order = Just SAM_V1_6_File_Level_Metadata_Sorting_Order { sam_v1_6_file_level_metadata_sorting_order_value = fromString  "coordinate" } , sam_v1_6_file_level_metadata_alignment_grouping = Nothing , sam_v1_6_file_level_metadata_subsorting_order = Nothing } , sam_v1_6_reference_sequence_dictionary = Just (fromList [SAM_V1_6_Reference_Sequence_Dictionary { sam_v1_6_reference_sequence_dictionary_reference_sequence_name = SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name { sam_v1_6_reference_sequence_dictionary_reference_sequence_name_value = fromString  "ref" } , sam_v1_6_reference_sequence_dictionary_reference_sequence_length = SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length { sam_v1_6_reference_sequence_dictionary_reference_sequence_length_value = fromString  "45" } , sam_v1_6_reference_sequence_dictionary_alternative_locus = Nothing , sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names = Nothing , sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier = Nothing , sam_v1_6_reference_sequence_dictionary_description = Nothing , sam_v1_6_reference_sequence_dictionary_md5_checksum = Nothing , sam_v1_6_reference_sequence_dictionary_species = Nothing , sam_v1_6_reference_sequence_dictionary_molecule_topology = Nothing , sam_v1_6_reference_sequence_dictionary_uri = Nothing }]) , sam_v1_6_read_group = Nothing , sam_v1_6_program = Nothing , sam_v1_6_one_line_comment = Nothing , sam_v1_6_alignment = fromList [SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r001" , sam_v1_6_alignment_flag = 99 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 7 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "8M2I4M1D3M" , sam_v1_6_alignment_rnext = fromString  "=" , sam_v1_6_alignment_pnext = 37 , sam_v1_6_alignment_tlen = 39 , sam_v1_6_alignment_seq = fromString  "TTAGATAAAGGATACTG" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r002" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 9 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "3S6M1P1I4M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "AAAAGATAAGGATA" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r003" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 9 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "5S6M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "GCCTAAGCTAA" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Just SAM_V1_6_Alignment_ZOPT { sam_v1_6_alignment_zopt_tag = fromString  "SA" , sam_v1_6_alignment_zopt_value = fromString  "ref,29,-,6H5M,17,0;" } , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r004" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 16 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "6M14N5M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "ATAGCTTCAGC" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r003" , sam_v1_6_alignment_flag = 2064 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 29 , sam_v1_6_alignment_mapq = 17 , sam_v1_6_alignment_cigar = fromString  "6H5M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "TAGGC" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Just SAM_V1_6_Alignment_ZOPT { sam_v1_6_alignment_zopt_tag = fromString  "SA" , sam_v1_6_alignment_zopt_value = fromString  "ref,9,+,5S6M,30,1;" } , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r001" , sam_v1_6_alignment_flag = 147 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 37 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "9M" , sam_v1_6_alignment_rnext = fromString  "=" , sam_v1_6_alignment_pnext = 7 , sam_v1_6_alignment_tlen = -39 , sam_v1_6_alignment_seq = fromString  "CAGCGGCAT" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Just SAM_V1_6_Alignment_IOPT { sam_v1_6_alignment_iopt_tag = fromString  "NM" , sam_v1_6_alignment_iopt_value = 1 } , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing }] } 
    toy4sam = SAM_V1_6 { sam_v1_6_file_level_metadata = Nothing , sam_v1_6_reference_sequence_dictionary = Nothing , sam_v1_6_read_group = Nothing , sam_v1_6_program = Nothing , sam_v1_6_one_line_comment = Nothing , sam_v1_6_alignment = fromList [SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r001" , sam_v1_6_alignment_flag = 163 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 7 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "8M4I4M1D3M" , sam_v1_6_alignment_rnext = fromString  "=" , sam_v1_6_alignment_pnext = 37 , sam_v1_6_alignment_tlen = 39 , sam_v1_6_alignment_seq = fromString  "TTAGATAAAGAGGATACTG" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Just SAM_V1_6_Alignment_BOPT { sam_v1_6_alignment_bopt_int8 = Nothing , sam_v1_6_alignment_bopt_word8 = Nothing , sam_v1_6_alignment_bopt_int16 = Nothing , sam_v1_6_alignment_bopt_word16 = Just SAM_V1_6_Alignment_BOPT_Word16 { sam_v1_6_alignment_bopt_word16_tag  = fromList [88,88] , sam_v1_6_alignment_bopt_word16_type = 83 , sam_v1_6_alignment_bopt_word16_value = fromList [49,50,53,54,49,44,50,44,50,48,44,49,49,50] } , sam_v1_6_alignment_bopt_int32 = Nothing , sam_v1_6_alignment_bopt_word32 = Nothing , sam_v1_6_alignment_bopt_float = Nothing } },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r002" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 9 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "1S2I6M1P1I1P1I4M2I" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "AAAAGATAAGGGATAAA" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r003" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 9 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "5H6M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "AGCTAA" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r004" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 16 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "6M14N1I5M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "ATAGCTCTCAGC" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r003" , sam_v1_6_alignment_flag = 16 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 29 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "6H5M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "TAGGC" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r001" , sam_v1_6_alignment_flag = 83 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 37 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "9M" , sam_v1_6_alignment_rnext = fromString  "=" , sam_v1_6_alignment_pnext = 7 , sam_v1_6_alignment_tlen = -39 , sam_v1_6_alignment_seq = fromString  "CAGCGCCAT" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x1" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 1 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "20M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "aggttttataaaacaaataa" , sam_v1_6_alignment_qual = fromString  "????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x2" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 2 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "21M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "ggttttataaaacaaataatt" , sam_v1_6_alignment_qual = fromString  "?????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x3" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 6 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "9M4I13M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "ttataaaacAAATaattaagtctaca" , sam_v1_6_alignment_qual = fromString  "??????????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x4" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 10 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "25M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "CaaaTaattaagtctacagagcaac" , sam_v1_6_alignment_qual = fromString  "?????????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x5" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 12 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "24M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "aaTaattaagtctacagagcaact" , sam_v1_6_alignment_qual = fromString  "????????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x6" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 14 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "23M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "Taattaagtctacagagcaacta" , sam_v1_6_alignment_qual = fromString  "???????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing }] } 
    toy5sam = SAM_V1_6 { sam_v1_6_file_level_metadata = Nothing , sam_v1_6_reference_sequence_dictionary = Nothing , sam_v1_6_read_group = Nothing , sam_v1_6_program = Nothing , sam_v1_6_one_line_comment = Nothing , sam_v1_6_alignment = fromList [SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r001" , sam_v1_6_alignment_flag = 163 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 7 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "8M4I4M1D3M" , sam_v1_6_alignment_rnext = fromString  "=" , sam_v1_6_alignment_pnext = 37 , sam_v1_6_alignment_tlen = 39 , sam_v1_6_alignment_seq = fromString  "TTAGATAAAGAGGATACTG" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r002" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 9 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "1S2I6M1P1I1P1I4M2I" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "AAAAGATAAGGGATAAA" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r003" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 9 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "5H6M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "AGCTAA" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r004" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 16 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "6M14N1I5M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "ATAGCTCTCAGC" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r003" , sam_v1_6_alignment_flag = 16 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 29 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "6H5M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "TAGGC" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "r001" , sam_v1_6_alignment_flag = 83 , sam_v1_6_alignment_rname = fromString  "ref" , sam_v1_6_alignment_pos = 37 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "9M" , sam_v1_6_alignment_rnext = fromString  "=" , sam_v1_6_alignment_pnext = 7 , sam_v1_6_alignment_tlen = -39 , sam_v1_6_alignment_seq = fromString  "CAGCGCCAT" , sam_v1_6_alignment_qual = fromString  "*" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x1" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 1 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "20M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "aggttttataaaacaaataa" , sam_v1_6_alignment_qual = fromString  "????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x2" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 2 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "21M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "ggttttataaaacaaataatt" , sam_v1_6_alignment_qual = fromString  "?????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x3" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 6 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "9M4I13M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "ttataaaacAAATaattaagtctaca" , sam_v1_6_alignment_qual = fromString  "??????????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x4" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 10 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "25M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "CaaaTaattaagtctacagagcaac" , sam_v1_6_alignment_qual = fromString  "?????????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x5" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 12 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "24M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "aaTaattaagtctacagagcaact" , sam_v1_6_alignment_qual = fromString  "????????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing },SAM_V1_6_Alignment { sam_v1_6_alignment_qname = fromString  "x6" , sam_v1_6_alignment_flag = 0 , sam_v1_6_alignment_rname = fromString  "ref2" , sam_v1_6_alignment_pos = 14 , sam_v1_6_alignment_mapq = 30 , sam_v1_6_alignment_cigar = fromString  "23M" , sam_v1_6_alignment_rnext = fromString  "*" , sam_v1_6_alignment_pnext = 0 , sam_v1_6_alignment_tlen = 0 , sam_v1_6_alignment_seq = fromString  "Taattaagtctacagagcaacta" , sam_v1_6_alignment_qual = fromString  "???????????????????????" , sam_v1_6_alignment_aopt = Nothing , sam_v1_6_alignment_iopt = Nothing , sam_v1_6_alignment_fopt = Nothing , sam_v1_6_alignment_zopt = Nothing , sam_v1_6_alignment_hopt = Nothing , sam_v1_6_alignment_bopt = Nothing }] }
