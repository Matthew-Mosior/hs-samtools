{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.SAM.Version1_6.Read.Error
-- Copyright   :  (c) Matthew Mosior 2023
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.SAM.Version1_6.Read.Error ( -- * SAM version 1.6 data type
                                        SAM_V1_6_Error(..)
                                      ) where

import Data.Data
import Generics.Deriving.Base

-- | Custom @"SAM_V1_6"@ (SAM version 1.6) error data type.
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Error = -- | @HD tag not in accepted format (only important
                      -- if @HD tag is present).
                      SAM_V1_6_Error_File_Level_Metadata_Tag_Incorrect_Format
                      -- | VN tag missing (only important
                      -- if @HD tag is present).
                    | SAM_V1_6_Error_File_Level_Metadata_Format_Version_Tag_Missing
                      -- | VN tag not in accepted format (only important
                      -- if @HD tag is present).
                    | SAM_V1_6_Error_File_Level_Metadata_Format_Version_Tag_Incorrect_Format
                      -- | VN value not in accepted format (only important
                      -- if @HD tag is present).
                    | SAM_V1_6_Error_File_Level_Metadata_Format_Version_Value_Incorrect_Format
                      -- | Sorting order tag not in accepted format (only important
                      -- if @HD tag is present).
                    | SAM_V1_6_Error_File_Level_Metadata_Sorting_Order_Tag_Incorrect_Format
                      -- | Sorting order invalid value (only important
                      -- if @HD tag is present).
                    | SAM_V1_6_Error_File_Level_Metadata_Sorting_Order_Invalid_Value
                      -- | Grouping of alignments no in accepted format (only important
                      -- if @HD tag is present).
                    | SAM_V1_6_Error_File_Level_Metadata_Grouping_Of_Alignments_Tag_Incorrect_Format
                      -- | Grouping of Alignments invalid value (only important
                      -- if @HD tag is present).
                    | SAM_V1_6_Error_File_Level_Metadata_Grouping_Of_Alignments_Invalid_Value
                      -- | Subsorting order tag not in accepted format (only important
                      -- if @HD tag is present).
                    | SAM_V1_6_Error_File_Level_Metadata_Subsorting_Order_Tag_Incorrect_Format
                      -- | Subsorting order of alignments not in accepted format (only
                      -- if @HD tag is present.
                    | SAM_V1_6_Error_File_Level_Metadata_Subsorting_Order_Incorrect_Format
                      -- | SN tag missing (only important
                      -- if @SQ tag is present).
                    | SAM_V1_6_Error_Reference_Sequence_Dictionary_Reference_Sequence_Name_Tag_Missing
                      -- | Reference Sequence Name not in accepted format (only important
                      -- if @SQ tag is present.
                    | SAM_V1_6_Error_Reference_Sequence_Dictionary_Reference_Sequence_Name_Incorrect_Format
                      -- | Reference Sequence Name invalid value (only important
                      -- if @SQ tag is present).
                    | SAM_V1_6_Error_Reference_Sequence_Dictionary_Reference_Sequence_Name_Invalid_Value
                      -- | LN tag missing (only important
                      -- if @SQ tag is present).
                    |  SAM_V1_6_Error_Reference_Sequence_Dictionary_Reference_Sequence_Length_Missing
                      -- | Reference Sequence Length not in accepted format (only important
                      -- if @SQ tag is present.
                    | SAM_V1_6_Error_Reference_Sequence_Dictionary_Reference_Sequence_Length_Incorrect_Format
                      -- | Reference Sequence Length invalid value (only important
                      -- if @SQ tag is present).
                    | SAM_V1_6_Error_Reference_Sequence_Dictionary_Reference_Sequence_Length_Invalid_Value
                      -- | Alternative locus not in accepted format (only important
                      -- if @SQ tag is present).
                    | SAM_V1_6_Error_Reference_Sequence_Dictionary_Alternative_Locus_Incorrect_Format
                      -- | Alternative Reference Sequence Names not in accepted format (only important
                      -- if @SQ tag is present).
                    | SAM_V1_6_Error_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names_Incorrect_Format
                      -- | Alternative Reference Sequence Names invalid value (only important
                      -- if @SQ tag is present).
                    | SAM_V1_6_Error_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names_Invalid_Value
                      -- | Genome Assembly Identifier not in accepted format (only important
                      -- if @SQ tag is present).
                    | SAM_V1_6_Error_Reference_Sequence_Dictionary_Genome_Assembly_Identifier_Incorrect_Format
                      -- | Description not in accepted format (only important
                      -- if @SQ tag is present).
                    | SAM_V1_6_Error_Reference_Sequence_Dictionary_Description_Incorrect_Format
                      -- | MD5 checksum not in accepted format (only important
                      -- if @SQ tag is present).
                    | SAM_V1_6_Error_Reference_Sequence_Dictionary_MD5_Checksum_Incorrect_Format
                      -- | Species not in accepted format (only important
                      -- if @SQ tag is present).
                    | SAM_V1_6_Error_Reference_Sequence_Dictionary_Species_Incorrect_Format
                      -- | Molecule topology not in accepted format (only important
                      -- if @SQ tag is present).
                    | SAM_V1_6_Error_Reference_Sequence_Dictionary_Molecule_Topology_Incorrect_Format
                      -- | Molecule topology invalid value (only important
                      -- if @SQ tag is present).
                    | SAM_V1_6_Error_Reference_Sequence_Dictionary_Molecule_Topology_Invalid_Value
                      -- | URI not in accepted format (only important
                      -- if @SQ tag is present).
                    | SAM_V1_6_Error_Reference_Sequence_Dictionary_URI_Incorrect_Format
                      -- | ID tag not accepted format (only important
                      -- if @RG tag is present).
                    | SAM_V1_6_Error_Read_Group_Read_Group_Identifier_Incorrect_Format
                      -- | ID tag is missing (only important
                      -- if @RG tag is present).
                    | SAM_V1_6_Error_Read_Group_Read_Group_Identifier_Tag_Missing
                      -- | BC tag not in accepted format (only important
                      -- if @RG tag is present).
                    | SAM_V1_6_Error_Read_Group_Barcode_Sequence_Incorrect_Format
                      -- | CN tag not in accepted format (only important
                      -- if @RG tag is present).
                    | SAM_V1_6_Error_Read_Group_Sequencing_Center_Incorrect_Format
                      -- | DS tag not in accepted format (only important
                      -- if @RG tag is present).
                    | SAM_V1_6_Error_Read_Group_Description_Incorrect_Format
                      -- | DT tag not in accepted format (only important
                      -- if @RG tag is present).
                    | SAM_V1_6_Error_Read_Group_Date_Run_Produced_Incorrect_Format
                      -- | FO tag not in accepted format (only important
                      -- if @RG tag is present).
                    | SAM_V1_6_Error_Read_Group_Flow_Order_Incorrect_Format
                      -- | KS tag not in accepted format (only important
                      -- if @RG tag is present).
                    | SAM_V1_6_Error_Read_Group_Key_Sequence_Incorrect_Format
                      -- | LB tag not in accepted format (only important
                      -- if @RG tag is present).
                    | SAM_V1_6_Error_Read_Group_Library_Incorrect_Format
                      -- | PG tag not in accepted format (only important
                      -- if @RG tag is present).
                    | SAM_V1_6_Error_Read_Group_Programs_Incorrect_Format
                      -- | PI tag not in accepted format (only important
                      -- if @RG tag is present).
                    | SAM_V1_6_Error_Read_Group_Predicted_Median_Insert_Size_Incorrect_Format
                      -- | PL tag not in accepted format (only important
                      -- if @RG tag is present).
                    | SAM_V1_6_Error_Read_Group_Platform_Incorrect_Format
                      -- | PM tag not in accepted format (only important
                      -- if @RG tag is present).
                    | SAM_V1_6_Error_Read_Group_Platform_Model_Incorrect_Format
                      -- | PU tag not in accepted format (only important
                      -- if @RG tag is present).
                    | SAM_V1_6_Error_Read_Group_Platform_Unit_Incorrect_Format
                      -- | SM tag not in accepted format (only important
                      -- if @RG tag is present).
                    | SAM_V1_6_Error_Read_Group_Sample_Incorrect_Format
                      -- | RG tag not in accepted format.
                    | SAM_V1_6_Error_Read_Group_Tag_Incorrect_Format
                      -- | ID tag not in accepted format (only important
                      -- if @PG tag is present).
                    | SAM_V1_6_Error_Program_Identifier_Incorrect_Format
                      -- | PN tag not in accepted format (only important
                      -- if @PG tag is present).
                    | SAM_V1_6_Error_Program_Name_Incorrect_Format
                      -- | CL tag not in accepted format (only important
                      -- if @PG tag is present).
                    | SAM_V1_6_Error_Program_Command_Line_Incorrect_Format
                      -- | PP tag not in accepted format (only important
                      -- if @PG tag is present).
                    | SAM_V1_6_Error_Program_Previous_PG_ID_Incorrect_Format
                      -- | DS tag not in accepted format (only important
                      -- if @PG tag is present).
                    | SAM_V1_6_Error_Program_Description_Incorrect_Format
                      -- | VN tag not in accepted format (only important
                      -- if @PG tag is present).
                    | SAM_V1_6_Error_Program_Version_Incorrect_Format
                      -- | PG tag not in accepted format.
                    | SAM_V1_6_Error_Program_Tag_Incorrect_Format
                      -- | CO tag not in accepted format.
                    | SAM_V1_6_Error_One_Line_Comment_Tag_Incorrect_Format
                      -- | QNAME of alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_QNAME_Incorrect_Format
                      -- | FLAG of alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_FLAG_Incorrect_Format
                      -- | RNAME of alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_RNAME_Incorrect_Format
                      -- | POS of alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_POS_Incorrect_Format
                      -- | MAPQ of alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_MAPQ_Incorrect_Format
                      -- | CIGAR of alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_CIGAR_Incorrect_Format
                      -- | RNEXT of alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_RNEXT_Incorrect_Format
                      -- | PNEXT of alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_PNEXT_Incorrect_Format
                      -- | TLEN of alignment section not in accpepted format.
                    | SAM_V1_6_Error_Alignment_TLEN_Incorrect_Format
                      -- | SEQ of alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_SEQ_Incorrect_Format
                      -- | QUAL of alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_QUAL_Incorrect_Format
                      -- | AOPT tag of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_AOPT_Tag_Incorrect_Format
                      -- | AOPT type of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_AOPT_Type_Incorrect_Format
                      -- | AOPT value of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_AOPT_Value_Incorrect_Format
                      -- | IOPT tag of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_IOPT_Tag_Incorrect_Format
                      -- | IOPT type of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_IOPT_Type_Incorrect_Format
                      -- | IOPT value of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_IOPT_Value_Incorrect_Format
                      -- | FOPT tag of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_FOPT_Tag_Incorrect_Format
                      -- | FOPT type of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_FOPT_Type_Incorrect_Format
                      -- | FOPT value of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_FOPT_Value_Incorrect_Format
                      -- | ZOPT tag of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_ZOPT_Tag_Incorrect_Format
                      -- | ZOPT type of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_ZOPT_Type_Incorrect_Format
                      -- | ZOPT value of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_ZOPT_Value_Incorrect_Format
                      -- | HOPT tag of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_HOPT_Tag_Incorrect_Format
                      -- | HOPT type of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_HOPT_Type_Incorrect_Format
                      -- | HOPT value of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_HOPT_Value_Incorrect_Format
                      -- | BOPT tag of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_BOPT_Tag_Incorrect_Format
                      -- | BOPT type of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_BOPT_Type_Incorrect_Format
                      -- | BOPT value type of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_BOPT_Value_Type_Incorrect_Format
                      -- | BOPT value data of the alignment section not in accepted format.
                    | SAM_V1_6_Error_Alignment_BOPT_Value_Data_Incorrect_Format
  deriving (Eq,Generic,Show,Typeable)
