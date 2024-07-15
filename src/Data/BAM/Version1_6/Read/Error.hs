{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.Read.Error
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.Read.Error ( -- * BAM version 1.6 data type
                                        BAM_V1_6_Read_Error(..)
                                      ) where

import Data.Data
import Generics.Deriving.Base

-- | Custom @"BAM_V1_6_Read_Error"@ (BAM version 1.6) error data type.
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_Read_Error = -- | Identification 1 (IDentification 1; ID1) field
                           -- of the GZip header not in accepted format.
                           BAM_V1_6_Read_Error_GZipHeader_ID1_Incorrect_Format
                           -- | Identification 2 (IDentification 2; ID2) field
                           -- of the GZip header not in accepted format.
                         | BAM_V1_6_Read_Error_GZipHeader_ID2_Incorrect_Format
                           -- | Compression Method (CM) field of the GZip header not
                           -- in accepted format.
                         | BAM_V1_6_Read_Error_GZipHeader_Compression_Method_Incorrect_Format
                           -- | Flag (FLG) field of the GZip header not in accepted format.
                         | BAM_V1_6_Read_Error_GZipHeader_Flag_Incorrect_Format
                           -- | Modification TIME (MTIME) field of the GZip header not
                           -- in accepted format.
                         | BAM_V1_6_Read_Error_GZipHeader_Modification_TIME_Incorrect_Format
                           -- | Extra flags (eXtra FLags; XFL) field of the GZip header
                           -- not in accepted format.
                         | BAM_V1_6_Read_Error_GZipHeader_Extra_Flags_Incorrect_Format
                           -- | Operating System (OS) field of the GZip header
                           -- not in accepted format.
                         | BAM_V1_6_Read_Error_GZipHeader_Operating_System_Incorrect_Format
                           -- | Extra Length (eXtra LENgth; XLEN) field of the
                           -- GZip header is not in accepted format.
                         | BAM_V1_6_Read_Error_GZipHeader_Extra_Length_Incorrect_Format
                           -- | Subfield Identifier 1 (Subfield Identifier1; SI1)
                           -- field of the GZip header is not in accepted format.
                         | BAM_V1_6_Read_Error_GZipHeader_Subfield_Identifier_1_Incorrect_Format
                           -- | Subfield Identifier 2 (Subfield Identifier2; SI2)
                           -- field of the GZip header is not in accepted format.
                         | BAM_V1_6_Read_Error_GZipHeader_Subfield_Identifier_2_Incorrect_Format
                           -- | Subfield Length (Subfield LENgth; SLEN)
                           -- field of the GZip header is not in accepted format.
                         | BAM_V1_6_Read_Error_GZipHeader_Subfield_Length_Incorrect_Format
                           -- | Total block size minus 1 (total Block SIZE minus 1; BSIZE)
                           -- field of the GZip header is not in accepted format.
                         | BAM_V1_6_Read_Error_GZipHeader_Total_Block_SIZE_Minus_1_Incorrect_Format
                           -- | Compressed DATA by zlib::deflate() (CDATA) is not
                           -- in accepted format.
                         | BAM_V1_6_Read_Error_GZipHeader_Compressed_DATA_Incorrect_Format
                           -- | @HD tag not in accepted format (only important
                           -- if @HD tag is present).
                         | BAM_V1_6_Read_Error_File_Level_Metadata_Tag_Incorrect_Format
                           -- | VN tag missing (only important
                           -- if @HD tag is present).
                         | BAM_V1_6_Read_Error_File_Level_Metadata_Format_Version_Tag_Missing
                           -- | VN tag not in accepted format (only important
                           -- if @HD tag is present).
                         | BAM_V1_6_Read_Error_File_Level_Metadata_Format_Version_Tag_Incorrect_Format
                           -- | VN value not in accepted format (only important
                           -- if @HD tag is present).
                         | BAM_V1_6_Read_Error_File_Level_Metadata_Format_Version_Value_Incorrect_Format
                           -- | Sorting order tag not in accepted format (only important
                           -- if @HD tag is present).
                         | BAM_V1_6_Read_Error_File_Level_Metadata_Sorting_Order_Tag_Incorrect_Format
                           -- | Sorting order invalid value (only important
                           -- if @HD tag is present).
                         | BAM_V1_6_Read_Error_File_Level_Metadata_Sorting_Order_Invalid_Value
                           -- | Grouping of alignments no in accepted format (only important
                           -- if @HD tag is present).
                         | BAM_V1_6_Read_Error_File_Level_Metadata_Grouping_Of_Alignments_Tag_Incorrect_Format
                           -- | Grouping of Alignments invalid value (only important
                           -- if @HD tag is present).
                         | BAM_V1_6_Read_Error_File_Level_Metadata_Grouping_Of_Alignments_Invalid_Value
                           -- | Subsorting order tag not in accepted format (only important
                           -- if @HD tag is present).
                         | BAM_V1_6_Read_Error_File_Level_Metadata_Subsorting_Order_Tag_Incorrect_Format
                           -- | Subsorting order of alignments not in accepted format (only
                           -- if @HD tag is present.
                         | BAM_V1_6_Read_Error_File_Level_Metadata_Subsorting_Order_Incorrect_Format
                           -- | The BAM magic string in the BAM header is not BAM\SOH (BAM\1).
                         | BAM_V1_6_Read_Error_BAMHeader_BAM_Magic_String_Invalid_Value
                           -- | SN tag missing (only important
                           -- if @SQ tag is present).
                         | BAM_V1_6_Read_Error_Reference_Sequence_Dictionary_Reference_Sequence_Name_Tag_Missing
                           -- | Reference Sequence Name not in accepted format (only important
                           -- if @SQ tag is present.
                         | BAM_V1_6_Read_Error_Reference_Sequence_Dictionary_Reference_Sequence_Name_Incorrect_Format
                           -- | Reference Sequence Name invalid value (only important
                           -- if @SQ tag is present).
                         | BAM_V1_6_Read_Error_Reference_Sequence_Dictionary_Reference_Sequence_Name_Invalid_Value
                           -- | LN tag missing (only important
                           -- if @SQ tag is present).
                         |  BAM_V1_6_Error_Reference_Sequence_Dictionary_Reference_Sequence_Length_Missing
                           -- | Reference Sequence Length not in accepted format (only important
                           -- if @SQ tag is present.
                         | BAM_V1_6_Read_Error_Reference_Sequence_Dictionary_Reference_Sequence_Length_Incorrect_Format
                           -- | Reference Sequence Length invalid value (only important
                           -- if @SQ tag is present).
                         | BAM_V1_6_Read_Error_Reference_Sequence_Dictionary_Reference_Sequence_Length_Invalid_Value
                           -- | Alternative locus not in accepted format (only important
                           -- if @SQ tag is present).
                         | BAM_V1_6_Read_Error_Reference_Sequence_Dictionary_Alternative_Locus_Incorrect_Format
                           -- | Alternative Reference Sequence Names not in accepted format (only important
                           -- if @SQ tag is present).
                         | BAM_V1_6_Read_Error_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names_Incorrect_Format
                           -- | Alternative Reference Sequence Names invalid value (only important
                           -- if @SQ tag is present).
                         | BAM_V1_6_Read_Error_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names_Invalid_Value
                           -- | Genome Assembly Identifier not in accepted format (only important
                           -- if @SQ tag is present).
                         | BAM_V1_6_Read_Error_Reference_Sequence_Dictionary_Genome_Assembly_Identifier_Incorrect_Format
                           -- | Description not in accepted format (only important
                           -- if @SQ tag is present).
                         | BAM_V1_6_Read_Error_Reference_Sequence_Dictionary_Description_Incorrect_Format
                           -- | MD5 checksum not in accepted format (only important
                           -- if @SQ tag is present).
                         | BAM_V1_6_Read_Error_Reference_Sequence_Dictionary_MD5_Checksum_Incorrect_Format
                           -- | Species not in accepted format (only important
                           -- if @SQ tag is present).
                         | BAM_V1_6_Read_Error_Reference_Sequence_Dictionary_Species_Incorrect_Format
                           -- | Molecule topology not in accepted format (only important
                           -- if @SQ tag is present).
                         | BAM_V1_6_Read_Error_Reference_Sequence_Dictionary_Molecule_Topology_Incorrect_Format
                           -- | Molecule topology invalid value (only important
                           -- if @SQ tag is present).
                         | BAM_V1_6_Read_Error_Reference_Sequence_Dictionary_Molecule_Topology_Invalid_Value
                           -- | URI not in accepted format (only important
                           -- if @SQ tag is present).
                         | BAM_V1_6_Read_Error_Reference_Sequence_Dictionary_URI_Incorrect_Format
                           -- | ID tag not accepted format (only important
                           -- if @RG tag is present).
                         | BAM_V1_6_Read_Error_Read_Group_Read_Group_Identifier_Incorrect_Format
                           -- | ID tag is missing (only important
                           -- if @RG tag is present).
                         | BAM_V1_6_Read_Error_Read_Group_Read_Group_Identifier_Tag_Missing
                           -- | BC tag not in accepted format (only important
                           -- if @RG tag is present).
                         | BAM_V1_6_Read_Error_Read_Group_Barcode_Sequence_Incorrect_Format
                           -- | CN tag not in accepted format (only important
                           -- if @RG tag is present).
                         | BAM_V1_6_Read_Error_Read_Group_Sequencing_Center_Incorrect_Format
                           -- | DS tag not in accepted format (only important
                           -- if @RG tag is present).
                         | BAM_V1_6_Read_Error_Read_Group_Description_Incorrect_Format
                           -- | DT tag not in accepted format (only important
                           -- if @RG tag is present).
                         | BAM_V1_6_Read_Error_Read_Group_Date_Run_Produced_Incorrect_Format
                           -- | FO tag not in accepted format (only important
                           -- if @RG tag is present).
                         | BAM_V1_6_Read_Error_Read_Group_Flow_Order_Incorrect_Format
                           -- | KS tag not in accepted format (only important
                           -- if @RG tag is present).
                         | BAM_V1_6_Read_Error_Read_Group_Key_Sequence_Incorrect_Format
                           -- | LB tag not in accepted format (only important
                           -- if @RG tag is present).
                         | BAM_V1_6_Read_Error_Read_Group_Library_Incorrect_Format
                           -- | PG tag not in accepted format (only important
                           -- if @RG tag is present).
                         | BAM_V1_6_Read_Error_Read_Group_Programs_Incorrect_Format
                           -- | PI tag not in accepted format (only important
                           -- if @RG tag is present).
                         | BAM_V1_6_Read_Error_Read_Group_Predicted_Median_Insert_Size_Incorrect_Format
                           -- | PL tag not in accepted format (only important
                           -- if @RG tag is present).
                         | BAM_V1_6_Read_Error_Read_Group_Platform_Incorrect_Format
                           -- | PM tag not in accepted format (only important
                           -- if @RG tag is present).
                         | BAM_V1_6_Read_Error_Read_Group_Platform_Model_Incorrect_Format
                           -- | PU tag not in accepted format (only important
                           -- if @RG tag is present).
                         | BAM_V1_6_Read_Error_Read_Group_Platform_Unit_Incorrect_Format
                           -- | SM tag not in accepted format (only important
                           -- if @RG tag is present).
                         | BAM_V1_6_Read_Error_Read_Group_Sample_Incorrect_Format
                           -- | RG tag not in accepted format.
                         | BAM_V1_6_Read_Error_Read_Group_Tag_Incorrect_Format
                           -- | ID tag not in accepted format (only important
                           -- if @PG tag is present).
                         | BAM_V1_6_Read_Error_Program_Identifier_Incorrect_Format
                           -- | PN tag not in accepted format (only important
                           -- if @PG tag is present).
                         | BAM_V1_6_Read_Error_Program_Name_Incorrect_Format
                           -- | CL tag not in accepted format (only important
                           -- if @PG tag is present).
                         | BAM_V1_6_Read_Error_Program_Command_Line_Incorrect_Format
                           -- | PP tag not in accepted format (only important
                           -- if @PG tag is present).
                         | BAM_V1_6_Read_Error_Program_Previous_PG_ID_Incorrect_Format
                           -- | DS tag not in accepted format (only important
                           -- if @PG tag is present).
                         | BAM_V1_6_Read_Error_Program_Description_Incorrect_Format
                           -- | VN tag not in accepted format (only important
                           -- if @PG tag is present).
                         | BAM_V1_6_Read_Error_Program_Version_Incorrect_Format
                           -- | PG tag not in accepted format.
                         | BAM_V1_6_Read_Error_Program_Tag_Incorrect_Format
                           -- | CO tag not in accepted format.
                         | BAM_V1_6_Read_Error_One_Line_Comment_Tag_Incorrect_Format
                           -- | QNAME of alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_QNAME_Incorrect_Format
                           -- | FLAG of alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_FLAG_Incorrect_Format
                           -- | RNAME of alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_RNAME_Incorrect_Format
                           -- | POS of alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_POS_Incorrect_Format
                           -- | MAPQ of alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_MAPQ_Incorrect_Format
                           -- | CIGAR of alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_CIGAR_Incorrect_Format
                           -- | RNEXT of alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_RNEXT_Incorrect_Format
                           -- | PNEXT of alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_PNEXT_Incorrect_Format
                           -- | TLEN of alignment section not in accpepted format.
                         | BAM_V1_6_Read_Error_Alignment_TLEN_Incorrect_Format
                           -- | SEQ of alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_SEQ_Incorrect_Format
                           -- | QUAL of alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_QUAL_Incorrect_Format
                           -- | Optional field is missing a type.
                         | BAM_V1_6_Read_Error_Alignment_OptionalFields_Missing_Type
                           -- | Optional field BOPT is missing a type.
                         | BAM_V1_6_Read_Error_Alignment_OptionalFields_BOPT_Missing_Type
                           -- | Optional field doesn't have a type.
                         | BAM_V1_6_Read_Error_Alignment_OptionalFields_Index_Missing
                           -- | Optional field has left-over bytes following parsing.
                         | BAM_V1_6_Read_Error_Alignment_OptionalFields_Remaining_Bytes
                           -- | AOPT tag of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_AOPT_Tag_Incorrect_Format
                           -- | AOPT type of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_AOPT_Type_Incorrect_Format
                           -- | AOPT value of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_AOPT_Value_Incorrect_Format
                           -- | BigCOPT tag of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_BigCOPT_Tag_Incorrect_Format
                           -- | BigCOPT type of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_BigCOPT_Type_Incorrect_Format
                           -- | BigIOPT tag of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_BigIOPT_Tag_Incorrect_Format
                           -- | BigIOPT type of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_BigIOPT_Type_Incorrect_Format
                           -- | BigSOPT tag of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_BigSOPT_Tag_Incorrect_Format
                           -- | BigSOPT type of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_BigSOPT_Type_Incorrect_Format
                           -- | SmallCOPT tag of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_SmallCOPT_Tag_Incorrect_Format
                           -- | SmallCOPT type of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_SmallCOPT_Type_Incorrect_Format
                           -- | SmallIOPT tag of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_SmallIOPT_Tag_Incorrect_Format
                           -- | SmallIOPT type of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_SmallIOPT_Type_Incorrect_Format
                           -- | SmallSOPT tag of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_SmallSOPT_Tag_Incorrect_Format
                           -- | SmallSOPT type of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_SmallSOPT_Type_Incorrect_Format
                           -- | FOPT tag of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_FOPT_Tag_Incorrect_Format
                           -- | FOPT type of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_FOPT_Type_Incorrect_Format
                           -- | FOPT value of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_FOPT_Value_Incorrect_Format
                           -- | ZOPT tag of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_ZOPT_Tag_Incorrect_Format
                           -- | ZOPT type of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_ZOPT_Type_Incorrect_Format
                           -- | ZOPT value of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_ZOPT_Value_Incorrect_Format
                           -- | HOPT tag of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_HOPT_Tag_Incorrect_Format
                           -- | HOPT type of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_HOPT_Type_Incorrect_Format
                           -- | HOPT value of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_HOPT_Value_Incorrect_Format
                           -- | BOPT tag of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_BOPT_Tag_Incorrect_Format
                           -- | BOPT type of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_BOPT_Type_Incorrect_Format
                           -- | BOPT value type of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_BOPT_Value_Type_Incorrect_Format
                           -- | BOPT value data of the alignment section not in accepted format.
                         | BAM_V1_6_Read_Error_Alignment_BOPT_Value_Data_Incorrect_Format
                           -- | The CDATA field of a BGZF block not in accepted format.
                         | BAM_V1_6_Read_Error_CDATA_Incorrect_Format
                           -- | A CDATA BAM block alignment optional fields
                           -- are not in accepted format.
                         | BAM_V1_6_Read_Error_CDATA_BAM_Alignment_Optional_Fields_Incorrect_Format
                           -- | CRC-32 (CRC32) field of the GZip header
                           -- not in accepted format.
                         | BAM_V1_6_Read_Error_GZipHeader_CRC32_Incorrect_Format
                           -- | Input Size (Input SIZE, i.e. length of uncompressed data)
                           -- not in accepted format.
                         | BAM_V1_6_Read_Error_GZipHeader_Input_Size_Incorrect_Format
                           -- | The calculated crc32 of the uncompressed CDATA
                           -- is not equivalent to the CRC32 field of the BGZF block.
                         | BAM_V1_6_Read_Error_Calculated_CRC32_Not_Equivalent_To_CRC32
                           -- | The last 28 bytes of the BAM file are not equivalent
                           -- to the standardized 28 byte end-of-file marker.
                         | BAM_V1_6_Read_Error_End_Of_File_Marker_Incorrect_Format 
  deriving (Eq,Generic,Show,Typeable)
