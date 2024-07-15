{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.BAM.Header.RG
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.BAM.Header.RG ( -- * BAM version 1.6 Read group data type
                                           BAM_V1_6_Read_Group(..),
                                           -- * BAM version 1.6 Read group data types
                                           BAM_V1_6_Read_Group_Identifier(..),
                                           BAM_V1_6_Read_Group_Barcode_Sequence(..),
                                           BAM_V1_6_Read_Group_Sequencing_Center(..),
                                           BAM_V1_6_Read_Group_Description(..),
                                           BAM_V1_6_Read_Group_Run_Date(..),
                                           BAM_V1_6_Read_Group_Flow_Order(..),
                                           BAM_V1_6_Read_Group_Key_Sequence(..),
                                           BAM_V1_6_Read_Group_Library(..),
                                           BAM_V1_6_Read_Group_Programs(..),
                                           BAM_V1_6_Read_Group_Predicted_Median_Insert_Size(..),
                                           BAM_V1_6_Read_Group_Platform(..),
                                           BAM_V1_6_Read_Group_Platform_Model(..),
                                           BAM_V1_6_Read_Group_Platform_Unit(..),
                                           BAM_V1_6_Read_Group_Sample(..)
                                         ) where

import Data.ByteString
import Data.Data
import Generics.Deriving.Base

-- | Custom BAM (version 1.6) @"BAM_V1_6_Read_Group"@ data type.
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_Read_Group = BAM_V1_6_Read_Group { bam_v1_6_read_group_identifier                   :: BAM_V1_6_Read_Group_Identifier
                                               , bam_v1_6_read_group_barcode_sequence             :: Maybe BAM_V1_6_Read_Group_Barcode_Sequence
                                               , bam_v1_6_read_group_sequencing_center            :: Maybe BAM_V1_6_Read_Group_Sequencing_Center
                                               , bam_v1_6_read_group_description                  :: Maybe BAM_V1_6_Read_Group_Description
                                               , bam_v1_6_read_group_run_date                     :: Maybe BAM_V1_6_Read_Group_Run_Date
                                               , bam_v1_6_read_group_flow_order                   :: Maybe BAM_V1_6_Read_Group_Flow_Order
                                               , bam_v1_6_read_group_key_sequence                 :: Maybe BAM_V1_6_Read_Group_Key_Sequence
                                               , bam_v1_6_read_group_library                      :: Maybe BAM_V1_6_Read_Group_Library
                                               , bam_v1_6_read_group_programs                     :: Maybe BAM_V1_6_Read_Group_Programs
                                               , bam_v1_6_read_group_predicted_median_insert_size :: Maybe BAM_V1_6_Read_Group_Predicted_Median_Insert_Size
                                               , bam_v1_6_read_group_platform                     :: Maybe BAM_V1_6_Read_Group_Platform
                                               , bam_v1_6_read_group_platform_model               :: Maybe BAM_V1_6_Read_Group_Platform_Model
                                               , bam_v1_6_read_group_platform_unit                :: Maybe BAM_V1_6_Read_Group_Platform_Unit
                                               , bam_v1_6_read_group_sample                       :: Maybe BAM_V1_6_Read_Group_Sample
                                               }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Read_Group where
  BAM_V1_6_Read_Group bam_v1_6_read_group_identifier1
                      bam_v1_6_read_group_barcode_sequence1
                      bam_v1_6_read_group_sequencing_center1
                      bam_v1_6_read_group_description1
                      bam_v1_6_read_group_run_date1
                      bam_v1_6_read_group_flow_order1
                      bam_v1_6_read_group_key_sequence1
                      bam_v1_6_read_group_library1
                      bam_v1_6_read_group_programs1
                      bam_v1_6_read_group_predicted_median_insert_size1
                      bam_v1_6_read_group_platform1
                      bam_v1_6_read_group_platform_model1
                      bam_v1_6_read_group_platform_unit1
                      bam_v1_6_read_group_sample1 ==
    BAM_V1_6_Read_Group bam_v1_6_read_group_identifier2
                        bam_v1_6_read_group_barcode_sequence2
                        bam_v1_6_read_group_sequencing_center2
                        bam_v1_6_read_group_description2
                        bam_v1_6_read_group_run_date2
                        bam_v1_6_read_group_flow_order2
                        bam_v1_6_read_group_key_sequence2
                        bam_v1_6_read_group_library2
                        bam_v1_6_read_group_programs2
                        bam_v1_6_read_group_predicted_median_insert_size2
                        bam_v1_6_read_group_platform2
                        bam_v1_6_read_group_platform_model2
                        bam_v1_6_read_group_platform_unit2
                        bam_v1_6_read_group_sample2 =
      bam_v1_6_read_group_identifier1                   == bam_v1_6_read_group_identifier2                   &&
      bam_v1_6_read_group_barcode_sequence1             == bam_v1_6_read_group_barcode_sequence2             &&
      bam_v1_6_read_group_sequencing_center1            == bam_v1_6_read_group_sequencing_center2            &&
      bam_v1_6_read_group_description1                  == bam_v1_6_read_group_description2                  &&
      bam_v1_6_read_group_run_date1                     == bam_v1_6_read_group_run_date2                     &&
      bam_v1_6_read_group_flow_order1                   == bam_v1_6_read_group_flow_order2                   &&
      bam_v1_6_read_group_key_sequence1                 == bam_v1_6_read_group_key_sequence2                 &&
      bam_v1_6_read_group_library1                      == bam_v1_6_read_group_library2                      &&
      bam_v1_6_read_group_programs1                     == bam_v1_6_read_group_programs2                     &&
      bam_v1_6_read_group_predicted_median_insert_size1 == bam_v1_6_read_group_predicted_median_insert_size2 &&
      bam_v1_6_read_group_platform1                     == bam_v1_6_read_group_platform2                     &&
      bam_v1_6_read_group_platform_model1               == bam_v1_6_read_group_platform_model2               &&
      bam_v1_6_read_group_platform_unit1                == bam_v1_6_read_group_platform_unit2                &&
      bam_v1_6_read_group_sample1                       == bam_v1_6_read_group_sample2

instance Show BAM_V1_6_Read_Group where
  show (BAM_V1_6_Read_Group group_identifier
                            barcode_sequence
                            sequencing_center
                            description
                            run_date
                            flow_order
                            key_sequence
                            library
                            programs
                            predicted_median_insert_size
                            platform
                            platform_model
                            platform_unit
                            sample
       ) =
    "BAM_V1_6_Read_Group { "                                      ++
    "bam_v1_6_read_group_identifier = "                           ++
    (show group_identifier)                                       ++
    " , bam_v1_6_read_group_barcode_sequence = "                  ++
    (show barcode_sequence)                                       ++
    " , bam_v1_6_read_group_sequencing_center = "                 ++
    (show sequencing_center)                                      ++
    " , bam_v1_6_read_group_description = "                       ++
    (show description)                                            ++
    " , bam_v1_6_read_group_run_date = "                          ++
    (show run_date)                                               ++
    " , bam_v1_6_read_group_flow_order = "                        ++
    (show flow_order)                                             ++
    " , bam_v1_6_read_group_key_sequence = "                      ++
    (show key_sequence)                                           ++
    " , bam_v1_6_read_group_library = "                           ++
    (show library)                                                ++
    " , bam_v1_6_read_group_programs = "                          ++
    (show programs)                                               ++
    " , bam_v1_6_read_group_show_predicted_median_insert_size = " ++
    (show predicted_median_insert_size)                           ++
    " , bam_v1_6_read_group_platform = "                          ++
    (show platform)                                               ++
    " , bam_v1_6_read_group_platform_model = "                    ++
    (show platform_model)                                         ++
    " , bam_v1_6_read_group_platform_unit = "                     ++
    (show platform_unit)                                          ++
    " , bam_v1_6_read_group_sample = "                            ++
    (show sample)                                                 ++
    " }"

-- | ID tag for @"BAM_V1_6_Read_Group"@.
newtype BAM_V1_6_Read_Group_Identifier = BAM_V1_6_Read_Group_Identifier { bam_v1_6_read_group_identifier_value :: ByteString
                                                                        }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Read_Group_Identifier where
  BAM_V1_6_Read_Group_Identifier bam_v1_6_read_group_identifier_value1 ==
    BAM_V1_6_Read_Group_Identifier bam_v1_6_read_group_identifier_value2 =
      bam_v1_6_read_group_identifier_value1 == bam_v1_6_read_group_identifier_value2

instance Show BAM_V1_6_Read_Group_Identifier where
  show (BAM_V1_6_Read_Group_Identifier value) =
    "BAM_V1_6_Read_Group_Identifier { "       ++
    "bam_v1_6_read_group_identifier_value = " ++
    (show value)                              ++
    " }"

-- | BC tag for @"BAM_V1_6_Read_Group"@.
newtype BAM_V1_6_Read_Group_Barcode_Sequence = BAM_V1_6_Read_Group_Barcode_Sequence { bam_v1_6_read_group_barcode_sequence_value :: ByteString
                                                                                    }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Read_Group_Barcode_Sequence where
  BAM_V1_6_Read_Group_Barcode_Sequence bam_v1_6_read_group_barcode_sequence_value1 ==
    BAM_V1_6_Read_Group_Barcode_Sequence bam_v1_6_read_group_barcode_sequence_value2 =
      bam_v1_6_read_group_barcode_sequence_value1 == bam_v1_6_read_group_barcode_sequence_value2

instance Show BAM_V1_6_Read_Group_Barcode_Sequence where
  show (BAM_V1_6_Read_Group_Barcode_Sequence value) =
    "BAM_V1_6_Read_Group_Barcode_Sequence { "       ++
    "bam_v1_6_read_group_barcode_sequence_value = " ++
    (show value)                                    ++
    " }"

-- | CN tag for @"BAM_V1_6_Read_Group"@.
newtype BAM_V1_6_Read_Group_Sequencing_Center = BAM_V1_6_Read_Group_Sequencing_Center { bam_v1_6_read_group_sequencing_center_value :: ByteString
                                                                                      }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Read_Group_Sequencing_Center where
  BAM_V1_6_Read_Group_Sequencing_Center bam_v1_6_read_group_sequencing_center_value1 ==
    BAM_V1_6_Read_Group_Sequencing_Center bam_v1_6_read_group_sequencing_center_value2 =
      bam_v1_6_read_group_sequencing_center_value1 == bam_v1_6_read_group_sequencing_center_value2

instance Show BAM_V1_6_Read_Group_Sequencing_Center where
  show (BAM_V1_6_Read_Group_Sequencing_Center value) =
    "BAM_V1_6_Read_Group_Sequencing_Center { "       ++
    "bam_v1_6_read_group_sequencing_center_value = " ++
    (show value)                                     ++
    " }"

-- | DS tag for @"BAM_V1_6_Read_Group"@.
newtype BAM_V1_6_Read_Group_Description = BAM_V1_6_Read_Group_Description { bam_v1_6_read_group_description_value :: ByteString
                                                                          }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Read_Group_Description where
  BAM_V1_6_Read_Group_Description bam_v1_6_read_group_description_value1 ==
    BAM_V1_6_Read_Group_Description bam_v1_6_read_group_description_value2 =
      bam_v1_6_read_group_description_value1 == bam_v1_6_read_group_description_value2

instance Show BAM_V1_6_Read_Group_Description where
  show (BAM_V1_6_Read_Group_Description value) =
    "BAM_V1_6_Read_Group_Description { "       ++
    "bam_v1_6_read_group_description_value = " ++
    (show value)                               ++
    " }"

-- | DT tag for @"BAM_V1_6_Read_Group"@.
newtype BAM_V1_6_Read_Group_Run_Date = BAM_V1_6_Read_Group_Run_Date { bam_v1_6_read_group_run_date_value :: ByteString
                                                                    }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Read_Group_Run_Date where
  BAM_V1_6_Read_Group_Run_Date bam_v1_6_read_group_run_date_value1 ==
    BAM_V1_6_Read_Group_Run_Date bam_v1_6_read_group_run_date_value2 =
      bam_v1_6_read_group_run_date_value1 == bam_v1_6_read_group_run_date_value2

instance Show BAM_V1_6_Read_Group_Run_Date where
  show (BAM_V1_6_Read_Group_Run_Date value) =
    "BAM_V1_6_Read_Group_Run_Date { "       ++
    "bam_v1_6_read_group_run_date_value = " ++
    (show value)                            ++
    " }"

-- | FO tag for @"BAM_V1_6_Read_Group"@.
newtype BAM_V1_6_Read_Group_Flow_Order = BAM_V1_6_Read_Group_Flow_Order { bam_v1_6_read_group_flow_order_value :: ByteString
                                                                        }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Read_Group_Flow_Order where
  BAM_V1_6_Read_Group_Flow_Order bam_v1_6_one_line_comment_value1 ==
    BAM_V1_6_Read_Group_Flow_Order bam_v1_6_read_group_flow_order_value2 =
      bam_v1_6_one_line_comment_value1 == bam_v1_6_read_group_flow_order_value2

instance Show BAM_V1_6_Read_Group_Flow_Order where
  show (BAM_V1_6_Read_Group_Flow_Order value) =
    "BAM_V1_6_Read_Group_Flow_Order { "       ++
    "bam_v1_6_read_group_flow_order_value = " ++
    (show value)                              ++
    " }"

-- | KS tag for @"BAM_V1_6_Read_Group"@.
newtype BAM_V1_6_Read_Group_Key_Sequence = BAM_V1_6_Read_Group_Key_Sequence { bam_v1_6_read_group_key_sequence_value :: ByteString
                                                                            }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Read_Group_Key_Sequence where
  BAM_V1_6_Read_Group_Key_Sequence bam_v1_6_read_group_key_sequence_value1 ==
    BAM_V1_6_Read_Group_Key_Sequence bam_v1_6_read_group_key_sequence_value2 =
      bam_v1_6_read_group_key_sequence_value1 == bam_v1_6_read_group_key_sequence_value2

instance Show BAM_V1_6_Read_Group_Key_Sequence where
  show (BAM_V1_6_Read_Group_Key_Sequence value) =
    "BAM_V1_6_Read_Group_Key_Sequence { "       ++
    "bam_v1_6_read_group_key_sequence_value = " ++
    (show value)                                ++
    " }"

-- | LB tag for @"BAM_V1_6_Read_Group"@.
newtype BAM_V1_6_Read_Group_Library = BAM_V1_6_Read_Group_Library { bam_v1_6_read_group_library_value :: ByteString
                                                                  }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Read_Group_Library where
  BAM_V1_6_Read_Group_Library bam_v1_6_read_group_library_value1 ==
    BAM_V1_6_Read_Group_Library bam_v1_6_read_group_library_value2 =
      bam_v1_6_read_group_library_value1 == bam_v1_6_read_group_library_value2

instance Show BAM_V1_6_Read_Group_Library where
  show (BAM_V1_6_Read_Group_Library value) =
    "BAM_V1_6_Read_Group_Library { "       ++
    "bam_v1_6_read_group_library_value = " ++
    (show value)                           ++
    " }"

-- | PG tag for @"BAM_V1_6_Read_Group"@.
newtype BAM_V1_6_Read_Group_Programs = BAM_V1_6_Read_Group_Programs { bam_v1_6_read_group_programs_value :: ByteString
                                                                    }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Read_Group_Programs where
  BAM_V1_6_Read_Group_Programs bam_v1_6_read_group_programs_value1 ==
    BAM_V1_6_Read_Group_Programs bam_v1_6_read_group_programs_value2 =
      bam_v1_6_read_group_programs_value1 == bam_v1_6_read_group_programs_value2

instance Show BAM_V1_6_Read_Group_Programs where
  show (BAM_V1_6_Read_Group_Programs value) =
    "BAM_V1_6_Read_Group_Programs { "       ++
    "bam_v1_6_read_group_programs_value = " ++
    (show value)                            ++
    " }"

-- | PI tag for @"BAM_V1_6_Read_Group"@.
newtype BAM_V1_6_Read_Group_Predicted_Median_Insert_Size = BAM_V1_6_Read_Group_Predicted_Median_Insert_Size { bam_v1_6_read_group_predicted_median_insert_size_value :: ByteString
                                                                                                            }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Read_Group_Predicted_Median_Insert_Size where
  BAM_V1_6_Read_Group_Predicted_Median_Insert_Size bam_v1_6_read_group_predicted_median_insert_size_value1 ==
    BAM_V1_6_Read_Group_Predicted_Median_Insert_Size bam_v1_6_read_group_predicted_median_insert_size_value2 =
      bam_v1_6_read_group_predicted_median_insert_size_value1 == bam_v1_6_read_group_predicted_median_insert_size_value2

instance Show BAM_V1_6_Read_Group_Predicted_Median_Insert_Size where
  show (BAM_V1_6_Read_Group_Predicted_Median_Insert_Size value) =
    "BAM_V1_6_Read_Group_Predicted_Median_Insert_Size { "       ++
    "bam_v1_6_read_group_predicted_median_insert_size_value = " ++
    (show value)                                                ++
    " }"

-- | PL tag for @"BAM_V1_6_Read_Group"@.
newtype BAM_V1_6_Read_Group_Platform = BAM_V1_6_Read_Group_Platform { bam_v1_6_read_group_platform_value :: ByteString
                                                                    }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Read_Group_Platform where
  BAM_V1_6_Read_Group_Platform bam_v1_6_read_group_platform_value1 ==
    BAM_V1_6_Read_Group_Platform bam_v1_6_read_group_platform_value2 =
      bam_v1_6_read_group_platform_value1 == bam_v1_6_read_group_platform_value2

instance Show BAM_V1_6_Read_Group_Platform where
  show (BAM_V1_6_Read_Group_Platform value) =
    "BAM_V1_6_Read_Group_Platform { "       ++
    "bam_v1_6_read_group_platform_value = " ++
    (show value)                            ++
    " }"

-- | PM tag for @"BAM_V1_6_Read_Group"@.
newtype BAM_V1_6_Read_Group_Platform_Model = BAM_V1_6_Read_Group_Platform_Model { bam_v1_6_read_group_platform_model_value :: ByteString
                                                                                }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Read_Group_Platform_Model where
  BAM_V1_6_Read_Group_Platform_Model bam_v1_6_read_group_platform_model_value1 ==
    BAM_V1_6_Read_Group_Platform_Model bam_v1_6_read_group_platform_model_value2 =
      bam_v1_6_read_group_platform_model_value1 == bam_v1_6_read_group_platform_model_value2

instance Show BAM_V1_6_Read_Group_Platform_Model where
  show (BAM_V1_6_Read_Group_Platform_Model value) =
    "BAM_V1_6_Read_Group_Platform_Model { "       ++
    "bam_v1_6_read_group_platform_model_value = " ++
    (show value)                                  ++
    " }"

-- | PU tag for @"BAM_V1_6_Read_Group"@.
newtype BAM_V1_6_Read_Group_Platform_Unit = BAM_V1_6_Read_Group_Platform_Unit { bam_v1_6_read_group_platform_unit_value :: ByteString
                                                                              }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Read_Group_Platform_Unit where
  BAM_V1_6_Read_Group_Platform_Unit bam_v1_6_read_group_platform_unit_value1 ==
    BAM_V1_6_Read_Group_Platform_Unit bam_v1_6_read_group_platform_unit_value2 =
      bam_v1_6_read_group_platform_unit_value1 == bam_v1_6_read_group_platform_unit_value2

instance Show BAM_V1_6_Read_Group_Platform_Unit where
  show (BAM_V1_6_Read_Group_Platform_Unit value) =
    "BAM_V1_6_Read_Group_Platform_Unit { "       ++
    "bam_v1_6_read_group_platform_unit_value = " ++
    (show value)                                 ++
    " }"

-- | SM tag for @"BAM_V1_6_Read_Group"@.
newtype BAM_V1_6_Read_Group_Sample = BAM_V1_6_Read_Group_Sample { bam_v1_6_read_group_sample_value :: ByteString
                                                                }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Read_Group_Sample where
  BAM_V1_6_Read_Group_Sample bam_v1_6_read_group_sample_value1 ==
    BAM_V1_6_Read_Group_Sample bam_v1_6_read_group_sample_value2 =
      bam_v1_6_read_group_sample_value1 == bam_v1_6_read_group_sample_value2

instance Show BAM_V1_6_Read_Group_Sample where
  show (BAM_V1_6_Read_Group_Sample value) =
    "BAM_V1_6_Read_Group_Sample { "       ++
    "bam_v1_6_read_group_sample_value = " ++
    (show value)                          ++
    " }"
