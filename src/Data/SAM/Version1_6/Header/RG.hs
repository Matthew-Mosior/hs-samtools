{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# Language QuasiQuotes           #-}

-- |
-- Module      :  Data.SAM.Version1_6.Header.RG
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

module Data.SAM.Version1_6.Header.RG ( -- * SAM version 1.6 Read group data type
                                       SAM_V1_6_Read_Group(..),
                                       -- * SAM version 1.6 Read group data types
                                       SAM_V1_6_Read_Group_Identifier(..),
                                       SAM_V1_6_Read_Group_Barcode_Sequence(..),
                                       SAM_V1_6_Read_Group_Sequencing_Center(..),
                                       SAM_V1_6_Read_Group_Description(..),
                                       SAM_V1_6_Read_Group_Run_Date(..),
                                       SAM_V1_6_Read_Group_Flow_Order(..),
                                       SAM_V1_6_Read_Group_Key_Sequence(..),
                                       SAM_V1_6_Read_Group_Library(..),
                                       SAM_V1_6_Read_Group_Programs(..),
                                       SAM_V1_6_Read_Group_Predicted_Median_Insert_Size(..),
                                       SAM_V1_6_Read_Group_Platform(..),
                                       SAM_V1_6_Read_Group_Platform_Model(..),
                                       SAM_V1_6_Read_Group_Platform_Unit(..),
                                       SAM_V1_6_Read_Group_Sample(..)
                                     ) where

import Data.ByteString
import Data.Data
import Data.Sequence
import Data.Word
import Generics.Deriving.Base


-- | Custom SAM (version 1.6) @"SAM_V1_6_Read_Group"@ data type.
-- See section 1.3 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Read_Group = SAM_V1_6_Read_Group { sam_v1_6_read_group_identifer                    :: SAM_V1_6_Read_Group_Identifier
                                               , sam_v1_6_read_group_barcode_sequence             :: Maybe SAM_V1_6_Read_Group_Barcode_Sequence
                                               , sam_v1_6_read_group_sequencing_center            :: Maybe SAM_V1_6_Read_Group_Sequencing_Center
                                               , sam_v1_6_read_group_description                  :: Maybe SAM_V1_6_Read_Group_Description
                                               , sam_v1_6_read_group_run_date                     :: Maybe SAM_V1_6_Read_Group_Run_Date
                                               , sam_v1_6_read_group_flow_order                   :: Maybe SAM_V1_6_Read_Group_Flow_Order
                                               , sam_v1_6_read_group_key_sequence                 :: Maybe SAM_V1_6_Read_Group_Key_Sequence
                                               , sam_v1_6_read_group_library                      :: Maybe SAM_V1_6_Read_Group_Library
                                               , sam_v1_6_read_group_programs                     :: Maybe SAM_V1_6_Read_Group_Programs
                                               , sam_v1_6_read_group_predicted_median_insert_size :: Maybe SAM_V1_6_Read_Group_Predicted_Median_Insert_Size
                                               , sam_v1_6_read_group_platform                     :: Maybe SAM_V1_6_Read_Group_Platform
                                               , sam_v1_6_read_group_platform_model               :: Maybe SAM_V1_6_Read_Group_Platform_Model
                                               , sam_v1_6_read_group_platform_unit                :: Maybe SAM_V1_6_Read_Group_Platform_Unit
                                               , sam_v1_6_read_group_sample                       :: Maybe SAM_V1_6_Read_Group_Sample
                                               }

instance Show SAM_V1_6_Read_Group where
  show (SAM_V1_6_Read_Group group_identifier
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
    "SAM_V1_6_Read_Group { "                  ++
    "group_identifier = "                     ++
    (show group_identifier)                   ++
    " , barcode_sequence = "                  ++
    (show barcode_sequence)                   ++
    " , sequencing_center = "                 ++
    (show sequencing_center)                  ++
    " , description = "                       ++
    (show description)                        ++
    " , run_date = "                          ++
    (show run_date)                           ++
    " , flow_order = "                        ++
    (show flow_order)                         ++
    " , key_sequence = "                      ++
    (show key_sequence)                       ++
    " , library = "                           ++
    (show library)                            ++
    " , programs = "                          ++
    (show programs)                           ++
    " , show_predicted_median_insert_size = " ++
    (show predicted_median_insert_size)       ++
    " , platform = "                          ++
    (show platform)                           ++
    " , platform_model = "                    ++
    (show platform_model)                     ++
    " , platform_unit = "                     ++
    (show platform_unit)                      ++
    " , sample = "                            ++
    (show sample)                             ++
    " }"

-- | ID tag for @"SAM_V1_6_Read_Group"@.
data SAM_V1_6_Read_Group_Identifier = SAM_V1_6_Read_Group_Identifier { sam_v1_6_read_group_identifer_tag   :: Seq Word8
                                                                     , sam_v1_6_read_group_identifer_value :: ByteString
                                                                     }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Identifier where
  SAM_V1_6_Read_Group_Identifier sam_v1_6_read_group_identifier_tag1 sam_v1_6_read_group_identifier_value1 == SAM_V1_6_Read_Group_Identifier sam_v1_6_read_group_identifier_tag2 sam_v1_6_read_group_identifier_value2 = sam_v1_6_read_group_identifier_tag1 == sam_v1_6_read_group_identifier_tag2 && sam_v1_6_read_group_identifier_value1 == sam_v1_6_read_group_identifier_value2

instance Show SAM_V1_6_Read_Group_Identifier where
  show (SAM_V1_6_Read_Group_Identifier tag value) =
    "SAM_V1_6_Read_Group_Identifier { " ++
    "tag = "                            ++
    (show tag)                          ++
    " , value = "                       ++
    (show value)                        ++
    " }"

-- | BC tag for @"SAM_V1_6_Read_Group"@.
data SAM_V1_6_Read_Group_Barcode_Sequence = SAM_V1_6_Read_Group_Barcode_Sequence { sam_v1_6_read_group_barcode_sequence_tag   :: Seq Word8
                                                                                 , sam_v1_6_read_group_barcode_sequence_value :: ByteString
                                                                                 }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Barcode_Sequence where
  SAM_V1_6_Read_Group_Barcode_Sequence sam_v1_6_read_group_barcode_sequence_tag1 sam_v1_6_read_group_barcode_sequence_value1 == SAM_V1_6_Read_Group_Barcode_Sequence sam_v1_6_read_group_barcode_sequence_tag2 sam_v1_6_read_group_barcode_sequence_value2 = sam_v1_6_read_group_barcode_sequence_tag1 == sam_v1_6_read_group_barcode_sequence_tag2 && sam_v1_6_read_group_barcode_sequence_value1 == sam_v1_6_read_group_barcode_sequence_value2

instance Show SAM_V1_6_Read_Group_Barcode_Sequence where
  show (SAM_V1_6_Read_Group_Barcode_Sequence tag value) =
    "SAM_V1_6_Read_Group_Barcode_Sequence { " ++
    "tag = "                                  ++
    (show tag)                                ++
    " , value = "                             ++
    (show value)                              ++
    " }"

-- | CN tag for @"SAM_V1_6_Read_Group"@.
data SAM_V1_6_Read_Group_Sequencing_Center = SAM_V1_6_Read_Group_Sequencing_Center { sam_v1_6_read_group_sequencing_center_tag   :: Seq Word8
                                                                                   , sam_v1_6_read_group_sequencing_center_value :: ByteString
                                                                                   }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Sequencing_Center where
  SAM_V1_6_Read_Group_Sequencing_Center sam_v1_6_read_group_sequencing_center_tag1 sam_v1_6_read_group_sequencing_center_value1 == SAM_V1_6_Read_Group_Sequencing_Center sam_v1_6_read_group_sequencing_center_tag2 sam_v1_6_read_group_sequencing_center_value2 = sam_v1_6_read_group_sequencing_center_tag1 == sam_v1_6_read_group_sequencing_center_tag2 && sam_v1_6_read_group_sequencing_center_value1 == sam_v1_6_read_group_sequencing_center_value2

instance Show SAM_V1_6_Read_Group_Sequencing_Center where
  show (SAM_V1_6_Read_Group_Sequencing_Center tag value) =
    "SAM_V1_6_Read_Group_Sequencing_Center { " ++
    "tag = "                                   ++
    (show tag)                                 ++
    " , value = "                              ++
    (show value)                               ++
    " }"

-- | DS tag for @"SAM_V1_6_Read_Group"@.
data SAM_V1_6_Read_Group_Description = SAM_V1_6_Read_Group_Description { sam_v1_6_read_group_description_tag   :: Seq Word8
                                                                       , sam_v1_6_read_group_description_value :: ByteString
                                                                       }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Description where
  SAM_V1_6_Read_Group_Description sam_v1_6_read_group_description_tag1 sam_v1_6_read_group_description_value1 == SAM_V1_6_Read_Group_Description sam_v1_6_read_group_description_tag2 sam_v1_6_read_group_description_value2 = sam_v1_6_read_group_description_tag1 == sam_v1_6_read_group_description_tag2 && sam_v1_6_read_group_description_value1 == sam_v1_6_read_group_description_value2

instance Show SAM_V1_6_Read_Group_Description where
  show (SAM_V1_6_Read_Group_Description tag value) =
    "SAM_V1_6_Read_Group_Description { " ++
    "tag = "                             ++
    (show tag)                           ++
    " , value = "                        ++
    (show value)                         ++
    " }"

-- | DT tag for @"SAM_V1_6_Read_Group"@.
data SAM_V1_6_Read_Group_Run_Date = SAM_V1_6_Read_Group_Run_Date { sam_v1_6_read_group_run_date_tag   :: Seq Word8
                                                                 , sam_v1_6_read_group_run_date_value :: ByteString
                                                                 }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Run_Date where
  SAM_V1_6_Read_Group_Run_Date sam_v1_6_read_group_run_date_tag1 sam_v1_6_read_group_run_date_value1 == SAM_V1_6_Read_Group_Run_Date sam_v1_6_read_group_run_date_tag2 sam_v1_6_read_group_run_date_value2 = sam_v1_6_read_group_run_date_tag1 == sam_v1_6_read_group_run_date_tag2 && sam_v1_6_read_group_run_date_value1 == sam_v1_6_read_group_run_date_value2

instance Show SAM_V1_6_Read_Group_Run_Date where
  show (SAM_V1_6_Read_Group_Run_Date tag value) =
    "SAM_V1_6_Read_Group_Run_Date { " ++
    "tag = "                          ++
    (show tag)                        ++
    " , value = "                     ++
    (show value)                      ++
    " }"

-- | FO tag for @"SAM_V1_6_Read_Group"@.
data SAM_V1_6_Read_Group_Flow_Order = SAM_V1_6_Read_Group_Flow_Order { sam_v1_6_read_group_flow_order_tag :: Seq Word8
                                                                     , sam_v1_6_read_group_flow_order_value :: ByteString
                                                                     }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Flow_Order where
  SAM_V1_6_Read_Group_Flow_Order sam_v1_6_read_group_flow_order_tag1 sam_v1_6_one_line_comment_value1 == SAM_V1_6_Read_Group_Flow_Order sam_v1_6_read_group_flow_order_tag2 sam_v1_6_read_group_flow_order_value2 = sam_v1_6_read_group_flow_order_tag1 == sam_v1_6_read_group_flow_order_tag2 && sam_v1_6_one_line_comment_value1 == sam_v1_6_read_group_flow_order_value2

instance Show SAM_V1_6_Read_Group_Flow_Order where
  show (SAM_V1_6_Read_Group_Flow_Order tag value) =
    "SAM_V1_6_Read_Group_Flow_Order { " ++
    "tag = "                            ++
    (show tag)                          ++
    " , value = "                       ++
    (show value)                        ++
    " }"

-- | KS tag for @"SAM_V1_6_Read_Group"@.
data SAM_V1_6_Read_Group_Key_Sequence = SAM_V1_6_Read_Group_Key_Sequence { sam_v1_6_read_group_key_sequence_tag   :: Seq Word8
                                                                         , sam_v1_6_read_group_key_sequence_value :: ByteString
                                                                         }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Key_Sequence where
  SAM_V1_6_Read_Group_Key_Sequence sam_v1_6_read_group_key_sequence_tag1 sam_v1_6_read_group_key_sequence_value1 == SAM_V1_6_Read_Group_Key_Sequence sam_v1_6_read_group_key_sequence_tag2 sam_v1_6_read_group_key_sequence_value2 = sam_v1_6_read_group_key_sequence_tag1 == sam_v1_6_read_group_key_sequence_tag2 && sam_v1_6_read_group_key_sequence_value1 == sam_v1_6_read_group_key_sequence_value2

instance Show SAM_V1_6_Read_Group_Key_Sequence where
  show (SAM_V1_6_Read_Group_Key_Sequence tag value) =
    "SAM_V1_6_Read_Group_Key_Sequence { " ++
    "tag = "                              ++
    (show tag)                            ++
    " , value = "                         ++
    (show value)                          ++
    " }"

-- | LB tag for @"SAM_V1_6_Read_Group"@.
data SAM_V1_6_Read_Group_Library = SAM_V1_6_Read_Group_Library { sam_v1_6_read_group_library_tag   :: Seq Word8
                                                               , sam_v1_6_read_group_library_value :: ByteString
                                                               }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Library where
  SAM_V1_6_Read_Group_Library sam_v1_6_read_group_library_tag1 sam_v1_6_read_group_library_value1 == SAM_V1_6_Read_Group_Library sam_v1_6_read_group_library_tag2 sam_v1_6_read_group_library_value2 = sam_v1_6_read_group_library_tag1 == sam_v1_6_read_group_library_tag2 && sam_v1_6_read_group_library_value1 == sam_v1_6_read_group_library_value2

instance Show SAM_V1_6_Read_Group_Library where
  show (SAM_V1_6_Read_Group_Library tag value) =
    "SAM_V1_6_Read_Group_Library { " ++
    "tag = "                         ++
    (show tag)                       ++
    " , value = "                    ++
    (show value)                     ++
    " }"

-- | PG tag for @"SAM_V1_6_Read_Group"@.
data SAM_V1_6_Read_Group_Programs = SAM_V1_6_Read_Group_Programs { sam_v1_6_read_group_programs_tag   :: Seq Word8
                                                                 , sam_v1_6_read_group_programs_value :: ByteString
                                                                 }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Programs where
  SAM_V1_6_Read_Group_Programs sam_v1_6_read_group_programs_tag1 sam_v1_6_read_group_programs_value1 == SAM_V1_6_Read_Group_Programs sam_v1_6_read_group_programs_tag2 sam_v1_6_read_group_programs_value2 = sam_v1_6_read_group_programs_tag1 == sam_v1_6_read_group_programs_tag2 && sam_v1_6_read_group_programs_value1 == sam_v1_6_read_group_programs_value2

instance Show SAM_V1_6_Read_Group_Programs where
  show (SAM_V1_6_Read_Group_Programs tag value) =
    "SAM_V1_6_Read_Group_Programs { " ++
    "tag = "                          ++
    (show tag)                        ++
    " , value = "                     ++
    (show value)                      ++
    " }"

-- | PI tag for @"SAM_V1_6_Read_Group"@.
data SAM_V1_6_Read_Group_Predicted_Median_Insert_Size = SAM_V1_6_Read_Group_Predicted_Median_Insert_Size { sam_v1_6_read_group_predicted_median_insert_size_tag   :: Seq Word8
                                                                                                         , sam_v1_6_read_group_predicted_median_insert_size_value :: ByteString
                                                                                                         }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Predicted_Median_Insert_Size where
  SAM_V1_6_Read_Group_Predicted_Median_Insert_Size sam_v1_6_read_group_predicted_median_insert_size_tag1 sam_v1_6_one_line_comment_value1 == SAM_V1_6_Read_Group_Predicted_Median_Insert_Size sam_v1_6_read_group_predicted_median_insert_size_tag2 sam_v1_6_read_group_predicted_median_insert_size_value2 = sam_v1_6_read_group_predicted_median_insert_size_tag1 == sam_v1_6_read_group_predicted_median_insert_size_tag2 && sam_v1_6_one_line_comment_value1 == sam_v1_6_read_group_predicted_median_insert_size_value2

instance Show SAM_V1_6_Read_Group_Predicted_Median_Insert_Size where
  show (SAM_V1_6_Read_Group_Predicted_Median_Insert_Size tag value) =
    "SAM_V1_6_Read_Group_Predicted_Median_Insert_Size { " ++
    "tag = "                                              ++
    (show tag)                                            ++
    " , value = "                                         ++
    (show value)                                          ++
    " }"

-- | PL tag for @"SAM_V1_6_Read_Group"@.
data SAM_V1_6_Read_Group_Platform = SAM_V1_6_Read_Group_Platform { sam_v1_6_read_group_platform_tag   :: Seq Word8
                                                                 , sam_v1_6_read_group_platform_value :: ByteString
                                                                 }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Platform where
  SAM_V1_6_Read_Group_Platform sam_v1_6_read_group_platform_tag1 sam_v1_6_read_group_platform_value1 == SAM_V1_6_Read_Group_Platform sam_v1_6_read_group_platform_tag2 sam_v1_6_read_group_platform_value2 = sam_v1_6_read_group_platform_tag1 == sam_v1_6_read_group_platform_tag2 && sam_v1_6_read_group_platform_value1 == sam_v1_6_read_group_platform_value2

instance Show SAM_V1_6_Read_Group_Platform where
  show (SAM_V1_6_Read_Group_Platform tag value) =
    "SAM_V1_6_Read_Group_Platform { " ++
    "tag = "                          ++
    (show tag)                        ++
    " , value = "                     ++
    (show value)                      ++
    " }"

-- | PM tag for @"SAM_V1_6_Read_Group"@.
data SAM_V1_6_Read_Group_Platform_Model = SAM_V1_6_Read_Group_Platform_Model { sam_v1_6_read_group_platform_model_tag   :: Seq Word8
                                                                             , sam_v1_6_read_group_platform_model_value :: ByteString
                                                                             }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Platform_Model where
  SAM_V1_6_Read_Group_Platform_Model sam_v1_6_read_group_platform_model_tag1 sam_v1_6_read_group_platform_model_value1 == SAM_V1_6_Read_Group_Platform_Model sam_v1_6_read_group_platform_model_tag2 sam_v1_6_read_group_platform_model_value2 = sam_v1_6_read_group_platform_model_tag1 == sam_v1_6_read_group_platform_model_tag2 && sam_v1_6_read_group_platform_model_value1 == sam_v1_6_read_group_platform_model_value2

instance Show SAM_V1_6_Read_Group_Platform_Model where
  show (SAM_V1_6_Read_Group_Platform_Model tag value) =
    "SAM_V1_6_Read_Group_Platform_Model { " ++
    "tag = "                                ++
    (show tag)                              ++
    " , value = "                           ++
    (show value)                            ++
    " }"

-- | PU tag for @"SAM_V1_6_Read_Group"@.
data SAM_V1_6_Read_Group_Platform_Unit = SAM_V1_6_Read_Group_Platform_Unit { sam_v1_6_read_group_platform_unit_tag   :: Seq Word8
                                                                           , sam_v1_6_read_group_platform_unit_value :: ByteString
                                                                           }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Platform_Unit where
  SAM_V1_6_Read_Group_Platform_Unit sam_v1_6_read_group_platform_unit_tag1 sam_v1_6_read_group_platform_unit_value1 == SAM_V1_6_Read_Group_Platform_Unit sam_v1_6_read_group_platform_unit_tag2 sam_v1_6_read_group_platform_unit_value2 = sam_v1_6_read_group_platform_unit_tag1 == sam_v1_6_read_group_platform_unit_tag2 && sam_v1_6_read_group_platform_unit_value1 == sam_v1_6_read_group_platform_unit_value2

instance Show SAM_V1_6_Read_Group_Platform_Unit where
  show (SAM_V1_6_Read_Group_Platform_Unit tag value) =
    "SAM_V1_6_Read_Group_Platform_Unit { " ++
    "tag = "                               ++
    (show tag)                             ++
    " , value = "                          ++
    (show value)                           ++
    " }"

-- | SM tag for @"SAM_V1_6_Read_Group"@.
data SAM_V1_6_Read_Group_Sample = SAM_V1_6_Read_Group_Sample { sam_v1_6_read_group_sample_tag   :: Seq Word8
                                                             , sam_v1_6_read_group_sample_value :: ByteString
                                                             }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Sample where
  SAM_V1_6_Read_Group_Sample sam_v1_6_read_group_sample_tag1 sam_v1_6_read_group_sample_value1 == SAM_V1_6_Read_Group_Sample sam_v1_6_read_group_sample_tag2 sam_v1_6_read_group_sample_value2 = sam_v1_6_read_group_sample_tag1 == sam_v1_6_read_group_sample_tag2 && sam_v1_6_read_group_sample_value1 == sam_v1_6_read_group_sample_value2

instance Show SAM_V1_6_Read_Group_Sample where
  show (SAM_V1_6_Read_Group_Sample tag value) =
    "SAM_V1_6_Read_Group_Sample { " ++
    "tag = "                        ++
    (show tag)                      ++
    " , value = "                   ++
    (show value)                    ++
    " }"
