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
import Generics.Deriving.Base

-- | Custom SAM (version 1.6) @"SAM_V1_6_Read_Group"@ data type.
--
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
    "read_group_identifier = "                ++
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
newtype SAM_V1_6_Read_Group_Identifier = SAM_V1_6_Read_Group_Identifier { sam_v1_6_read_group_identifier_value :: ByteString
                                                                        }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Identifier where
  SAM_V1_6_Read_Group_Identifier sam_v1_6_read_group_identifier_value1 == SAM_V1_6_Read_Group_Identifier sam_v1_6_read_group_identifier_value2 = sam_v1_6_read_group_identifier_value1 == sam_v1_6_read_group_identifier_value2

instance Show SAM_V1_6_Read_Group_Identifier where
  show (SAM_V1_6_Read_Group_Identifier value) =
    "SAM_V1_6_Read_Group_Identifier { " ++
    "value = "                          ++
    (show value)                        ++
    " }"

-- | BC tag for @"SAM_V1_6_Read_Group"@.
newtype SAM_V1_6_Read_Group_Barcode_Sequence = SAM_V1_6_Read_Group_Barcode_Sequence { sam_v1_6_read_group_barcode_sequence_value :: ByteString
                                                                                    }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Barcode_Sequence where
  SAM_V1_6_Read_Group_Barcode_Sequence sam_v1_6_read_group_barcode_sequence_value1 == SAM_V1_6_Read_Group_Barcode_Sequence sam_v1_6_read_group_barcode_sequence_value2 = sam_v1_6_read_group_barcode_sequence_value1 == sam_v1_6_read_group_barcode_sequence_value2

instance Show SAM_V1_6_Read_Group_Barcode_Sequence where
  show (SAM_V1_6_Read_Group_Barcode_Sequence value) =
    "SAM_V1_6_Read_Group_Barcode_Sequence { " ++
    "value = "                                ++
    (show value)                              ++
    " }"

-- | CN tag for @"SAM_V1_6_Read_Group"@.
newtype SAM_V1_6_Read_Group_Sequencing_Center = SAM_V1_6_Read_Group_Sequencing_Center { sam_v1_6_read_group_sequencing_center_value :: ByteString
                                                                                      }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Sequencing_Center where
  SAM_V1_6_Read_Group_Sequencing_Center sam_v1_6_read_group_sequencing_center_value1 == SAM_V1_6_Read_Group_Sequencing_Center sam_v1_6_read_group_sequencing_center_value2 = sam_v1_6_read_group_sequencing_center_value1 == sam_v1_6_read_group_sequencing_center_value2

instance Show SAM_V1_6_Read_Group_Sequencing_Center where
  show (SAM_V1_6_Read_Group_Sequencing_Center value) =
    "SAM_V1_6_Read_Group_Sequencing_Center { " ++
    "value = "                                 ++
    (show value)                               ++
    " }"

-- | DS tag for @"SAM_V1_6_Read_Group"@.
newtype SAM_V1_6_Read_Group_Description = SAM_V1_6_Read_Group_Description { sam_v1_6_read_group_description_value :: ByteString
                                                                          }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Description where
  SAM_V1_6_Read_Group_Description sam_v1_6_read_group_description_value1 == SAM_V1_6_Read_Group_Description sam_v1_6_read_group_description_value2 = sam_v1_6_read_group_description_value1 == sam_v1_6_read_group_description_value2

instance Show SAM_V1_6_Read_Group_Description where
  show (SAM_V1_6_Read_Group_Description value) =
    "SAM_V1_6_Read_Group_Description { " ++
    "value = "                           ++
    (show value)                         ++
    " }"

-- | DT tag for @"SAM_V1_6_Read_Group"@.
newtype SAM_V1_6_Read_Group_Run_Date = SAM_V1_6_Read_Group_Run_Date { sam_v1_6_read_group_run_date_value :: ByteString
                                                                    }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Run_Date where
  SAM_V1_6_Read_Group_Run_Date sam_v1_6_read_group_run_date_value1 == SAM_V1_6_Read_Group_Run_Date sam_v1_6_read_group_run_date_value2 = sam_v1_6_read_group_run_date_value1 == sam_v1_6_read_group_run_date_value2

instance Show SAM_V1_6_Read_Group_Run_Date where
  show (SAM_V1_6_Read_Group_Run_Date value) =
    "SAM_V1_6_Read_Group_Run_Date { " ++
    "value = "                        ++
    (show value)                      ++
    " }"

-- | FO tag for @"SAM_V1_6_Read_Group"@.
newtype SAM_V1_6_Read_Group_Flow_Order = SAM_V1_6_Read_Group_Flow_Order { sam_v1_6_read_group_flow_order_value :: ByteString
                                                                        }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Flow_Order where
  SAM_V1_6_Read_Group_Flow_Order sam_v1_6_one_line_comment_value1 == SAM_V1_6_Read_Group_Flow_Order sam_v1_6_read_group_flow_order_value2 = sam_v1_6_one_line_comment_value1 == sam_v1_6_read_group_flow_order_value2

instance Show SAM_V1_6_Read_Group_Flow_Order where
  show (SAM_V1_6_Read_Group_Flow_Order value) =
    "SAM_V1_6_Read_Group_Flow_Order { " ++
    "value = "                          ++
    (show value)                        ++
    " }"

-- | KS tag for @"SAM_V1_6_Read_Group"@.
newtype SAM_V1_6_Read_Group_Key_Sequence = SAM_V1_6_Read_Group_Key_Sequence { sam_v1_6_read_group_key_sequence_value :: ByteString
                                                                            }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Key_Sequence where
  SAM_V1_6_Read_Group_Key_Sequence sam_v1_6_read_group_key_sequence_value1 == SAM_V1_6_Read_Group_Key_Sequence sam_v1_6_read_group_key_sequence_value2 = sam_v1_6_read_group_key_sequence_value1 == sam_v1_6_read_group_key_sequence_value2

instance Show SAM_V1_6_Read_Group_Key_Sequence where
  show (SAM_V1_6_Read_Group_Key_Sequence value) =
    "SAM_V1_6_Read_Group_Key_Sequence { " ++
    "value = "                            ++
    (show value)                          ++
    " }"

-- | LB tag for @"SAM_V1_6_Read_Group"@.
newtype SAM_V1_6_Read_Group_Library = SAM_V1_6_Read_Group_Library { sam_v1_6_read_group_library_value :: ByteString
                                                                  }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Library where
  SAM_V1_6_Read_Group_Library sam_v1_6_read_group_library_value1 == SAM_V1_6_Read_Group_Library sam_v1_6_read_group_library_value2 = sam_v1_6_read_group_library_value1 == sam_v1_6_read_group_library_value2

instance Show SAM_V1_6_Read_Group_Library where
  show (SAM_V1_6_Read_Group_Library value) =
    "SAM_V1_6_Read_Group_Library { " ++
    "value = "                       ++
    (show value)                     ++
    " }"

-- | PG tag for @"SAM_V1_6_Read_Group"@.
newtype SAM_V1_6_Read_Group_Programs = SAM_V1_6_Read_Group_Programs { sam_v1_6_read_group_programs_value :: ByteString
                                                                    }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Programs where
  SAM_V1_6_Read_Group_Programs sam_v1_6_read_group_programs_value1 == SAM_V1_6_Read_Group_Programs sam_v1_6_read_group_programs_value2 = sam_v1_6_read_group_programs_value1 == sam_v1_6_read_group_programs_value2

instance Show SAM_V1_6_Read_Group_Programs where
  show (SAM_V1_6_Read_Group_Programs value) =
    "SAM_V1_6_Read_Group_Programs { " ++
    "value = "                        ++
    (show value)                      ++
    " }"

-- | PI tag for @"SAM_V1_6_Read_Group"@.
newtype SAM_V1_6_Read_Group_Predicted_Median_Insert_Size = SAM_V1_6_Read_Group_Predicted_Median_Insert_Size { sam_v1_6_read_group_predicted_median_insert_size_value :: ByteString
                                                                                                            }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Predicted_Median_Insert_Size where
  SAM_V1_6_Read_Group_Predicted_Median_Insert_Size sam_v1_6_read_group_predicted_median_insert_size_value1 == SAM_V1_6_Read_Group_Predicted_Median_Insert_Size sam_v1_6_read_group_predicted_median_insert_size_value2 = sam_v1_6_read_group_predicted_median_insert_size_value1 == sam_v1_6_read_group_predicted_median_insert_size_value2

instance Show SAM_V1_6_Read_Group_Predicted_Median_Insert_Size where
  show (SAM_V1_6_Read_Group_Predicted_Median_Insert_Size value) =
    "SAM_V1_6_Read_Group_Predicted_Median_Insert_Size { " ++
    "value = "                                            ++
    (show value)                                          ++
    " }"

-- | PL tag for @"SAM_V1_6_Read_Group"@.
newtype SAM_V1_6_Read_Group_Platform = SAM_V1_6_Read_Group_Platform { sam_v1_6_read_group_platform_value :: ByteString
                                                                    }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Platform where
  SAM_V1_6_Read_Group_Platform sam_v1_6_read_group_platform_value1 == SAM_V1_6_Read_Group_Platform sam_v1_6_read_group_platform_value2 = sam_v1_6_read_group_platform_value1 == sam_v1_6_read_group_platform_value2

instance Show SAM_V1_6_Read_Group_Platform where
  show (SAM_V1_6_Read_Group_Platform value) =
    "SAM_V1_6_Read_Group_Platform { " ++
    "value = "                        ++
    (show value)                      ++
    " }"

-- | PM tag for @"SAM_V1_6_Read_Group"@.
newtype SAM_V1_6_Read_Group_Platform_Model = SAM_V1_6_Read_Group_Platform_Model { sam_v1_6_read_group_platform_model_value :: ByteString
                                                                                }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Platform_Model where
  SAM_V1_6_Read_Group_Platform_Model sam_v1_6_read_group_platform_model_value1 == SAM_V1_6_Read_Group_Platform_Model sam_v1_6_read_group_platform_model_value2 = sam_v1_6_read_group_platform_model_value1 == sam_v1_6_read_group_platform_model_value2

instance Show SAM_V1_6_Read_Group_Platform_Model where
  show (SAM_V1_6_Read_Group_Platform_Model value) =
    "SAM_V1_6_Read_Group_Platform_Model { " ++
    "value = "                              ++
    (show value)                            ++
    " }"

-- | PU tag for @"SAM_V1_6_Read_Group"@.
newtype SAM_V1_6_Read_Group_Platform_Unit = SAM_V1_6_Read_Group_Platform_Unit { sam_v1_6_read_group_platform_unit_value :: ByteString
                                                                              }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Platform_Unit where
  SAM_V1_6_Read_Group_Platform_Unit sam_v1_6_read_group_platform_unit_value1 == SAM_V1_6_Read_Group_Platform_Unit sam_v1_6_read_group_platform_unit_value2 = sam_v1_6_read_group_platform_unit_value1 == sam_v1_6_read_group_platform_unit_value2

instance Show SAM_V1_6_Read_Group_Platform_Unit where
  show (SAM_V1_6_Read_Group_Platform_Unit value) =
    "SAM_V1_6_Read_Group_Platform_Unit { " ++
    "value = "                             ++
    (show value)                           ++
    " }"

-- | SM tag for @"SAM_V1_6_Read_Group"@.
newtype SAM_V1_6_Read_Group_Sample = SAM_V1_6_Read_Group_Sample { sam_v1_6_read_group_sample_value :: ByteString
                                                                }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Read_Group_Sample where
  SAM_V1_6_Read_Group_Sample sam_v1_6_read_group_sample_value1 == SAM_V1_6_Read_Group_Sample sam_v1_6_read_group_sample_value2 = sam_v1_6_read_group_sample_value1 == sam_v1_6_read_group_sample_value2

instance Show SAM_V1_6_Read_Group_Sample where
  show (SAM_V1_6_Read_Group_Sample value) =
    "SAM_V1_6_Read_Group_Sample { " ++
    "value = "                      ++
    (show value)                    ++
    " }"
