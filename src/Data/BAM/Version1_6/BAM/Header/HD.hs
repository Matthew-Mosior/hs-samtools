{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.BAM.Header.HD
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.BAM.Header.HD ( -- * BAM version 1.6 File-level metadata data type
                                           BAM_V1_6_File_Level_Metadata(..),
                                           -- * BAM version 1.6 File-Level Metadata data types
                                           BAM_V1_6_File_Level_Metadata_Format_Version(..),
                                           BAM_V1_6_File_Level_Metadata_Sorting_Order(..),
                                           BAM_V1_6_File_Level_Metadata_Alignment_Grouping(..),
                                           BAM_V1_6_File_Level_Metadata_SubSorting_Order(..)
                                         ) where

import Data.ByteString
import Data.Data
import Generics.Deriving.Base

-- | Custom BAM (version 1.6) @"BAM_V1_6_File_Level_Metadata"@ data type.
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_File_Level_Metadata = BAM_V1_6_File_Level_Metadata { bam_v1_6_file_level_metadata_format_version     :: BAM_V1_6_File_Level_Metadata_Format_Version
                                                                 , bam_v1_6_file_level_metadata_sorting_order      :: Maybe BAM_V1_6_File_Level_Metadata_Sorting_Order
                                                                 , bam_v1_6_file_level_metadata_alignment_grouping :: Maybe BAM_V1_6_File_Level_Metadata_Alignment_Grouping
                                                                 , bam_v1_6_file_level_metadata_subsorting_order   :: Maybe BAM_V1_6_File_Level_Metadata_SubSorting_Order
                                                                 }
   deriving (Generic,Typeable)

instance Eq BAM_V1_6_File_Level_Metadata where
  BAM_V1_6_File_Level_Metadata bam_v1_6_file_level_metadata_format_version1
                               bam_v1_6_file_level_metadata_sorting_order1
                               bam_v1_6_file_level_metadata_alignment_grouping1
                               bam_v1_6_file_level_metadata_subsorting_order1 ==
    BAM_V1_6_File_Level_Metadata bam_v1_6_file_level_metadata_format_version2
                                 bam_v1_6_file_level_metadata_sorting_order2
                                 bam_v1_6_file_level_metadata_alignment_grouping2
                                 bam_v1_6_file_level_metadata_subsorting_order2 =
      bam_v1_6_file_level_metadata_format_version1     == bam_v1_6_file_level_metadata_format_version2     &&
      bam_v1_6_file_level_metadata_sorting_order1      == bam_v1_6_file_level_metadata_sorting_order2      &&
      bam_v1_6_file_level_metadata_alignment_grouping1 == bam_v1_6_file_level_metadata_alignment_grouping2 &&
      bam_v1_6_file_level_metadata_subsorting_order1   == bam_v1_6_file_level_metadata_subsorting_order2

instance Show BAM_V1_6_File_Level_Metadata where
  show (BAM_V1_6_File_Level_Metadata version sorting_order alignment_grouping subsorting_order) =
    "BAM_V1_6_File_Level_Metadata { "                       ++
    "bam_v1_6_file_level_metadata_format_version = "        ++
    (show version)                                          ++
    " , bam_v1_6_file_level_metadata_sorting_order = "      ++
    (show sorting_order)                                    ++
    " , bam_v1_6_file_level_metadata_alignment_grouping = " ++
    (show alignment_grouping)                               ++
    " , bam_v1_6_file_level_metadata_subsorting_order = "   ++
    (show subsorting_order)                                 ++
    " }" 

-- | VN tag for @"BAM_V1_6_File_Level_Metadata"@.
newtype BAM_V1_6_File_Level_Metadata_Format_Version = BAM_V1_6_File_Level_Metadata_Format_Version { bam_v1_6_file_level_metadata_format_version_value :: ByteString
                                                                                                  }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_File_Level_Metadata_Format_Version where
  BAM_V1_6_File_Level_Metadata_Format_Version bam_v1_6_file_level_metadata_format_version_value1 ==
    BAM_V1_6_File_Level_Metadata_Format_Version bam_v1_6_file_level_metadata_format_version_value2 =
      bam_v1_6_file_level_metadata_format_version_value1 == bam_v1_6_file_level_metadata_format_version_value2

instance Show BAM_V1_6_File_Level_Metadata_Format_Version where
  show (BAM_V1_6_File_Level_Metadata_Format_Version value) =
    "BAM_V1_6_File_Level_Metadata_Format_Version { "       ++
    "bam_v1_6_file_level_metadata_format_version_value = " ++
    (show value)                                           ++
    " }"

-- | SO tag for @"BAM_V1_6_File_Level_Metadata"@.
newtype BAM_V1_6_File_Level_Metadata_Sorting_Order = BAM_V1_6_File_Level_Metadata_Sorting_Order { bam_v1_6_file_level_metadata_sorting_order_value :: ByteString
                                                                                                }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_File_Level_Metadata_Sorting_Order where
  BAM_V1_6_File_Level_Metadata_Sorting_Order bam_v1_6_file_level_metadata_sorting_order_value1 ==
    BAM_V1_6_File_Level_Metadata_Sorting_Order bam_v1_6_file_level_metadata_sorting_order_value2 =
      bam_v1_6_file_level_metadata_sorting_order_value1 == bam_v1_6_file_level_metadata_sorting_order_value2

instance Show BAM_V1_6_File_Level_Metadata_Sorting_Order where
  show (BAM_V1_6_File_Level_Metadata_Sorting_Order value) =
    "BAM_V1_6_File_Level_Metadata_Sorting_Order { "       ++
    "bam_v1_6_file_level_metadata_sorting_order_value = " ++
    (show value)                                          ++
    " }"

-- | GO tag for @"BAM_V1_6_File_Level_Metadata"@.
newtype BAM_V1_6_File_Level_Metadata_Alignment_Grouping = BAM_V1_6_File_Level_Metadata_Alignment_Grouping { bam_v1_6_file_level_metadata_alignment_grouping_value :: ByteString
                                                                                                          }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_File_Level_Metadata_Alignment_Grouping where
  BAM_V1_6_File_Level_Metadata_Alignment_Grouping bam_v1_6_file_level_metadata_alignment_grouping_value1 ==
    BAM_V1_6_File_Level_Metadata_Alignment_Grouping bam_v1_6_file_level_metadata_alignment_grouping_value2 =
      bam_v1_6_file_level_metadata_alignment_grouping_value1 == bam_v1_6_file_level_metadata_alignment_grouping_value2

instance Show BAM_V1_6_File_Level_Metadata_Alignment_Grouping where
  show (BAM_V1_6_File_Level_Metadata_Alignment_Grouping value) =
    "BAM_V1_6_File_Level_Metadata_Alignment_Grouping { "       ++
    "bam_v1_6_file_level_metadata_alignment_grouping_value = " ++
    (show value)                                               ++
    " }"

-- | SS tag for @"BAM_V1_6_File_Level_Metadata"@.
newtype BAM_V1_6_File_Level_Metadata_SubSorting_Order = BAM_V1_6_File_Level_Metadata_SubSorting_Order { bam_v1_6_file_level_metadata_subsorting_order_value :: ByteString
                                                                                                      }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_File_Level_Metadata_SubSorting_Order where
  BAM_V1_6_File_Level_Metadata_SubSorting_Order bam_v1_6_file_level_metadata_subsorting_order_value1 ==
    BAM_V1_6_File_Level_Metadata_SubSorting_Order bam_v1_6_file_level_metadata_subsorting_order_value2 =
      bam_v1_6_file_level_metadata_subsorting_order_value1 == bam_v1_6_file_level_metadata_subsorting_order_value2

instance Show BAM_V1_6_File_Level_Metadata_SubSorting_Order where
  show (BAM_V1_6_File_Level_Metadata_SubSorting_Order value) =
    "BAM_V1_6_File_Level_Metadata_SubSorting_Order { "       ++
    "bam_v1_6_file_level_metadata_subsorting_order_value = " ++
    (show value)                                             ++
    " }"
