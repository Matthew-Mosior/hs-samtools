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
-- Module      :  Data.SAM.Version1_6.Header.HD
-- Copyright   :  (c) Matthew Mosior 2023
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.SAM.Version1_6.Header.HD ( -- * SAM version 1.6 File-level metadata data type
                                       SAM_V1_6_File_Level_Metadata(..),
                                       -- * SAM version 1.6 File-Level Metadata data types
                                       SAM_V1_6_File_Level_Metadata_Format_Version(..),
                                       SAM_V1_6_File_Level_Metadata_Sorting_Order(..),
                                       SAM_V1_6_File_Level_Metadata_Alignment_Grouping(..),
                                       SAM_V1_6_File_Level_Metadata_SubSorting_Order(..)
                                     ) where

import Data.ByteString
import Data.Data
import Generics.Deriving.Base

-- | Custom SAM (version 1.6) @"SAM_V1_6_File_Level_Metadata"@ data type.
--
-- See section 1.3 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_File_Level_Metadata = SAM_V1_6_File_Level_Metadata { sam_v1_6_file_level_metadata_format_version     :: SAM_V1_6_File_Level_Metadata_Format_Version
                                                                 , sam_v1_6_file_level_metadata_sorting_order      :: Maybe SAM_V1_6_File_Level_Metadata_Sorting_Order
                                                                 , sam_v1_6_file_level_metadata_alignment_grouping :: Maybe SAM_V1_6_File_Level_Metadata_Alignment_Grouping
                                                                 , sam_v1_6_file_level_metadata_subsorting_order   :: Maybe SAM_V1_6_File_Level_Metadata_SubSorting_Order
                                                                 }
   deriving (Generic,Typeable)

instance Eq SAM_V1_6_File_Level_Metadata where
  SAM_V1_6_File_Level_Metadata sam_v1_6_file_level_metadata_format_version1
                               sam_v1_6_file_level_metadata_sorting_order1
                               sam_v1_6_file_level_metadata_alignment_grouping1
                               sam_v1_6_file_level_metadata_subsorting_order1 == SAM_V1_6_File_Level_Metadata sam_v1_6_file_level_metadata_format_version2
                                                                                                              sam_v1_6_file_level_metadata_sorting_order2
                                                                                                              sam_v1_6_file_level_metadata_alignment_grouping2
                                                                                                              sam_v1_6_file_level_metadata_subsorting_order2 = sam_v1_6_file_level_metadata_format_version1     == sam_v1_6_file_level_metadata_format_version2     &&
                                                                                                                                                               sam_v1_6_file_level_metadata_sorting_order1      == sam_v1_6_file_level_metadata_sorting_order2      &&
                                                                                                                                                               sam_v1_6_file_level_metadata_alignment_grouping1 == sam_v1_6_file_level_metadata_alignment_grouping2 &&
                                                                                                                                                               sam_v1_6_file_level_metadata_subsorting_order1   == sam_v1_6_file_level_metadata_subsorting_order2

instance Show SAM_V1_6_File_Level_Metadata where
  show (SAM_V1_6_File_Level_Metadata version sorting_order alignment_grouping subsorting_order) =
    "SAM_V1_6_File_Level_Metadata { " ++
    "version = "                      ++
    (show version)                    ++
    " , sorting_order = "             ++
    (show sorting_order)              ++
    " , alignment_grouping = "        ++
    (show alignment_grouping)         ++
    " , subsorting_order = "          ++
    (show subsorting_order)           ++
    " }" 

-- | VN tag for @"SAM_V1_6_File_Level_Metadata"@.
newtype SAM_V1_6_File_Level_Metadata_Format_Version = SAM_V1_6_File_Level_Metadata_Format_Version { sam_v1_6_file_level_metadata_format_version_value :: ByteString
                                                                                                  }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_File_Level_Metadata_Format_Version where
  SAM_V1_6_File_Level_Metadata_Format_Version sam_v1_6_file_level_metadata_format_version_value1 == SAM_V1_6_File_Level_Metadata_Format_Version sam_v1_6_file_level_metadata_format_version_value2 = sam_v1_6_file_level_metadata_format_version_value1 == sam_v1_6_file_level_metadata_format_version_value2

instance Show SAM_V1_6_File_Level_Metadata_Format_Version where
  show (SAM_V1_6_File_Level_Metadata_Format_Version value) =
    "SAM_V1_6_File_Level_Metadata_Format_Version { " ++
    "value = "                                       ++
    (show value)                                     ++
    " }"

-- | SO tag for @"SAM_V1_6_File_Level_Metadata"@.
newtype SAM_V1_6_File_Level_Metadata_Sorting_Order = SAM_V1_6_File_Level_Metadata_Sorting_Order { sam_v1_6_file_level_metadata_sorting_order_value :: ByteString
                                                                                                }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_File_Level_Metadata_Sorting_Order where
  SAM_V1_6_File_Level_Metadata_Sorting_Order sam_v1_6_file_level_metadata_sorting_order_value1 == SAM_V1_6_File_Level_Metadata_Sorting_Order sam_v1_6_file_level_metadata_sorting_order_value2 = sam_v1_6_file_level_metadata_sorting_order_value1 == sam_v1_6_file_level_metadata_sorting_order_value2

instance Show SAM_V1_6_File_Level_Metadata_Sorting_Order where
  show (SAM_V1_6_File_Level_Metadata_Sorting_Order value) =
    "SAM_V1_6_File_Level_Metadata_Sorting_Order { " ++
    "value = "                                      ++
    (show value)                                    ++
    " }"

-- | GO tag for @"SAM_V1_6_File_Level_Metadata"@.
newtype SAM_V1_6_File_Level_Metadata_Alignment_Grouping = SAM_V1_6_File_Level_Metadata_Alignment_Grouping { sam_v1_6_file_level_metadata_alignment_grouping_value :: ByteString
                                                                                                          }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_File_Level_Metadata_Alignment_Grouping where
  SAM_V1_6_File_Level_Metadata_Alignment_Grouping sam_v1_6_file_level_metadata_alignment_grouping_value1 == SAM_V1_6_File_Level_Metadata_Alignment_Grouping sam_v1_6_file_level_metadata_alignment_grouping_value2 = sam_v1_6_file_level_metadata_alignment_grouping_value1 == sam_v1_6_file_level_metadata_alignment_grouping_value2

instance Show SAM_V1_6_File_Level_Metadata_Alignment_Grouping where
  show (SAM_V1_6_File_Level_Metadata_Alignment_Grouping value) =
    "SAM_V1_6_File_Level_Metadata_Alignment_Grouping { " ++
    "value = "                                           ++
    (show value)                                         ++
    " }"

-- | SS tag for @"SAM_V1_6_File_Level_Metadata"@.
newtype SAM_V1_6_File_Level_Metadata_SubSorting_Order = SAM_V1_6_File_Level_Metadata_SubSorting_Order { sam_v1_6_file_level_metadata_subsorting_order_value :: ByteString
                                                                                                      }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_File_Level_Metadata_SubSorting_Order where
  SAM_V1_6_File_Level_Metadata_SubSorting_Order sam_v1_6_file_level_metadata_subsorting_order_value1 == SAM_V1_6_File_Level_Metadata_SubSorting_Order sam_v1_6_file_level_metadata_subsorting_order_value2 = sam_v1_6_file_level_metadata_subsorting_order_value1 == sam_v1_6_file_level_metadata_subsorting_order_value2

instance Show SAM_V1_6_File_Level_Metadata_SubSorting_Order where
  show (SAM_V1_6_File_Level_Metadata_SubSorting_Order value) =
    "SAM_V1_6_File_Level_Metadata_SubSorting_Order { " ++
    "value = "                                         ++
    (show value)                                       ++
    " }"
