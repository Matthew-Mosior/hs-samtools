{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.SAM.Version1_6.Base
-- Copyright   :  (c) Matthew Mosior 2023
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.SAM.Version1_6.Base ( -- * SAM version 1.6 data type
                                  SAM_V1_6(..)
                                ) where

import Data.SAM.Version1_6.Alignment
import Data.SAM.Version1_6.Header

import Data.Data
import Data.Sequence
import Generics.Deriving.Base

-- | Custom @"SAM_V1_6"@ (SAM version 1.6) data type.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6 = SAM_V1_6 { sam_v1_6_file_level_metadata           :: Maybe SAM_V1_6_File_Level_Metadata                 -- ^ File-level metadata.
                                                                                                                        -- Optional. If present,
                                                                                                                        -- there must be only one
                                                                                                                        -- @HD line and it must be
                                                                                                                        -- the first line of the file. 
                         , sam_v1_6_reference_sequence_dictionary :: Maybe (Seq SAM_V1_6_Reference_Sequence_Dictionary) -- ^ Reference sequence dictionary.
                                                                                                                        -- The order of @SQ lines defines the
                                                                                                                        -- alignment sorting order.
                         , sam_v1_6_read_group                    :: Maybe (Seq SAM_V1_6_Read_Group)                    -- ^ Read group.
                                                                                                                        -- Unordered multiple @RG
                                                                                                                        -- lines are allowed.
                         , sam_v1_6_program                       :: Maybe SAM_V1_6_Program                             -- ^ Program.
                         , sam_v1_6_one_line_comment              :: Maybe (Seq SAM_V1_6_One_Line_Comment)              -- ^ One-line text comment.
                                                                                                                        -- Unordered multiple @CO lines
                                                                                                                        -- are allowed. UTF-8 encoding
                                                                                                                        -- may be used.
                         , sam_v1_6_alignment                     :: Seq SAM_V1_6_Alignment                             -- ^ The alignment section (mandatory
                                                                                                                        -- and optional fields).
                         }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6 where
  SAM_V1_6 sam_v1_6_file_level_metadata1
           sam_v1_6_reference_sequence_dictionary1
           sam_v1_6_read_group1
           sam_v1_6_program1
           sam_v1_6_one_line_comment1
           sam_v1_6_alignment1 == SAM_V1_6 sam_v1_6_file_level_metadata2
                                           sam_v1_6_reference_sequence_dictionary2
                                           sam_v1_6_read_group2
                                           sam_v1_6_program2
                                           sam_v1_6_one_line_comment2
                                           sam_v1_6_alignment2 = sam_v1_6_file_level_metadata1           == sam_v1_6_file_level_metadata2           &&
                                                                 sam_v1_6_reference_sequence_dictionary1 == sam_v1_6_reference_sequence_dictionary2 &&
                                                                 sam_v1_6_read_group1                    == sam_v1_6_read_group2                    &&
                                                                 sam_v1_6_program1                       == sam_v1_6_program2                       &&
                                                                 sam_v1_6_one_line_comment1              == sam_v1_6_one_line_comment2              &&
                                                                 sam_v1_6_alignment1                     == sam_v1_6_alignment2

instance Show SAM_V1_6 where
  show (SAM_V1_6 file_level_metadata
                 reference_sequence_dictionary
                 read_group program
                 one_line_comment
                 alignment
       ) =
    "SAM_V1_6 { "                                  ++
    "sam_v1_6_file_level_metadata = "              ++
    (show file_level_metadata)                     ++
    " , sam_v1_6_reference_sequence_dictionary = " ++
    (show reference_sequence_dictionary)           ++
    " , sam_v1_6_read_group = "                    ++
    (show read_group)                              ++
    " , sam_v1_6_program = "                       ++
    (show program)                                 ++
    " , sam_v1_6_one_line_comment = "              ++
    (show one_line_comment)                        ++
    " , sam_v1_6_alignment = "                     ++
    (show alignment)                               ++
    " }"
