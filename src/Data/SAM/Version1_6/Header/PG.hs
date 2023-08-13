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
-- Module      :  Data.SAM.Version1_6.Header.PG
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

module Data.SAM.Version1_6.Header.PG ( -- * SAM version 1.6 program data type
                                       SAM_V1_6_Program(..),
                                       -- * SAM version 1.6 program data types
                                       SAM_V1_6_Program_Record_Identifier(..),
                                       SAM_V1_6_Program_Name(..),
                                       SAM_V1_6_Program_Command_Line(..),
                                       SAM_V1_6_Program_Previous_PG_ID(..),
                                       SAM_V1_6_Program_Description(..),
                                       SAM_V1_6_Program_Version(..)
                                     ) where

import Data.ByteString
import Data.Data
import Data.Sequence
import Data.Word
import Generics.Deriving.Base


-- | Custom SAM (version 1.6) @"SAM_V1_6_Program"@ data type.
-- See section 1.3 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Program = SAM_V1_6_Program { sam_v1_6_program_record_identifier :: SAM_V1_6_Program_Record_Identifier
                                         , sam_v1_6_program_name              :: Maybe SAM_V1_6_Program_Name
                                         , sam_v1_6_program_command_line      :: Maybe SAM_V1_6_Program_Command_Line
                                         , sam_v1_6_program_previous_pg_id    :: Maybe SAM_V1_6_Program_Previous_PG_ID
                                         , sam_v1_6_program_description       :: Maybe SAM_V1_6_Program_Description
                                         , sam_v1_6_program_version           :: Maybe SAM_V1_6_Program_Version
                                         }
  deriving (Generic,Typeable)

-- | ID tag for @"SAM_V1_6_Program"@.
data SAM_V1_6_Program_Record_Identifier = SAM_V1_6_Program_Record_Identifier { sam_v1_6_program_record_identifier_tag   :: Seq Word8
                                                                             , sam_v1_6_program_record_identifier_value :: ByteString
                                                                             }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Program_Record_Identifier where
  SAM_V1_6_Program_Record_Identifier sam_v1_6_program_record_identifier_tag1 sam_v1_6_program_record_identifier_value1 == SAM_V1_6_Program_Record_Identifier sam_v1_6_program_record_identifier_tag2 sam_v1_6_program_record_identifier_value2 = sam_v1_6_program_record_identifier_tag1 == sam_v1_6_program_record_identifier_tag2 && sam_v1_6_program_record_identifier_value1 == sam_v1_6_program_record_identifier_value2

instance Show SAM_V1_6_Program_Record_Identifier where
  show (SAM_V1_6_Program_Record_Identifier tag value) = "SAM_V1_6_Program_Record_Identifier { " ++
                                                        "tag = "                                ++
                                                        (show tag)                              ++
                                                        " , value = "                           ++
                                                        (show value)                            ++
                                                        " }"

-- | PN tag for @"SAM_V1_6_Program"@.
data SAM_V1_6_Program_Name = SAM_V1_6_Program_Name { sam_v1_6_program_name_tag   :: Seq Word8
                                                   , sam_v1_6_program_name_value :: ByteString
                                                   }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Program_Name where
  SAM_V1_6_Program_Name sam_v1_6_program_name_tag1 sam_v1_6_program_name_value1 == SAM_V1_6_Program_Name sam_v1_6_program_name_tag2 sam_v1_6_program_name_value2 = sam_v1_6_program_name_tag1 == sam_v1_6_program_name_tag2 && sam_v1_6_program_name_value1 == sam_v1_6_program_name_value2

instance Show SAM_V1_6_Program_Name where
  show (SAM_V1_6_Program_Name tag value) = "SAM_V1_6_Program_Name { " ++
                                           "tag = "                   ++
                                           (show tag)                 ++
                                           " , value = "              ++
                                           (show value)               ++
                                           " }"

-- | CL tag for @"SAM_V1_6_Program"@.
data SAM_V1_6_Program_Command_Line = SAM_V1_6_Program_Command_Line { sam_v1_6_program_command_line_tag   :: Seq Word8
                                                                   , sam_v1_6_program_command_line_value :: ByteString
                                                                   }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Program_Command_Line where
  SAM_V1_6_Program_Command_Line sam_v1_6_program_command_line_tag1 sam_v1_6_program_command_line_value1 == SAM_V1_6_Program_Command_Line sam_v1_6_program_command_line_tag2 sam_v1_6_program_command_line_value2 = sam_v1_6_program_command_line_tag1 == sam_v1_6_program_command_line_tag2 && sam_v1_6_program_command_line_value1 == sam_v1_6_program_command_line_value2

instance Show SAM_V1_6_Program_Command_Line where
  show (SAM_V1_6_Program_Command_Line tag value) = "SAM_V1_6_Program_Command_Line { " ++
                                                   "tag = "                           ++
                                                   (show tag)                         ++
                                                   " , value = "                      ++
                                                   (show value)                       ++
                                                   " }"

-- | PP tag for @"SAM_V1_6_Program"@.
data SAM_V1_6_Program_Previous_PG_ID = SAM_V1_6_Program_Previous_PG_ID { sam_v1_6_program_previous_pg_id_tag   :: Seq Word8
                                                                       , sam_v1_6_program_previous_pg_id_value :: ByteString
                                                                       }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Program_Previous_PG_ID where
  SAM_V1_6_Program_Previous_PG_ID sam_v1_6_program_previous_pg_id_tag1 sam_v1_6_program_previous_pg_id_value1 == SAM_V1_6_Program_Previous_PG_ID sam_v1_6_program_previous_pg_id_tag2 sam_v1_6_program_previous_pg_id_value2 = sam_v1_6_program_previous_pg_id_tag1 == sam_v1_6_program_previous_pg_id_tag2 && sam_v1_6_program_previous_pg_id_value1 == sam_v1_6_program_previous_pg_id_value2

instance Show SAM_V1_6_Program_Previous_PG_ID where
  show (SAM_V1_6_Program_Previous_PG_ID tag value) = "SAM_V1_6_Program_Previous_PG_ID { " ++
                                                     "tag = "                             ++
                                                     (show tag)                           ++
                                                     " , value = "                        ++
                                                     (show value)                         ++
                                                     " }"

-- | DS tag for @"SAM_V1_6_Program"@.
data SAM_V1_6_Program_Description = SAM_V1_6_Program_Description { sam_v1_6_program_description_tag   :: Seq Word8
                                                                 , sam_v1_6_program_description_value :: ByteString
                                                                 }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Program_Description where
  SAM_V1_6_Program_Description sam_v1_6_program_description_tag1 sam_v1_6_program_description_value1 == SAM_V1_6_Program_Description sam_v1_6_program_description_tag2 sam_v1_6_program_description_value2 = sam_v1_6_program_description_tag1 == sam_v1_6_program_description_tag2 && sam_v1_6_program_description_value1 == sam_v1_6_program_description_value2

instance Show SAM_V1_6_Program_Description where
  show (SAM_V1_6_Program_Description tag value) = "SAM_V1_6_Program_Description { " ++
                                                  "tag = "                          ++
                                                  (show tag)                        ++
                                                  " , value = "                     ++
                                                  (show value)                      ++
                                                  " }"

-- | VN tag for @"SAM_V1_6_Program"@.
data SAM_V1_6_Program_Version = SAM_V1_6_Program_Version { sam_v1_6_program_version_tag   :: Seq Word8
                                                         , sam_v1_6_program_version_value :: ByteString
                                                         }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Program_Version where
  SAM_V1_6_Program_Version sam_v1_6_program_version_tag1 sam_v1_6_program_version_value1 == SAM_V1_6_Program_Version sam_v1_6_program_version_tag2 sam_v1_6_program_version_value2 = sam_v1_6_program_version_tag1 == sam_v1_6_program_version_tag2 && sam_v1_6_program_version_value1 == sam_v1_6_program_version_value2

instance Show SAM_V1_6_Program_Version where
  show (SAM_V1_6_Program_Version tag value) = "SAM_V1_6_Program_Version { " ++
                                              "tag = "                      ++
                                              (show tag)                    ++
                                              " , value = "                 ++
                                              (show value)                  ++
                                              " }"
