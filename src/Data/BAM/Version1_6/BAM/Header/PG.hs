{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.BAM.Header.PG
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.BAM.Header.PG ( -- * BAM version 1.6 program data type
                                           BAM_V1_6_Program(..),
                                           -- * BAM version 1.6 program data types
                                           BAM_V1_6_Program_Record_Identifier(..),
                                           BAM_V1_6_Program_Name(..),
                                           BAM_V1_6_Program_Command_Line(..),
                                           BAM_V1_6_Program_Previous_PG_ID(..),
                                           BAM_V1_6_Program_Description(..),
                                           BAM_V1_6_Program_Version(..)
                                         ) where

import Data.ByteString
import Data.Data
import Generics.Deriving.Base

-- | Custom BAM (version 1.6) @"BAM_V1_6_Program"@ data type.
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_Program = BAM_V1_6_Program { bam_v1_6_program_record_identifier :: BAM_V1_6_Program_Record_Identifier
                                         , bam_v1_6_program_name              :: Maybe BAM_V1_6_Program_Name
                                         , bam_v1_6_program_command_line      :: Maybe BAM_V1_6_Program_Command_Line
                                         , bam_v1_6_program_previous_pg_id    :: Maybe BAM_V1_6_Program_Previous_PG_ID
                                         , bam_v1_6_program_description       :: Maybe BAM_V1_6_Program_Description
                                         , bam_v1_6_program_version           :: Maybe BAM_V1_6_Program_Version
                                         }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Program where
  BAM_V1_6_Program bam_v1_6_program_record_identifier1
                   bam_v1_6_program_name1
                   bam_v1_6_program_command_line1
                   bam_v1_6_program_previous_pg_id1
                   bam_v1_6_program_description1
                   bam_v1_6_program_version1 ==
    BAM_V1_6_Program bam_v1_6_program_record_identifier2
                     bam_v1_6_program_name2
                     bam_v1_6_program_command_line2
                     bam_v1_6_program_previous_pg_id2
                     bam_v1_6_program_description2
                     bam_v1_6_program_version2 =
      bam_v1_6_program_record_identifier1 == bam_v1_6_program_record_identifier2 &&
      bam_v1_6_program_name1              == bam_v1_6_program_name2              &&
      bam_v1_6_program_command_line1      == bam_v1_6_program_command_line2      &&
      bam_v1_6_program_previous_pg_id1    == bam_v1_6_program_previous_pg_id2    &&
      bam_v1_6_program_description1       == bam_v1_6_program_description2       &&
      bam_v1_6_program_version1           == bam_v1_6_program_version2

instance Show BAM_V1_6_Program where
  show (BAM_V1_6_Program record_identifier name command_line previous_pg_id description version) =
    "BAM_V1_6_Program { "                    ++
    "rbam_v1_6_program_record_identifier = " ++
    (show record_identifier)                 ++
    " , bam_v1_6_program_name = "            ++
    (show name)                              ++
    " , bam_v1_6_program_command_line = "    ++
    (show command_line)                      ++
    " , bam_v1_6_program_previous_pg_id = "  ++
    (show previous_pg_id)                    ++
    " , bam_v1_6_program_description = "     ++
    (show description)                       ++
    " , bam_v1_6_program_version = "         ++
    (show version)                           ++
    " }"

-- | ID tag for @"BAM_V1_6_Program"@.
newtype BAM_V1_6_Program_Record_Identifier = BAM_V1_6_Program_Record_Identifier { bam_v1_6_program_record_identifier_value :: ByteString
                                                                                }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Program_Record_Identifier where
  BAM_V1_6_Program_Record_Identifier bam_v1_6_program_record_identifier_value1 ==
    BAM_V1_6_Program_Record_Identifier bam_v1_6_program_record_identifier_value2 =
      bam_v1_6_program_record_identifier_value1 == bam_v1_6_program_record_identifier_value2

instance Show BAM_V1_6_Program_Record_Identifier where
  show (BAM_V1_6_Program_Record_Identifier value) =
    "BAM_V1_6_Program_Record_Identifier { "       ++
    "bam_v1_6_program_record_identifier_value = " ++
    (show value)                                  ++
    " }"

-- | PN tag for @"BAM_V1_6_Program"@.
newtype BAM_V1_6_Program_Name = BAM_V1_6_Program_Name { bam_v1_6_program_name_value :: ByteString
                                                      }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Program_Name where
  BAM_V1_6_Program_Name bam_v1_6_program_name_value1 ==
    BAM_V1_6_Program_Name bam_v1_6_program_name_value2 =
      bam_v1_6_program_name_value1 == bam_v1_6_program_name_value2

instance Show BAM_V1_6_Program_Name where
  show (BAM_V1_6_Program_Name value) =
    "BAM_V1_6_Program_Name { "       ++
    "bam_v1_6_program_name_value = " ++
    (show value)                     ++
    " }"

-- | CL tag for @"BAM_V1_6_Program"@.
newtype BAM_V1_6_Program_Command_Line = BAM_V1_6_Program_Command_Line { bam_v1_6_program_command_line_value :: ByteString
                                                                      }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Program_Command_Line where
  BAM_V1_6_Program_Command_Line bam_v1_6_program_command_line_value1 ==
    BAM_V1_6_Program_Command_Line bam_v1_6_program_command_line_value2 =
      bam_v1_6_program_command_line_value1 == bam_v1_6_program_command_line_value2

instance Show BAM_V1_6_Program_Command_Line where
  show (BAM_V1_6_Program_Command_Line value) =
    "BAM_V1_6_Program_Command_Line { "       ++
    "bam_v1_6_program_command_line_value = " ++
    (show value)                             ++
    " }"

-- | PP tag for @"BAM_V1_6_Program"@.
newtype BAM_V1_6_Program_Previous_PG_ID = BAM_V1_6_Program_Previous_PG_ID { bam_v1_6_program_previous_pg_id_value :: ByteString
                                                                          }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Program_Previous_PG_ID where
  BAM_V1_6_Program_Previous_PG_ID bam_v1_6_program_previous_pg_id_value1 ==
    BAM_V1_6_Program_Previous_PG_ID bam_v1_6_program_previous_pg_id_value2 =
      bam_v1_6_program_previous_pg_id_value1 == bam_v1_6_program_previous_pg_id_value2

instance Show BAM_V1_6_Program_Previous_PG_ID where
  show (BAM_V1_6_Program_Previous_PG_ID value) =
    "BAM_V1_6_Program_Previous_PG_ID { "       ++
    "bam_v1_6_program_previous_pg_id_value = " ++
    (show value)                               ++
    " }"

-- | DS tag for @"BAM_V1_6_Program"@.
newtype BAM_V1_6_Program_Description = BAM_V1_6_Program_Description { bam_v1_6_program_description_value :: ByteString
                                                                    }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Program_Description where
  BAM_V1_6_Program_Description bam_v1_6_program_description_value1 ==
    BAM_V1_6_Program_Description bam_v1_6_program_description_value2 =
      bam_v1_6_program_description_value1 == bam_v1_6_program_description_value2

instance Show BAM_V1_6_Program_Description where
  show (BAM_V1_6_Program_Description value) =
    "BAM_V1_6_Program_Description { "       ++
    "bam_v1_6_program_description_value = " ++
    (show value)                            ++
    " }"

-- | VN tag for @"BAM_V1_6_Program"@.
newtype BAM_V1_6_Program_Version = BAM_V1_6_Program_Version { bam_v1_6_program_version_value :: ByteString
                                                            }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Program_Version where
  BAM_V1_6_Program_Version bam_v1_6_program_version_value1 ==
    BAM_V1_6_Program_Version bam_v1_6_program_version_value2 =
      bam_v1_6_program_version_value1 == bam_v1_6_program_version_value2

instance Show BAM_V1_6_Program_Version where
  show (BAM_V1_6_Program_Version value) =
    "BAM_V1_6_Program_Version { "       ++
    "bam_v1_6_program_version_value = " ++
    (show value)                        ++
    " }"
