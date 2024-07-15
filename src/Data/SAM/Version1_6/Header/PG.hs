{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.SAM.Version1_6.Header.PG
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
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
import Generics.Deriving.Base

-- | Custom SAM (version 1.6) @"SAM_V1_6_Program"@ data type.
--
-- See section 1.3 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Program = SAM_V1_6_Program
  { sam_v1_6_program_record_identifier :: SAM_V1_6_Program_Record_Identifier
  , sam_v1_6_program_name              :: Maybe SAM_V1_6_Program_Name
  , sam_v1_6_program_command_line      :: Maybe SAM_V1_6_Program_Command_Line
  , sam_v1_6_program_previous_pg_id    :: Maybe SAM_V1_6_Program_Previous_PG_ID
  , sam_v1_6_program_description       :: Maybe SAM_V1_6_Program_Description
  , sam_v1_6_program_version           :: Maybe SAM_V1_6_Program_Version
  } deriving (Generic,Typeable)

instance Eq SAM_V1_6_Program where
  SAM_V1_6_Program sam_v1_6_program_record_identifier1
                   sam_v1_6_program_name1
                   sam_v1_6_program_command_line1
                   sam_v1_6_program_previous_pg_id1
                   sam_v1_6_program_description1
                   sam_v1_6_program_version1 ==
    SAM_V1_6_Program sam_v1_6_program_record_identifier2
                     sam_v1_6_program_name2
                     sam_v1_6_program_command_line2
                     sam_v1_6_program_previous_pg_id2
                     sam_v1_6_program_description2
                     sam_v1_6_program_version2 =
      sam_v1_6_program_record_identifier1 == sam_v1_6_program_record_identifier2 &&
      sam_v1_6_program_name1              == sam_v1_6_program_name2              &&
      sam_v1_6_program_command_line1      == sam_v1_6_program_command_line2      &&
      sam_v1_6_program_previous_pg_id1    == sam_v1_6_program_previous_pg_id2    &&
      sam_v1_6_program_description1       == sam_v1_6_program_description2       &&
      sam_v1_6_program_version1           == sam_v1_6_program_version2

instance Show SAM_V1_6_Program where
  show (SAM_V1_6_Program record_identifier name command_line previous_pg_id description version) =
    "SAM_V1_6_Program { "                    ++
    "rsam_v1_6_program_record_identifier = " ++
    (show record_identifier)                 ++
    " , sam_v1_6_program_name = "            ++
    (show name)                              ++
    " , sam_v1_6_program_command_line = "    ++
    (show command_line)                      ++
    " , sam_v1_6_program_previous_pg_id = "  ++
    (show previous_pg_id)                    ++
    " , sam_v1_6_program_description = "     ++
    (show description)                       ++
    " , sam_v1_6_program_version = "         ++
    (show version)                           ++
    " }"

-- | ID tag for @"SAM_V1_6_Program"@.
newtype SAM_V1_6_Program_Record_Identifier = SAM_V1_6_Program_Record_Identifier
  { sam_v1_6_program_record_identifier_value :: ByteString
  } deriving (Generic,Typeable)

instance Eq SAM_V1_6_Program_Record_Identifier where
  SAM_V1_6_Program_Record_Identifier sam_v1_6_program_record_identifier_value1 ==
    SAM_V1_6_Program_Record_Identifier sam_v1_6_program_record_identifier_value2 =
      sam_v1_6_program_record_identifier_value1 == sam_v1_6_program_record_identifier_value2

instance Show SAM_V1_6_Program_Record_Identifier where
  show (SAM_V1_6_Program_Record_Identifier value) =
    "SAM_V1_6_Program_Record_Identifier { "       ++
    "sam_v1_6_program_record_identifier_value = " ++
    (show value)                                  ++
    " }"

-- | PN tag for @"SAM_V1_6_Program"@.
newtype SAM_V1_6_Program_Name = SAM_V1_6_Program_Name
  { sam_v1_6_program_name_value :: ByteString
  } deriving (Generic,Typeable)

instance Eq SAM_V1_6_Program_Name where
  SAM_V1_6_Program_Name sam_v1_6_program_name_value1 ==
    SAM_V1_6_Program_Name sam_v1_6_program_name_value2 =
      sam_v1_6_program_name_value1 == sam_v1_6_program_name_value2

instance Show SAM_V1_6_Program_Name where
  show (SAM_V1_6_Program_Name value) =
    "SAM_V1_6_Program_Name { "       ++
    "sam_v1_6_program_name_value = " ++
    (show value)                     ++
    " }"

-- | CL tag for @"SAM_V1_6_Program"@.
newtype SAM_V1_6_Program_Command_Line = SAM_V1_6_Program_Command_Line
  { sam_v1_6_program_command_line_value :: ByteString
  } deriving (Generic,Typeable)

instance Eq SAM_V1_6_Program_Command_Line where
  SAM_V1_6_Program_Command_Line sam_v1_6_program_command_line_value1 ==
    SAM_V1_6_Program_Command_Line sam_v1_6_program_command_line_value2 =
      sam_v1_6_program_command_line_value1 == sam_v1_6_program_command_line_value2

instance Show SAM_V1_6_Program_Command_Line where
  show (SAM_V1_6_Program_Command_Line value) =
    "SAM_V1_6_Program_Command_Line { "       ++
    "sam_v1_6_program_command_line_value = " ++
    (show value)                             ++
    " }"

-- | PP tag for @"SAM_V1_6_Program"@.
newtype SAM_V1_6_Program_Previous_PG_ID = SAM_V1_6_Program_Previous_PG_ID
  { sam_v1_6_program_previous_pg_id_value :: ByteString
  } deriving (Generic,Typeable)

instance Eq SAM_V1_6_Program_Previous_PG_ID where
  SAM_V1_6_Program_Previous_PG_ID sam_v1_6_program_previous_pg_id_value1 ==
    SAM_V1_6_Program_Previous_PG_ID sam_v1_6_program_previous_pg_id_value2 =
      sam_v1_6_program_previous_pg_id_value1 == sam_v1_6_program_previous_pg_id_value2

instance Show SAM_V1_6_Program_Previous_PG_ID where
  show (SAM_V1_6_Program_Previous_PG_ID value) =
    "SAM_V1_6_Program_Previous_PG_ID { "       ++
    "sam_v1_6_program_previous_pg_id_value = " ++
    (show value)                               ++
    " }"

-- | DS tag for @"SAM_V1_6_Program"@.
newtype SAM_V1_6_Program_Description = SAM_V1_6_Program_Description
  { sam_v1_6_program_description_value :: ByteString
  } deriving (Generic,Typeable)

instance Eq SAM_V1_6_Program_Description where
  SAM_V1_6_Program_Description sam_v1_6_program_description_value1 ==
    SAM_V1_6_Program_Description sam_v1_6_program_description_value2 =
      sam_v1_6_program_description_value1 == sam_v1_6_program_description_value2

instance Show SAM_V1_6_Program_Description where
  show (SAM_V1_6_Program_Description value) =
    "SAM_V1_6_Program_Description { "       ++
    "sam_v1_6_program_description_value = " ++
    (show value)                            ++
    " }"

-- | VN tag for @"SAM_V1_6_Program"@.
newtype SAM_V1_6_Program_Version = SAM_V1_6_Program_Version
  { sam_v1_6_program_version_value :: ByteString
  } deriving (Generic,Typeable)

instance Eq SAM_V1_6_Program_Version where
  SAM_V1_6_Program_Version sam_v1_6_program_version_value1 ==
    SAM_V1_6_Program_Version sam_v1_6_program_version_value2 =
      sam_v1_6_program_version_value1 == sam_v1_6_program_version_value2

instance Show SAM_V1_6_Program_Version where
  show (SAM_V1_6_Program_Version value) =
    "SAM_V1_6_Program_Version { "       ++
    "sam_v1_6_program_version_value = " ++
    (show value)                        ++
    " }"
