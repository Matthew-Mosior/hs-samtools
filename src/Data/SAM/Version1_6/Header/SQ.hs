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
-- Module      :  Data.SAM.Version1_6.Header.SQ
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

module Data.SAM.Version1_6.Header.SQ ( -- * SAM version 1.6 Reference sequence dictionary data type
                                       SAM_V1_6_Reference_Sequence_Dictionary(..),
                                       -- * SAM version 1.6 Reference sequence dictionary data types
                                       SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name(..),
                                       SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length(..),
                                       SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus(..),
                                       SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names(..),
                                       SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier(..),
                                       SAM_V1_6_Reference_Sequence_Dictionary_Description(..),
                                       SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum(..),
                                       SAM_V1_6_Reference_Sequence_Dictionary_Species(..),
                                       SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology(..),
                                       SAM_V1_6_Reference_Sequence_Dictionary_URI(..)
                                     ) where

import Data.ByteString
import Data.Data
import Data.Sequence
import Data.Word
import Generics.Deriving.Base


-- | Custom SAM (version 1.6) @"SAM_V1_6_Reference_Sequence_Dictionary"@ data type.
-- See section 1.3 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Reference_Sequence_Dictionary = SAM_V1_6_Reference_Sequence_Dictionary { sam_v1_6_reference_sequence_dictionary_reference_sequence_name                        :: SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name
                                                                                     , sam_v1_6_reference_sequence_dictionary_reference_sequence_length                      :: SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length
                                                                                     , sam_v1_6_reference_sequence_dictionary_reference_alternative_locus                    :: Maybe SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus
                                                                                     , sam_v1_6_reference_sequence_dictionary_reference_alternative_reference_sequence_names :: Maybe SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names
                                                                                     , sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier                     :: Maybe SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier
                                                                                     , sam_v1_6_reference_sequence_dictionary_description                                    :: Maybe SAM_V1_6_Reference_Sequence_Dictionary_Description
                                                                                     , sam_v1_6_reference_sequence_dictionary_md5_checksum                                   :: Maybe SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum
                                                                                     , sam_v1_6_reference_sequence_dictionary_species                                        :: Maybe SAM_V1_6_Reference_Sequence_Dictionary_Species
                                                                                     , sam_v1_6_reference_sequence_dictionary_molecule_topology                              :: Maybe SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology                                       
                                                                                     , sam_v1_6_reference_sequence_dictionary_uri                                            :: Maybe SAM_V1_6_Reference_Sequence_Dictionary_URI 
                                                                                     }
  deriving (Generic,Typeable)

instance Show SAM_V1_6_Reference_Sequence_Dictionary where
  show (SAM_V1_6_Reference_Sequence_Dictionary reference_sequence_name
                                               reference_sequence_length
                                               reference_alternative_locus
                                               reference_alternative_sequence_names
                                               genome_assembly_identifier
                                               description
                                               md5_checksum
                                               species
                                               molecule_topology
                                               uri
       ) =
    "SAM_V1_6_Reference_Sequence_Dictionary { "  ++
    "reference_sequence_name = "                 ++
    (show reference_sequence_name)               ++
    " , reference_sequence_length = "            ++
    (show reference_sequence_length)             ++
    " , reference_alternative_locus = "          ++
    (show reference_alternative_locus)           ++
    " , reference_alternative_sequence_names = " ++
    (show reference_alternative_sequence_names)  ++
    " , genome_assembly_identifier = "           ++
    (show genome_assembly_identifier)            ++
    " , description = "                          ++
    (show description)                           ++
    " , md5_checksum = "                         ++
    (show md5_checksum)                          ++
    " , species = "                              ++
    (show species)                               ++
    " , molecule_topology = "                    ++
    (show molecule_topology)                     ++
    " , uri = "                                  ++
    (show uri)                                   ++
    " }"

-- | SN tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
data SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name = SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name { sam_v1_6_reference_sequence_dictionary_reference_sequence_name_tag   :: Seq Word8
                                                                                                                                     , sam_v1_6_reference_sequence_dictionary_reference_sequence_name_value :: ByteString 
                         }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name where
  SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name sam_v1_6_reference_sequence_dictionary_reference_sequence_name_tag1 sam_v1_6_reference_sequence_dictionary_reference_sequence_name_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name sam_v1_6_reference_sequence_dictionary_reference_sequence_name_tag2 sam_v1_6_reference_sequence_dictionary_reference_sequence_name_value2 = sam_v1_6_reference_sequence_dictionary_reference_sequence_name_tag1   == sam_v1_6_reference_sequence_dictionary_reference_sequence_name_tag2 && sam_v1_6_reference_sequence_dictionary_reference_sequence_name_value1 == sam_v1_6_reference_sequence_dictionary_reference_sequence_name_value2  

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name tag value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name { " ++
    "tag = "                                                            ++
    (show tag)                                                          ++
    " , value = "                                                       ++
    (show value)                                                        ++
    " }"

-- | LN tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
data SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length = SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length { sam_v1_6_reference_sequence_dictionary_reference_sequence_length_tag   :: Seq Word8
                                                                                                                                         , sam_v1_6_reference_sequence_dictionary_reference_sequence_length_value :: ByteString
                             }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length where
  SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length sam_v1_6_reference_sequence_dictionary_reference_sequence_length_tag1 sam_v1_6_reference_sequence_dictionary_reference_sequence_length_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length sam_v1_6_reference_sequence_dictionary_reference_sequence_length_tag2 sam_v1_6_reference_sequence_dictionary_reference_sequence_length_value2 = sam_v1_6_reference_sequence_dictionary_reference_sequence_length_tag1 == sam_v1_6_reference_sequence_dictionary_reference_sequence_length_tag2 && sam_v1_6_reference_sequence_dictionary_reference_sequence_length_value1 == sam_v1_6_reference_sequence_dictionary_reference_sequence_length_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length tag value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length { " ++
    "tag = "                                                              ++
    (show tag)                                                            ++
    " , value = "                                                         ++
    (show value)                                                          ++
    " }"

-- | AH tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
data SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus = SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus { sam_v1_6_reference_sequence_dictionary_reference_alternative_locus_tag   :: Seq Word8
                                                                                                                         , sam_v1_6_reference_sequence_dictionary_reference_alternative_locus_value :: ByteString
             }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus where
  SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus sam_v1_6_reference_sequence_dictionary_alternative_locus_tag1 sam_v1_6_reference_sequence_dictionary_alternative_locus_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus sam_v1_6_reference_sequence_dictionary_alternative_locus_tag2 sam_v1_6_reference_sequence_dictionary_alternative_locus_value2 = sam_v1_6_reference_sequence_dictionary_alternative_locus_tag1 == sam_v1_6_reference_sequence_dictionary_alternative_locus_tag2 && sam_v1_6_reference_sequence_dictionary_alternative_locus_value1 == sam_v1_6_reference_sequence_dictionary_alternative_locus_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus tag value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus { " ++
    "tag = "                                                      ++
    (show tag)                                                    ++
    " , value = "                                                 ++
    (show value)                                                  ++
    " }"

-- | AN tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
data SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names = SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names { sam_v1_6_reference_sequence_dictionary_reference_alternative_reference_sequence_names_tag   :: Seq Word8
                                                                                                                                                               , sam_v1_6_reference_sequence_dictionary_reference_alternative_reference_sequence_names_value :: ByteString
                                                                                                                                                               }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names where
  SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_tag1 sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_tag2 sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value2 = sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_tag1 == sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_tag2 && sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value1 == sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names tag value) = "SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names { " ++
                                                                                                 "tag = "                                                                         ++
                                                                                                 (show tag)                                                                       ++
                                                                                                 " , value = "                                                                    ++
                                                                                                 (show value)                                                                     ++
                                                                                                 " }"

-- | AS tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
data SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier = SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier { sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_tag   :: Seq Word8
                                                                                                                                           , sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value :: ByteString
                                                                                                                                           }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier where
  SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_tag1 sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_tag2 sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value2 = sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_tag1 == sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_tag2 && sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value1 == sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier tag value) = "SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier { " ++
                                                                                       "tag = "                                                               ++
                                                                                       (show tag)                                                             ++
                                                                                       " , value = "                                                          ++
                                                                                       (show value)                                                           ++
                                                                                       " }"

-- | DS tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
data SAM_V1_6_Reference_Sequence_Dictionary_Description = SAM_V1_6_Reference_Sequence_Dictionary_Description { sam_v1_6_reference_sequence_dictionary_description_tag   :: Seq Word8
                                                                                                             , sam_v1_6_reference_sequence_dictionary_description_value :: ByteString
                                                                                                             }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Description where
  SAM_V1_6_Reference_Sequence_Dictionary_Description sam_v1_6_reference_sequence_dictionary_description_tag1 sam_v1_6_reference_sequence_dictionary_description_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Description sam_v1_6_reference_sequence_dictionary_description_tag2 sam_v1_6_reference_sequence_dictionary_description_value2 = sam_v1_6_reference_sequence_dictionary_description_tag1 == sam_v1_6_reference_sequence_dictionary_description_tag2 && sam_v1_6_reference_sequence_dictionary_description_value1 == sam_v1_6_reference_sequence_dictionary_description_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Description where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Description tag value) = "SAM_V1_6_Reference_Sequence_Dictionary_Description { " ++
                                                                        "tag = "                                                ++
                                                                        (show tag)                                              ++
                                                                        " , value = "                                           ++
                                                                        (show value)                                            ++
                                                                        " }"

-- | M5 tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
data SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum = SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum { sam_v1_6_reference_sequence_dictionary_md5_checksum_tag   :: Seq Word8
                                                                                                               , sam_v1_6_reference_sequence_dictionary_md5_checksum_value :: ByteString
                                                                                                               }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum where
  SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum sam_v1_6_reference_sequence_dictionary_md5_checksum_tag1 sam_v1_6_reference_sequence_dictionary_md5_checksum_value1 == SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum sam_v1_6_reference_sequence_dictionary_md5_checksum_tag2 sam_v1_6_reference_sequence_dictionary_md5_checksum_value2 = sam_v1_6_reference_sequence_dictionary_md5_checksum_tag1 == sam_v1_6_reference_sequence_dictionary_md5_checksum_tag2 && sam_v1_6_reference_sequence_dictionary_md5_checksum_value1 == sam_v1_6_reference_sequence_dictionary_md5_checksum_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum where
  show (SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum tag value) = "SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum { " ++
                                                                         "tag = "                                                 ++
                                                                         (show tag)                                               ++
                                                                         " , value = "                                            ++
                                                                         (show value)                                             ++
                                                                         " }"

-- | SP tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
data SAM_V1_6_Reference_Sequence_Dictionary_Species = SAM_V1_6_Reference_Sequence_Dictionary_Species { sam_v1_6_reference_sequence_dictionary_species_tag   :: Seq Word8
                                                                                                     , sam_v1_6_reference_sequence_dictionary_species_value :: ByteString
                                                                                                     }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Species where
  SAM_V1_6_Reference_Sequence_Dictionary_Species sam_v1_6_reference_sequence_dictionary_species_tag1 sam_v1_6_reference_sequence_dictionary_species_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Species sam_v1_6_reference_sequence_dictionary_species_tag2 sam_v1_6_reference_sequence_dictionary_species_value2 = sam_v1_6_reference_sequence_dictionary_species_tag1 == sam_v1_6_reference_sequence_dictionary_species_tag2 && sam_v1_6_reference_sequence_dictionary_species_value1 == sam_v1_6_reference_sequence_dictionary_species_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Species where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Species tag value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_Species { " ++
    "tag = "                                            ++
    (show tag)                                          ++
    " , value = "                                       ++
    (show value)                                        ++
    " }"

-- | TP tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
data SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology = SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology { sam_v1_6_reference_sequence_dictionary_molecule_topology_tag   :: Seq Word8
                                                                                                                         , sam_v1_6_reference_sequence_dictionary_molecule_topology_value :: ByteString
                                                                                                                         }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology where
  SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology sam_v1_6_reference_sequence_dictionary_molecule_topology_tag1 sam_v1_6_reference_sequence_dictionary_molecule_topology_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology sam_v1_6_reference_sequence_dictionary_molecule_topology_tag2 sam_v1_6_reference_sequence_dictionary_molecule_topology_value2 = sam_v1_6_reference_sequence_dictionary_molecule_topology_tag1 == sam_v1_6_reference_sequence_dictionary_molecule_topology_tag2 && sam_v1_6_reference_sequence_dictionary_molecule_topology_value1 == sam_v1_6_reference_sequence_dictionary_molecule_topology_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology tag value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology { " ++
    "tag = "                                                      ++
    (show tag)                                                    ++
    " , value = "                                                 ++
    (show value)                                                  ++
    " }"

-- | UR tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
data SAM_V1_6_Reference_Sequence_Dictionary_URI = SAM_V1_6_Reference_Sequence_Dictionary_URI { sam_v1_6_reference_sequence_dictionary_uri_tag   :: Seq Word8
                                                                                             , sam_v1_6_reference_sequence_dictionary_uri_value :: ByteString
                                                                                             }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_URI where
  SAM_V1_6_Reference_Sequence_Dictionary_URI sam_v1_6_reference_sequence_dictionary_uri_tag1 sam_v1_6_reference_sequence_dictionary_uri_value1 == SAM_V1_6_Reference_Sequence_Dictionary_URI sam_v1_6_reference_sequence_dictionary_uri_tag2 sam_v1_6_reference_sequence_dictionary_uri_value2 = sam_v1_6_reference_sequence_dictionary_uri_tag1 == sam_v1_6_reference_sequence_dictionary_uri_tag2 && sam_v1_6_reference_sequence_dictionary_uri_value1 == sam_v1_6_reference_sequence_dictionary_uri_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_URI where
  show (SAM_V1_6_Reference_Sequence_Dictionary_URI tag value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_URI { " ++
    "tag = "                                        ++
    (show tag)                                      ++
    " , value = "                                   ++
    (show value)                                    ++
    " }"
