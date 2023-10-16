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
import Generics.Deriving.Base

-- | Custom SAM (version 1.6) @"SAM_V1_6_Reference_Sequence_Dictionary"@ data type.
--
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

instance Eq SAM_V1_6_Reference_Sequence_Dictionary where
  SAM_V1_6_Reference_Sequence_Dictionary sam_v1_6_reference_sequence_dictionary_reference_sequence_name1
                                         sam_v1_6_reference_sequence_dictionary_reference_sequence_length1
                                         sam_v1_6_reference_sequence_dictionary_reference_alternative_locus1
                                         sam_v1_6_reference_sequence_dictionary_reference_alternative_reference_sequence_names1
                                         sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier1
                                         sam_v1_6_reference_sequence_dictionary_description1
                                         sam_v1_6_reference_sequence_dictionary_md5_checksum1
                                         sam_v1_6_reference_sequence_dictionary_species1
                                         sam_v1_6_reference_sequence_dictionary_molecule_topology1
                                         sam_v1_6_reference_sequence_dictionary_uri1 == SAM_V1_6_Reference_Sequence_Dictionary sam_v1_6_reference_sequence_dictionary_reference_sequence_name2
                                                                                                                               sam_v1_6_reference_sequence_dictionary_reference_sequence_length2
                                                                                                                               sam_v1_6_reference_sequence_dictionary_reference_alternative_locus2
                                                                                                                               sam_v1_6_reference_sequence_dictionary_reference_alternative_reference_sequence_names2
                                                                                                                               sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier2
                                                                                                                               sam_v1_6_reference_sequence_dictionary_description2
                                                                                                                               sam_v1_6_reference_sequence_dictionary_md5_checksum2
                                                                                                                               sam_v1_6_reference_sequence_dictionary_species2
                                                                                                                               sam_v1_6_reference_sequence_dictionary_molecule_topology2
                                                                                                                               sam_v1_6_reference_sequence_dictionary_uri2 = sam_v1_6_reference_sequence_dictionary_reference_sequence_name1                        == sam_v1_6_reference_sequence_dictionary_reference_sequence_name2                        &&
                                                                                                                                                                             sam_v1_6_reference_sequence_dictionary_reference_sequence_length1                      == sam_v1_6_reference_sequence_dictionary_reference_sequence_length2                      &&
                                                                                                                                                                             sam_v1_6_reference_sequence_dictionary_reference_alternative_locus1                    == sam_v1_6_reference_sequence_dictionary_reference_alternative_locus2                    &&
                                                                                                                                                                             sam_v1_6_reference_sequence_dictionary_reference_alternative_reference_sequence_names1 == sam_v1_6_reference_sequence_dictionary_reference_alternative_reference_sequence_names2 &&
                                                                                                                                                                             sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier1                     == sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier2                     &&
                                                                                                                                                                             sam_v1_6_reference_sequence_dictionary_description1                                    == sam_v1_6_reference_sequence_dictionary_description2                                    &&
                                                                                                                                                                             sam_v1_6_reference_sequence_dictionary_md5_checksum1                                   == sam_v1_6_reference_sequence_dictionary_md5_checksum2                                   &&
                                                                                                                                                                             sam_v1_6_reference_sequence_dictionary_species1                                        == sam_v1_6_reference_sequence_dictionary_species2                                        &&
                                                                                                                                                                             sam_v1_6_reference_sequence_dictionary_molecule_topology1                              == sam_v1_6_reference_sequence_dictionary_molecule_topology2                              &&
                                                                                                                                                                             sam_v1_6_reference_sequence_dictionary_uri1                                            == sam_v1_6_reference_sequence_dictionary_uri2

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
newtype SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name = SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name { sam_v1_6_reference_sequence_dictionary_reference_sequence_name_value :: ByteString 
                                                                                                                                        }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name where
  SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name sam_v1_6_reference_sequence_dictionary_reference_sequence_name_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name sam_v1_6_reference_sequence_dictionary_reference_sequence_name_value2 = sam_v1_6_reference_sequence_dictionary_reference_sequence_name_value1 == sam_v1_6_reference_sequence_dictionary_reference_sequence_name_value2  

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name { " ++
    "value = "                                                          ++
    (show value)                                                        ++
    " }"

-- | LN tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
newtype SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length = SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length { sam_v1_6_reference_sequence_dictionary_reference_sequence_length_value :: ByteString
                                                                                                                                            }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length where
  SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length sam_v1_6_reference_sequence_dictionary_reference_sequence_length_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length sam_v1_6_reference_sequence_dictionary_reference_sequence_length_value2 = sam_v1_6_reference_sequence_dictionary_reference_sequence_length_value1 == sam_v1_6_reference_sequence_dictionary_reference_sequence_length_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length { " ++
    "value = "                                                            ++
    (show value)                                                          ++
    " }"

-- | AH tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
newtype SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus = SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus { sam_v1_6_reference_sequence_dictionary_alternative_locus_value :: ByteString
                                                                                                                            }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus where
  SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus sam_v1_6_reference_sequence_dictionary_alternative_locus_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus sam_v1_6_reference_sequence_dictionary_alternative_locus_value2 = sam_v1_6_reference_sequence_dictionary_alternative_locus_value1 == sam_v1_6_reference_sequence_dictionary_alternative_locus_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus { " ++
    "value = "                                                    ++
    (show value)                                                  ++
    " }"

-- | AN tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
newtype SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names = SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names { sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value :: ByteString
                                                                                                                                                                  }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names where
  SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value2 = sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value1 == sam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names { " ++
    "value = "                                                                       ++
    (show value)                                                                     ++
    " }"

-- | AS tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
newtype SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier = SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier { sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value :: ByteString
                                                                                                                                              }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier where
  SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value2 = sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value1 == sam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier { " ++
    "value = "                                                             ++
    (show value)                                                           ++
    " }"

-- | DS tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
newtype SAM_V1_6_Reference_Sequence_Dictionary_Description = SAM_V1_6_Reference_Sequence_Dictionary_Description { sam_v1_6_reference_sequence_dictionary_description_value :: ByteString
                                                                                                                }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Description where
  SAM_V1_6_Reference_Sequence_Dictionary_Description sam_v1_6_reference_sequence_dictionary_description_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Description sam_v1_6_reference_sequence_dictionary_description_value2 = sam_v1_6_reference_sequence_dictionary_description_value1 == sam_v1_6_reference_sequence_dictionary_description_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Description where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Description value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_Description { " ++
    "value = "                                              ++
    (show value)                                            ++
    " }"

-- | M5 tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
newtype SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum = SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum { sam_v1_6_reference_sequence_dictionary_md5_checksum_value :: ByteString
                                                                                                                  }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum where
  SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum sam_v1_6_reference_sequence_dictionary_md5_checksum_value1 == SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum sam_v1_6_reference_sequence_dictionary_md5_checksum_value2 = sam_v1_6_reference_sequence_dictionary_md5_checksum_value1 == sam_v1_6_reference_sequence_dictionary_md5_checksum_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum where
  show (SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum { " ++
    "value = "                                               ++
    (show value)                                             ++
    " }"

-- | SP tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
newtype SAM_V1_6_Reference_Sequence_Dictionary_Species = SAM_V1_6_Reference_Sequence_Dictionary_Species { sam_v1_6_reference_sequence_dictionary_species_value :: ByteString
                                                                                                        }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Species where
  SAM_V1_6_Reference_Sequence_Dictionary_Species sam_v1_6_reference_sequence_dictionary_species_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Species sam_v1_6_reference_sequence_dictionary_species_value2 = sam_v1_6_reference_sequence_dictionary_species_value1 == sam_v1_6_reference_sequence_dictionary_species_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Species where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Species value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_Species { " ++
    "value = "                                          ++
    (show value)                                        ++
    " }"

-- | TP tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
newtype SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology = SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology { sam_v1_6_reference_sequence_dictionary_molecule_topology_value :: ByteString
                                                                                                                            }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology where
  SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology sam_v1_6_reference_sequence_dictionary_molecule_topology_value1 == SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology sam_v1_6_reference_sequence_dictionary_molecule_topology_value2 = sam_v1_6_reference_sequence_dictionary_molecule_topology_value1 == sam_v1_6_reference_sequence_dictionary_molecule_topology_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology where
  show (SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology { " ++
    "value = "                                                    ++
    (show value)                                                  ++
    " }"

-- | UR tag for @"SAM_V1_6_Reference_Sequence_Dictionary"@.
newtype SAM_V1_6_Reference_Sequence_Dictionary_URI = SAM_V1_6_Reference_Sequence_Dictionary_URI { sam_v1_6_reference_sequence_dictionary_uri_value :: ByteString
                                                                                                }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_Reference_Sequence_Dictionary_URI where
  SAM_V1_6_Reference_Sequence_Dictionary_URI sam_v1_6_reference_sequence_dictionary_uri_value1 == SAM_V1_6_Reference_Sequence_Dictionary_URI sam_v1_6_reference_sequence_dictionary_uri_value2 = sam_v1_6_reference_sequence_dictionary_uri_value1 == sam_v1_6_reference_sequence_dictionary_uri_value2

instance Show SAM_V1_6_Reference_Sequence_Dictionary_URI where
  show (SAM_V1_6_Reference_Sequence_Dictionary_URI value) =
    "SAM_V1_6_Reference_Sequence_Dictionary_URI { " ++
    "value = "                                      ++
    (show value)                                    ++
    " }"
