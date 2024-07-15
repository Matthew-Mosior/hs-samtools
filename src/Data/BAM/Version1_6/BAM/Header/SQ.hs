{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.BAM.Header.SQ
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.BAM.Header.SQ ( -- * BAM version 1.6 Reference sequence dictionary data type
                                           BAM_V1_6_Reference_Sequence_Dictionary(..),
                                           -- * BAM version 1.6 Reference sequence dictionary data types
                                           BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name(..),
                                           BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length(..),
                                           BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus(..),
                                           BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names(..),
                                           BAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier(..),
                                           BAM_V1_6_Reference_Sequence_Dictionary_Description(..),
                                           BAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum(..),
                                           BAM_V1_6_Reference_Sequence_Dictionary_Species(..),
                                           BAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology(..),
                                           BAM_V1_6_Reference_Sequence_Dictionary_URI(..)
                                         ) where

import Data.ByteString
import Data.Data
import Generics.Deriving.Base

-- | Custom BAM (version 1.6) @"BAM_V1_6_Reference_Sequence_Dictionary"@ data type.
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_Reference_Sequence_Dictionary = BAM_V1_6_Reference_Sequence_Dictionary { bam_v1_6_reference_sequence_dictionary_reference_sequence_name                        :: BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name
                                                                                     , bam_v1_6_reference_sequence_dictionary_reference_sequence_length                      :: BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length
                                                                                     , bam_v1_6_reference_sequence_dictionary_alternative_locus                              :: Maybe BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus
                                                                                     , bam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names           :: Maybe BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names
                                                                                     , bam_v1_6_reference_sequence_dictionary_genome_assembly_identifier                     :: Maybe BAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier
                                                                                     , bam_v1_6_reference_sequence_dictionary_description                                    :: Maybe BAM_V1_6_Reference_Sequence_Dictionary_Description
                                                                                     , bam_v1_6_reference_sequence_dictionary_md5_checksum                                   :: Maybe BAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum
                                                                                     , bam_v1_6_reference_sequence_dictionary_species                                        :: Maybe BAM_V1_6_Reference_Sequence_Dictionary_Species
                                                                                     , bam_v1_6_reference_sequence_dictionary_molecule_topology                              :: Maybe BAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology                                       
                                                                                     , bam_v1_6_reference_sequence_dictionary_uri                                            :: Maybe BAM_V1_6_Reference_Sequence_Dictionary_URI 
                                                                                     }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Reference_Sequence_Dictionary where
  BAM_V1_6_Reference_Sequence_Dictionary bam_v1_6_reference_sequence_dictionary_reference_sequence_name1
                                         bam_v1_6_reference_sequence_dictionary_reference_sequence_length1
                                         bam_v1_6_reference_sequence_dictionary_reference_alternative_locus1
                                         bam_v1_6_reference_sequence_dictionary_reference_alternative_reference_sequence_names1
                                         bam_v1_6_reference_sequence_dictionary_genome_assembly_identifier1
                                         bam_v1_6_reference_sequence_dictionary_description1
                                         bam_v1_6_reference_sequence_dictionary_md5_checksum1
                                         bam_v1_6_reference_sequence_dictionary_species1
                                         bam_v1_6_reference_sequence_dictionary_molecule_topology1
                                         bam_v1_6_reference_sequence_dictionary_uri1 ==
    BAM_V1_6_Reference_Sequence_Dictionary bam_v1_6_reference_sequence_dictionary_reference_sequence_name2
                                           bam_v1_6_reference_sequence_dictionary_reference_sequence_length2
                                           bam_v1_6_reference_sequence_dictionary_reference_alternative_locus2
                                           bam_v1_6_reference_sequence_dictionary_reference_alternative_reference_sequence_names2
                                           bam_v1_6_reference_sequence_dictionary_genome_assembly_identifier2
                                           bam_v1_6_reference_sequence_dictionary_description2
                                           bam_v1_6_reference_sequence_dictionary_md5_checksum2
                                           bam_v1_6_reference_sequence_dictionary_species2
                                           bam_v1_6_reference_sequence_dictionary_molecule_topology2
                                           bam_v1_6_reference_sequence_dictionary_uri2 =
      bam_v1_6_reference_sequence_dictionary_reference_sequence_name1                        == bam_v1_6_reference_sequence_dictionary_reference_sequence_name2                        &&
      bam_v1_6_reference_sequence_dictionary_reference_sequence_length1                      == bam_v1_6_reference_sequence_dictionary_reference_sequence_length2                      &&
      bam_v1_6_reference_sequence_dictionary_reference_alternative_locus1                    == bam_v1_6_reference_sequence_dictionary_reference_alternative_locus2                    &&
      bam_v1_6_reference_sequence_dictionary_reference_alternative_reference_sequence_names1 == bam_v1_6_reference_sequence_dictionary_reference_alternative_reference_sequence_names2 &&
      bam_v1_6_reference_sequence_dictionary_genome_assembly_identifier1                     == bam_v1_6_reference_sequence_dictionary_genome_assembly_identifier2                     &&
      bam_v1_6_reference_sequence_dictionary_description1                                    == bam_v1_6_reference_sequence_dictionary_description2                                    &&
      bam_v1_6_reference_sequence_dictionary_md5_checksum1                                   == bam_v1_6_reference_sequence_dictionary_md5_checksum2                                   &&
      bam_v1_6_reference_sequence_dictionary_species1                                        == bam_v1_6_reference_sequence_dictionary_species2                                        &&
      bam_v1_6_reference_sequence_dictionary_molecule_topology1                              == bam_v1_6_reference_sequence_dictionary_molecule_topology2                              &&
      bam_v1_6_reference_sequence_dictionary_uri1                                            == bam_v1_6_reference_sequence_dictionary_uri2

instance Show BAM_V1_6_Reference_Sequence_Dictionary where
  show (BAM_V1_6_Reference_Sequence_Dictionary reference_sequence_name
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
    "BAM_V1_6_Reference_Sequence_Dictionary { "                                         ++
    "bam_v1_6_reference_sequence_dictionary_reference_sequence_name = "                 ++
    (show reference_sequence_name)                                                      ++
    " , bam_v1_6_reference_sequence_dictionary_reference_sequence_length = "            ++
    (show reference_sequence_length)                                                    ++
    " , bam_v1_6_reference_sequence_dictionary_alternative_locus = "                    ++
    (show reference_alternative_locus)                                                  ++
    " , bam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names = " ++
    (show reference_alternative_sequence_names)                                         ++
    " , bam_v1_6_reference_sequence_dictionary_genome_assembly_identifier = "           ++
    (show genome_assembly_identifier)                                                   ++
    " , bam_v1_6_reference_sequence_dictionary_description = "                          ++
    (show description)                                                                  ++
    " , bam_v1_6_reference_sequence_dictionary_md5_checksum = "                         ++
    (show md5_checksum)                                                                 ++
    " , bam_v1_6_reference_sequence_dictionary_species = "                              ++
    (show species)                                                                      ++
    " , bam_v1_6_reference_sequence_dictionary_molecule_topology = "                    ++
    (show molecule_topology)                                                            ++
    " , bam_v1_6_reference_sequence_dictionary_uri = "                                  ++
    (show uri)                                                                          ++
    " }"

-- | SN tag for @"BAM_V1_6_Reference_Sequence_Dictionary"@.
newtype BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name = BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name { bam_v1_6_reference_sequence_dictionary_reference_sequence_name_value :: ByteString 
                                                                                                                                        }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name where
  BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name bam_v1_6_reference_sequence_dictionary_reference_sequence_name_value1 ==
    BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name bam_v1_6_reference_sequence_dictionary_reference_sequence_name_value2 =
      bam_v1_6_reference_sequence_dictionary_reference_sequence_name_value1 == bam_v1_6_reference_sequence_dictionary_reference_sequence_name_value2  

instance Show BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name where
  show (BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name value) =
    "BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Name { "       ++
    "bam_v1_6_reference_sequence_dictionary_reference_sequence_name_value = " ++
    (show value)                                                              ++
    " }"

-- | LN tag for @"BAM_V1_6_Reference_Sequence_Dictionary"@.
newtype BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length = BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length { bam_v1_6_reference_sequence_dictionary_reference_sequence_length_value :: ByteString
                                                                                                                                            }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length where
  BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length bam_v1_6_reference_sequence_dictionary_reference_sequence_length_value1 ==
    BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length bam_v1_6_reference_sequence_dictionary_reference_sequence_length_value2 =
      bam_v1_6_reference_sequence_dictionary_reference_sequence_length_value1 == bam_v1_6_reference_sequence_dictionary_reference_sequence_length_value2

instance Show BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length where
  show (BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length value) =
    "BAM_V1_6_Reference_Sequence_Dictionary_Reference_Sequence_Length { "       ++
    "bam_v1_6_reference_sequence_dictionary_reference_sequence_length_value = " ++
    (show value)                                                                ++
    " }"

-- | AH tag for @"BAM_V1_6_Reference_Sequence_Dictionary"@.
newtype BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus = BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus { bam_v1_6_reference_sequence_dictionary_alternative_locus_value :: ByteString
                                                                                                                            }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus where
  BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus bam_v1_6_reference_sequence_dictionary_alternative_locus_value1 ==
    BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus bam_v1_6_reference_sequence_dictionary_alternative_locus_value2 =
      bam_v1_6_reference_sequence_dictionary_alternative_locus_value1 == bam_v1_6_reference_sequence_dictionary_alternative_locus_value2

instance Show BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus where
  show (BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus value) =
    "BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Locus { "       ++
    "bam_v1_6_reference_sequence_dictionary_alternative_locus_value = " ++
    (show value)                                                        ++
    " }"

-- | AN tag for @"BAM_V1_6_Reference_Sequence_Dictionary"@.
newtype BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names = BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names { bam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value :: ByteString
                                                                                                                                                                  }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names where
  BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names bam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value1 ==
    BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names bam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value2 =
      bam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value1 == bam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value2

instance Show BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names where
  show (BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names value) =
    "BAM_V1_6_Reference_Sequence_Dictionary_Alternative_Reference_Sequence_Names { "       ++
    "bam_v1_6_reference_sequence_dictionary_alternative_reference_sequence_names_value = " ++
    (show value)                                                                           ++
    " }"

-- | AS tag for @"BAM_V1_6_Reference_Sequence_Dictionary"@.
newtype BAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier = BAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier { bam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value :: ByteString
                                                                                                                                              }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier where
  BAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier bam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value1 ==
    BAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier bam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value2 =
      bam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value1 == bam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value2

instance Show BAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier where
  show (BAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier value) =
    "BAM_V1_6_Reference_Sequence_Dictionary_Genome_Assembly_Identifier { "       ++
    "bam_v1_6_reference_sequence_dictionary_genome_assembly_identifier_value = " ++
    (show value)                                                                 ++
    " }"

-- | DS tag for @"BAM_V1_6_Reference_Sequence_Dictionary"@.
newtype BAM_V1_6_Reference_Sequence_Dictionary_Description = BAM_V1_6_Reference_Sequence_Dictionary_Description { bam_v1_6_reference_sequence_dictionary_description_value :: ByteString
                                                                                                                }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Reference_Sequence_Dictionary_Description where
  BAM_V1_6_Reference_Sequence_Dictionary_Description bam_v1_6_reference_sequence_dictionary_description_value1 ==
    BAM_V1_6_Reference_Sequence_Dictionary_Description bam_v1_6_reference_sequence_dictionary_description_value2 =
      bam_v1_6_reference_sequence_dictionary_description_value1 == bam_v1_6_reference_sequence_dictionary_description_value2

instance Show BAM_V1_6_Reference_Sequence_Dictionary_Description where
  show (BAM_V1_6_Reference_Sequence_Dictionary_Description value) =
    "BAM_V1_6_Reference_Sequence_Dictionary_Description { "       ++
    "bam_v1_6_reference_sequence_dictionary_description_value = " ++
    (show value)                                                  ++
    " }"

-- | M5 tag for @"BAM_V1_6_Reference_Sequence_Dictionary"@.
newtype BAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum = BAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum { bam_v1_6_reference_sequence_dictionary_md5_checksum_value :: ByteString
                                                                                                                  }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum where
  BAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum bam_v1_6_reference_sequence_dictionary_md5_checksum_value1 ==
    BAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum bam_v1_6_reference_sequence_dictionary_md5_checksum_value2 =
      bam_v1_6_reference_sequence_dictionary_md5_checksum_value1 == bam_v1_6_reference_sequence_dictionary_md5_checksum_value2

instance Show BAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum where
  show (BAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum value) =
    "BAM_V1_6_Reference_Sequence_Dictionary_MD5_Checksum { "       ++
    "bam_v1_6_reference_sequence_dictionary_md5_checksum_value = " ++
    (show value)                                                   ++
    " }"

-- | SP tag for @"BAM_V1_6_Reference_Sequence_Dictionary"@.
newtype BAM_V1_6_Reference_Sequence_Dictionary_Species = BAM_V1_6_Reference_Sequence_Dictionary_Species { bam_v1_6_reference_sequence_dictionary_species_value :: ByteString
                                                                                                        }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Reference_Sequence_Dictionary_Species where
  BAM_V1_6_Reference_Sequence_Dictionary_Species bam_v1_6_reference_sequence_dictionary_species_value1 ==
    BAM_V1_6_Reference_Sequence_Dictionary_Species bam_v1_6_reference_sequence_dictionary_species_value2 =
      bam_v1_6_reference_sequence_dictionary_species_value1 == bam_v1_6_reference_sequence_dictionary_species_value2

instance Show BAM_V1_6_Reference_Sequence_Dictionary_Species where
  show (BAM_V1_6_Reference_Sequence_Dictionary_Species value) =
    "BAM_V1_6_Reference_Sequence_Dictionary_Species { "       ++
    "bam_v1_6_reference_sequence_dictionary_species_value = " ++
    (show value)                                              ++
    " }"

-- | TP tag for @"BAM_V1_6_Reference_Sequence_Dictionary"@.
newtype BAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology = BAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology { bam_v1_6_reference_sequence_dictionary_molecule_topology_value :: ByteString
                                                                                                                            }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology where
  BAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology bam_v1_6_reference_sequence_dictionary_molecule_topology_value1 ==
    BAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology bam_v1_6_reference_sequence_dictionary_molecule_topology_value2 =
      bam_v1_6_reference_sequence_dictionary_molecule_topology_value1 == bam_v1_6_reference_sequence_dictionary_molecule_topology_value2

instance Show BAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology where
  show (BAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology value) =
    "BAM_V1_6_Reference_Sequence_Dictionary_Molecule_Topology { "       ++
    "bam_v1_6_reference_sequence_dictionary_molecule_topology_value = " ++
    (show value)                                                        ++
    " }"

-- | UR tag for @"BAM_V1_6_Reference_Sequence_Dictionary"@.
newtype BAM_V1_6_Reference_Sequence_Dictionary_URI = BAM_V1_6_Reference_Sequence_Dictionary_URI { bam_v1_6_reference_sequence_dictionary_uri_value :: ByteString
                                                                                                }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_Reference_Sequence_Dictionary_URI where
  BAM_V1_6_Reference_Sequence_Dictionary_URI bam_v1_6_reference_sequence_dictionary_uri_value1 ==
    BAM_V1_6_Reference_Sequence_Dictionary_URI bam_v1_6_reference_sequence_dictionary_uri_value2 =
      bam_v1_6_reference_sequence_dictionary_uri_value1 == bam_v1_6_reference_sequence_dictionary_uri_value2

instance Show BAM_V1_6_Reference_Sequence_Dictionary_URI where
  show (BAM_V1_6_Reference_Sequence_Dictionary_URI value) =
    "BAM_V1_6_Reference_Sequence_Dictionary_URI { "       ++
    "bam_v1_6_reference_sequence_dictionary_uri_value = " ++
    (show value)                                          ++
    " }"
