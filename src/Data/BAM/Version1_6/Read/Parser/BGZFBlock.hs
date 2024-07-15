{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.Read.Parser.BGZFBlock
-- Copyright   :  (c) Matthew Mosior 2024
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

module Data.BAM.Version1_6.Read.Parser.BGZFBlock ( -- * BAM_V1_6 parser - BGZF block section.
                                                   parse_BAM_V1_6_BGZFBlock
                                                 ) where

import           Data.BAM.Version1_6.Internal
import           Data.BAM.Version1_6.Read.Parser.BAM.Base
import           Data.BAM.Version1_6.BGZFBlock
import           Data.BAM.Version1_6.GZipHeader
import           Data.BAM.Version1_6.Read.Error

import           Codec.Compression.Zlib.Raw        as CCZlibR (decompress)
import           Data.Attoparsec.ByteString.Lazy   as DABL
import qualified Data.ByteString                   as DB      (fromStrict,unpack)
import           Data.Digest.CRC32                            (crc32)
import           Data.Word

-- | Defines a parser for a single BGZF block.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
parse_BAM_V1_6_BGZFBlock :: Parser BAM_V1_6_BGZFBlock
parse_BAM_V1_6_BGZFBlock = do
  gzipheaderidentification1field <- do
    gzipheaderidentification1fieldp <-
      DABL.take 1
    -- Parse GZip header Identification 1 field.
    case ( ( word8sToWord8LE $
               DB.unpack gzipheaderidentification1fieldp
           ) == 0x1f
         ) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_GZipHeader_ID1_Incorrect_Format
      True  -> do
        -- Identification field 1 is in the accepted format.
        return gzipheaderidentification1fieldp
  gzipheaderidentification2field <- do
    gzipheaderidentification2fieldp <-
      DABL.take 1
    -- Parse GZip header Identification 2 field.
    case ( ( word8sToWord8LE $
               DB.unpack gzipheaderidentification2fieldp
           ) == 0x8b
         ) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_GZipHeader_ID2_Incorrect_Format
      True  ->
        -- Identification field 2 is in the accepted format.
        return gzipheaderidentification2fieldp 
  gzipheadercompressionmethodfield <- do
    gzipheadercompressionmethodfieldp <-
      DABL.take 1
    -- Parse GZip header Compression Method field.
    case ( elem
           ( word8sToWord8LE $
               DB.unpack gzipheadercompressionmethodfieldp
           )
           compressionmethodpossiblebytes
         ) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_GZipHeader_Compression_Method_Incorrect_Format
      True  ->
        -- Compression Method is in the accepted format.
        return gzipheadercompressionmethodfieldp
  gzipheaderflagsfield <- do
    gzipheaderflagsfieldp <-
      DABL.take 1
    -- Parse GZip Flags (FLaGs) field.
    case ( elem
           ( word8sToWord8LE $
               DB.unpack gzipheaderflagsfieldp
           )
           flagpossiblebytes
         ) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_GZipHeader_Flag_Incorrect_Format
      True  ->
        -- Flags (FLaGs) is in the accepted format.
        return gzipheaderflagsfieldp
  gzipheadermodificationtimefield <-
    DABL.take 4
  gzipheaderextraflagsfield <-
    DABL.take 1
  gzipheaderoperatingsystemfield <- do
    gzipheaderoperatingsystemfieldp <-
      DABL.take 1
    -- Parse GZip header Operating System field.
    case ( elem
             ( word8sToWord8LE $
                 DB.unpack gzipheaderoperatingsystemfieldp
             )
           operatingsystempossiblebytes
         ) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_GZipHeader_Operating_System_Incorrect_Format
      True  ->
        -- Operating System is in the accepted format.
        return gzipheaderoperatingsystemfieldp
  gzipheaderextralengthfield <-
    DABL.take 2
  let gzipheader = BAM_V1_6_GZipHeader
                     { bam_v1_6_gzip_header_gzip_identifier_1  = word8sToWord8LE $
                                                                   DB.unpack gzipheaderidentification1field
                     , bam_v1_6_gzip_header_gzip_identifier_2  = word8sToWord8LE $
                                                                   DB.unpack gzipheaderidentification2field
                     , bam_v1_6_gzip_header_compression_method = word8sToWord8LE $
                                                                   DB.unpack gzipheadercompressionmethodfield
                     , bam_v1_6_gzip_header_header_flags       = word8sToWord8LE $
                                                                   DB.unpack gzipheaderflagsfield
                     , bam_v1_6_gzip_header_modification_time  = word8sToWord32LE $
                                                                   DB.unpack gzipheadermodificationtimefield
                     , bam_v1_6_gzip_header_extra_flags        = word8sToWord8LE $
                                                                   DB.unpack gzipheaderextraflagsfield
                     , bam_v1_6_gzip_header_operating_system   = word8sToWord8LE $
                                                                   DB.unpack gzipheaderoperatingsystemfield
                     , bam_v1_6_gzip_header_extra_length       = word8sToWord16LE $
                                                                   DB.unpack gzipheaderextralengthfield
                     }
  bgzfblocksubfieldidentifier1field <- do
    bgzfblocksubfieldidentifier1fieldp <-
      DABL.take 1
    -- Parse GZip header Subfield Identifier 1 field.
    case ( ( word8sToWord8LE $
               DB.unpack bgzfblocksubfieldidentifier1fieldp
           ) == 0x42
         ) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_GZipHeader_Subfield_Identifier_1_Incorrect_Format
      True  ->
        -- Subfield Identifier 1 is in the accepted format.
        return bgzfblocksubfieldidentifier1fieldp
  bgzfblocksubfieldidentifier2field <- do
    bgzfblocksubfieldidentifier2fieldp <-
      DABL.take 1
    -- Parse GZip header Subfield Identifier 2 field.
    case ( ( word8sToWord8LE $
               DB.unpack bgzfblocksubfieldidentifier2fieldp
           ) == 0x43
         ) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_GZipHeader_Subfield_Identifier_2_Incorrect_Format
      True  ->
        -- Subfield Identifier 2 is in the accepted format.
        return bgzfblocksubfieldidentifier2fieldp
  bgzfblocksubfieldlengthfield <-
    DABL.take 2
  bgzfblocktotalblocksizeminus1field <-
    DABL.take 2
  bgzfblockcdata <-
    DABL.take $
      ( fromIntegral       $
          word8sToWord16LE $
            DB.unpack bgzfblocktotalblocksizeminus1field
      ) -
      ( fromIntegral       $
          bam_v1_6_gzip_header_extra_length gzipheader
      ) -
      19
  let bgzfblockcdataf = CCZlibR.decompress $
                          DB.fromStrict bgzfblockcdata
  case (parseOnly parse_BAM_V1_6_BAM bgzfblockcdataf) of
    Left _                 ->
      fail $
        show BAM_V1_6_Read_Error_CDATA_Incorrect_Format
    Right bgzfblockcdataf' -> do
      bgzfblockcrc32field <-
        DABL.take 4
      case (crc32 bgzfblockcdataf == ( word8sToWord32LE $
                                         DB.unpack bgzfblockcrc32field
                                     )
           ) of
        False ->
          fail $
            show BAM_V1_6_Read_Error_Calculated_CRC32_Not_Equivalent_To_CRC32
        True  -> do
          bgzfblockisizefield <-
            DABL.take 4
          return BAM_V1_6_BGZFBlock
                   { bam_v1_6_bgzfblock_gzip_header                = gzipheader
                   , bam_v1_6_bgzfblock_subfield_identifier_one    = word8sToWord8LE $
                                                                       DB.unpack bgzfblocksubfieldidentifier1field
                   , bam_v1_6_bgzfblock_subfield_identifier_two    = word8sToWord8LE $
                                                                       DB.unpack bgzfblocksubfieldidentifier2field
                   , bam_v1_6_bgzfblock_subfield_length            = word8sToWord16LE $
                                                                       DB.unpack bgzfblocksubfieldlengthfield
                   , bam_v1_6_bgzfblock_total_block_size_minus_one = word8sToWord16LE $
                                                                       DB.unpack bgzfblocktotalblocksizeminus1field
                   , bam_v1_6_bgzfblock_cdata                      = bgzfblockcdataf'
                   , bam_v1_6_bgzfblock_crc32                      = word8sToWord32LE $
                                                                       DB.unpack bgzfblockcrc32field
                   , bam_v1_6_bgzfblock_isize                      = word8sToWord32LE $
                                                                       DB.unpack bgzfblockisizefield
                   }
  where
    compressionmethodpossiblebytes = [ 0x00
                                     , 0x01
                                     , 0x02
                                     , 0x03
                                     , 0x04
                                     , 0x05
                                     , 0x06
                                     , 0x07
                                     , 0x08
                                     ] :: [Word8]
    flagpossiblebytes = [ 0x00
                        , 0x01
                        , 0x02
                        , 0x03
                        , 0x04
                        , 0x05
                        , 0x06
                        , 0x07
                        ] :: [Word8]
    operatingsystempossiblebytes = [ 0x00
                                   , 0x01
                                   , 0x02
                                   , 0x03
                                   , 0x04
                                   , 0x05
                                   , 0x06
                                   , 0x07
                                   , 0x08
                                   , 0x09
                                   , 0x0A
                                   , 0x0B
                                   , 0x0C
                                   , 0x0D
                                   , 0xFF
                                   ] :: [Word8]
