{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.Read.Parser.BAM.BAMHeader
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.Read.Parser.BAM.BAMHeader ( -- * BAM_V1_6_BAM_BAMHeader parser
                                                       parse_BAM_V1_6_BAM_BAMHeader
                                                     ) where

import Data.BAM.Version1_6.Internal
import Data.BAM.Version1_6.BAM.BAMHeader
import Data.BAM.Version1_6.Read.Error
import Data.BAM.Version1_6.Read.Parser.BAM.ReferenceInformation.Base

import Data.Attoparsec.ByteString.Lazy as DABL
import Data.ByteString                 as DB
import Data.Sequence                   as DSeq

-- | Define the @"BAM_V1_6_BAM_BAMHeader"@ parser.
parse_BAM_V1_6_BAM_BAMHeader :: Parser BAM_V1_6_BAM_BAMHeader
parse_BAM_V1_6_BAM_BAMHeader = do
  magic                 <- do
    magicp <-
      DABL.take 4
    case (magicp == bammagicstring) of
      False ->
        fail $
          show BAM_V1_6_Read_Error_BAMHeader_BAM_Magic_String_Invalid_Value
      True ->
        return magicp
  l_text                <-
    DABL.take 4
  text                  <-
    DABL.take
      ( fromIntegral       $
          word8sToWord32LE $
            DB.unpack l_text -- :: Int
      )
  n_ref                 <-
    DABL.take 4
  reference_information <-
    DABL.count ( fromIntegral       $
                   word8sToWord32LE $
                     DB.unpack n_ref :: Int
               )
               parse_BAM_V1_6_BAM_Reference_Information
  return BAM_V1_6_BAM_BAMHeader
           { bam_v1_6_bam_bamheader_magic                 = magic
           , bam_v1_6_bam_bamheader_l_text                = word8sToWord32LE $
                                                              DB.unpack l_text
           , bam_v1_6_bam_bamheader_text                  = text
           , bam_v1_6_bam_bamheader_n_ref                 = word8sToWord32LE $
                                                              DB.unpack n_ref
           , bam_v1_6_bam_bamheader_reference_information = DSeq.fromList reference_information
           }
  where
    bammagicstring = DB.pack [ 0x42
                             , 0x41
                             , 0x4D
                             , 0x01
                             ]
