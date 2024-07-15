{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.Read.Base
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.Read.Base ( -- * Reading
                                       readBAM_V1_6
                                     ) where

import Data.BAM.Version1_6.Base
import Data.BAM.Version1_6.Read.Error 
import Data.BAM.Version1_6.Read.Parser.BGZFBlock

import           Data.Attoparsec.ByteString.Lazy   as DABL
import           Data.ByteString                   as DB
import           Data.ByteString.Lazy              as DBL
import           Data.Sequence                     as DSeq
import qualified Streamly.Data.Stream              as S
import           Streamly.External.ByteString.Lazy as StreamlyLByteString  (fromChunksIO)
import           Streamly.Internal.FileSystem.File as StreamlyInternalFile (chunkReader)
import qualified System.IO                         as SIO

-- | Define the @"BAM_V1_6"@ parser.
parse_BAM_V1_6 :: Parser BAM_V1_6
parse_BAM_V1_6 = do
  bgzfblocks <-
    DABL.many' parse_BAM_V1_6_BGZFBlock
  return BAM_V1_6
          { bam_v1_6 = DSeq.fromList bgzfblocks
          }

-- | Run the @"SAM_V1_6"@ parser.
readBAM_V1_6_LBS :: DBL.ByteString
                 -> IO BAM_V1_6
readBAM_V1_6_LBS lbs =
  case (DABL.parseOnly parse_BAM_V1_6 lbs) of 
    Left  samparseerror ->
      error samparseerror
    Right sam           ->
      return sam

-- | Read a @"BAM_V1_6"@ from a file.
--
-- The file is checked for errors as it is parsed.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
readBAM_V1_6 :: FilePath -- ^ Input path to BAM file.
             -> IO BAM_V1_6
readBAM_V1_6 fp = do
  -- Ensure that BAM file is intact
  -- by checking for end-of-file
  -- marker.
  bamfileh        <-
    SIO.openBinaryFile fp
                       SIO.ReadMode
  _               <-
    SIO.hSeek bamfileh
              SIO.SeekFromEnd
              (-28)
  endoffilemarker <-
    DB.hGetContents bamfileh
  case ( endoffilemarker == endoffilemarkerbytes
       ) of
    False ->
      error $
        show BAM_V1_6_Read_Error_End_Of_File_Marker_Incorrect_Format
    True  -> do
      let lazybamfile = S.unfold StreamlyInternalFile.chunkReader fp
      lazybamfilef    <-
        StreamlyLByteString.fromChunksIO lazybamfile
      readBAM_V1_6_LBS lazybamfilef
  where
    endoffilemarkerbytes = DB.pack 
                             [ 0x1f
                             , 0x8b
                             , 0x08
                             , 0x04
                             , 0x00
                             , 0x00
                             , 0x00
                             , 0x00
                             , 0x00
                             , 0xff
                             , 0x06
                             , 0x00
                             , 0x42
                             , 0x43
                             , 0x02
                             , 0x00
                             , 0x1b
                             , 0x00
                             , 0x03
                             , 0x00
                             , 0x00
                             , 0x00
                             , 0x00
                             , 0x00
                             , 0x00
                             , 0x00
                             , 0x00
                             , 0x00
                             ]
