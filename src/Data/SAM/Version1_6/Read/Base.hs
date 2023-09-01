{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# Language QuasiQuotes           #-}

-- |
-- Module      :  Data.SAM.Version1_6.Read.Base
-- Copyright   :  (c) Matthew Mosior 2023
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.SAM.Version1_6.Read.Base ( -- * Reading
                                       readSAM_V1_6
                                     ) where

import Data.SAM.Version1_6.Base
import Data.SAM.Version1_6.Read.Parser.Header.HD.Base
import Data.SAM.Version1_6.Read.Parser.Header.SQ.Base
import Data.SAM.Version1_6.Read.Parser.Header.RG.Base
import Data.SAM.Version1_6.Read.Parser.Header.PG.Base
import Data.SAM.Version1_6.Read.Parser.Header.CO.Base
import Data.SAM.Version1_6.Read.Parser.Alignment.Base

import Data.Attoparsec.ByteString.Char8  as DABC8
import Data.Attoparsec.ByteString.Lazy   as DABL
import Data.ByteString.Lazy              as DBL
import Data.Sequence                     as DSeq
import qualified Streamly.Data.Stream    as S
import Streamly.External.ByteString.Lazy as StreamlyLByteString (fromChunksIO)
import Streamly.Internal.FileSystem.File as StreamlyInternalFile (chunkReader)

-- | Make a parser optional, return Nothing if there is no match.
maybeOption :: Parser a
            -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

-- | Define the @"SAM_V1_6"@ parser.
parse_SAM_V1_6 :: Parser SAM_V1_6
parse_SAM_V1_6 = do
  filelevelmetadata           <- maybeOption $ parse_SAM_V1_6_File_Level_Metadata <* endOfLine
  _                           <- word8 10
  referencesequencedictionary <- maybeOption $ DABL.many' $ parse_SAM_V1_6_Reference_Sequence_Dictionary <* endOfLine
  _                           <- word8 10
  readgroup                   <- maybeOption $ DABL.many' $ parse_SAM_V1_6_Read_Group <* endOfLine
  _                           <- word8 10
  program                     <- maybeOption $ parse_SAM_V1_6_Program <* endOfLine
  _                           <- word8 10
  onelinecomment              <- maybeOption $ DABL.many' $ parse_SAM_V1_6_One_Line_Comment <* endOfLine 
  _                           <- word8 10
  alignment                   <- DABL.many' $ parse_SAM_V1_6_Alignment <* endOfLine
  return SAM_V1_6 { sam_v1_6_file_level_metadata           = filelevelmetadata
                  , sam_v1_6_reference_sequence_dictionary = case referencesequencedictionary of
                                                               Nothing                           -> Nothing
                                                               Just referencesequencedictionaryf -> Just $ DSeq.fromList referencesequencedictionaryf
                  , sam_v1_6_read_group                    = case readgroup of
                                                               Nothing         -> Nothing
                                                               Just readgroupf -> Just $ DSeq.fromList readgroupf
                  , sam_v1_6_program                       = program
                  , sam_v1_6_one_line_comment              = case onelinecomment of
                                                               Nothing              -> Nothing
                                                               Just onelinecommentf -> Just $ DSeq.fromList onelinecommentf
                  , sam_v1_6_alignment                     = DSeq.fromList alignment
                  } 

-- | Run the @"SAM_V1_6"@ parser.
readSAM_V1_6_LBS :: DBL.ByteString
                 -> IO SAM_V1_6
readSAM_V1_6_LBS lbs =
  case (DABL.parseOnly parse_SAM_V1_6 lbs) of
    Left  samparseerror -> error samparseerror
    Right sam           -> return sam

-- | Read a @"SAM_V1_6"@ from a file.
--
-- The file is checked for errors as it parses the SAM file.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
readSAM_V1_6 :: FilePath -- ^ Path to SAM file.
             -> IO SAM_V1_6
readSAM_V1_6 fp = do
  let lazysamfile = S.unfold StreamlyInternalFile.chunkReader fp
  lazysamfilef    <- StreamlyLByteString.fromChunksIO lazysamfile
  readSAM_V1_6_LBS lazysamfilef
