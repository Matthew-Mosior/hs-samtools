{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.SAM.Version1_6.Read.Base
-- Copyright   :  (c) Matthew Mosior 2024
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
import Data.SAM.Version1_6.Internal
import Data.SAM.Version1_6.Read.Parser.Header.HD.Base
import Data.SAM.Version1_6.Read.Parser.Header.SQ.Base
import Data.SAM.Version1_6.Read.Parser.Header.RG.Base
import Data.SAM.Version1_6.Read.Parser.Header.PG.Base
import Data.SAM.Version1_6.Read.Parser.Header.CO.Base
import Data.SAM.Version1_6.Read.Parser.Alignment.Base

import            Control.Applicative.Permutations                          (intercalateEffect,toPermutationWithDefault)
import            Data.Attoparsec.ByteString.Char8  as DABC8                (endOfLine)
import            Data.Attoparsec.ByteString.Lazy   as DABL
import            Data.ByteString.Lazy              as DBL
import            Data.Sequence                     as DSeq
import qualified  Streamly.Data.Stream    as S
import            Streamly.External.ByteString.Lazy as StreamlyLByteString  (fromChunksIO)
import            Streamly.Internal.FileSystem.File as StreamlyInternalFile (chunkReader)

-- | Define the @"SAM_V1_6"@ parser.
parse_SAM_V1_6 :: Parser SAM_V1_6
parse_SAM_V1_6 = do
  filelevelmetadata <-
    maybeOption parse_SAM_V1_6_File_Level_Metadata
  case filelevelmetadata of
    Nothing  -> do
      samwoalignment <-
        intercalateEffect endOfLine $
          (,,,)
            <$> toPermutationWithDefault Nothing
                                         (Just <$> DABL.many1' parse_SAM_V1_6_Reference_Sequence_Dictionary)
            <*> toPermutationWithDefault Nothing
                                         (Just <$> DABL.many1' parse_SAM_V1_6_Read_Group)
            <*> toPermutationWithDefault Nothing
                                         (Just <$> parse_SAM_V1_6_Program)
            <*> toPermutationWithDefault Nothing
                                         (Just <$> DABL.many1' parse_SAM_V1_6_One_Line_Comment)
      alignment <-
        DABL.many1' parse_SAM_V1_6_Alignment
      return SAM_V1_6 { sam_v1_6_file_level_metadata           = Nothing
                      , sam_v1_6_reference_sequence_dictionary = (\(a,_,_,_) ->
                                                                   case a of
                                                                     Nothing     ->
                                                                       Nothing
                                                                     Just finala ->
                                                                       Just $
                                                                         DSeq.fromList finala
                                                                 ) samwoalignment
                      , sam_v1_6_read_group                    = (\(_,b,_,_) ->
                                                                   case b of
                                                                     Nothing     ->
                                                                       Nothing
                                                                     Just finalb ->
                                                                       Just $
                                                                         DSeq.fromList finalb
                                                                 ) samwoalignment
                      , sam_v1_6_program                       = (\(_,_,c,_) -> c) samwoalignment
                      , sam_v1_6_one_line_comment              = (\(_,_,_,d) ->
                                                                   case d of
                                                                     Nothing     ->
                                                                       Nothing
                                                                     Just finald ->
                                                                       Just $
                                                                         DSeq.fromList finald
                                                                 ) samwoalignment
                      , sam_v1_6_alignment                     = DSeq.fromList alignment
                      }
    Just flm -> do
      samwoalignment <-
        intercalateEffect endOfLine $
          (,,,)
            <$> toPermutationWithDefault Nothing
                                         (Just <$> DABL.many1' parse_SAM_V1_6_Reference_Sequence_Dictionary)
            <*> toPermutationWithDefault Nothing
                                         (Just <$> DABL.many1' parse_SAM_V1_6_Read_Group)
            <*> toPermutationWithDefault Nothing
                                         (Just <$> parse_SAM_V1_6_Program)
            <*> toPermutationWithDefault Nothing
                                         (Just <$> DABL.many1' parse_SAM_V1_6_One_Line_Comment)
      alignment <-
        DABL.many1' parse_SAM_V1_6_Alignment
      return SAM_V1_6 { sam_v1_6_file_level_metadata           = Just flm
                      , sam_v1_6_reference_sequence_dictionary = (\(a,_,_,_) ->
                                                                   case a of
                                                                     Nothing     ->
                                                                       Nothing
                                                                     Just finala ->
                                                                       Just $
                                                                         DSeq.fromList finala
                                                                 ) samwoalignment
                      , sam_v1_6_read_group                    = (\(_,b,_,_) ->
                                                                   case b of
                                                                     Nothing     ->
                                                                       Nothing
                                                                     Just finalb ->
                                                                       Just $
                                                                         DSeq.fromList finalb
                                                                 ) samwoalignment
                      , sam_v1_6_program                       = (\(_,_,c,_) -> c) samwoalignment
                      , sam_v1_6_one_line_comment              = (\(_,_,_,d) ->
                                                                   case d of
                                                                     Nothing     ->
                                                                       Nothing
                                                                     Just finald ->
                                                                       Just $
                                                                         DSeq.fromList finald
                                                                 ) samwoalignment
                      , sam_v1_6_alignment                     = DSeq.fromList alignment
                      }

-- | Run the @"SAM_V1_6"@ parser.
readSAM_V1_6_LBS :: DBL.ByteString
                 -> IO SAM_V1_6
readSAM_V1_6_LBS lbs =
  case (DABL.parseOnly parse_SAM_V1_6 lbs) of 
    Left  samparseerror ->
      error samparseerror
    Right sam           ->
      return sam

-- | Read a @"SAM_V1_6"@ from a file.
--
-- The file is checked for errors as it is parsed.
--
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
readSAM_V1_6 :: FilePath -- ^ Input path to SAM file.
             -> IO SAM_V1_6
readSAM_V1_6 fp = do
  let lazysamfile = S.unfold StreamlyInternalFile.chunkReader fp
  lazysamfilef    <-
    StreamlyLByteString.fromChunksIO lazysamfile
  readSAM_V1_6_LBS lazysamfilef
