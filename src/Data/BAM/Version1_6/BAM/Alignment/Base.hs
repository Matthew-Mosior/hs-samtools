{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StrictData                  #-}
{-# LANGUAGE TypeFamilies                #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Module      :  Data.BAM.Version1_6.BAM.Alignment.Base
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.BAM.Alignment.Base ( -- * BAM version 1.6 alignment mandatory and optional data types
                                                BAM_V1_6_BAM_Alignment(..)
                                              ) where

import Data.BAM.Version1_6.BAM.Alignment.OptionalFields.Base

import Data.ByteString
import Data.Int
import Data.Sequence
import Data.Word
import Data.Data
import Generics.Deriving.Base

-- | Custom BAM (version 1.6) @"BAM_V1_6_BAM_Alignment"@ data type.
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_BAM_Alignment = BAM_V1_6_BAM_Alignment
  { bam_v1_6_bam_alignment_block_size     :: Word32                                    -- ^ Total length of the alignment
                                                                                       -- record, excluding this field.
  , bam_v1_6_bam_alignment_refID          :: Int32                                     -- ^ Reference sequence ID, -1 <= refID <= n_ref;
                                                                                       -- -1 for a read without a mapping position.
  , bam_v1_6_bam_alignment_pos            :: Int32                                     -- ^ 0-based leftmost coordinate (= POS - 1).
  , bam_v1_6_bam_alignment_l_read_name    :: Word8                                     -- ^ Length of read_name below (= length(QNAME) + 1).
  , bam_v1_6_bam_alignment_mapq           :: Word8                                     -- ^ Mapping quality (= MAPQ).
  , bam_v1_6_bam_alignment_bin            :: Word16                                    -- ^ BAI index bin, see Section 4.2.1.
  , bam_v1_6_bam_alignment_n_cigar_op     :: Word16                                    -- ^ Number of operations in CIGAR,
                                                                                       -- see section 4.2.2.
  , bam_v1_6_bam_alignment_flag           :: Word16                                    -- ^ Bitwise flags (= FLAG).
  , bam_v1_6_bam_alignment_l_seq          :: Word32                                    -- ^ Length of SEQ.
  , bam_v1_6_bam_alignment_next_refID     :: Int32                                     -- ^ Ref-ID of the next seqment
                                                                                       -- (-1 <= next_refID <= n_ref).
  , bam_v1_6_bam_alignment_next_pos       :: Int32                                     -- ^ 0-based leftmost pos of the
                                                                                       -- next segment (= PNEXT - 1).
  , bam_v1_6_bam_alignment_tlen           :: Int32                                     -- ^ Template length (= TLEN).
  , bam_v1_6_bam_alignment_read_name      :: ByteString                                -- ^ Read name; NUL-terminated
                                                                                       -- (QNAME with trailing 0x00/'\0').
  , bam_v1_6_bam_alignment_cigar          :: Seq Word32                                -- ^ CIGAR; op_len<<4<<|op.
                                                                                       -- 'MIDNSHP=X' -> '012345678'.
  , bam_v1_6_bam_alignment_seq            :: Seq Word8                                 -- ^ 4-bit encoded read:
                                                                                       -- '=ACMGRSVTWYHKDBN' -> [0,15].
                                                                                       -- See section 4.2.3.
  , bam_v1_6_bam_alignment_qual           :: ByteString                                -- ^ Phred-scaled base qualities.
                                                                                       -- See section 4.2.3.
  , bam_v1_6_bam_alignment_optionalfields :: Seq BAM_V1_6_BAM_Alignment_OptionalFields -- ^ List of auxiliary data. 
                                                                                       -- See section 4.2.4
  } deriving (Generic,Typeable)

instance Eq BAM_V1_6_BAM_Alignment where
  BAM_V1_6_BAM_Alignment bam_v1_6_bam_alignment_block_size1
                         bam_v1_6_bam_alignment_refID1
                         bam_v1_6_bam_alignment_pos1
                         bam_v1_6_bam_alignment_l_read_name1
                         bam_v1_6_bam_alignment_mapq1
                         bam_v1_6_bam_alignment_bin1
                         bam_v1_6_bam_alignment_n_cigar_op1
                         bam_v1_6_bam_alignment_flag1
                         bam_v1_6_bam_alignment_l_seq1
                         bam_v1_6_bam_alignment_next_refID1
                         bam_v1_6_bam_alignment_next_pos1
                         bam_v1_6_bam_alignment_tlen1
                         bam_v1_6_bam_alignment_read_name1
                         bam_v1_6_bam_alignment_cigar1
                         bam_v1_6_bam_alignment_seq1
                         bam_v1_6_bam_alignment_qual1
                         bam_v1_6_bam_alignment_optional_fields1 ==
    BAM_V1_6_BAM_Alignment bam_v1_6_bam_alignment_block_size2
                           bam_v1_6_bam_alignment_refID2
                           bam_v1_6_bam_alignment_pos2
                           bam_v1_6_bam_alignment_l_read_name2
                           bam_v1_6_bam_alignment_mapq2
                           bam_v1_6_bam_alignment_bin2
                           bam_v1_6_bam_alignment_n_cigar_op2
                           bam_v1_6_bam_alignment_flag2
                           bam_v1_6_bam_alignment_l_seq2
                           bam_v1_6_bam_alignment_next_refID2
                           bam_v1_6_bam_alignment_next_pos2
                           bam_v1_6_bam_alignment_tlen2
                           bam_v1_6_bam_alignment_read_name2
                           bam_v1_6_bam_alignment_cigar2
                           bam_v1_6_bam_alignment_seq2
                           bam_v1_6_bam_alignment_qual2
                           bam_v1_6_bam_alignment_optional_fields2 =
      bam_v1_6_bam_alignment_block_size1      ==  bam_v1_6_bam_alignment_block_size2  &&
      bam_v1_6_bam_alignment_refID1           ==  bam_v1_6_bam_alignment_refID2       &&
      bam_v1_6_bam_alignment_pos1             ==  bam_v1_6_bam_alignment_pos2         &&
      bam_v1_6_bam_alignment_l_read_name1     ==  bam_v1_6_bam_alignment_l_read_name2 &&
      bam_v1_6_bam_alignment_mapq1            ==  bam_v1_6_bam_alignment_mapq2        &&
      bam_v1_6_bam_alignment_bin1             ==  bam_v1_6_bam_alignment_bin2         &&
      bam_v1_6_bam_alignment_n_cigar_op1      ==  bam_v1_6_bam_alignment_n_cigar_op2  &&
      bam_v1_6_bam_alignment_flag1            ==  bam_v1_6_bam_alignment_flag2        &&
      bam_v1_6_bam_alignment_l_seq1           ==  bam_v1_6_bam_alignment_l_seq2       &&
      bam_v1_6_bam_alignment_next_refID1      ==  bam_v1_6_bam_alignment_next_refID2  &&
      bam_v1_6_bam_alignment_next_pos1        ==  bam_v1_6_bam_alignment_next_pos2    &&
      bam_v1_6_bam_alignment_tlen1            ==  bam_v1_6_bam_alignment_tlen2        &&
      bam_v1_6_bam_alignment_read_name1       ==  bam_v1_6_bam_alignment_read_name2   &&
      bam_v1_6_bam_alignment_cigar1           ==  bam_v1_6_bam_alignment_cigar2       &&
      bam_v1_6_bam_alignment_seq1             ==  bam_v1_6_bam_alignment_seq2         &&
      bam_v1_6_bam_alignment_qual1            ==  bam_v1_6_bam_alignment_qual2        &&
      bam_v1_6_bam_alignment_optional_fields1 ==  bam_v1_6_bam_alignment_optional_fields2

instance Show BAM_V1_6_BAM_Alignment where
  show ( BAM_V1_6_BAM_Alignment block_size
                                refID
                                pos
                                l_read_name
                                mapq
                                bin
                                n_cigar_op
                                flag
                                l_seq
                                next_refID
                                next_pos
                                tlen
                                read_name
                                cigar
                                seq
                                qual
                                optionalfields
       ) =
    "BAM_V1_6_BAM_Alignment { "                   ++
    "bam_v1_6_bam_alignment_block_size = "        ++
    (show block_size)                             ++
    " , bam_v1_6_bam_alignment_refID = "          ++
    (show refID)                                  ++
    " , bam_v1_6_bam_alignment_pos = "            ++
    (show pos)                                    ++
    " , bam_v1_6_bam_alignment_l_read_name = "    ++
    (show l_read_name)                            ++
    " , bam_v1_6_bam_alignment_mapq = "           ++
    (show mapq)                                   ++
    " , bam_v1_6_bam_alignment_bin = "            ++
    (show bin)                                    ++
    " , bam_v1_6_bam_alignment_n_cigar_op = "     ++
    (show n_cigar_op)                             ++
    " , bam_v1_6_bam_alignment_flag = "           ++
    (show flag)                                   ++
    " , bam_v1_6_bam_alignment_l_seq = "          ++
    (show l_seq)                                  ++
    " , bam_v1_6_bam_alignment_next_refID = "     ++
    (show next_refID)                             ++
    " , bam_v1_6_bam_alignment_next_pos = "       ++
    (show next_pos)                               ++
    " , bam_v1_6_bam_alignment_tlen = "           ++
    (show tlen)                                   ++
    " , bam_v1_6_bam_alignment_read_name = "      ++
    (show read_name)                              ++
    " , bam_v1_6_bam_alignment_cigar = "          ++
    (show cigar)                                  ++
    " , bam_v1_6_bam_alignment_seq = "            ++
    (show seq)                                    ++
    " , bam_v1_6_bam_alignment_qual = "           ++
    (show qual)                                   ++
    " , bam_v1_6_bam_alignment_optionalfields = " ++
    (show optionalfields)                         ++
    " }"
