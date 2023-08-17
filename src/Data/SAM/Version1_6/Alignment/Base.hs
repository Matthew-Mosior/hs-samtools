{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedLists             #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE PackageImports              #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# Language QuasiQuotes                 #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Module      :  Data.SAM.Version1_6.Alignment.Base
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

module Data.SAM.Version1_6.Alignment.Base ( -- * SAM version 1.6 alignment mandatory and optional data types
                                            SAM_V1_6_Alignment(..)
                                          ) where

import Data.SAM.Version1_6.Alignment.BOPT

import Data.Bit
import Data.ByteString
import Data.Data
import Data.Sequence
import Data.Word
import Generics.Deriving.Base


-- | Custom SAM (version 1.6) @"SAM_V1_6_Alignment"@ data type.
-- See section 1.4 and 1.5 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_Alignment = SAM_V1_6_Alignment { sam_v1_6_alignment_qname :: ByteString                    -- ^ Query template NAME.
                                                                                                         -- reads/segments having identical QNAME are regarded to come from
                                                                                                         -- the same template. A QNAME ‘*’ indicates the information
                                                                                                         -- is unavailable. In a SAM file, a read may
                                                                                                         -- occupy multiple alignment lines, when its alignment is chimeric
                                                                                                         -- or when multiple mappings are given.
                                             , sam_v1_6_alignment_flag  :: Vector Bit                    -- ^ Combination of bitwise FLAGs.
                                             , sam_v1_6_alignment_rname :: ByteString                    -- ^ Reference sequence NAME of the alignment.
                                                                                                         -- If @SQ header lines are present, RNAME (if not
                                                                                                         -- ‘*’) must be present in one of the SQ-SN tag.
                                                                                                         -- An unmapped segment without coordinate has a ‘*’ at
                                                                                                         -- this field. However, an unmapped segment may also have
                                                                                                         -- an ordinary coordinate such that it can be
                                                                                                         -- placed at a desired position after sorting.
                                                                                                         -- If RNAME is ‘*’, no assumptions can be made about POS
                                                                                                         -- and CIGAR.
                                             , sam_v1_6_alignment_pos   :: Integer                       -- ^ 1-based leftmost mapping POSition of the first CIGAR
                                                                                                         -- operation that “consumes” a reference
                                                                                                         -- base. The first base in a reference sequence has coordinate 1.
                                                                                                         -- POS is set as 0 for an unmapped read without coordinate.
                                                                                                         -- If POS is 0, no assumptions can be made about RNAME and CIGAR.
                                             , sam_v1_6_alignment_mapq  :: Int                           -- ^ MAPping Quality. It equals −10 log10 Pr{mapping position is wrong},
                                                                                                         -- rounded to the nearest integer. A value 255 indicates that the
                                                                                                         -- mapping quality is not available.
                                             , sam_v1_6_alignment_cigar :: ByteString                    -- ^ CIGAR string (set ‘*’ if unavailable).
                                             , sam_v1_6_alignment_rnext :: ByteString                    -- ^ Reference sequence name of the primary alignment of the
                                                                                                         -- NEXT read in the template. For the last read, the next read
                                                                                                         -- is the first read in the template. If @SQ header lines are present,
                                                                                                         -- RNEXT (if not ‘*’ or ‘=’) must be present in one of the SQ-SN tag.
                                                                                                         -- This field is set as ‘*’ when the information is unavailable,
                                                                                                         -- and set as ‘=’ if RNEXT is identical RNAME. If not ‘=’ and the next
                                                                                                         -- read in the template has one primary mapping (see also bit 0x100 in FLAG),
                                                                                                         -- this field is identical to RNAME at the primary line of the next read.
                                                                                                         -- If RNEXT is ‘*’, no assumptions can be made on PNEXT and bit 0x20.
                                             , sam_v1_6_alignment_pnext :: Integer                       -- ^ 1-based Position of the primary alignment of the NEXT read in
                                                                                                         -- the template. Set as 0 when the information is unavailable.
                                                                                                         -- This field equals POS at the primary line of the next read.
                                                                                                         -- If PNEXT is 0, no assumptions can be made on RNEXT and bit 0x20.
                                             , sam_v1_6_alignment_tlen  :: Integer                       -- ^ signed observed Template LENgth. For primary reads where the primary
                                                                                                         -- alignments of all reads in the template are mapped to the same reference
                                                                                                         -- sequence, the absolute value of TLEN equals the distance between the
                                                                                                         -- mapped end of the template and the mapped start of the template,
                                                                                                         -- inclusively (i.e., end − start + 1).
                                                                                                         -- Note that mapped base is defined to be one that aligns to the
                                                                                                         -- reference as described by CIGAR, hence excludes soft-clipped bases.
                                                                                                         -- The TLEN field is positive for the leftmost segment of the template,
                                                                                                         -- negative for the rightmost, and the sign for any middle segment is undefined.
                                                                                                         -- If segments cover the same coordinates then the choice of which is leftmost
                                                                                                         -- and rightmost is arbitrary, but the two ends must still have differing signs.
                                                                                                         -- It is set as 0 for a single-segment template or when the information
                                                                                                         -- is unavailable (e.g., when the first or last segment of a multi-segment
                                                                                                         -- template is unmapped or when the two are mapped to
                                                                                                         -- different reference sequences).
                                             , sam_v1_6_alignment_seq   :: ByteString                    -- ^ segment SEQuence. This field can be a ‘*’ when the sequence
                                                                                                         -- is not stored. If not a ‘*’, the length of the sequence must
                                                                                                         -- equal the sum of lengths of M/I/S/=/X operations in CIGAR.
                                                                                                         -- An ‘=’ denotes the base is identical to the reference base.
                                                                                                         -- No assumptions can be made on the letter cases.
                                             , sam_v1_6_alignment_qual  :: ByteString                    -- ^ ASCII of base QUALity plus 33 (same as the quality string
                                                                                                         -- in the Sanger FASTQ format). A base quality is the phred-scaled
                                                                                                         -- base error probability which equals −10 log10 Pr{base is wrong}.
                                                                                                         -- This field can be a ‘*’ when quality is not stored.
                                                                                                         -- If not a ‘*’, SEQ must not be a ‘*’ and the length of the quality
                                                                                                         -- string ought to equal the length of SEQ.
                                             , sam_v1_6_alignment_aopt  :: Maybe ByteString              -- ^ A - [!-~] - Printable characters.
                                             , sam_v1_6_alignment_iopt  :: Maybe Integer                 -- ^ i - [-+]?[0-9]+ - Signed integer.
                                             , sam_v1_6_alignment_fopt  :: Maybe Float                   -- ^ f - [-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)? - Single-precision floating number.
                                             , sam_v1_6_alignment_zopt  :: Maybe ByteString              -- ^ Z - [ !-~]* - Printable string, including space.
                                             , sam_v1_6_alignment_hopt  :: Maybe (Seq Word8)             -- ^ H - ([0-9A-F][0-9A-F])* - Byte array in the Hex format.
                                             , sam_v1_6_alignment_bopt  :: Maybe SAM_V1_6_Alignment_BOPT -- ^ B - [cCsSiIf]&#8203;(,[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)* - Integer or numeric array.
                                             }
  deriving (Generic,Typeable)

instance Show SAM_V1_6_Alignment where
  show (SAM_V1_6_Alignment qname flag rname pos mapq cigar rnext pnext tlen seq qual aopt iopt fopt zopt hopt bopt) =
    "SAM_V1_6_Alignment { " ++
    "qname = "              ++
    (show qname)            ++
    " , flag = "            ++
    (show flag)             ++
    " , rname = "           ++
    (show rname)            ++
    " , pos = "             ++
    (show pos)              ++
    " , mapq = "            ++
    (show mapq)             ++
    " , cigar = "           ++
    (show cigar)            ++
    " , rnext = "           ++
    (show rnext)            ++
    " , pnext = "           ++
    (show pnext)            ++
    " , tlen = "            ++
    (show tlen)             ++
    " , seq = "             ++
    (show seq)              ++
    " , qual = "            ++
    (show qual)             ++
    " , aopt = "            ++
    ( show aopt)            ++
    " , iopt = "            ++
    (show iopt)             ++
    " , fopt = "            ++
    (show fopt)             ++
    " , zopt = "            ++
    (show zopt)             ++
    " , hopt = "            ++
    (show hopt)             ++
    " , bopt = "            ++
    (show bopt)             ++
    " }"
