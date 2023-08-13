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
-- Module      :  Data.SAM.Version1_6.Internal
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

module Data.SAM.Version1_6.Internal ( -- * SAM version 1.6 data type
                                      SAM_V1_6(..)
                                    ) where

import Data.SAM.Version1_6.Header.CO
import Data.SAM.Version1_6.Header.HD
import Data.SAM.Version1_6.Header.PG
import Data.SAM.Version1_6.Header.RG
import Data.SAM.Version1_6.Header.SQ

import Data.Data
import Data.Sequence
import Generics.Deriving.Base

-- | Custom @"SAM_V1_6"@ (SAM version 1.6) data type.
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6 = SAM_V1_6 { sam_v1_6_file_level_metadata           :: Maybe SAM_V1_6_File_Level_Metadata           -- ^ File-level metadata.
                                                                                                                  -- Optional. If present,
                                                                                                                  -- there must be only one
                                                                                                                  -- @HD line and it must be
                                                                                                                  -- the first line of the file. 
                         , sam_v1_6_reference_sequence_dictionary :: Maybe SAM_V1_6_Reference_Sequence_Dictionary -- ^ Reference sequence dictionary.
                                                                                                                  -- The order of @SQ lines defines the
                                                                                                                  -- alignment sorting order.
                         , sam_v1_6_read_group                    :: Maybe (Seq SAM_V1_6_Read_Group)              -- ^ Read group.
                                                                                                                  -- Unordered multiple @RG
                                                                                                                  -- lines are allowed.
                         , sam_v1_6_program                       :: Maybe SAM_V1_6_Program                       -- ^ Program.
                         , sam_v1_6_one_line_comment              :: Maybe (Seq SAM_V1_6_One_Line_Comment)        -- ^ One-line text comment.
                                                                                                                  -- Unordered multiple @CO lines
                                                                                                                  -- are allowed. UTF-8 encoding
                                                                                                                  -- may be used.
                         }
  deriving (Generic,Typeable)
