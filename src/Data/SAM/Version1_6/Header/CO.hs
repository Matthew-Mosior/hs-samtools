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
-- Module      :  Data.SAM.Version1_6.Header.CO
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

module Data.SAM.Version1_6.Header.CO ( -- * SAM version 1.6 One-line text comment data type
                                       SAM_V1_6_One_Line_Comment(..)
                                     ) where

import Data.ByteString
import Data.Data
import Data.Sequence
import Data.Word
import Generics.Deriving.Base


-- | Custom SAM (version 1.6) @"SAM_V1_6_One_Line_Comment"@ data type.
-- See section 1.3 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_One_Line_Comment = SAM_V1_6_One_Line_Comment { sam_v1_6_one_line_comment_tag   :: Seq Word8
                                                           , sam_v1_6_one_line_comment_value :: ByteString
                                                           }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_One_Line_Comment where
  SAM_V1_6_One_Line_Comment sam_v1_6_one_line_comment_tag1 sam_v1_6_one_line_comment_value1 == SAM_V1_6_One_Line_Comment sam_v1_6_one_line_comment_tag2 sam_v1_6_one_line_comment_value2 = sam_v1_6_one_line_comment_tag1 == sam_v1_6_one_line_comment_tag2 && sam_v1_6_one_line_comment_value1 == sam_v1_6_one_line_comment_value2

instance Show SAM_V1_6_One_Line_Comment where
  show (SAM_V1_6_One_Line_Comment tag value) = "SAM_V1_6_One_Line_Comment { " ++
                                               "tag = "                                                            ++
                                               (show tag)                                                          ++
                                               " , value = "                                                       ++
                                               (show value)                                                        ++
                                               " }"
