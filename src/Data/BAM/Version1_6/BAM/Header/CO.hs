{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.BAM.Header.CO
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.BAM.Header.CO ( -- * BAM version 1.6 One-line text comment data type
                                           BAM_V1_6_One_Line_Comment(..)
                                         ) where

import Data.ByteString
import Data.Data
import Generics.Deriving.Base

-- | Custom BAM (version 1.6) @"SAM_V1_6_One_Line_Comment"@ data type.
--
-- See section 4.2 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
newtype BAM_V1_6_One_Line_Comment = BAM_V1_6_One_Line_Comment { bam_v1_6_one_line_comment_value :: ByteString
                                                              }
  deriving (Generic,Typeable)

instance Eq BAM_V1_6_One_Line_Comment where
  BAM_V1_6_One_Line_Comment bam_v1_6_one_line_comment_value1 ==
    BAM_V1_6_One_Line_Comment bam_v1_6_one_line_comment_value2 =
      bam_v1_6_one_line_comment_value1 == bam_v1_6_one_line_comment_value2

instance Show BAM_V1_6_One_Line_Comment where
  show (BAM_V1_6_One_Line_Comment value) = "BAM_V1_6_One_Line_Comment { "       ++
                                           "bam_v1_6_one_line_comment_value = " ++
                                           (show value)                         ++
                                           " }"
