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
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.SAM.Version1_6.Header.CO ( -- * SAM version 1.6 One-line text comment data type
                                       SAM_V1_6_One_Line_Comment(..)
                                     ) where

import Data.ByteString
import Data.Data
import Generics.Deriving.Base

-- | Custom SAM (version 1.6) @"SAM_V1_6_One_Line_Comment"@ data type.
--
-- See section 1.3 of the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data SAM_V1_6_One_Line_Comment = SAM_V1_6_One_Line_Comment { sam_v1_6_one_line_comment_value :: ByteString
                                                           }
  deriving (Generic,Typeable)

instance Eq SAM_V1_6_One_Line_Comment where
  SAM_V1_6_One_Line_Comment sam_v1_6_one_line_comment_value1 == SAM_V1_6_One_Line_Comment sam_v1_6_one_line_comment_value2 = sam_v1_6_one_line_comment_value1 == sam_v1_6_one_line_comment_value2

instance Show SAM_V1_6_One_Line_Comment where
  show (SAM_V1_6_One_Line_Comment value) = "SAM_V1_6_One_Line_Comment { " ++
                                               "value = "                     ++
                                               (show value)                   ++
                                               " }"
