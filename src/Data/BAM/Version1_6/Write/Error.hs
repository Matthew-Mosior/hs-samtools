{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.BAM.Version1_6.Write.Error
-- Copyright   :  (c) Matthew Mosior 2024
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Description
--
-- This library enables the decoding/encoding of SAM, BAM and CRAM file formats.

module Data.BAM.Version1_6.Write.Error ( -- * BAM version 1.6 data type
                                         BAM_V1_6_Write_Error(..)
                                       ) where

import Data.Data
import Generics.Deriving.Base

-- | Custom @"BAM_V1_6"@ (BAM version 1.6) error data type.
-- See the [SAM v1.6](http://samtools.github.io/hts-specs/SAMv1.pdf) specification documentation.
data BAM_V1_6_Write_Error = -- | No data to write to file.
                            BAM_V1_6_Write_Error_No_Data
                          | -- | HOPT alignment optional field is not hex-formatted byte array.
                            BAM_V1_6_Write_Error_HOPT_Incorrect_Format
  deriving (Eq,Generic,Show,Typeable)
