{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      :  Data.SAM.Version1_6.Header
-- Copyright   :  (c) Matthew Mosior 2024
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

module Data.SAM.Version1_6.Header ( -- * SAM version 1.6 header section data type (RE-EXPORT)
                                    module Data.SAM.Version1_6.Header.CO,
                                    module Data.SAM.Version1_6.Header.HD,
                                    module Data.SAM.Version1_6.Header.PG,
                                    module Data.SAM.Version1_6.Header.RG,
                                    module Data.SAM.Version1_6.Header.SQ
                                  ) where

-- | Re-exports.
import Data.SAM.Version1_6.Header.CO
import Data.SAM.Version1_6.Header.HD
import Data.SAM.Version1_6.Header.PG
import Data.SAM.Version1_6.Header.RG
import Data.SAM.Version1_6.Header.SQ
