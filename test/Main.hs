module Main (main) where

import Data.SAM.Version1_6.Read.Base

import Test.Hspec

main :: IO ()
main = do --hspec $ do
 -- describe "Data.SAM.Version1_6.Read.Base" $ do
 --   describe "readSAM_V1_6" $ do
 --     describe "toy1.sam" $ do
 --       it "Ensures that readSAM_V1_6 can read and parse: toy1.sam" $ do
 toy1sam <- readSAM_V1_6 "test/examples/toy4.sam"
 print toy1sam 
