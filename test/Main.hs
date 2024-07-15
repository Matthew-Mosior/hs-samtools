module Main (main) where

import Data.BAM.Version1_6.Read.Base
import Data.BAM.Version1_6.Write.Base
import Data.SAM.Version1_6.Read.Base
import Data.SAM.Version1_6.Write.Base

import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Data.SAM.Version1_6.Write.Base" $ do
      describe "writeSAM_V1_6" $ do
        describe "SAM/toy5.sam" $ do
          it "Ensures writeSAM_V1_6 produces the a sam file equivalent to what was parsed via readSAM_V1_6." $ do
            toy5 <- readSAM_V1_6 "test/examples/SAM/toy5.sam"
            writeSAM_V1_6 "test/examples/SAM/toy5f.sam" toy5
            toy5f <- readSAM_V1_6 "test/examples/SAM/toy5f.sam"
            toy5 `shouldBe` toy5f
        describe "SAM/toy4.sam" $ do
          it "Ensures writeSAM_V1_6 produces the a sam file equivalent to what was parsed via readSAM_V1_6." $ do
            toy4 <- readSAM_V1_6 "test/examples/SAM/toy4.sam"
            writeSAM_V1_6 "test/examples/SAM/toy4f.sam" toy4
            toy4f <- readSAM_V1_6 "test/examples/SAM/toy4f.sam"
            toy4 `shouldBe` toy4f
        describe "SAM/toy2.sam" $ do
          it "Ensures writeSAM_V1_6 produces the a sam file equivalent to what was parsed via readSAM_V1_6." $ do
            toy2 <- readSAM_V1_6 "test/examples/SAM/toy2.sam"
            writeSAM_V1_6 "test/examples/SAM/toy2f.sam" toy2
            toy2f <- readSAM_V1_6 "test/examples/SAM/toy2f.sam"
            toy2 `shouldBe` toy2f
        describe "SAM/toy1.sam" $ do
          it "Ensures writeSAM_V1_6 produces the a sam file equivalent to what was parsed via readSAM_V1_6." $ do
            toy1 <- readSAM_V1_6 "test/examples/SAM/toy1.sam"
            writeSAM_V1_6 "test/examples/SAM/toy1f.sam" toy1
            toy1f <- readSAM_V1_6 "test/examples/SAM/toy1f.sam"
            toy1 `shouldBe` toy1f
    describe "Data.BAM.Version1_6.Write.Base" $ do
      describe "WriteBAM_V1_6" $ do
        describe "BAM/ex1ff_sorted.bam" $ do
          it "Ensures writeBAM_V1_6 produces a bam file equivalent to what was parsed via readBAM_V1_6." $ do
            _        <- readBAM_V1_6 "test/examples/BAM/ex1ff_sorted.bam" >>= writeBAM_V1_6 "test/examples/BAM/ex1ff_sortedf.bam"
            bamv16f  <- readBAM_V1_6 "test/examples/BAM/ex1ff_sortedf.bam"
            _        <- writeBAM_V1_6 "test/examples/BAM/ex1ff_sortedff.bam" bamv16f
            bamv16ff <- readBAM_V1_6 "test/examples/BAM/ex1ff_sortedff.bam"
            bamv16f `shouldBe` bamv16ff
