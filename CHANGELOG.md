# Revision history for hs-samtools

## 0.1.0.0 -- 2023-08-12

* First version. Released on an unsuspecting world.

## 0.1.0.1 -- 2023-08-12

* Added package bounds for ascii, bytestring, containers, crypton, and flatparse.

## 0.2.0.0 -- 2023-08-13

* Added Eq, Generic, Show and Typeable classes/instances to Data.SAM.Version1_6.Header.CO, Data.SAM.Version1_6.Header.HD, Data.SAM.Version1_6.Header.PG, Data.SAM.Version1_6.Header.RG, Data.SAM.Version1_6.Header.SQ data types.

## 0.2.0.1 -- 2023-08-13

* Removed unnecessary Eq and Show instance comments.

## 0.3.0.0 -- 2023-08-14

* Created Data.SAM.Version1_6.Alignment and Data.SAM.Version1_6.Header (re-exports).
* Renamed Data.SAM.Version1_6.Internal to Data.SAM.Version1_6.Base.
* Added Data.SAM.Version1_6.Alignment.Base and Data.SAM.Version1_6.Alignment.BOPT.

## 0.3.1.0 -- 2023-08-14

* Added SAM_V1_6_Alignment_BOPT_Float(..) to Data.SAM.Version1_6.Alignment.BOPT.

## 0.4.0.0 -- 2023-08-16

* Added comments to SAM_V1_6_Alignment(..) in Data.SAM.Version1_6.Alignment.Base.
* Changed sam_v1_6_alignment_flag (SAM_V1_6_Alignment(..)) from Int to Vector Bit in Data.SAM.Version1_6.Alignment.Base.

## 0.4.0.1 -- 2023-08-16

* Fixed comments to SAM_V1_6_Alignment(..) in Data.SAM.Version1_6.Alignment.Base.

## 0.4.0.2 -- 2023-08-16

* Fixed comments to SAM_V1_6_Alignment(..) in Data.SAM.Version1_6.Alignment.Base.

## 0.4.0.3 -- 2023-08-16

* Fixed comments to SAM_V1_6_Alignment(..) in Data.SAM.Version1_6.Alignment.Base.

## 0.4.0.4 -- 2023-08-17

* Added comment to sam_v1_6_alignment (SAM_V1_6(..)) in Data.SAM.Version1_6.Base.

## 0.5.0.0 -- 2023-09-01

* Added attoparsec parsers for alignment and header section of SAM_V1_6(..).
* Added attoparsec parser for SAM_V1_6(..).
* Added SAM_V1_6(..) reading functionality.

## 0.5.0.1 -- 2023-09-01

* Fixed documentation for readSAM_V1_6.

## 0.5.0.2 -- 2023-09-01

* Fixed/updated lots of documentation across entire codebase.

## 0.5.0.3 -- 2023-09-01

* Added homepage and bug-reports fields to cabal file.

## 0.5.0.4 -- 2023-09-01

* Added small initial test suite.

## 0.6.0.0 -- 2023-09-04

* Removed tag section of headers and changed those affected from data to newtypes.
