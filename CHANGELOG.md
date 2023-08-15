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
