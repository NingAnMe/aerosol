C     List of input files for aerosol direct broadcast processing.

C *** INPUT DATA FILES ***

c MOD02 250M 
c --- Fortran OPEN 
      integer		L1B_250M_UNIT
      parameter		(L1B_250M_UNIT = 30) 
      character*11	L1B_250M_FORM
      parameter		(L1B_250M_FORM = 'UNFORMATTED')
      character*6	L1B_250M_ACCESS
      parameter		(L1B_250M_ACCESS = 'DIRECT')
      character*3	L1B_250M_STATUS
      parameter		(L1B_250M_STATUS = 'OLD')

c MOD02 500M
c --- Fortran OPEN 
      integer		L1B_500M_UNIT
      parameter		(L1B_500M_UNIT = 31) 
      character*11	L1B_500M_FORM
      parameter		(L1B_500M_FORM = 'UNFORMATTED')
      character*6	L1B_500M_ACCESS
      parameter		(L1B_500M_ACCESS = 'DIRECT')
      character*3	L1B_500M_STATUS
      parameter		(L1B_500M_STATUS = 'OLD')

c MOD02 1KM 
c --- Fortran OPEN 
      integer		L1B_1KM_UNIT
      parameter		(L1B_1KM_UNIT = 32) 
      character*11	L1B_1KM_FORM
      parameter		(L1B_1KM_FORM = 'UNFORMATTED')
      character*6	L1B_1KM_ACCESS
      parameter		(L1B_1KM_ACCESS = 'DIRECT')
      character*3	L1B_1KM_STATUS
      parameter		(L1B_1KM_STATUS = 'OLD')


c MOD03 GE0
c --- Fortran OPEN 
      integer		GEO_1KM_UNIT
      parameter		(GEO_1KM_UNIT = 33) 
      character*11	GEO_1KM_FORM
      parameter		(GEO_1KM_FORM = 'UNFORMATTED')
      character*6	GEO_1KM_ACCESS
      parameter		(GEO_1KM_ACCESS = 'DIRECT')
      character*3	GEO_1KM_STATUS
      parameter		(GEO_1KM_STATUS = 'OLD')

c MOD02 Scan Metadata 
c --- Fortran OPEN 
      integer		SCAN_1KM_UNIT
      parameter		(SCAN_1KM_UNIT = 34) 
      character*3	SCAN_1KM_STATUS
      parameter		(SCAN_1KM_STATUS = 'OLD')
      character*11	SCAN_1KM_FORM
      parameter		(SCAN_1KM_FORM = 'UNFORMATTED')
      character*6	SCAN_1KM_ACCESS
      parameter		(SCAN_1KM_ACCESS = 'DIRECT')
      integer		SCAN_1KM_RECL
      parameter		(SCAN_1KM_RECL = 2)

c ... ANCILLARY DATA SETS ...

c OZONE FILE
c --- Fortran OPEN 
      integer		ANC_OZONE_UNIT
      parameter		(ANC_OZONE_UNIT = 35) 
      character*3	ANC_OZONE_STATUS
      parameter		(ANC_OZONE_STATUS = 'OLD')
      character*11	ANC_OZONE_FORM
      parameter		(ANC_OZONE_FORM = 'UNFORMATTED')
      character*6	ANC_OZONE_ACCESS
      parameter		(ANC_OZONE_ACCESS = 'DIRECT')
c --- NOTE: record length is the entire array
c     (360 lines) X (181 elements/line) X (4 bytes/parm)
      integer           ANC_OZONE_RECL
      parameter         (ANC_OZONE_RECL = 360 * 181 * 4)

c ... GDAS MODEL FILE
c --- Fortran OPEN
      integer           ANC_MET_UNIT
      parameter         (ANC_MET_UNIT = 36)
      character*3       ANC_MET_STATUS
      parameter         (ANC_MET_STATUS = 'OLD')
      character*11      ANC_MET_FORM
      parameter         (ANC_MET_FORM = 'UNFORMATTED')
      character*6       ANC_MET_ACCESS
      parameter         (ANC_MET_ACCESS = 'DIRECT')
c --- NOTE: record length is the entire array
c     (360 lines) X (181 elements/line) X (54 params/element) X (4 bytes/parm)
      integer           ANC_MET_RECL
      parameter         (ANC_MET_RECL = 360 * 181 * 54 * 4)


C ... OTHER MODIS PRODUCT INPUTS (Binary and Header files) ...

C ... STATIC COEFFICIENT FILES 

C ... LAND FILES
c --- Fortran OPEN
      integer           LUT466_UNIT
      parameter         (LUT466_UNIT = 50)
      character*3       LUT466_STATUS
      parameter         (LUT466_STATUS = 'OLD')
      character*9       LUT466_FORM
      parameter         (LUT466_FORM = 'FORMATTED')
      character*10      LUT466_ACCESS
      parameter         (LUT466_ACCESS = 'SEQUENTIAL')

c --- Fortran OPEN
      integer           LUT553_UNIT
      parameter         (LUT553_UNIT = 51)
      character*3       LUT553_STATUS
      parameter         (LUT553_STATUS = 'OLD')
      character*9       LUT553_FORM
      parameter         (LUT553_FORM = 'FORMATTED')
      character*10      LUT553_ACCESS
      parameter         (LUT553_ACCESS = 'SEQUENTIAL')

c --- Fortran OPEN
      integer           LUT644_UNIT
      parameter         (LUT644_UNIT = 52)
      character*3       LUT644_STATUS
      parameter         (LUT644_STATUS = 'OLD')
      character*9       LUT644_FORM
      parameter         (LUT644_FORM = 'FORMATTED')
      character*10      LUT644_ACCESS
      parameter         (LUT644_ACCESS = 'SEQUENTIAL')

c --- Fortran OPEN
      integer           LUT213_UNIT
      parameter         (LUT213_UNIT = 53)
      character*3       LUT213_STATUS
      parameter         (LUT213_STATUS = 'OLD')
      character*9       LUT213_FORM
      parameter         (LUT213_FORM = 'FORMATTED')
      character*10      LUT213_ACCESS
      parameter         (LUT213_ACCESS = 'SEQUENTIAL')

c --- Fortran OPEN
      integer           LUTMAP_UNIT
      parameter         (LUTMAP_UNIT = 54)
      character*3       LUTMAP_STATUS
      parameter         (LUTMAP_STATUS = 'OLD')
      character*9       LUTMAP_FORM
      parameter         (LUTMAP_FORM = 'FORMATTED')
      character*10      LUTMAP_ACCESS
      parameter         (LUTMAP_ACCESS = 'SEQUENTIAL')

c --- Fortran OPEN
      integer           INSCI_UNIT
      parameter         (INSCI_UNIT = 55)
      character*3       INSCI_STATUS
      parameter         (INSCI_STATUS = 'OLD')
      character*9       INSCI_FORM
      parameter         (INSCI_FORM = 'FORMATTED')
      character*10      INSCI_ACCESS
      parameter         (INSCI_ACCESS = 'SEQUENTIAL')

c     Small mode aerosol tables for ocean algorithm
c --- Fortran OPEN
      integer           SMALL1_UNIT
      parameter         (SMALL1_UNIT = 56)
      character*3       SMALL1_STATUS
      parameter         (SMALL1_STATUS = 'OLD')
      character*9       SMALL1_FORM
      parameter         (SMALL1_FORM = 'FORMATTED')
      character*10      SMALL1_ACCESS
      parameter         (SMALL1_ACCESS = 'SEQUENTIAL')

c --- Fortran OPEN
      integer           SMALL2_UNIT
      parameter         (SMALL2_UNIT = 57)
      character*3       SMALL2_STATUS
      parameter         (SMALL2_STATUS = 'OLD')
      character*9       SMALL2_FORM
      parameter         (SMALL2_FORM = 'FORMATTED')
      character*10      SMALL2_ACCESS
      parameter         (SMALL2_ACCESS = 'SEQUENTIAL')

c --- Fortran OPEN
      integer           SMALL3_UNIT
      parameter         (SMALL3_UNIT = 58)
      character*3       SMALL3_STATUS
      parameter         (SMALL3_STATUS = 'OLD')
      character*9       SMALL3_FORM
      parameter         (SMALL3_FORM = 'FORMATTED')
      character*10      SMALL3_ACCESS
      parameter         (SMALL3_ACCESS = 'SEQUENTIAL')

c --- Large mode aerosol tables for ocean algorithm
c --- Fortran OPEN
      integer           BIG1_UNIT
      parameter         (BIG1_UNIT = 59)
      character*3       BIG1_STATUS
      parameter         (BIG1_STATUS = 'OLD')
      character*9       BIG1_FORM
      parameter         (BIG1_FORM = 'FORMATTED')
      character*10      BIG1_ACCESS
      parameter         (BIG1_ACCESS = 'SEQUENTIAL')

c --- Fortran OPEN
      integer           BIG2_UNIT
      parameter         (BIG2_UNIT = 60)
      character*3       BIG2_STATUS
      parameter         (BIG2_STATUS = 'OLD')
      character*9       BIG2_FORM
      parameter         (BIG2_FORM = 'FORMATTED')
      character*10      BIG2_ACCESS
      parameter         (BIG2_ACCESS = 'SEQUENTIAL')

c --- Fortran OPEN
      integer           BIG3_UNIT
      parameter         (BIG3_UNIT = 61)
      character*3       BIG3_STATUS
      parameter         (BIG3_STATUS = 'OLD')
      character*9       BIG3_FORM
      parameter         (BIG3_FORM = 'FORMATTED')
      character*10      BIG3_ACCESS
      parameter         (BIG3_ACCESS = 'SEQUENTIAL')


c MOD35 CLOUDMASK  
c --- Fortran OPEN 
      integer		MOD35_MASK_UNIT
      parameter		(MOD35_MASK_UNIT = 40) 
      character*7	MOD35_MASK_STATUS
      parameter		(MOD35_MASK_STATUS = 'UNKNOWN')
      character*11	MOD35_MASK_FORM
      parameter		(MOD35_MASK_FORM = 'UNFORMATTED')
      character*6	MOD35_MASK_ACCESS
      parameter		(MOD35_MASK_ACCESS = 'DIRECT')

c --- Fortran OPEN 
      integer		MOD35_HDR_UNIT
      parameter		(MOD35_HDR_UNIT = 41) 
      character*7	MOD35_HDR_STATUS
      parameter		(MOD35_HDR_STATUS = 'UNKNOWN')
      character*9	MOD35_HDR_FORM
      parameter		(MOD35_HDR_FORM = 'FORMATTED')
      character*10	MOD35_HDR_ACCESS
      parameter		(MOD35_HDR_ACCESS = 'SEQUENTIAL')

c MOD35 QUALITY CONTROL  
c --- Fortran OPEN 
      integer		MOD35_QA_UNIT
      parameter		(MOD35_QA_UNIT = 42) 
      character*7	MOD35_QA_STATUS
      parameter		(MOD35_QA_STATUS = 'UNKNOWN')
      character*11	MOD35_QA_FORM
      parameter		(MOD35_QA_FORM = 'UNFORMATTED')
      character*6	MOD35_QA_ACCESS
      parameter		(MOD35_QA_ACCESS = 'DIRECT')

c --- Fortran OPEN 
      integer		MOD35_QAHDR_UNIT
      parameter		(MOD35_QAHDR_UNIT = 43) 
      character*7	MOD35_QAHDR_STATUS
      parameter		(MOD35_QAHDR_STATUS = 'UNKNOWN')
      character*9	MOD35_QAHDR_FORM
      parameter		(MOD35_QAHDR_FORM = 'FORMATTED')
      character*10	MOD35_QAHDR_ACCESS
      parameter		(MOD35_QAHDR_ACCESS = 'SEQUENTIAL')

C *** OUTPUT FILES ***

c OUTPUT AEROSOL MOD04 PRODUCT
c --- Fortran OPEN
      integer           MOD04_UNIT
      parameter         (MOD04_UNIT = 90)
      character*7       MOD04_STATUS
      parameter         (MOD04_STATUS = 'UNKNOWN')
      character*11      MOD04_FORM
      parameter         (MOD04_FORM = 'UNFORMATTED')
      character*6       MOD04_ACCESS
      parameter         (MOD04_ACCESS = 'DIRECT')

c --- Fortran OPEN 
      integer		MOD04_HDR_UNIT
      parameter		(MOD04_HDR_UNIT = 92) 
      character*7	MOD04_HDR_STATUS
      parameter		(MOD04_HDR_STATUS = 'UNKNOWN')
      character*9	MOD04_HDR_FORM
      parameter		(MOD04_HDR_FORM = 'FORMATTED')
      character*10	MOD04_HDR_ACCESS
      parameter		(MOD04_HDR_ACCESS = 'SEQUENTIAL')

c RUNTIME DEBUG 
c --- Fortran OPEN 
      integer		DEBUG_UNIT
      parameter		(DEBUG_UNIT = 93) 
      character*7	DEBUG_STATUS
      parameter		(DEBUG_STATUS = 'UNKNOWN')
      character*9	DEBUG_FORM
      parameter		(DEBUG_FORM = 'FORMATTED')
      character*10	DEBUG_ACCESS
      parameter		(DEBUG_ACCESS = 'SEQUENTIAL')
