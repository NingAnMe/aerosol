!----------------------------------------------------------------------------
!               mapi.inc utilities header file
!                        PROTOTYPE 
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
! !F90-INC
!
! !Description: All M-API Functions available in the M-API software library are
!               declared to have INTEGER return values.
!
!               Data type constants are provided to label the data type of data 
!               contained in a data object.  They are acceptable inputs for the 
!               data_type parameter of createMODISarray and createMODIStable. 
!               They may be used as inputs to the MODISsizeof routine, which
!               returns the number of bytes of memory required to store the 
!               data type.
!
! !Revision History:
!  Revision 2.1 1998/06/02  jmurthy 
!     converted f77 to f90 format
!  Id: mapi.inc,v 2.0 1997/07/29 12:54:40 fshaw Exp fshaw $
!  Log: mapi.inc,v $
!  Revision 2.0  1997/07/29  12:54:40  fshaw
! vers 2.2 copied from ftp site
!
!# Revision 1.5  1997/03/05  16:40:21  qhuang
!# Added a 'D' in parameter QAPERCENTINTERPOLATEDATA.
!#
!# Revision 1.4  1996/06/05  19:47:23  qhuang
!# *** empty log message ***
!#
!
!               WRITTEN BY:
!
!               Vicky Lin                06/08/95
!               Research and Data systems Corporation
!               SAIC/GSC MODIS Science Data Support Office
!               7501 Forbes Blvd
!               Seabrood, MD 20706
!
!               vlin@modis-xl.gsfc.nasa.gov
!
!               Mitchell Weiss           07/21/95
!               Modification and Development
!
!               Frederick J. Shaw        04/03/1996
!               modified for MODLAND Version 1 Metadata Dictionary
!               Revision 1.02 March 26, 1996
!		
!		Jayshree Murthy  06/02/1998
!		modified for f90 compatibility
!
!		Arvind Solanki	11/03/1999
!		Added SingleDateTime group macros
!
! !Team-unique Header: 
!               This software is developed by the MODIS Science Data Support
!               Team for the National Aeronautics and Space Administration,
!               Goddard Space Flight Center, under contract NAS5-32373.
!
! !References and Credits:
!               Portions developed at the National Center for Supercomputing
!               Applications at the Univ. of Illinois at Urbana-Champaign.
!
! !Design Notes
!		Formatted for BOTH free and fixed format f90
!
! !END
!------------------------------------------------------------------------------

      include  'PGS_MET.f'

!  Declarations of return values for M-API functions
      
      INTEGER CLMFIL, CPMFIL, CRMAR, CRMGRP, CRMTBL
      INTEGER EMOBJ, RMFH, CMFH
      INTEGER GMARDM, GMAR, GMFLDS, GMFIN, GMTBL
      INTEGER GMDMIN, GMDNAM, GMARIN, GMECIN, GMHOID
      INTEGER MSIZE
      INTEGER OPMFIL
      INTEGER PMAR, PMFIN, PMTBL, PMARIN
      INTEGER PMDMIN, PMDNAM
      INTEGER SMECIN, SRMGRP

!  M-API Data Type Constants

!   DATATYPELENMAX     Minimum string length required to hold a data type

      INTEGER DATATYPELENMAX
      INTEGER MAX_ECS_NAME_L, MODIS_ALL_TYPES
      INTEGER MODIS_ARRAY, MODIS_TABLE

      CHARACTER*(*) I8, I16, I32, I64
      CHARACTER*(*) R32, R64
      CHARACTER*(*) UI8, UI16, UI32
      CHARACTER*(*) TXT
      PARAMETER (DATATYPELENMAX =  14)
      PARAMETER (MAX_ECS_NAME_L = 256)
      PARAMETER (MODIS_ALL_TYPES = 0)
      PARAMETER (MODIS_ARRAY = 720)
      PARAMETER (MODIS_TABLE = 1962)
      PARAMETER (I8 = 'INTEGER*1', I16 = 'INTEGER*2')
      PARAMETER (I32 = 'INTEGER*4', I64 = 'INTEGER*8')
      PARAMETER (R32 = 'REAL*4', R64 = 'REAL*8')
      PARAMETER (UI8 = 'UINTEGER*1', UI16 = 'UINTEGER*2')
      PARAMETER (UI32 = 'UINTEGER*4')
      PARAMETER (TXT = 'CHARACTER*(*)')

!C*********************** METADATA ****************************************
!
!C***************** HDF Standard Attribute Names***************************

! HDF standard SDS array structure label string (SDgetdatastrs)
!  and dimension label string (SDgetdimstrs) attribute name 
      CHARACTER*(*) MLONG_NAME
      PARAMETER ( MLONG_NAME = 'long_name')

! HDF standard SDS array structure units string (SDgetdatastrs) 
! and dimension units string (SDgetdimstrs) attribute name 
      CHARACTER*(*)   MUNITS
      PARAMETER ( MUNITS = 'units')

! HDF standard SDS array structure format string (SDgetdatastrs) 
! and dimension format string (SDgetdimstrs) attribute name 
      CHARACTER*(*)   MFORMAT
      PARAMETER ( MFORMAT = 'format')

! HDF standard SDS array structure coordinate system string (SDgetdatastrs) 
! attribute name 
      CHARACTER*(*)   MCOORD_SYS
      PARAMETER ( MCOORD_SYS = 'cordsys')

! HDF Standard SDS array structure Calibration factor (SDgetcal)
! attribute name
      CHARACTER*(*)   MSLOPE
      PARAMETER ( MSLOPE = 'scale_factor')

! HDF Standard SDS array structure Calibration factor error (SDgetcal)
! attribute name 
      CHARACTER*(*)   MSLOPE_ERROR
      PARAMETER ( MSLOPE_ERROR = 'scale_factor_err')

! HDF Standard SDS array structure uncalibrated offset (SDgetcal)
! attribute name 
      CHARACTER*(*)   MOFFSET
      PARAMETER ( MOFFSET = 'add_offset')

! HDF Standard SDS array structure uncalibrated offset error (SDgetcal)
! attribute name 
      CHARACTER*(*)   MOFFSET_ERROR
      PARAMETER ( MOFFSET_ERROR = 'add_offset_err')

! HDF Standard SDS array structure uncalibrated data HDF number type (SDgetcal)
! attribute name 
      CHARACTER*(*)   MNUM_TYPE
      PARAMETER ( MNUM_TYPE = 'calibrated_nt')

! HDF standard data valid range (SDgetrange)[minimum,maximum] attribute name 
      CHARACTER*(*)   MDATA_RANGE
      PARAMETER ( MDATA_RANGE = 'valid_range')

! HDF Standard SDS array structure Fill Value (SDgetfillvalue)  
! attribute name 
      CHARACTER*(*)   MFILL_VALUE
      PARAMETER ( MFILL_VALUE = '_FillValue')


!C***************** ECS core metadata names********************************

! ECS core metadata global attribute name 
      CHARACTER*(*)   MECS_CORE
      PARAMETER ( MECS_CORE = 'CoreMetadata.0')

! The following metadata are encoded in the = 'CoreMetadata.0') attribute string
! in the ECS-defined metadata language.

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
! OBJECT = ADDITIONALATTRIBUTENAME
      CHARACTER*(*) MCORE_ADD_ATTRIBUTENAME
      PARAMETER ( MCORE_ADD_ATTRIBUTENAME = 'ADDITIONALATTRIBUTENAME')

! OBJECT = ANCILLARYINPUTTYPE
      CHARACTER*(*) MCORE_ANCIL_INPUT_TYPE  
      PARAMETER ( MCORE_ANCIL_INPUT_TYPE =  'ANCILLARYINPUTTYPE')

! OBJECT = ANCILLARYINPUTPOINTER
! DEFINITION = References to all ancillary input files, i.e. all input files
! other than MODIS products.
      CHARACTER*(*)   MCORE_ANCIL_POINTER
      PARAMETER ( MCORE_ANCIL_POINTER  = 'ANCILLARYINPUTPOINTER')

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8

! OBJECT = ASSOCIATEDINSTRUMENTSHORTNAME obsolete
      CHARACTER*(*) MCORE_AISHORTNAME
      PARAMETER (MCORE_AISHORTNAME = 'ASSOCIATEDINSTRUMENTSHORTNAME')

! OBJECT = ASSOCIATEDPLATFORMSHORTNAME obsolete
      CHARACTER*(*) MCORE_APSHORTNAME
      PARAMETER (MCORE_APSHORTNAME = 'ASSOCIATEDPLATFORMSHORTNAME')

! OBJECT = ASSOCIATEDSENSORSHORTNAME obsolete
      CHARACTER*(*) MCORE_ASSHORTNAME
      PARAMETER (MCORE_ASSHORTNAME = 'ASSOCIATEDSENSORSHORTNAME')

! OBJECT = AUTOMATICQUALITYFLAG
! DEFINITION = Indicates the results of QA performed during product
! generation.
      CHARACTER*(*) MCORE_AUTO_QUALITY
      PARAMETER (MCORE_AUTO_QUALITY  = 'AUTOMATICQUALITYFLAG')

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
! OBJECT = AUTOMATICQUALITYFLAGEXPLANATION
      CHARACTER*(*) MCORE_AQUAL_FLG 
      PARAMETER (MCORE_AQUAL_FLG = 'AUTOMATICQUALITYFLAGEXPLANATION')

! OBJECT = DAYNIGHTFLAG
      CHARACTER*(*) MCORE_DAYNIGHTFLAG
      PARAMETER (MCORE_DAYNIGHTFLAG = 'DAYNIGHTFLAG')

! OBJECT = EASTBOUNDINGCOORDINATE
! DEFINITION = Easternmost longitude of the granule spatial coverage.
      CHARACTER*(*)   MCORE_EAST_BOUND
      PARAMETER ( MCORE_EAST_BOUND  = 'EASTBOUNDINGCOORDINATE')

! OBJECT = EQUATORCROSSINGLONGITUDE
      CHARACTER*(*) MCORE_EQUATCROSSINGLONG 
      PARAMETER (MCORE_EQUATCROSSINGLONG =  'EQUATORCROSSINGLONGITUDE')

! OBJECT = EQUATORCROSSINGDATE
      CHARACTER*(*) MCORE_EQUATCROSSINGDATE
      parameter (MCORE_EQUATCROSSINGDATE = 'EQUATORCROSSINGDATE')

! OBJECT = EQUATORCROSSINGTIME
      CHARACTER*(*) MCORE_EQUATCROSSINGTIME
      parameter (MCORE_EQUATCROSSINGTIME = 'EQUATORCROSSINGTIME')

! OBJECT = EXCLUSIONGRINGFLAG.1
! DEFINITION = Flag indicating whether points are on an inner (exclusion)
! G-ring.  
      CHARACTER*(*)   MCORE_EXCLUS_GRING_FLG
      PARAMETER ( MCORE_EXCLUS_GRING_FLG = 'EXCLUSIONGRINGFLAG.1')

! OBJECT = GRANULEPOINTER
! DEFINITION = Self-reference to granule. 
! obsolete 
      CHARACTER*(*)   MCORE_GRAN_POINTER
      PARAMETER ( MCORE_GRAN_POINTER  = 'GRANULEPOINTER')

! OBJECT = GRINGPOINTLATITUDE.1
! DEFINITION = Latitudes of a series of points representing the perimeter of
! the granule spatial coverage (i.e., corners).
      CHARACTER*(*)   MCORE_GRING_POINT_LAT
      PARAMETER (MCORE_GRING_POINT_LAT  = 'GRINGPOINTLATITUDE.1')

! OBJECT = GRINGPOINTLONGITUDE.1
! DEFINITION = Longitudes of a series of points representing the perimeter of
! the granule spatial coverage.
      CHARACTER*(*)   MCORE_GRING_POINT_LON
      PARAMETER (MCORE_GRING_POINT_LON  = 'GRINGPOINTLONGITUDE.1')

! OBJECT = GRINGPOINTSEQUENCENO.1
! DEFINITION = Sequence numbers corresponding to perimeter latitudes and
! longitudes.
      CHARACTER*(*)   MCORE_GRING_POINT_NUM
      PARAMETER (MCORE_GRING_POINT_NUM  = 'GRINGPOINTSEQUENCENO.1')

! OBJECT = INPUTPOINTER
! DEFINITION = References to other MODIS product granules used as input for
! this product.
      CHARACTER*(*) MCORE_INPUT_POINTER
      PARAMETER (MCORE_INPUT_POINTER  = 'INPUTPOINTER')

! OBJECT = INSTRUMENTSHORTNAME
      CHARACTER*(*) MCORE_INSTRUMENTSHORTNAME
      PARAMETER (MCORE_INSTRUMENTSHORTNAME = 'INSTRUMENTSHORTNAME')

! OBJECT = LocalGranuleID
      CHARACTER*(*) MCORE_LOCALGRANULEID 
      PARAMETER (MCORE_LOCALGRANULEID =    'LOCALGRANULEID')

! OBJECT = LocalVersionID
      CHARACTER*(*) MCORE_LOCALVERSIONID 
      PARAMETER (MCORE_LOCALVERSIONID = 'LOCALVERSIONID')

! OBJECT = LONGNAME
! DEFINITION = A descriptive name for the data collection.
      CHARACTER*(*) MCORE_LONG_NAME
      PARAMETER (MCORE_LONG_NAME    = 'LONGNAME')

! OBJECT = NORTHBOUNDINGCOORDINATE
! DEFINITION = Northernmost latitude of the granule spatial coverage.
      CHARACTER*(*)   MCORE_NORTH_BOUND
      PARAMETER (MCORE_NORTH_BOUND  = 'NORTHBOUNDINGCOORDINATE')

! OBJECT = OPERATIONALQUALITYFLAG
! DEFINITION = The granule level flag applying both generally to the granule
! and specifically to the parameters at the granule level. When applied to a
! parameter, the flag refers to the quality of that parameter in the granule.
      CHARACTER*(*)  MCORE_OPER_QUAL_FLAG,                              &
     & MCORE_OPERATIONALQUALITYFLAG
      PARAMETER (MCORE_OPER_QUAL_FLAG   =    'OPERATIONALQUALITYFLAG')
      PARAMETER (MCORE_OPERATIONALQUALITYFLAG = MCORE_OPER_QUAL_FLAG)

! OBJECT = OPERATIONALQUALITYFLAGEXPLANATION
! DEFINITION =
!
      CHARACTER*(*) MCORE_OQFLG_EXPL
      PARAMETER(MCORE_OQFLG_EXPL = 'OPERATIONALQUALITYFLAGEXPLANATION')

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8

! OBJECT = ORBITNUMBER
! DEFINITION = Number of satellite orbit during which the granule data were
! collected.
      CHARACTER*(*) MCORE_ORBIT_NUM
      PARAMETER (MCORE_ORBIT_NUM = 'ORBITNUMBER')

! OBJECT = PARAMETERNAME
! DEFINITION =
      CHARACTER*(*) MCORE_PARAMETERNAME 
      PARAMETER (MCORE_PARAMETERNAME = 'PARAMETERNAME')

! OBJECT = PARAMETERVALUE
! DEFINITION =
      CHARACTER*(*) MCORE_PARAMETERVALUE
      PARAMETER (MCORE_PARAMETERVALUE = 'PARAMETERVALUE')

! OBJECT = PGEVERSION
! DEFINITION =
      CHARACTER*(*) MCORE_PGEVERSION 
      PARAMETER (MCORE_PGEVERSION  = 'PGEVERSION')

! OBJECT = PLATFORMSHORTNAME
! DEFINITION =
      CHARACTER*(*) MCORE_PLATFORMSHORTNAME
      PARAMETER (MCORE_PLATFORMSHORTNAME = 'PLATFORMSHORTNAME')

! OBJECT = PRODUCTIONDATETIME
! DEFINITION = Reference to processing history file.
!
      CHARACTER*(*) MCORE_PRODUCTIONDATETIME
      PARAMETER (MCORE_PRODUCTIONDATETIME = 'PRODUCTIONDATETIME')

! OBJECT = PROCESSINGHISTORYPOINTER
! DEFINITION = Reference to processing history file.
!
      CHARACTER*(*) MCORE_HISTORY_POINTER 
      PARAMETER (MCORE_HISTORY_POINTER = 'PROCESSINGHISTORYPOINTER')

! OBJECT = QAPERCENTCLOUDCOVER
! DEFINITION = Value indicating the percent of interpolated data in the
! granule

      CHARACTER*(*) MCORE_QAPERCENTCLOUDCOVER 
      PARAMETER (MCORE_QAPERCENTCLOUDCOVER = 'QAPERCENTCLOUDCOVER')

! OBJECT = QAPERCENTINTERPOLATEDDATA
! DEFINITION = Value indicating the percent of interpolated data in the
! granule
      CHARACTER*(*) MCORE_PERCENT_INTERP
      PARAMETER ( MCORE_PERCENT_INTERP = 'QAPERCENTINTERPOLATEDDATA')

! OBJECT = QAPERCENTMISSINGDATA
! DEFINITION = Value indicating the percent of missing data in the granule.
      CHARACTER*(*) MCORE_PERCENT_MISSING
      PARAMETER (MCORE_PERCENT_MISSING  = 'QAPERCENTMISSINGDATA')

! OBJECT = QAPERCENTOUTOFBOUNDSDATA
! DEFINITION = Value indicating the percent of data in the granule outside of
! acceptable limits.
      CHARACTER*(*) MCORE_PERCENT_OUT
      PARAMETER (MCORE_PERCENT_OUT  = 'QAPERCENTOUTOFBOUNDSDATA')

! OBJECT = QUALITYFLAGEXPLANATION
! DEFINITION = A text explanation of the criteria used to set each quality
! flag; including thresholds or other criteria.
      CHARACTER*(*) MCORE_QUAL_EXPL
      PARAMETER (MCORE_QUAL_EXPL    = 'QUALITYFLAGEXPLANATION')

! OBJECT = RANGEBEGINNINGDATETIME
! DEFINITION = The date and time when the temporal coverage period of this
! granule began.
!     obsolete
      CHARACTER*(*) MCORE_RANGE_START
      PARAMETER (MCORE_RANGE_START  = 'RANGEBEGINNINGDATETIME')

      CHARACTER*(*) MCORE_RANGE_BEG_DATE
      PARAMETER (MCORE_RANGE_BEG_DATE = 'RANGEBEGINNINGDATE')

      CHARACTER*(*) MCORE_RANGE_BEG_TIME
      PARAMETER (MCORE_RANGE_BEG_TIME = 'RANGEBEGINNINGTIME')

! OBJECT = RANGEENDINGDATETIME
! DEFINITION = The date and time when the temporal coverage period of this
! granule ended.
!     obsolete
      CHARACTER*(*) MCORE_RANGE_END
      PARAMETER (MCORE_RANGE_END  = 'RANGEENDINGDATETIME')

      CHARACTER*(*) MCORE_RANGE_ENDING_DATE
      PARAMETER (MCORE_RANGE_ENDING_DATE = 'RANGEENDINGDATE')

      CHARACTER*(*) MCORE_RANGE_ENDING_TIME
      PARAMETER (MCORE_RANGE_ENDING_TIME = 'RANGEENDINGTIME')

! OBJECT = SINGLEDATETIME
! DEFINITION = The date and time used when specifying a single date and/or
! single point in time covered by a data collection, granule or event.
      CHARACTER*(*) MCORE_TIME_OF_DAY
      PARAMETER (MCORE_TIME_OF_DAY = 'TimeofDay')
      
      CHARACTER*(*) MCORE_CALENDAR_DATE
      PARAMETER (MCORE_CALENDAR_DATE = 'CalendarDate')

! DEFINITION = Indicator of what reprocessing is planned for the granule.
      CHARACTER*(*) MCORE_TO_BE_REDONE
      PARAMETER (MCORE_TO_BE_REDONE  = 'REPROCESSINGPLANNED')

! OBJECT = REPROCESSINGACTUAL
! DEFINITION = Indicator of the reprocessing status of the granule.
      CHARACTER*(*)  MCORE_ACTUALLY_REDONE
      PARAMETER ( MCORE_ACTUALLY_REDONE  = 'REPROCESSINGACTUAL')

! OBJECT = SCIENCEQUALITYFLAG
! DEFINITION = The granule level flag applying both generally to the granule
! and specifically to the parameters at the granule level. When applied to a
! parameter, the flag refers to the quality of that parameter in the granule
      CHARACTER*(*) MCORE_SCIENCE_QUAL_FLG, MCORE_SCIENCEQUALITYFLAG
      PARAMETER( MCORE_SCIENCE_QUAL_FLG  = 'SCIENCEQUALITYFLAG')
      PARAMETER(MCORE_SCIENCEQUALITYFLAG = MCORE_SCIENCE_QUAL_FLG )

! OBJECT = SCIENCEQUALITYFLAGEXPLANATION
! DEFINITION = The granule level flag applying both generally to the granule
! and specifically to the parameters at the granule level. When applied to a
! parameter, the flag refers to the quality of that parameter in the granule.
!
      CHARACTER*(*) MCORE_SCIENCE_QUAL_FLG_EXPL
      PARAMETER(MCORE_SCIENCE_QUAL_FLG_EXPL =                           &
     &  'SCIENCEQUALITYFLAGEXPLANATION')

! OBJECT = SENSORSHORTNAME
      CHARACTER*(*) MCORE_SENSOR_SHORT_NAME
      PARAMETER (MCORE_SENSOR_SHORT_NAME = 'SENSORSHORTNAME')

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
! OBJECT = SensorCharacteristicName  obsolete
      CHARACTER*(*) MCORE_SENSORCHARNAME
      PARAMETER (MCORE_SENSORCHARNAME = 'SENSORCHARACTERISTICNAME')

! OBJECT = SensorCharacteristicValue obsolete
      CHARACTER*(*) MCORE_SENSORCHARVALUE
      PARAMETER (MCORE_SENSORCHARVALUE = 'SENSORCHARACTERISTICVALUE')

! OBJECT = SHORTNAME
! DEFINITION = The identifier for the data collection.
      CHARACTER*(*) MCORE_SHORT_NAME
      PARAMETER ( MCORE_SHORT_NAME = 'SHORTNAME')

! OBJECT = SIZEMBECSDATAGRANULE
! DEFINITION = The size of the data granule in megabytes.
      CHARACTER*(*) MCORE_SIZE_OF_GRANULE
      PARAMETER ( MCORE_SIZE_OF_GRANULE  = 'SIZEMBECSDATAGRANULE')

! OBJECT = SOUTHBOUNDINGCOORDINATE
! DEFINITION = Southernmost latitude of the granule spatial coverage.
      CHARACTER*(*) MCORE_SOUTH_BOUND
      PARAMETER ( MCORE_SOUTH_BOUND  = 'SOUTHBOUNDINGCOORDINATE')
      
! OBJECT = WESTBOUNDINGCOORDINATE
! DEFINITION = Westernmost longitude of the granule spatial coverage.
      CHARACTER*(*) MCORE_WEST_BOUND
      PARAMETER ( MCORE_WEST_BOUND  = 'WESTBOUNDINGCOORDINATE')

      CHARACTER*(*) MCORE_VERSIONID
      PARAMETER ( MCORE_VERSIONID  = 'VERSIONID')

!C*******************ECS product specific metadata*****************************

!  ECS product specific  metadata global attribute name
      CHARACTER*(*) MECS_ARCHIVE
      PARAMETER ( MECS_ARCHIVE  = 'ArchiveMetadata.0')

!  ECS product specific  metadata global attribute name
      CHARACTER*(*)   MECS_PRODUCT
      PARAMETER ( MECS_PRODUCT  = MECS_ARCHIVE)
!     PARAMETER ( MECS_PRODUCT  = 'ProductMetadata.0')

!C*******************E-Product-Inventory Metadata*****************************

! OBJECT = MODISPRODUCTFILENAME
! DEFINITION = The MODIS filename for this granule.
      CHARACTER*(*)   MPROD_FILENAME
      PARAMETER ( MPROD_FILENAME = 'MODISPRODUCTFILENAME')

! OBJECT = OPERATIONMODE
! DEFINITION = MODIS mode of operation.
      CHARACTER*(*)   MPROD_OPERATIONMODE
      PARAMETER ( MPROD_OPERATIONMODE  =     'OPERATIONMODE')

! OBJECT = PROCESSINGDATETIME
! DEFINITION = This field contains the date and time the process that
! created this file was started.
      CHARACTER*(*)   MPROD_PROC_DATE_TIME
      PARAMETER ( MPROD_PROC_DATE_TIME  = 'PROCESSINGDATETIME')

! OBJECT = SPSOPARAMETERS
! DEFINITION = The SPSO parameters for all data contained in this file, as
! listed in the SPSO database.
      CHARACTER*(*)   MPROD_SPSO_PARAM
      PARAMETER ( MPROD_SPSO_PARAM  = 'SPSOPARAMETERS')

!C**********E-Product-Inventory-A Metadata ***********************************

! OBJECT = GRANULENUMBER
! DEFINITION = The number of this MODIS granule.
      CHARACTER*(*)   MPROD_GRANULE_NUM
      PARAMETER ( MPROD_GRANULE_NUM  = 'GRANULENUMBER')

!C******************E-Product-Inventory-B Metadata*****************************

! OBJECT = GRIDTYPE
! DEFINITION = The type of MODLAND global grid used for L2G, L3 and L4
! products.
      CHARACTER*(*)   MPROD_GRID_TYPE
      PARAMETER ( MPROD_GRID_TYPE         = 'GRIDTYPE')

! OBJECT = HORIZONTALTILENUMBER
! DEFINITION = The horizontal tile number of this tile in the MODLAND
! integerized sinusoidal grid.
      CHARACTER*(*)   MPROD_HORIZ_TILE_NUM
      PARAMETER ( MPROD_HORIZ_TILE_NUM = 'HORIZONTALTILENUMBER')

! OBJECT = VERTICALTILENUMBER
! DEFINITION = The vertical tile number of this tile in the MODLAND
! integerized sinusoidal grid.
      CHARACTER*(*)   MPROD_VERT_TILE_NUM
      PARAMETER ( MPROD_VERT_TILE_NUM  = 'VERTICALTILENUMBER')

!C*****************E-Product-Archive Metadata*********************************
! OBJECT = ALGORITHMPACKAGEACCEPTANCEDATE
! DEFINITION = The date this algorithm package version successfully passed
! AI&T procedures and was accepted as an ECS standard algorithm.
      CHARACTER*(*)   MPROD_ALGO_PCK_ACPT_DATE
      PARAMETER ( MPROD_ALGO_PCK_ACPT_DATE = 'ALGOPACKAGEACCEPTDATE')

! OBJECT = ALGORITHMPACKAGEMATURITYCODE
! DEFINITION = This specifies the maturity of the algorithm package.
      CHARACTER*(*)   MPROD_ALGO_PACK_MAT_CODE
      PARAMETER ( MPROD_ALGO_PACK_MAT_CODE = 'ALGOPACKAGEMATURITYCODE')

! OBJECT = ALGORITHMPACKAGENAME
! DEFINITION = The name given to the complete delivered algorithm package
! ksubmitted for algorithm integration and test.
      CHARACTER*(*)   MPROD_ALGO_PACK_NAME
      PARAMETER ( MPROD_ALGO_PACK_NAME  = 'ALGORITHMPACKAGENAME')

! OBJECT = ALGORITHMPACKAGEVERSION
! DEFINITION = The version of the algorithm package.
      CHARACTER*(*)   MPROD_ALGO_PACK_VER
      PARAMETER ( MPROD_ALGO_PACK_VER = 'ALGORITHMPACKAGEVERSION')

! OBJECT = INSTRUMENTNAME
! DEFINITION = The long name by which the instrument is known.
      CHARACTER*(*)   MPROD_INSTR_NAME
      PARAMETER ( MPROD_INSTR_NAME  =  'INSTRUMENTNAME')

! OBJECT = LocalInputGranuleID
      CHARACTER*(*) MCORE_LOCALINPUTGRANULEID
      PARAMETER (MCORE_LOCALINPUTGRANULEID = 'LOCALINPUTGRANULEID')

! OBJECT = PLATFORMSHORTNAME
! DEFINITION = The short name assigned to the platform carrying the
! instrument.
      CHARACTER*(*)   MPROD_PLATFORM_SHORT_NAM
      PARAMETER ( MPROD_PLATFORM_SHORT_NAM  =  'PLATFORMSHORTNAME')

! OBJECT = PROCESSINGCENTER
! DEFINITION = DAAC where product is processed.
      CHARACTER*(*)   MPROD_PROC_CENTER
      PARAMETER ( MPROD_PROC_CENTER  = 'PROCESSINGCENTER')

!C****************** ECS product specific metadata ***************************

!  HDF standard Long Name metadata attribute name
      CHARACTER*(*) LNAME
      PARAMETER (LNAME = 'long_name')

!  HDF standard Units metadata attribute name
      CHARACTER*(*) UNITS
      PARAMETER (UNITS = 'units')

!  HDF standard Calibration factor array attribute name
      CHARACTER*(*) CAL
      PARAMETER (CAL = 'scale_factor')

!  HDF standard Uncalibrated offset array attribute name
!  DEFINITION = HDF predefined attribute; additive calibration coefficie
      CHARACTER*(*) OFFSET
      PARAMETER (OFFSET = 'add_offset')

!  C-like file access types (for OPMFIL)
      CHARACTER*(*) CREATE_FILE, APPND_FILE, RD_FILE, OVERWRITE_FILE
      PARAMETER (CREATE_FILE = 'w', APPND_FILE = 'a', RD_FILE = 'r')
      PARAMETER (OVERWRITE_FILE = 'w')

!  M-API Return Values
!  MODFILLEN  Size of modfil M-API file array structure
      INTEGER MFAIL, MAPIOK, MODFILLEN
      PARAMETER (MFAIL=-1, MAPIOK=0)
      PARAMETER (MODFILLEN = 5)


! Internal M-API constants
!       P_SDID                  modfil array location of a file's sd_id
!       P_HDFID                 modfil array location of a file's hdf file_id
!       P_ACCESS                modfil array location of a file's DFACC_*
!                               access type number
!       P_ADDR                  address of MODFILE structure returned
!                               from call to C routine openMODISfile
      INTEGER P_SDID, P_HDFID, P_ACCESS, DFACC_MFAIL, P_ADDR, P_ADDR_EXT
      PARAMETER (P_SDID = 1, P_HDFID = 2, P_ACCESS = 3, P_ADDR = 4)
      PARAMETER (P_ADDR_EXT = 5)
      PARAMETER (DFACC_MFAIL = 0)
