C----------------------------------------------------------------------------
C               mapi.inc utilities header file
C                        PROTOTYPE 
C----------------------------------------------------------------------------
C
C----------------------------------------------------------------------------
C !F77-INC
C
C !Description: All M-API Functions available in the M-API software library are
C               declared to have INTEGER return values.
C
C               Data type constants are provided to label the data type of data 
C               contained in a data object.  They are acceptable inputs for the 
C               data_type parameter of createMODISarray and createMODIStable. 
C               They may be used as inputs to the MODISsizeof routine, which
C               returns the number of bytes of memory required to store the 
C               data type.
C
C !Revision History:
C
C  Id: mapi.inc,v 2.0 1997/07/29 12:54:40 fshaw Exp fshaw $
C  Log: mapi.inc,v $
C  Revision 2.0  1997/07/29  12:54:40  fshaw
C vers 2.2 copied from ftp site
C
C# Revision 1.5  1997/03/05  16:40:21  qhuang
C# Added a 'D' in parameter QAPERCENTINTERPOLATEDATA.
C#
C# Revision 1.4  1996/06/05  19:47:23  qhuang
C# *** empty log message ***
C#
C
C !References and Credits: 
C
C               WRITTEN BY:
C
C               Vicky Lin                06/08/95
C               Research and Data systems Corporation
C               SAIC/GSC MODIS Science Data Support Office
C               7501 Forbes Blvd
C               Seabrood, MD 20706
C
C               vlin@modis-xl.gsfc.nasa.gov
C
C               Mitchell Weiss           07/21/95
C               Modification and Development
C
C               Frederick J. Shaw        04/03/1996
C               modified for MODLAND Version 1 Metadata Dictionary
C               Revision 1.02 March 26, 1996
C
C !Team-unique Header: 
C
C               This software is developed by the MODIS Science Data Support
C               Team for the National Aeronautics and Space Administration,
C               Goddard Space Flight Center, under contract NAS5-32373.
C
C               Portions developed at the National Center for Supercomputing
C               Applications at the Univ. of Illinois at Urbana-Champaign.
C
C !Design Notes
C
C !END
C------------------------------------------------------------------------------

      include  'PGS_MET.f'

C  Declarations of return values for M-API functions
      
      INTEGER CLMFIL, CPMFIL, CRMAR, CRMGRP, CRMTBL
      INTEGER EMOBJ, RMFH, CMFH
      INTEGER GMARDM, GMAR, GMFLDS, GMFIN, GMTBL
      INTEGER GMDMIN, GMDNAM, GMARIN, GMECIN, GMHOID
      INTEGER MSIZE
      INTEGER OPMFIL
      INTEGER PMAR, PMFIN, PMTBL, PMARIN
      INTEGER PMDMIN, PMDNAM
      INTEGER SMECIN, SRMGRP

C  M-API Data Type Constants

C   DATATYPELENMAX     Minimum string length required to hold a data type

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
C
!C***************** HDF Standard Attribute Names***************************

C HDF standard SDS array structure label string (SDgetdatastrs)
C  and dimension label string (SDgetdimstrs) attribute name 
      CHARACTER*(*) MLONG_NAME
      PARAMETER ( MLONG_NAME = 'long_name')

C HDF standard SDS array structure units string (SDgetdatastrs) 
C and dimension units string (SDgetdimstrs) attribute name 
      CHARACTER*(*)   MUNITS
      PARAMETER ( MUNITS = 'units')

C HDF standard SDS array structure format string (SDgetdatastrs) 
C and dimension format string (SDgetdimstrs) attribute name 
      CHARACTER*(*)   MFORMAT
      PARAMETER ( MFORMAT = 'format')

C HDF standard SDS array structure coordinate system string (SDgetdatastrs) 
C attribute name 
      CHARACTER*(*)   MCOORD_SYS
      PARAMETER ( MCOORD_SYS = 'cordsys')

C HDF Standard SDS array structure Calibration factor (SDgetcal)
C attribute name
      CHARACTER*(*)   MSLOPE
      PARAMETER ( MSLOPE = 'scale_factor')

C HDF Standard SDS array structure Calibration factor error (SDgetcal)
C attribute name 
      CHARACTER*(*)   MSLOPE_ERROR
      PARAMETER ( MSLOPE_ERROR = 'scale_factor_err')

C HDF Standard SDS array structure uncalibrated offset (SDgetcal)
C attribute name 
      CHARACTER*(*)   MOFFSET
      PARAMETER ( MOFFSET = 'add_offset')

C HDF Standard SDS array structure uncalibrated offset error (SDgetcal)
C attribute name 
      CHARACTER*(*)   MOFFSET_ERROR
      PARAMETER ( MOFFSET_ERROR = 'add_offset_err')

C HDF Standard SDS array structure uncalibrated data HDF number type (SDgetcal)
C attribute name 
      CHARACTER*(*)   MNUM_TYPE
      PARAMETER ( MNUM_TYPE = 'calibrated_nt')

C HDF standard data valid range (SDgetrange)[minimum,maximum] attribute name 
      CHARACTER*(*)   MDATA_RANGE
      PARAMETER ( MDATA_RANGE = 'valid_range')

C HDF Standard SDS array structure Fill Value (SDgetfillvalue)  
C attribute name 
      CHARACTER*(*)   MFILL_VALUE
      PARAMETER ( MFILL_VALUE = '_FillValue')


!C***************** ECS core metadata names********************************

C ECS core metadata global attribute name 
      CHARACTER*(*)   MECS_CORE
      PARAMETER ( MECS_CORE = 'CoreMetadata.0')

C The following metadata are encoded in the = 'CoreMetadata.0') attribute string
C in the ECS-defined metadata language.

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C OBJECT = ADDITIONALATTRIBUTENAME
      CHARACTER*(*) MCORE_ADD_ATTRIBUTENAME
      PARAMETER ( MCORE_ADD_ATTRIBUTENAME = 'ADDITIONALATTRIBUTENAME')

C OBJECT = ANCILLARYINPUTTYPE
      CHARACTER*(*) MCORE_ANCIL_INPUT_TYPE  
      PARAMETER ( MCORE_ANCIL_INPUT_TYPE =  'ANCILLARYINPUTTYPE')

C OBJECT = ANCILLARYINPUTPOINTER
C DEFINITION = References to all ancillary input files, i.e. all input files
C other than MODIS products.
      CHARACTER*(*)   MCORE_ANCIL_POINTER
      PARAMETER ( MCORE_ANCIL_POINTER  = 'ANCILLARYINPUTPOINTER')

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8

C OBJECT = ASSOCIATEDINSTRUMENTSHORTNAME obsolete
      CHARACTER*(*) MCORE_AISHORTNAME
      PARAMETER (MCORE_AISHORTNAME = 'ASSOCIATEDINSTRUMENTSHORTNAME')

C OBJECT = ASSOCIATEDPLATFORMSHORTNAME obsolete
      CHARACTER*(*) MCORE_APSHORTNAME
      PARAMETER (MCORE_APSHORTNAME = 'ASSOCIATEDPLATFORMSHORTNAME')

C OBJECT = ASSOCIATEDSENSORSHORTNAME obsolete
      CHARACTER*(*) MCORE_ASSHORTNAME
      PARAMETER (MCORE_ASSHORTNAME = 'ASSOCIATEDSENSORSHORTNAME')

C OBJECT = AUTOMATICQUALITYFLAG
C DEFINITION = Indicates the results of QA performed during product
C generation.
      CHARACTER*(*) MCORE_AUTO_QUALITY
      PARAMETER (MCORE_AUTO_QUALITY  = 'AUTOMATICQUALITYFLAG')

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C OBJECT = AUTOMATICQUALITYFLAGEXPLANATION
      CHARACTER*(*) MCORE_AQUAL_FLG 
      PARAMETER (MCORE_AQUAL_FLG = 'AUTOMATICQUALITYFLAGEXPLANATION')

C OBJECT = DAYNIGHTFLAG
      CHARACTER*(*) MCORE_DAYNIGHTFLAG
      PARAMETER (MCORE_DAYNIGHTFLAG = 'DAYNIGHTFLAG')

C OBJECT = EASTBOUNDINGCOORDINATE
C DEFINITION = Easternmost longitude of the granule spatial coverage.
      CHARACTER*(*)   MCORE_EAST_BOUND
      PARAMETER ( MCORE_EAST_BOUND  = 'EASTBOUNDINGCOORDINATE')

C OBJECT = EQUATORCROSSINGLONGITUDE
      CHARACTER*(*) MCORE_EQUATCROSSINGLONG 
      PARAMETER (MCORE_EQUATCROSSINGLONG =  'EQUATORCROSSINGLONGITUDE')

C OBJECT = EQUATORCROSSINGDATE
      CHARACTER*(*) MCORE_EQUATCROSSINGDATE
      parameter (MCORE_EQUATCROSSINGDATE = 'EQUATORCROSSINGDATE')

C OBJECT = EQUATORCROSSINGTIME
      CHARACTER*(*) MCORE_EQUATCROSSINGTIME
      parameter (MCORE_EQUATCROSSINGTIME = 'EQUATORCROSSINGTIME')

C OBJECT = EXCLUSIONGRINGFLAG.1
C DEFINITION = Flag indicating whether points are on an inner (exclusion)
C G-ring.  
      CHARACTER*(*)   MCORE_EXCLUS_GRING_FLG
      PARAMETER ( MCORE_EXCLUS_GRING_FLG = 'EXCLUSIONGRINGFLAG.1')

C OBJECT = GRANULEPOINTER
C DEFINITION = Self-reference to granule. 
C obsolete 
      CHARACTER*(*)   MCORE_GRAN_POINTER
      PARAMETER ( MCORE_GRAN_POINTER  = 'GRANULEPOINTER')

C OBJECT = GRINGPOINTLATITUDE.1
C DEFINITION = Latitudes of a series of points representing the perimeter of
C the granule spatial coverage (i.e., corners).
      CHARACTER*(*)   MCORE_GRING_POINT_LAT
      PARAMETER (MCORE_GRING_POINT_LAT  = 'GRINGPOINTLATITUDE.1')

C OBJECT = GRINGPOINTLONGITUDE.1
C DEFINITION = Longitudes of a series of points representing the perimeter of
C the granule spatial coverage.
      CHARACTER*(*)   MCORE_GRING_POINT_LON
      PARAMETER (MCORE_GRING_POINT_LON  = 'GRINGPOINTLONGITUDE.1')

C OBJECT = GRINGPOINTSEQUENCENO.1
C DEFINITION = Sequence numbers corresponding to perimeter latitudes and
C longitudes.
      CHARACTER*(*)   MCORE_GRING_POINT_NUM
      PARAMETER (MCORE_GRING_POINT_NUM  = 'GRINGPOINTSEQUENCENO.1')

C OBJECT = INPUTPOINTER
C DEFINITION = References to other MODIS product granules used as input for
C this product.
      CHARACTER*(*) MCORE_INPUT_POINTER
      PARAMETER (MCORE_INPUT_POINTER  = 'INPUTPOINTER')

C OBJECT = INSTRUMENTSHORTNAME
      CHARACTER*(*) MCORE_INSTRUMENTSHORTNAME
      PARAMETER (MCORE_INSTRUMENTSHORTNAME = 'INSTRUMENTSHORTNAME')

C OBJECT = LocalGranuleID
      CHARACTER*(*) MCORE_LOCALGRANULEID 
      PARAMETER (MCORE_LOCALGRANULEID =    'LOCALGRANULEID')

C OBJECT = LocalVersionID
      CHARACTER*(*) MCORE_LOCALVERSIONID 
      PARAMETER (MCORE_LOCALVERSIONID = 'LOCALVERSIONID')

C OBJECT = LONGNAME
C DEFINITION = A descriptive name for the data collection.
      CHARACTER*(*) MCORE_LONG_NAME
      PARAMETER (MCORE_LONG_NAME    = 'LONGNAME')

C OBJECT = NORTHBOUNDINGCOORDINATE
C DEFINITION = Northernmost latitude of the granule spatial coverage.
      CHARACTER*(*)   MCORE_NORTH_BOUND
      PARAMETER (MCORE_NORTH_BOUND  = 'NORTHBOUNDINGCOORDINATE')

C OBJECT = OPERATIONALQUALITYFLAG
C DEFINITION = The granule level flag applying both generally to the granule
C and specifically to the parameters at the granule level. When applied to a
C parameter, the flag refers to the quality of that parameter in the granule.
      CHARACTER*(*)  MCORE_OPER_QUAL_FLAG,
     & MCORE_OPERATIONALQUALITYFLAG
      PARAMETER (MCORE_OPER_QUAL_FLAG   =    'OPERATIONALQUALITYFLAG')
      PARAMETER (MCORE_OPERATIONALQUALITYFLAG = MCORE_OPER_QUAL_FLAG)

C OBJECT = OPERATIONALQUALITYFLAGEXPLANATION
C DEFINITION =
C
      CHARACTER*(*) MCORE_OQFLG_EXPL
      PARAMETER(MCORE_OQFLG_EXPL = 'OPERATIONALQUALITYFLAGEXPLANATION')

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8

C OBJECT = ORBITNUMBER
C DEFINITION = Number of satellite orbit during which the granule data were
C collected.
      CHARACTER*(*) MCORE_ORBIT_NUM
      PARAMETER (MCORE_ORBIT_NUM = 'ORBITNUMBER')

C OBJECT = PARAMETERNAME
C DEFINITION =
      CHARACTER*(*) MCORE_PARAMETERNAME 
      PARAMETER (MCORE_PARAMETERNAME = 'PARAMETERNAME')

C OBJECT = PARAMETERVALUE
C DEFINITION =
      CHARACTER*(*) MCORE_PARAMETERVALUE
      PARAMETER (MCORE_PARAMETERVALUE = 'PARAMETERVALUE')

C OBJECT = PGEVERSION
C DEFINITION =
      CHARACTER*(*) MCORE_PGEVERSION 
      PARAMETER (MCORE_PGEVERSION  = 'PGEVERSION')

C OBJECT = PLATFORMSHORTNAME
C DEFINITION =
      CHARACTER*(*) MCORE_PLATFORMSHORTNAME
      PARAMETER (MCORE_PLATFORMSHORTNAME = 'PLATFORMSHORTNAME')

C OBJECT = PRODUCTIONDATETIME
C DEFINITION = Reference to processing history file.
C
      CHARACTER*(*) MCORE_PRODUCTIONDATETIME
      PARAMETER (MCORE_PRODUCTIONDATETIME = 'PRODUCTIONDATETIME')

C OBJECT = PROCESSINGHISTORYPOINTER
C DEFINITION = Reference to processing history file.
C
      CHARACTER*(*) MCORE_HISTORY_POINTER 
      PARAMETER (MCORE_HISTORY_POINTER = 'PROCESSINGHISTORYPOINTER')

C OBJECT = QAPERCENTCLOUDCOVER
C DEFINITION = Value indicating the percent of interpolated data in the
C granule

      CHARACTER*(*) MCORE_QAPERCENTCLOUDCOVER 
      PARAMETER (MCORE_QAPERCENTCLOUDCOVER = 'QAPERCENTCLOUDCOVER')

C OBJECT = QAPERCENTINTERPOLATEDDATA
C DEFINITION = Value indicating the percent of interpolated data in the
C granule
      CHARACTER*(*) MCORE_PERCENT_INTERP
      PARAMETER ( MCORE_PERCENT_INTERP = 'QAPERCENTINTERPOLATEDDATA')

C OBJECT = QAPERCENTMISSINGDATA
C DEFINITION = Value indicating the percent of missing data in the granule.
      CHARACTER*(*) MCORE_PERCENT_MISSING
      PARAMETER (MCORE_PERCENT_MISSING  = 'QAPERCENTMISSINGDATA')

C OBJECT = QAPERCENTOUTOFBOUNDSDATA
C DEFINITION = Value indicating the percent of data in the granule outside of
C acceptable limits.
      CHARACTER*(*) MCORE_PERCENT_OUT
      PARAMETER (MCORE_PERCENT_OUT  = 'QAPERCENTOUTOFBOUNDSDATA')

C OBJECT = QUALITYFLAGEXPLANATION
C DEFINITION = A text explanation of the criteria used to set each quality
C flag; including thresholds or other criteria.
      CHARACTER*(*) MCORE_QUAL_EXPL
      PARAMETER (MCORE_QUAL_EXPL    = 'QUALITYFLAGEXPLANATION')

C OBJECT = RANGEBEGINNINGDATETIME
C DEFINITION = The date and time when the temporal coverage period of this
C granule began.
C     obsolete
      CHARACTER*(*) MCORE_RANGE_START
      PARAMETER (MCORE_RANGE_START  = 'RANGEBEGINNINGDATETIME')

      CHARACTER*(*) MCORE_RANGE_BEG_DATE
      PARAMETER (MCORE_RANGE_BEG_DATE = 'RANGEBEGINNINGDATE')

      CHARACTER*(*) MCORE_RANGE_BEG_TIME
      PARAMETER (MCORE_RANGE_BEG_TIME = 'RANGEBEGINNINGTIME')

C OBJECT = RANGEENDINGDATETIME
C DEFINITION = The date and time when the temporal coverage period of this
C granule ended.
C     obsolete
      CHARACTER*(*) MCORE_RANGE_END
      PARAMETER (MCORE_RANGE_END  = 'RANGEENDINGDATETIME')

      CHARACTER*(*) MCORE_RANGE_ENDING_DATE
      PARAMETER (MCORE_RANGE_ENDING_DATE = 'RANGEENDINGDATE')

      CHARACTER*(*) MCORE_RANGE_ENDING_TIME
      PARAMETER (MCORE_RANGE_ENDING_TIME = 'RANGEENDINGTIME')

C OBJECT = SINGLEDATETIME
C DEFINITION = The date and time used when specifying a single date and/or
C single point in time covered by a data collection, granule or event.
      CHARACTER*(*) MCORE_TIME_OF_DAY
      PARAMETER (MCORE_TIME_OF_DAY = 'TimeofDay')
      
      CHARACTER*(*) MCORE_CALENDAR_DATE
      PARAMETER (MCORE_CALENDAR_DATE = 'CalendarDate')

C DEFINITION = Indicator of what reprocessing is planned for the granule.
      CHARACTER*(*) MCORE_TO_BE_REDONE
      PARAMETER (MCORE_TO_BE_REDONE  = 'REPROCESSINGPLANNED')

C OBJECT = REPROCESSINGACTUAL
C DEFINITION = Indicator of the reprocessing status of the granule.
      CHARACTER*(*)  MCORE_ACTUALLY_REDONE
      PARAMETER ( MCORE_ACTUALLY_REDONE  = 'REPROCESSINGACTUAL')

C OBJECT = SCIENCEQUALITYFLAG
C DEFINITION = The granule level flag applying both generally to the granule
C and specifically to the parameters at the granule level. When applied to a
C parameter, the flag refers to the quality of that parameter in the granule
      CHARACTER*(*) MCORE_SCIENCE_QUAL_FLG, MCORE_SCIENCEQUALITYFLAG
      PARAMETER( MCORE_SCIENCE_QUAL_FLG  = 'SCIENCEQUALITYFLAG')
      PARAMETER(MCORE_SCIENCEQUALITYFLAG = MCORE_SCIENCE_QUAL_FLG )

C OBJECT = SCIENCEQUALITYFLAGEXPLANATION
C DEFINITION = The granule level flag applying both generally to the granule
C and specifically to the parameters at the granule level. When applied to a
C parameter, the flag refers to the quality of that parameter in the granule.
C
      CHARACTER*(*) MCORE_SCIENCE_QUAL_FLG_EXPL
      PARAMETER(MCORE_SCIENCE_QUAL_FLG_EXPL = 
     &  'SCIENCEQUALITYFLAGEXPLANATION')

C OBJECT = SENSORSHORTNAME
      CHARACTER*(*) MCORE_SENSOR_SHORT_NAME
      PARAMETER (MCORE_SENSOR_SHORT_NAME = 'SENSORSHORTNAME')

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C OBJECT = SensorCharacteristicName  obsolete
      CHARACTER*(*) MCORE_SENSORCHARNAME
      PARAMETER (MCORE_SENSORCHARNAME = 'SENSORCHARACTERISTICNAME')

C OBJECT = SensorCharacteristicValue obsolete
      CHARACTER*(*) MCORE_SENSORCHARVALUE
      PARAMETER (MCORE_SENSORCHARVALUE = 'SENSORCHARACTERISTICVALUE')

C OBJECT = SHORTNAME
C DEFINITION = The identifier for the data collection.
      CHARACTER*(*) MCORE_SHORT_NAME
      PARAMETER ( MCORE_SHORT_NAME = 'SHORTNAME')

C OBJECT = SIZEMBECSDATAGRANULE
C DEFINITION = The size of the data granule in megabytes.
      CHARACTER*(*) MCORE_SIZE_OF_GRANULE
      PARAMETER ( MCORE_SIZE_OF_GRANULE  = 'SIZEMBECSDATAGRANULE')

C OBJECT = SOUTHBOUNDINGCOORDINATE
C DEFINITION = Southernmost latitude of the granule spatial coverage.
      CHARACTER*(*) MCORE_SOUTH_BOUND
      PARAMETER ( MCORE_SOUTH_BOUND  = 'SOUTHBOUNDINGCOORDINATE')
      
C OBJECT = WESTBOUNDINGCOORDINATE
C DEFINITION = Westernmost longitude of the granule spatial coverage.
      CHARACTER*(*) MCORE_WEST_BOUND
      PARAMETER ( MCORE_WEST_BOUND  = 'WESTBOUNDINGCOORDINATE')

      CHARACTER*(*) MCORE_VERSIONID
      PARAMETER ( MCORE_VERSIONID  = 'VERSIONID')

!C*******************ECS product specific metadata*****************************

C  ECS product specific  metadata global attribute name
      CHARACTER*(*) MECS_ARCHIVE
      PARAMETER ( MECS_ARCHIVE  = 'ArchiveMetadata.0')

C  ECS product specific  metadata global attribute name
      CHARACTER*(*)   MECS_PRODUCT
      PARAMETER ( MECS_PRODUCT  = MECS_ARCHIVE)
C     PARAMETER ( MECS_PRODUCT  = 'ProductMetadata.0')

!C*******************E-Product-Inventory Metadata*****************************

C OBJECT = MODISPRODUCTFILENAME
C DEFINITION = The MODIS filename for this granule.
      CHARACTER*(*)   MPROD_FILENAME
      PARAMETER ( MPROD_FILENAME = 'MODISPRODUCTFILENAME')

C OBJECT = OPERATIONMODE
C DEFINITION = MODIS mode of operation.
      CHARACTER*(*)   MPROD_OPERATIONMODE
      PARAMETER ( MPROD_OPERATIONMODE  =     'OPERATIONMODE')

C OBJECT = PROCESSINGDATETIME
C DEFINITION = This field contains the date and time the process that
C created this file was started.
      CHARACTER*(*)   MPROD_PROC_DATE_TIME
      PARAMETER ( MPROD_PROC_DATE_TIME  = 'PROCESSINGDATETIME')

C OBJECT = SPSOPARAMETERS
C DEFINITION = The SPSO parameters for all data contained in this file, as
C listed in the SPSO database.
      CHARACTER*(*)   MPROD_SPSO_PARAM
      PARAMETER ( MPROD_SPSO_PARAM  = 'SPSOPARAMETERS')

!C**********E-Product-Inventory-A Metadata ***********************************

C OBJECT = GRANULENUMBER
C DEFINITION = The number of this MODIS granule.
      CHARACTER*(*)   MPROD_GRANULE_NUM
      PARAMETER ( MPROD_GRANULE_NUM  = 'GRANULENUMBER')

!C******************E-Product-Inventory-B Metadata*****************************

C OBJECT = GRIDTYPE
C DEFINITION = The type of MODLAND global grid used for L2G, L3 and L4
C products.
      CHARACTER*(*)   MPROD_GRID_TYPE
      PARAMETER ( MPROD_GRID_TYPE         = 'GRIDTYPE')

C OBJECT = HORIZONTALTILENUMBER
C DEFINITION = The horizontal tile number of this tile in the MODLAND
C integerized sinusoidal grid.
      CHARACTER*(*)   MPROD_HORIZ_TILE_NUM
      PARAMETER ( MPROD_HORIZ_TILE_NUM = 'HORIZONTALTILENUMBER')

C OBJECT = VERTICALTILENUMBER
C DEFINITION = The vertical tile number of this tile in the MODLAND
C integerized sinusoidal grid.
      CHARACTER*(*)   MPROD_VERT_TILE_NUM
      PARAMETER ( MPROD_VERT_TILE_NUM  = 'VERTICALTILENUMBER')

!C*****************E-Product-Archive Metadata*********************************
C OBJECT = ALGORITHMPACKAGEACCEPTANCEDATE
C DEFINITION = The date this algorithm package version successfully passed
C AI&T procedures and was accepted as an ECS standard algorithm.
      CHARACTER*(*)   MPROD_ALGO_PCK_ACPT_DATE
      PARAMETER ( MPROD_ALGO_PCK_ACPT_DATE = 'ALGOPACKAGEACCEPTDATE')

C OBJECT = ALGORITHMPACKAGEMATURITYCODE
C DEFINITION = This specifies the maturity of the algorithm package.
      CHARACTER*(*)   MPROD_ALGO_PACK_MAT_CODE
      PARAMETER ( MPROD_ALGO_PACK_MAT_CODE = 'ALGOPACKAGEMATURITYCODE')

C OBJECT = ALGORITHMPACKAGENAME
C DEFINITION = The name given to the complete delivered algorithm package
C ksubmitted for algorithm integration and test.
      CHARACTER*(*)   MPROD_ALGO_PACK_NAME
      PARAMETER ( MPROD_ALGO_PACK_NAME  = 'ALGORITHMPACKAGENAME')

C OBJECT = ALGORITHMPACKAGEVERSION
C DEFINITION = The version of the algorithm package.
      CHARACTER*(*)   MPROD_ALGO_PACK_VER
      PARAMETER ( MPROD_ALGO_PACK_VER = 'ALGORITHMPACKAGEVERSION')

C OBJECT = INSTRUMENTNAME
C DEFINITION = The long name by which the instrument is known.
      CHARACTER*(*)   MPROD_INSTR_NAME
      PARAMETER ( MPROD_INSTR_NAME  =  'INSTRUMENTNAME')

C OBJECT = LocalInputGranuleID
      CHARACTER*(*) MCORE_LOCALINPUTGRANULEID
      PARAMETER (MCORE_LOCALINPUTGRANULEID = 'LOCALINPUTGRANULEID')

C OBJECT = PLATFORMSHORTNAME
C DEFINITION = The short name assigned to the platform carrying the
C instrument.
      CHARACTER*(*)   MPROD_PLATFORM_SHORT_NAM
      PARAMETER ( MPROD_PLATFORM_SHORT_NAM  =  'PLATFORMSHORTNAME')

C OBJECT = PROCESSINGCENTER
C DEFINITION = DAAC where product is processed.
      CHARACTER*(*)   MPROD_PROC_CENTER
      PARAMETER ( MPROD_PROC_CENTER  = 'PROCESSINGCENTER')

!C****************** ECS product specific metadata ***************************

C  HDF standard Long Name metadata attribute name
      CHARACTER*(*) LNAME
      PARAMETER (LNAME = 'long_name')

C  HDF standard Units metadata attribute name
      CHARACTER*(*) UNITS
      PARAMETER (UNITS = 'units')

C  HDF standard Calibration factor array attribute name
      CHARACTER*(*) CAL
      PARAMETER (CAL = 'scale_factor')

C  HDF standard Uncalibrated offset array attribute name
C  DEFINITION = HDF predefined attribute; additive calibration coefficie
      CHARACTER*(*) OFFSET
      PARAMETER (OFFSET = 'add_offset')

C  C-like file access types (for OPMFIL)
      CHARACTER*(*) CREATE_FILE, APPND_FILE, RD_FILE, OVERWRITE_FILE
      PARAMETER (CREATE_FILE = 'w', APPND_FILE = 'a', RD_FILE = 'r')
      PARAMETER (OVERWRITE_FILE = 'w')

C  M-API Return Values
C  MODFILLEN  Size of modfil M-API file array structure
      INTEGER MFAIL, MAPIOK, MODFILLEN
      PARAMETER (MFAIL=-1, MAPIOK=0)
      PARAMETER (MODFILLEN = 5)


C Internal M-API constants
C       P_SDID                  modfil array location of a file's sd_id
C       P_HDFID                 modfil array location of a file's hdf file_id
C       P_ACCESS                modfil array location of a file's DFACC_*
C                               access type number
C       P_ADDR                  address of MODFILE structure returned
C                               from call to C routine openMODISfile
      INTEGER P_SDID, P_HDFID, P_ACCESS, DFACC_MFAIL, P_ADDR, P_ADDR_EXT
      PARAMETER (P_SDID = 1, P_HDFID = 2, P_ACCESS = 3, P_ADDR = 4)
      PARAMETER (P_ADDR_EXT = 5)
      PARAMETER (DFACC_MFAIL = 0)
