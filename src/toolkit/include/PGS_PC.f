!-------------------------------------------------------------------------!
!                                                                         !
!  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  !
!  and suppliers.  ALL RIGHTS RESERVED.                                   !
!                                                                         !
!-------------------------------------------------------------------------!
!-----------------------------------------------------------------------------
! BEGIN_FILE_PROLOG:
!
! FILENAME:
!
!       PGS_PC.f
!
! DESCRIPTION:
!
!       This file contains the mode values to be used with the Process
!       Control Tool functions of the PGS Toolkit FORTRAN wrappers.
!
! AUTHOR:
!       Ray Milburn / Applied Research Corporation
!
! HISTORY:
!       25-Apr-94 RM Initial version
!       31-Aug-94 RM Updated for new release
!       05-Apr-95 RM Updated for TK5.  Added new variables which allow
!                       default file locations to be stored in the PCF
!                       as opposed to environment variables.
!	18-Oct-95 RM Removed obsolete environment variable declarations
!			and parameterizations.
!       01-Jul-97 DH Fixed per ECSed07367. Added parameter definitions
!                       PGSd_PC_UREF_LENGTH_MAX and PGSd_PC_LINE_LENGTH_MAX.
!                       
!
! END_FILE_PROLOG:
!-----------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------
!   Mode values
!-----------------------------------------------------------------------------
        INTEGER PGSd_PC_PRODUCTION_RUN_ID
        PARAMETER (PGSd_PC_PRODUCTION_RUN_ID = 1)

        INTEGER PGSd_PC_SOFTWARE_ID
        PARAMETER (PGSd_PC_SOFTWARE_ID = 2)

        INTEGER PGSd_PC_CONFIGURATION
        PARAMETER (PGSd_PC_CONFIGURATION = 5000)

        INTEGER PGSd_PC_INPUT_FILE_NAME
        PARAMETER (PGSd_PC_INPUT_FILE_NAME = 5100)

        INTEGER PGSd_PC_INPUT_FILE_ATTRIBUTE
        PARAMETER (PGSd_PC_INPUT_FILE_ATTRIBUTE = 5110)

        INTEGER PGSd_PC_INPUT_FILE_NUMFILES
        PARAMETER (PGSd_PC_INPUT_FILE_NUMFILES = 5120)

        INTEGER PGSd_PC_PRODUCT_IN_DEFLOC
        PARAMETER(PGSd_PC_PRODUCT_IN_DEFLOC = 5130)

        INTEGER PGSd_PC_OUTPUT_FILE_NAME
        PARAMETER (PGSd_PC_OUTPUT_FILE_NAME = 5200)

        INTEGER PGSd_PC_OUTPUT_FILE_ATTRIBUTE
        PARAMETER (PGSd_PC_OUTPUT_FILE_ATTRIBUTE = 5210)

        INTEGER PGSd_PC_PRODUCT_OUT_DEFLOC
        PARAMETER (PGSd_PC_PRODUCT_OUT_DEFLOC = 5220)

        INTEGER PGSd_PC_TEMPORARY_FILE
        PARAMETER (PGSd_PC_TEMPORARY_FILE = 5300)

        INTEGER PGSd_PC_DELETE_TEMP
        PARAMETER (PGSd_PC_DELETE_TEMP = 5310)

        INTEGER PGSd_PC_TEMP_FILE_DEFLOC
        PARAMETER (PGSd_PC_TEMP_FILE_DEFLOC = 5320)

        INTEGER PGSd_PC_INTERMEDIATE_INPUT
        PARAMETER (PGSd_PC_INTERMEDIATE_INPUT = 5400)

        INTEGER PGSd_PC_INTER_IN_DEFLOC
        PARAMETER (PGSd_PC_INTER_IN_DEFLOC = 5410)

        INTEGER PGSd_PC_INTERMEDIATE_OUTPUT
        PARAMETER (PGSd_PC_INTERMEDIATE_OUTPUT = 5500)

        INTEGER PGSd_PC_INTER_OUT_DEFLOC
        PARAMETER (PGSd_PC_INTER_OUT_DEFLOC = 5510)

        INTEGER PGSd_PC_SUPPORT_IN_NAME
        PARAMETER (PGSd_PC_SUPPORT_IN_NAME = 5600)

        INTEGER PGSd_PC_SUPPORT_IN_ATTR
        PARAMETER (PGSd_PC_SUPPORT_IN_ATTR = 5610)

        INTEGER PGSd_PC_SUPPORT_IN_DEFLOC
        PARAMETER (PGSd_PC_SUPPORT_IN_DEFLOC = 5620)

        INTEGER PGSd_PC_SUPPORT_OUT_NAME
        PARAMETER (PGSd_PC_SUPPORT_OUT_NAME = 5700)

        INTEGER PGSd_PC_SUPPORT_OUT_ATTR
        PARAMETER (PGSd_PC_SUPPORT_OUT_ATTR = 5710)

        INTEGER PGSd_PC_SUPPORT_OUT_DEFLOC
        PARAMETER (PGSd_PC_SUPPORT_OUT_DEFLOC = 5720)

        INTEGER PGSd_PC_ATTRIBUTE_LOCATION
        PARAMETER (PGSd_PC_ATTRIBUTE_LOCATION = 1)

        INTEGER PGSd_PC_ATTRIBUTE_STRING
        PARAMETER (PGSd_PC_ATTRIBUTE_STRING = 2)

        INTEGER PGSd_PC_MATCH
        PARAMETER (PGSd_PC_MATCH = 1)

        INTEGER PGSd_PC_NO_MATCH
        PARAMETER (PGSd_PC_NO_MATCH = 2)
!
!-----------------------------------------------------------------------------
!   String lengths
!-----------------------------------------------------------------------------
        INTEGER PGSd_PC_FILE_NAME_MAX
        PARAMETER (PGSd_PC_FILE_NAME_MAX = 256)
        INTEGER PGSd_PC_PATH_LENGTH_MAX
        PARAMETER (PGSd_PC_PATH_LENGTH_MAX = 768)
        INTEGER PGSd_PC_ID_LENGTH_MAX
        PARAMETER (PGSd_PC_ID_LENGTH_MAX = 200)
        INTEGER PGSd_PC_VALUE_LENGTH_MAX
        PARAMETER (PGSd_PC_VALUE_LENGTH_MAX = 200)
        INTEGER PGSd_PC_PRODDATA_LENGTH_MAX
        PARAMETER (PGSd_PC_PRODDATA_LENGTH_MAX = 200)
        INTEGER PGSd_PC_FILE_PATH_MAX
        PARAMETER (PGSd_PC_FILE_PATH_MAX = 1024)
        INTEGER PGSd_PC_LABEL_SIZE_MAX
        PARAMETER (PGSd_PC_LABEL_SIZE_MAX = 200)
        INTEGER PGSd_PC_UREF_LENGTH_MAX
        PARAMETER (PGSd_PC_UREF_LENGTH_MAX = 255)
        INTEGER PGSd_PC_LINE_LENGTH_MAX
        PARAMETER (PGSd_PC_LINE_LENGTH_MAX = 2000)
