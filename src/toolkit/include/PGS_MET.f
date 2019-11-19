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
!       PGS_MET.f
!
! DESCRIPTION:
!
!       This file contains all the various definitions 
!       that are required by the user
!
! AUTHOR:
!       Alward Siyyid
!       Carol S. W. Tsai / Space Applications Corporation
!       Abe Taaheri / Emergent Information Technologies, Inc.
!
! HISTORY:
!       15-Jun-95 ANS Initial version
!	18-Mar-96 ANS updated for tk5+
!       26-Nov-97 CSWT Added new ddfinition for strings "PGSd_MET_MAX_STRING_SET_L"
!                      and "PGSd_MET_MAX_ARRAY_ELEMENT_SIZE"
!       23-Jan-01 AT   Increased PGSd_MET_MAX_ARRAY_ELEMENT_SIZE to 2000
!       24-Apr-02 AT   Added new flags for opening HDF5 and HDF4 files using
!                      PGS_MET_SDstart()
!
! END_FILE_PROLOG:
!-----------------------------------------------------------------------------
!       GCT toolkit definitions
!-----------------------------------------------------------------------------
        INTEGER PGSd_MET_NAME_L
        PARAMETER (PGSd_MET_NAME_L = 100)

        INTEGER PGSd_MET_MAX_STRING_SET_L 
        PARAMETER (PGSd_MET_MAX_STRING_SET_L = 255)

        INTEGER PGSd_MET_MAX_ARRAY_ELEMENT_SIZE 
        PARAMETER (PGSd_MET_MAX_ARRAY_ELEMENT_SIZE = 2000)

        INTEGER PGSd_MET_NUM_OF_GROUPS
        PARAMETER (PGSd_MET_NUM_OF_GROUPS = 20)

        INTEGER PGSd_MET_GROUP_NAME_L
        PARAMETER (PGSd_MET_GROUP_NAME_L = 104)

        INTEGER PGSd_MET_INT_MAX
        PARAMETER (PGSd_MET_INT_MAX = 2147483647)

        DOUBLE PRECISION PGSd_MET_DBL_MAX
        PARAMETER (PGSd_MET_DBL_MAX = 1.797693D308)

        INTEGER PGSd_MET_MCF_FILE
        PARAMETER (PGSd_MET_MCF_FILE = 10250)

        CHARACTER PGSd_MET_STR_END*1
        PARAMETER (PGSd_MET_STR_END = '$')

        INTEGER HDF4_ACC_RDONLY
        PARAMETER (HDF4_ACC_RDONLY = 1)

        INTEGER HDF4_ACC_RDWR
        PARAMETER (HDF4_ACC_RDWR = 3)

        INTEGER HDF4_ACC_CREATE
        PARAMETER (HDF4_ACC_CREATE = 4)

        INTEGER HDF5_ACC_RDONLY
        PARAMETER (HDF5_ACC_RDONLY = 11)

        INTEGER  HDF5_ACC_RDWR
        PARAMETER (HDF5_ACC_RDWR = 13)

        INTEGER HDF5_ACC_CREATE
        PARAMETER (HDF5_ACC_CREATE = 14)

        INTEGER  HE5F_ACC_RDWR
        PARAMETER (HE5F_ACC_RDWR   = 100)

        INTEGER  HE5F_ACC_RDONLY
        PARAMETER (HE5F_ACC_RDONLY = 101)

        INTEGER  HE5F_ACC_TRUNC
        PARAMETER (HE5F_ACC_TRUNC  = 102)
