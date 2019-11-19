!----------------------------------------------------------------------------
!               mapic.inc utilities header file
!
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!  !F77-INC
!
!
!  !Purpose:   Utilities header file used by all subroutines contained in the
!              MODIS Applications Programming Interface (M-API).
!
!  !Description: The Header file mapic.inc is part of a larger software
!                system called the MODIS Applications Programming Interface (M-API)
!                utility.  The M-API Utility consists of
!                subroutines which allow MODIS Science Team-supplied software
!                to read in Level 1B radiance bands and write out output
!                products and metadata to HDF files.  The functionality of the
!                M-API is defined in the MODIS API User's Guide.
! 
!
!  !Revision History:
!   $Log: mapic.inc,v $
! Revision 1.13  2001/01/24  21:18:18  pliu
! Fixed comment errors in prlog. Use "!".
!
! Revision 1.12  1999/12/20  20:57:16  solanki
! fixed comments errors in prolog. # sign being mistaken for a
! preprocessing directive. Replaced by '!'.
!
! Revision 1.11  1999/12/14  23:34:06  solanki
! Modified for f90 compatibility.
!
! Revision 1.10  1998/04/08  19:20:06  fshaw
! reverted to previous version
!
! Revision 1.4  1997/12/26  15:16:32  fshaw
! updated for v2.3 release
!
!
!  !Team-unique header:
!
!     This software is developed by the MODIS Science Data Support
!     Team for the National Aeronautics and Space Administration,
!     Goddard Space Flight Center, under contract NAS5-32373.
!
!  !References and credits
!
!     Portions developed at the National Center for Supercomputing
!     Applications at the Univ. of Illinois at Urbana-Champaign.
!
!  !Design notes:
!
!     none.
!
!  !END
!------------------------------------------------------------------------------

      INCLUDE 'hdf.inc'
      INCLUDE 'netcdf.inc'
      INCLUDE 'dffunc.inc'
      INCLUDE 'PGS_SMF.f'
      INCLUDE 'PGS_MAPI_39602.f'
      INCLUDE 'mapi.inc'

!  Declarations of return values for internal M-API functions

      INTEGER DT2DF, PSTMD, SDSOK, DF2DT

!  Internal M-API Constants
!       MAX_REC                 Maximum Vdata record size
!       MAPI_SMF_MAX_MSGBUF_SIZE maximum length of untruncated SMF message
      INTEGER ADMGRP
      INTEGER MAX_REC, MAPI_SMF_MAX_MSGBUF_SIZE
      INTEGER    NO_OBJECT
      PARAMETER (NO_OBJECT = -2)
      PARAMETER (MAX_REC = 32767)
      PARAMETER (MAPI_SMF_MAX_MSGBUF_SIZE = 481)
