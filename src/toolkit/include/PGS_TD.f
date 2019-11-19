!-------------------------------------------------------------------------!
!                                                                         !
!  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  !
!  and suppliers.  ALL RIGHTS RESERVED.                                   !
!                                                                         !
!-------------------------------------------------------------------------!

!   This file defines constants used by the PGS toolkit Time and Date
!   (TD) tools.

      INTEGER PGSd_EOS_AM1
      PARAMETER(PGSd_EOS_AM1=2222)
      INTEGER PGSd_EOS_PM1
      PARAMETER(PGSd_EOS_PM1=3333)
      INTEGER PGSd_TRMM
      PARAMETER(PGSd_TRMM=4444)
      INTEGER PGSd_ADEOS_II
      PARAMETER(PGSd_ADEOS_II=5555)
      INTEGER PGSd_EOS_AURA
      PARAMETER(PGSd_EOS_AURA=6666)
      INTEGER PGSd_EOS_PM_GIRD
      PARAMETER(PGSd_EOS_PM_GIRD=7777)
      INTEGER PGSd_EOS_PM_GIIS
      PARAMETER(PGSd_EOS_PM_GIIS=8888)
! For backward compatibility

      INTEGER PGSd_EOS_AM
      PARAMETER(PGSd_EOS_AM=2222)
      INTEGER PGSd_EOS_PM
      PARAMETER(PGSd_EOS_PM=3333)

      INTEGER PGSd_GET
      PARAMETER(PGSd_GET=1)
      INTEGER PGSd_SET
      PARAMETER(PGSd_SET=-1)

      DOUBLE PRECISION PGSd_GEO_ERROR_VALUE
      PARAMETER(PGSd_GEO_ERROR_VALUE=1.0D50)

! For backward compatibility.      

      DOUBLE PRECISION PGS_GEO_ERROR_VALUE
      PARAMETER(PGS_GEO_ERROR_VALUE=1.0D50)
