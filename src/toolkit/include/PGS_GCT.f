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
!       PGS_GCT.f
!
! DESCRIPTION:
!
!       This file contains all the various definitions (freeware and GCT tool)
!       that are required by the user
!
! AUTHOR:
!       Alward Siyyid
!
! HISTORY:
!       02-01-95 ANS Initial version
!       02-08-95 ANS Fixed Bug 
!       02-08-95 ANS Fixed Bug ECSed00621 added definition of PI
!       27-Jun-95 ANS Fixed bug ECSed00959 by replacing PGS_ with PGSd_
!       21-July-95 ANS Introduced PGSd_GCT_IN_ERROR (ECSed00700)
!       23-Oct-00  AT  Updated for ISINUS projection, so that both codes
!                      31 and 99 can be used for this projection. 
!
! END_FILE_PROLOG:
!-----------------------------------------------------------------------------
!       GCT toolkit definitions
!-----------------------------------------------------------------------------
        DOUBLE PRECISION PGSd_GCT_IN_ERROR
        PARAMETER (PGSd_GCT_IN_ERROR = 1.0D51)

        INTEGER PGSd_GCT_FORWARD
        PARAMETER (PGSd_GCT_FORWARD = 0)

        INTEGER PGSd_GCT_INVERSE
        PARAMETER (PGSd_GCT_INVERSE = 1)

        DOUBLE PRECISION PGSd_GCT_IN_BREAK
        PARAMETER (PGSd_GCT_IN_BREAK = 1.0D50)

        INTEGER PGSd_GCT_NAD27
        PARAMETER (PGSd_GCT_NAD27 = 10200)

        INTEGER PGSd_GCT_NAD83
        PARAMETER (PGSd_GCT_NAD83 = 10201)

!-----------------------------------------------------------------------------
!       GCTPc definitions of proj.h
!-----------------------------------------------------------------------------
        INTEGER PGSd_UTM
        PARAMETER (PGSd_UTM = 1)

        INTEGER PGSd_SPCS
        PARAMETER (PGSd_SPCS = 2)

        INTEGER PGSd_ALBERS
        PARAMETER (PGSd_ALBERS = 3)

        INTEGER PGSd_LAMCC
        PARAMETER (PGSd_LAMCC = 4)

        INTEGER PGSd_MERCAT
        PARAMETER (PGSd_MERCAT = 5)

        INTEGER PGSd_PS
        PARAMETER (PGSd_PS= 6)

        INTEGER PGSd_POLYC
        PARAMETER (PGSd_POLYC = 7)

        INTEGER PGSd_EQUIDC
        PARAMETER (PGSd_EQUIDC = 8)

        INTEGER PGSd_TM
        PARAMETER (PGSd_TM = 9)

        INTEGER PGSd_STEREO
        PARAMETER (PGSd_STEREO = 10)

        INTEGER PGSd_LAMAZ
        PARAMETER (PGSd_LAMAZ = 11)

        INTEGER PGSd_AZMEQD
        PARAMETER (PGSd_AZMEQD = 12)

        INTEGER PGSd_GNOMON
        PARAMETER (PGSd_GNOMON = 13)

        INTEGER PGSd_ORTHO
        PARAMETER (PGSd_ORTHO = 14)

        INTEGER PGSd_GVNSP
        PARAMETER (PGSd_GVNSP = 15)

        INTEGER PGSd_SNSOID
        PARAMETER (PGSd_SNSOID = 16)

        INTEGER PGSd_EQRECT
        PARAMETER (PGSd_EQRECT = 17)

        INTEGER PGSd_MILLER
        PARAMETER (PGSd_MILLER = 18)

        INTEGER PGSd_VGRINT
        PARAMETER (PGSd_VGRINT = 19)

        INTEGER PGSd_HOM
        PARAMETER (PGSd_HOM = 20)

        INTEGER PGSd_ROBIN
        PARAMETER (PGSd_ROBIN = 21)

        INTEGER PGSd_SOM
        PARAMETER (PGSd_SOM = 22)

        INTEGER PGSd_ALASKA
        PARAMETER (PGSd_ALASKA = 23)

        INTEGER PGSd_GOOD
        PARAMETER (PGSd_GOOD = 24)

        INTEGER PGSd_MOLL
        PARAMETER (PGSd_MOLL = 25)

        INTEGER PGSd_IMOLL
        PARAMETER (PGSd_IMOLL = 26)

        INTEGER PGSd_HAMMER
        PARAMETER (PGSd_HAMMER = 27)

        INTEGER PGSd_WAGIV
        PARAMETER (PGSd_WAGIV = 28)

        INTEGER PGSd_WAGVII
        PARAMETER (PGSd_WAGVII = 29)

        INTEGER PGSd_OBEQA
        PARAMETER (PGSd_OBEQA = 30)

        INTEGER PGSd_ISINUS1
        PARAMETER (PGSd_ISINUS1 = 31)

        INTEGER PGSd_CEA
        PARAMETER (PGSd_CEA = 97)

        INTEGER PGSd_BCEA
        PARAMETER (PGSd_BCEA = 98)

        INTEGER PGSd_ISINUS
        PARAMETER (PGSd_ISINUS = 99)

        INTEGER PGSd_CLARK66
        PARAMETER (PGSd_CLARK66 = 0)

        INTEGER PGSd_GRS80_WGS84
        PARAMETER (PGSd_GRS80_WGS84 = 8)

