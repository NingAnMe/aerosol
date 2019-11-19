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
!       PGS_DEM.f
!
! DESCRIPTION:
!
!       This file contains the mode values to be used with the Digital
!       Elevation Model Tool functions of the PGS Toolkit FORTRAN wrappers
!
! AUTHOR:
!       Alexis Zubrow / Applied Research Corporation
!
! HISTORY:
!       17-Apr-97 AZ Initial version
!       10-Sep-97 Abe Taaheri added land/sea mask identifirers
!       5-June-00 Abe Taaheri added tags for 3km resolution
! END_FILE_PROLOG:
!-----------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------
!   Mode values
!-----------------------------------------------------------------------------
      integer PGSd_DEM_3ARC
      parameter (PGSd_DEM_3ARC=1)
      
      integer PGSd_DEM_30TEST
      parameter (PGSd_DEM_30TEST=3)
      
      integer PGSd_DEM_30ARC
      parameter (PGSd_DEM_30ARC=2)

      integer PGSd_DEM_90ARC
      parameter (PGSd_DEM_90ARC=4)
      
      integer PGSd_DEM_DEGREE
      parameter (PGSd_DEM_DEGREE=22)

      integer PGSd_DEM_PIXEL
      parameter (PGSd_DEM_PIXEL=11)

      integer PGSd_DEM_ELEV
      parameter (PGSd_DEM_ELEV=1)

      integer PGSd_DEM_STDEV_ELEV
      parameter (PGSd_DEM_STDEV_ELEV=10)

      integer PGSd_DEM_SLOPE
      parameter (PGSd_DEM_SLOPE=100)

      integer PGSd_DEM_STDEV_SLOPE
      parameter (PGSd_DEM_STDEV_SLOPE=1000)

      integer PGSd_DEM_ASPECT
      parameter (PGSd_DEM_ASPECT=10000)

      integer PGSd_DEM_TOP_OBSC
      parameter (PGSd_DEM_TOP_OBSC=100000)

      integer PGSd_DEM_TOP_SHAD
      parameter (PGSd_DEM_TOP_SHAD=1000000)

      integer PGSd_DEM_WATER_LAND
      parameter (PGSd_DEM_water_land=10000000)

      integer PGSd_DEM_SOURCE
      parameter (PGSd_DEM_SOURCE=5)

      integer PGSd_DEM_METHOD
      parameter (PGSd_DEM_METHOD=15)

      integer PGSd_DEM_GEOID
      parameter (PGSd_DEM_GEOID=25)
      
      integer PGSd_DEM_VERTICAL_ACCURACY
      parameter (PGSd_DEM_VERTICAL_ACCURACY=35)
      
      integer PGSd_DEM_HORIZONTAL_ACCURACY
      parameter (PGSd_DEM_HORIZONTAL_ACCURACY=45)

      integer PGSd_DEM_NEAREST_NEIGHBOR
      parameter (PGSd_DEM_NEAREST_NEIGHBOR=1)

      integer PGSd_DEM_BILINEAR
      parameter (PGSd_DEM_BILINEAR=2)

      integer PGSd_DEM_NO_COMPLETE_DATA
      parameter (PGSd_DEM_NO_COMPLETE_DATA=-3)

      integer PGS_DEM_SHALLOW_OCEAN
      parameter (PGS_DEM_SHALLOW_OCEAN=0)
      
      integer PGS_DEM_LAND
      parameter (PGS_DEM_LAND=1)
      
      integer PGS_DEM_SHORELINE
      parameter (PGS_DEM_SHORELINE=2)
      
      integer PGS_DEM_SHALLOW_INLAND_WATER
      parameter (PGS_DEM_SHALLOW_INLAND_WATER=3)
      
      integer PGS_DEM_EPHEMERAL_WATER
      parameter (PGS_DEM_EPHEMERAL_WATER=4)
      
      integer PGS_DEM_DEEP_INLAND_WATER
      parameter (PGS_DEM_DEEP_INLAND_WATER=5)
      
      integer PGS_DEM_MODERATE_OCEAN
      parameter (PGS_DEM_MODERATE_OCEAN=6)
      
      integer PGS_DEM_DEEP_OCEAN
      parameter (PGS_DEM_DEEP_OCEAN=7)
      
      integer PGSd_DEM_NO_FILLVALUE
      parameter (PGSd_DEM_NO_FILLVALUE=-8888)
      
      integer PGSd_DEM_FILLVALUE
      parameter (PGSd_DEM_FILLVALUE=-9999)
