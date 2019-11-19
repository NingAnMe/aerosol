C-----------------------------------------------------------------------
C!F77-INC
C
C!DESCRIPTION: This routine contains most COMMON statements used in 
C              other routines.
C
c!Input Parameters:  N/A
c!Output Parameters: N/A
C
C!REVISION HISTORY:
C $Log: COMMONS.inc,v $
C Revision 1.3  2004/07/15 22:33:08  kis
C New routines as I get closer to adding MOD04 DAAC subroutines.
C
c 01/28/98 fhliang
c fixed prolog.
c
C Revision 1.3  1996/11/01  19:27:01  jguu
C Baselined for Version 1.
C
C Revision 1.3  1996/11/01  19:27:01  jguu
C *** empty log message ***
C
C Revision 1.2  1996/08/30  15:03:05  jguu
C The original file name for this file is COMMONS_INC.f.
C
c Revision 1.1  1996/07/08  18:32:08  vlin
c Initial revision
c
c Revision 1.4  1995/11/01  15:24:38  vlin
c Before code walkthrough
c
c Revision 1.3  1995/10/18  17:22:04  vlin
c New delivery from CSF
c
C Revision 1.2  1995/04/20  21:30:00  qhuang
C Added prolog and data type declarations for all variables. The
C code now complies with ECS's and most of MODIS's software
C standards.
C
c!TEAM-UNIQUE HEADER:
c
c Developed by MODIS Aerosol/Solar Water Vapor Retrieval Team
c GSFC, Greenbelt, MD
c
C REFERENCES AND CREDITS
C    First version created by Dr. Bo-Cai Gao  
C    (gao@imagecube.gsfc.nasa.gov)
C                             1994 10 28
C
C DESIGN NOTES: most data and variables are passed with common 
C               statements in this routine.
C
c!END
C-----------------------------------------------------------------------

C--- NUMBER of 1km bands
      integer INBAND
      parameter( INBAND = 36 )

c ... Number of bytes per 1km pixel for cloud mask result
      integer nmask
      parameter( nmask = 6 )
  
c ... Number of bytes per 1km pixel for cloud mask result
      integer nmask_qa
      parameter( nmask_qa = 6 )

c ... Number of parmeters in the MOD07 file
      integer nbands_mod07
      parameter( nbands_mod07 = 83 )

C---Center wavelengths of MODIS channels near 0.86, 0.905, 0.935, 0.94
C      and 1.24 micron.

      REAL    WV_CNTR_0P86, WV_CNTR_0P905, WV_CNTR_0P935,
     &        WV_CNTR_0P94, WV_CNTR_1P24

C---Assign center wavelengths (micron) of 5 MODIS near-IR channels. These
C    wavelengths were obtained from weighted filter-bandpass averages.
C    The bandpasses are not final yet. May still need modifications 
C    in the future based Hughes Santa Barbara Research's updated filter
C    transmittance data

      PARAMETER ( WV_CNTR_0P86  = 0.855349 )
      PARAMETER ( WV_CNTR_0P905 = 0.904037 )
      PARAMETER ( WV_CNTR_0P935 = 0.935409 )
      PARAMETER ( WV_CNTR_0P94  = 0.935397 )
      PARAMETER ( WV_CNTR_1P24  = 1.24270  )
C
C---Mean solar irradiance values over the MODIS band passes

      REAL    SOL_IRR_0P86, SOL_IRR_0P905, SOL_IRR_0P935,
     &        SOL_IRR_0P94, SOL_IRR_1P24

      COMMON /SOLAR_IRR/  SOL_IRR_0P86, SOL_IRR_0P905, SOL_IRR_0P935,
     &                    SOL_IRR_0P94, SOL_IRR_1P24
C
C---Weighting factors for calculating 3-channel ratios

      REAL    WT_0P905_L, WT_0P905_R, WT_0P935_L, 
     &        WT_0P935_R, WT_0P94_L,  WT_0P94_R
          
      COMMON /WT_3CHNL_RATIOS/ WT_0P905_L, WT_0P905_R, WT_0P935_L, 
     &                         WT_0P935_R, WT_0P94_L,  WT_0P94_R
C
C---Transmittances and total vapor amounts (cm) for six LOWTRAN7 standard
C        model atmospheres (Tropical, Mid-summer, Mid_winter,
C        Sub-arctic summer, Sub-arctic winter, US STD 1976). N_GH2O is 
C        the number of water vapor values for transmittance calculations.
C
      INTEGER    N_GH2O
      PARAMETER (N_GH2O = 220)


      REAL      VAPTOT_1,  T_0P86_1, T_0P905_1,
     &          T_0P935_1, T_0P94_1, T_1P24_1

      DIMENSION VAPTOT_1(N_GH2O),  T_0P86_1(N_GH2O), T_0P905_1(N_GH2O),
     &          T_0P935_1(N_GH2O), T_0P94_1(N_GH2O), T_1P24_1(N_GH2O)

      REAL      VAPTOT_2,  T_0P86_2, T_0P905_2,
     &          T_0P935_2, T_0P94_2, T_1P24_2
   
      DIMENSION VAPTOT_2(N_GH2O),  T_0P86_2(N_GH2O), T_0P905_2(N_GH2O),
     &          T_0P935_2(N_GH2O), T_0P94_2(N_GH2O), T_1P24_2(N_GH2O)

      REAL      VAPTOT_3,  T_0P86_3, T_0P905_3,
     &          T_0P935_3, T_0P94_3, T_1P24_3

      DIMENSION VAPTOT_3(N_GH2O),  T_0P86_3(N_GH2O), T_0P905_3(N_GH2O),
     &          T_0P935_3(N_GH2O), T_0P94_3(N_GH2O), T_1P24_3(N_GH2O)

      REAL      VAPTOT_4,  T_0P86_4, T_0P905_4,
     &          T_0P935_4, T_0P94_4, T_1P24_4

      DIMENSION VAPTOT_4(N_GH2O),  T_0P86_4(N_GH2O), T_0P905_4(N_GH2O),
     &          T_0P935_4(N_GH2O), T_0P94_4(N_GH2O), T_1P24_4(N_GH2O)

      REAL      VAPTOT_5,  T_0P86_5, T_0P905_5,
     &          T_0P935_5, T_0P94_5, T_1P24_5

      DIMENSION VAPTOT_5(N_GH2O),  T_0P86_5(N_GH2O), T_0P905_5(N_GH2O),
     &          T_0P935_5(N_GH2O), T_0P94_5(N_GH2O), T_1P24_5(N_GH2O)

      REAL      VAPTOT_6,  T_0P86_6, T_0P905_6,
     &          T_0P935_6, T_0P94_6, T_1P24_6

      DIMENSION VAPTOT_6(N_GH2O),  T_0P86_6(N_GH2O), T_0P905_6(N_GH2O),
     &          T_0P935_6(N_GH2O), T_0P94_6(N_GH2O), T_1P24_6(N_GH2O)

C
C--- 3-channel ratios (e.g., T(0.94) / {WT_0P94_L * T(0.86) +
C      WT_0P94_R * T(1.24)} ) and 2-channel ratios (e.g., T(0.94)/T(0.86))
C      for six LOWTRAN7 standard atmospheric models.
C
      REAL      R_3CH_0P905_MDL1, R_2CH_0P905_MDL1,
     &          R_3CH_0P935_MDL1, R_2CH_0P935_MDL1,
     &          R_3CH_0P94_MDL1,  R_2CH_0P94_MDL1

      DIMENSION R_3CH_0P905_MDL1(N_GH2O), R_2CH_0P905_MDL1(N_GH2O),
     &          R_3CH_0P935_MDL1(N_GH2O), R_2CH_0P935_MDL1(N_GH2O),
     &          R_3CH_0P94_MDL1(N_GH2O),  R_2CH_0P94_MDL1(N_GH2O)
      COMMON /RATIOS_MDL1/ R_3CH_0P905_MDL1, R_2CH_0P905_MDL1,
     &                     R_3CH_0P935_MDL1, R_2CH_0P935_MDL1,
     &                     R_3CH_0P94_MDL1,  R_2CH_0P94_MDL1,
     &                     VAPTOT_1

      REAL      R_3CH_0P905_MDL2, R_2CH_0P905_MDL2,
     &          R_3CH_0P935_MDL2, R_2CH_0P935_MDL2,
     &          R_3CH_0P94_MDL2,  R_2CH_0P94_MDL2

      DIMENSION R_3CH_0P905_MDL2(N_GH2O), R_2CH_0P905_MDL2(N_GH2O),
     &          R_3CH_0P935_MDL2(N_GH2O), R_2CH_0P935_MDL2(N_GH2O),
     &          R_3CH_0P94_MDL2(N_GH2O),  R_2CH_0P94_MDL2(N_GH2O)
      COMMON /RATIOS_MDL2/ R_3CH_0P905_MDL2, R_2CH_0P905_MDL2,
     &                     R_3CH_0P935_MDL2, R_2CH_0P935_MDL2,
     &                     R_3CH_0P94_MDL2,  R_2CH_0P94_MDL2,
     &                     VAPTOT_2

      REAL      R_3CH_0P905_MDL3, R_2CH_0P905_MDL3,
     &          R_3CH_0P935_MDL3, R_2CH_0P935_MDL3,
     &          R_3CH_0P94_MDL3,  R_2CH_0P94_MDL3

      DIMENSION R_3CH_0P905_MDL3(N_GH2O), R_2CH_0P905_MDL3(N_GH2O),
     &          R_3CH_0P935_MDL3(N_GH2O), R_2CH_0P935_MDL3(N_GH2O),
     &          R_3CH_0P94_MDL3(N_GH2O),  R_2CH_0P94_MDL3(N_GH2O)
      COMMON /RATIOS_MDL3/ R_3CH_0P905_MDL3, R_2CH_0P905_MDL3,
     &                     R_3CH_0P935_MDL3, R_2CH_0P935_MDL3,
     &                     R_3CH_0P94_MDL3,  R_2CH_0P94_MDL3,
     &                     VAPTOT_3

      REAL      R_3CH_0P905_MDL4, R_2CH_0P905_MDL4,
     &          R_3CH_0P935_MDL4, R_2CH_0P935_MDL4,
     &          R_3CH_0P94_MDL4,  R_2CH_0P94_MDL4

      DIMENSION R_3CH_0P905_MDL4(N_GH2O), R_2CH_0P905_MDL4(N_GH2O),
     &          R_3CH_0P935_MDL4(N_GH2O), R_2CH_0P935_MDL4(N_GH2O),
     &          R_3CH_0P94_MDL4(N_GH2O),  R_2CH_0P94_MDL4(N_GH2O)
      COMMON /RATIOS_MDL4/ R_3CH_0P905_MDL4, R_2CH_0P905_MDL4,
     &                     R_3CH_0P935_MDL4, R_2CH_0P935_MDL4,
     &                     R_3CH_0P94_MDL4,  R_2CH_0P94_MDL4,
     &                     VAPTOT_4

      REAL      R_3CH_0P905_MDL5, R_2CH_0P905_MDL5,
     &          R_3CH_0P935_MDL5, R_2CH_0P935_MDL5,
     &          R_3CH_0P94_MDL5,  R_2CH_0P94_MDL5
    
      DIMENSION R_3CH_0P905_MDL5(N_GH2O), R_2CH_0P905_MDL5(N_GH2O),
     &          R_3CH_0P935_MDL5(N_GH2O), R_2CH_0P935_MDL5(N_GH2O),
     &          R_3CH_0P94_MDL5(N_GH2O),  R_2CH_0P94_MDL5(N_GH2O)
      COMMON /RATIOS_MDL5/ R_3CH_0P905_MDL5, R_2CH_0P905_MDL5,
     &                     R_3CH_0P935_MDL5, R_2CH_0P935_MDL5,
     &                     R_3CH_0P94_MDL5,  R_2CH_0P94_MDL5,
     &                     VAPTOT_5

      REAL      R_3CH_0P905_MDL6, R_2CH_0P905_MDL6,
     &          R_3CH_0P935_MDL6, R_2CH_0P935_MDL6,
     &          R_3CH_0P94_MDL6,  R_2CH_0P94_MDL6
   
      DIMENSION R_3CH_0P905_MDL6(N_GH2O), R_2CH_0P905_MDL6(N_GH2O),
     &          R_3CH_0P935_MDL6(N_GH2O), R_2CH_0P935_MDL6(N_GH2O),
     &          R_3CH_0P94_MDL6(N_GH2O),  R_2CH_0P94_MDL6(N_GH2O)
      COMMON /RATIOS_MDL6/ R_3CH_0P905_MDL6, R_2CH_0P905_MDL6,
     &                     R_3CH_0P935_MDL6, R_2CH_0P935_MDL6,
     &                     R_3CH_0P94_MDL6,  R_2CH_0P94_MDL6,
     &                     VAPTOT_6
C
C
      INTEGER  Max_Frames_Per_Line, No_Lines_Per_Scan
      PARAMETER (Max_Frames_Per_Line=1500, No_Lines_Per_Scan=10)
C
C---RADIANCES ((watt /(m^2 micron sr)) of MODIS channels centered near
C      0.86, 0.905, 0.935, 0.94, and 1.24 micron.
C
      REAL      RAD_0P86, RAD_0P905, RAD_0P935,
     &          RAD_0P94, RAD_1P24
  
      DIMENSION RAD_0P86  (Max_Frames_Per_Line, No_Lines_Per_Scan),
     2          RAD_0P905 (Max_Frames_Per_Line, No_Lines_Per_Scan),
     3          RAD_0P935 (Max_Frames_Per_Line, No_Lines_Per_Scan),
     4          RAD_0P94  (Max_Frames_Per_Line, No_Lines_Per_Scan), 
     5          RAD_1P24  (Max_Frames_Per_Line, No_Lines_Per_Scan)
      COMMON /MODIS_RAD/ RAD_0P86, RAD_0P905, RAD_0P935, RAD_0P94,
     &                   RAD_1P24
C
C---Ancillary data from MODIS cube, including solar zenith angle (degree),
C      view zenith angle, and surface elevation (km).
C
      REAL      ZENITH_SOL, ZENITH_VIEW, AZIMUTH_REL
  
      DIMENSION ZENITH_SOL  (Max_Frames_Per_Line, No_Lines_Per_Scan),
     2          ZENITH_VIEW (Max_Frames_Per_Line, No_Lines_Per_Scan), 
     3          AZIMUTH_REL (Max_Frames_Per_Line, No_Lines_Per_Scan)
      COMMON /ANCILLARY_1/ ZENITH_SOL, ZENITH_VIEW, AZIMUTH_REL
C
C---Geometrical factor for converting slant path vapor amount to
C      vertical column vapor amount
C
      REAL GEOM_FACTOR
C
C---Ancillary data - pixel indicators, land_water (I_LAND_WATER),
C      and cloud (I_CLOUD).
C
      INTEGER   I_LAND_WATER, I_CLOUD, I_GLINT
   
      DIMENSION 
     2   I_LAND_WATER (Max_Frames_Per_Line, No_Lines_Per_Scan),
     3   I_CLOUD      (Max_Frames_Per_Line, No_Lines_Per_Scan),
     4   I_GLINT      (Max_Frames_Per_Line, No_Lines_Per_Scan)
      COMMON /ANCILLARY_2/ I_LAND_WATER, I_CLOUD, I_GLINT

C---Additional ancillary data - surface elevation (m) and 
C              surface temperature (K)
      REAL      SURF_ELEV, SURF_T

      DIMENSION 
     2          SURF_ELEV   (Max_Frames_Per_Line, No_Lines_Per_Scan),
     3          SURF_T      (Max_Frames_Per_Line, No_Lines_Per_Scan)
      COMMON /ANCILLARY_3/ SURF_ELEV, SURF_T
C
C---MODIS apparent reflectances ( pi * Rad / (cos(ZENITH_SOL) * SOL_IRR) )
C
      REAL      REFL_0P86, REFL_0P905, REFL_0P935,
     &          REFL_0P94, REFL_1P24
     
      DIMENSION REFL_0P86  (Max_Frames_Per_Line, No_Lines_Per_Scan),
     2          REFL_0P905 (Max_Frames_Per_Line, No_Lines_Per_Scan), 
     3          REFL_0P935 (Max_Frames_Per_Line, No_Lines_Per_Scan),
     4          REFL_0P94  (Max_Frames_Per_Line, No_Lines_Per_Scan), 
     5          REFL_1P24  (Max_Frames_Per_Line, No_Lines_Per_Scan)
      COMMON /MODIS_REFL/ REFL_0P86, REFL_0P905, REFL_0P935, REFL_0P94, 
     &                    REFL_1P24
C
C--- 3-channel ratios (e.g., REFL(0.94) / {WT_0P94_L * REFL(0.86) +
C      WT_0P94_R * REFL(1.24)} ) and 2-channel ratios (e.g., 
C      REFL(0.94)/REFL(0.86)) calculated from MODIS apparent reflectances.

      REAL      R_3CH_0P905_MODIS, R_2CH_0P905_MODIS,
     &          R_3CH_0P935_MODIS, R_2CH_0P935_MODIS,
     &          R_3CH_0P94_MODIS,  R_2CH_0P94_MODIS

      DIMENSION 
     2   R_3CH_0P905_MODIS (Max_Frames_Per_Line, No_Lines_Per_Scan), 
     3   R_2CH_0P905_MODIS (Max_Frames_Per_Line, No_Lines_Per_Scan),
     4   R_3CH_0P935_MODIS (Max_Frames_Per_Line, No_Lines_Per_Scan), 
     5   R_2CH_0P935_MODIS (Max_Frames_Per_Line, No_Lines_Per_Scan),
     6   R_3CH_0P94_MODIS  (Max_Frames_Per_Line, No_Lines_Per_Scan),  
     7   R_2CH_0P94_MODIS  (Max_Frames_Per_Line, No_Lines_Per_Scan)
      COMMON /RATIOS_MODIS/ R_3CH_0P905_MODIS, R_2CH_0P905_MODIS,
     &                      R_3CH_0P935_MODIS, R_2CH_0P935_MODIS,
     &                      R_3CH_0P94_MODIS,  R_2CH_0P94_MODIS
C
C--- Weighting factor for performing weighted averages of water vapor values
C      from several channels.
C
      REAL WT_TOTAL, WT_VAP_0P905, WT_VAP_0P935, WT_VAP_0P94

      DIMENSION WT_VAP_0P905(N_GH2O), WT_VAP_0P935(N_GH2O), 
     &          WT_VAP_0P94(N_GH2O)
      COMMON /WEIGHT_VAPOR/WT_TOTAL,WT_VAP_0P905, WT_VAP_0P935,
     &          WT_VAP_0P94
C
C---Retrieved column water vapor amounts (cm) (VAPVRT = vapor vertical)
C
      REAL     VAPVRT     
   
      DIMENSION VAPVRT(Max_Frames_Per_Line, No_Lines_Per_Scan)
      COMMON /OUTPUT_VAPOR/ VAPVRT
C
C---A parameter for selecting sub-arctic winter model with 
C     temperature inversion during our water vapor retrievals
C
      REAL T_INV    
      PARAMETER (T_INV = 257.)
C
C---A parameter for flagging BAD or MISSING data
C
      REAL BAD_FLAG
      PARAMETER (BAD_FLAG = -9999.)
C
C---Parameters for assigning fill values to output HDF file
C
      REAL VAPVRT_MAX, VAPVRT_MIN, VAP_FILL_FLOAT
      PARAMETER ( VAPVRT_MAX = 30., VAPVRT_MIN = 0.000001 )
      PARAMETER ( VAP_FILL_FLOAT = - 0.099 )
C
C---Approximate scale height of the earth's atmosphere (roughly 8 km = 8000 m),
C     scaling exponent for water vapor in LOWTRAN code (0.9), and scale factor
C     ( = (exp(-surf_elev/scale_height))**scale_exponent)
C
      REAL SCALE_HEIGHT, SCALE_EXPONENT, SCALE_FACTOR
      PARAMETER ( SCALE_HEIGHT = 8000., SCALE_EXPONENT = 0.9 )
C
C---Logical variables for dealing with bad detectors, and for selecting
C     subset of VAP_0P905, VAP_0P935, and VAP_0P94 in final averaging
C     to get column water vapor values (VAPVRT) for each of 1-km pixels. 
C      
      LOGICAL L_VAP_0P905,  L_VAP_0P935,  L_VAP_0P94
