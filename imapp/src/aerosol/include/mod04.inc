c----------------------------------------------------------------------
C!F77-INC
C
C!DESCRIPTION:
C                This is an include file for the declaration of
C                variables used to write the Vdata and SDS arrays for
C                mod04 sea algorithm.
C
c!Input Parameters:  N/A
c!Output Parameters: N/A
c
C!REVISION HISTORY:
C $Log: mod04.inc,v $
c 01/28/98 fhliang
c fixed prolog.
c
c Revision 1.2  1996/06/25  23:31:04  rhucek
c Added PARAMETER statements FV_GEO, FV_L1B, and a complete set
c of FV_* PARAMETERS to hold _FillValue of geolocation parameter
c data.
c
C!TEAM-UNIQUE HEADER:
C
C    This software is developed by the MODIS Science Data Support
C    Team for the National Aeronautics and Space Administration,
C    Goddard Space Flight Center, under contract NAS5-32373.
C
C!REFERENCES and CREDITS:
C
C    Written by:
C       Vicky Lin                   11/30/95
C       Research and Data systems Corporation
C       SAIC/GSC MODIS Science Data Support Office
C       7501 Forbes Blvd, Seabrook MD 20706
C       vlin@ltpmail.gsfc.nasa.gov
C
C    Modified by:
C       Dr. Allen Chu               5/15/97
C       Code 913/SSAI
C       NASA Goddard Space Flight Center
C       Greenbelt, MD 20771
C       achu@dustdevil.gsfc.nasa.gov
C
C!DESIGN NOTES:
C
C    Internal Variables:
C         SDS*_N     nth SDS array name
C         DTYPE*     nth SDS data type
C         SCALE*     nth SDS's scale factor
C         OFFSET*    nth SDS's offset
C         dim_*      dimension names of the SDSs
C
C !END
c
C-----------------------------------------------------------------------


C Define parameters From Main

C      CHARACTER*3 modis_flag
C      PARAMETER  (modis_flag='YES')
      REAL PI,DTR,RTD
      PARAMETER (PI = 3.1415927, DTR = PI/180.0,RTD=180./PI)
      INTEGER Index_wave_550,Index_wave_865
      PARAMETER(Index_wave_550=2,Index_wave_865=4)
      INTEGER NWAVN,NTAUN,NUMCASES,NUMCASEB,IGRIDX,IGRIDY,
     *NUM_solutions,num_model_index
C      PARAMETER(NWAVN=7,NTAUN=6,NUMCASEB=6,NUMCASES=5,
      PARAMETER(NWAVN=7,NTAUN=6,NUMCASEB=5,NUMCASES=4,
     *          IGRIDX=10,IGRIDY=10,NUM_solutions=2,
     *          num_model_index=NUMCASES+NUMCASEB)
      REAL REFW470,REFW550,REFW659,REFW865,REFW124
      REAL REFW164,REFW213,ASUM,ref_allwav(NWAVN)
      REAL ALXWAV,TAUX55,AN,TAU_COMPUTED,TAU_SMALL_COMPUTED
      REAL TAU_BIG_COMPUTED
      REAL TAUAL,TAUX55SMALL,TAUX55BIG,REFSMALLL,REFBIGL
      INTEGER QCONTROL,QCONTROL1,Good_pixels,Quality_dust_flag_off_glint,
     *Quality_dust_flag_glint, Qcontrol_cirrus

      COMMON/ONE/REFSMALLL(NWAVN,NTAUN),REFBIGL(NWAVN,NTAUN),
     *TAUAL(NTAUN),TAUX55SMALL,TAUX55BIG,
     *REFW470,REFW550,REFW659,REFW865,REFW124,REFW164,REFW213,ASUM,
     *ALXWAV(NWAVN),TAUX55,AN,TAU_COMPUTED(NWAVN),
     *TAU_SMALL_COMPUTED(NWAVN),TAU_BIG_COMPUTED(NWAVN),QCONTROL,
     *QCONTROL1,ref_allwav,Good_pixels(NWAVN),
     *Quality_dust_flag_off_glint,Quality_dust_flag_glint,
     *Qcontrol_cirrus

C Define parameters for ocean (_S) and land (_L) lookup tables
C
      INTEGER NWAV_S,NTAU_S,NTH0_S,NTHET_S,NPHI_S
      INTEGER NWAV,NTAU,NTH0,NTHET,NPHI
      INTEGER NWAV_L,NTAU_L,NTH0_L,NTHET_L,NPHI_L
      REAL MINMTHET0,MAXMTHET0,MINMTHET,MAXMTHET,MINMPHI,MAXMPHI,MAXTAU
      PARAMETER (NWAV_S=7,NTAU_S=6,NTH0_S=9,NTHET_S=16,NPHI_S=16)
      PARAMETER (NWAV=7,NTAU=6,NTH0=9,NTHET=16,NPHI=16)
      PARAMETER (NWAV_L=2,NTAU_L=8,NTH0_L=9,NTHET_L=13,NPHI_L=31)
       REAL  GLINT_THRESHOLD
      PARAMETER( GLINT_THRESHOLD=40.0)
C
C Maxmum values of angle geometry for Look-up Tables
C
      PARAMETER(MINMTHET0=0.0,MAXMTHET0=72.0,MINMTHET=0.0,
     *MAXMTHET=72.0,MINMPHI=0.0,MAXMPHI=180.0,MAXTAU=5.0)
C
C From metadata
C
      INTEGER maxnum_scans,bsize
      PARAMETER (maxnum_scans=2000)
      INTEGER Dead_detectors(490),EV_frames(maxnum_scans)
      REAL*8 Scan_Start_Time(maxnum_Scans)
      CHARACTER*4 Scan_Type(maxnum_scans)
c
c  Parameters for cloud array
      INTEGER Buf_cldmsk,Buf_cldmsk_QA
c
      PARAMETER(Buf_cldmsk=6,Buf_cldmsk_QA=10)
C
C Others
C
      INTEGER Lines_Per_Read,Lines_Per_Write
      PARAMETER (Lines_Per_Read=10,Lines_Per_Write=10)
C
C Define cell numbers etc
C
C Define cell numbers etc
C
      INTEGER OUT_BAND
      PARAMETER (OUT_BAND=14)
      INTEGER NUMCELLS,ISWATH,ILINE,TOTCASES,MAXDIM,MAX_SAMP_LINE
      PARAMETER(NUMCELLS=2100,ISWATH=2100,ILINE=10,MAX_SAMP_LINE=1,
     *TOTCASES=(NUMCASES* NUMCASEB),
     *MAXDIM=NUMCELLS*Lines_Per_Write*TOTCASES)
C
C Define SDS array names & data types for time and geolocation
C
      CHARACTER*(*) SDS1_NE1,SDS1_NE2,SDS1_NE3,SDS1_NE4,SDS1_NE5,
     *              SDS1_NE6,SDS1_N,SDS2_N
      PARAMETER    (SDS1_NE1='Scan_Start_Time',
     *              SDS1_NE2 ='Solar_Zenith',SDS1_NE3='Solar_Azimuth',
     *              SDS1_NE4='Sensor_Zenith',SDS1_NE5='Sensor_Azimuth',
     *              SDS1_NE6='Cloud_Mask_QA',
     *              SDS1_N='Latitude',SDS2_N='Longitude')

C
C For both ocean and land array
C

      CHARACTER*(*) Fid_tau_land_and_ocean,Fid_scattering_angle,
     *  Fid_Ratio_Small_Land_And_Ocean,Fid_tau_land_and_ocean_img 
      PARAMETER(
     *Fid_tau_land_and_ocean='Optical_Depth_Land_And_Ocean',
     *Fid_tau_land_and_ocean_img='Image_Optical_Depth_Land_And_Ocean',
     *Fid_scattering_angle='Scattering_Angle',
     *Fid_Ratio_Small_Land_And_Ocean=
     *'Optical_Depth_Ratio_Small_Land_And_Ocean') 
c 1/2002New array 

C 
C For Ocean SDS arrays
C
      CHARACTER*(*) Fid_sol_small,Fid_sol_large,Fid_scatt_angle,
     *              Fid_AOT_Best,Fid_AOT_Average,Fid_AOT_best_small,
     *              Fid_AOT_average_small,Fid_AOT_best_large,
     *              Fid_AOT_average_large,Fid_mass_con,
     *              Fid_eff_rad,Fid_ccn,Fid_assy_best,Fid_assy_average,
     *              Fid_bcscatt_best,Fid_bcscatt_average,Fid_angs_coff1,
     *              Fid_angs_coff2,Fid_Byproducts_Ocean ,
     *              Fid_leasterror,
     *              Fid_smallmode,Fid_AOT_model,Fid_cldfrac,Fid_numpix,
     *              Fid_ref,Fid_refsd,Fid_qa
C                   ,Fid_Corrected_smallmode
C
C Define Ocean SDS arrays
C
      PARAMETER(
     *Fid_sol_small         = 'Solution_Index_Ocean_Small',
     *Fid_sol_large         = 'Solution_Index_Ocean_Large',
     *Fid_scatt_angle       = 'Scattering_Angle_Ocean',
     *Fid_AOT_Best          = 'Effective_Optical_Depth_Best_Ocean',
     *Fid_AOT_Average       = 'Effective_Optical_Depth_Average_Ocean',
     *Fid_AOT_best_small    = 'Optical_Depth_Small_Best_Ocean',
     *Fid_AOT_average_small = 'Optical_Depth_Small_Average_Ocean',
     *Fid_AOT_best_large    = 'Optical_Depth_Large_Best_Ocean',
     *Fid_AOT_average_large = 'Optical_Depth_Large_Average_Ocean',
     *Fid_mass_con          = 'Mass_Concentration_Ocean',
     *Fid_eff_rad           = 'Effective_Radius_Ocean',
     *Fid_ccn               = 'Cloud_Condensation_Nuclei_Ocean',
     *Fid_assy_best         = 'Asymmetry_Factor_Best_Ocean',
     *Fid_assy_average      = 'Asymmetry_Factor_Average_Ocean',
     *Fid_bcscatt_best      = 'Backscattering_Ratio_Best_Ocean',
     *Fid_bcscatt_average   = 'Backscattering_Ratio_Average_Ocean')
      PARAMETER(
     *Fid_angs_coff1        = 'Angstrom_Exponent_1_Ocean',
     *Fid_angs_coff2        = 'Angstrom_Exponent_2_Ocean',
     *Fid_Byproducts_Ocean = 'Aerosol_Cldmask_Byproducts_Ocean', 
     *Fid_leasterror        = 'Least_Squares_Error_Ocean',
     *Fid_smallmode         = 'Optical_Depth_Ratio_Small_Ocean_0.55micron',
     *Fid_AOT_model         = 'Optical_Depth_by_models_ocean',
     *Fid_cldfrac           = 'Cloud_Fraction_Ocean',
     *Fid_numpix            = 'Number_Pixels_Used_Ocean',
     *Fid_ref               = 'Mean_Reflectance_Ocean',
     *Fid_refsd             = 'STD_Reflectance_Ocean',
     *Fid_qa                = 'Quality_Assurance_Ocean')

C
C For Land SDS arrays
C

      CHARACTER*(*)Fid_aer_land,Fid_scatt_angle_land,
     *             Fid_AOT_Contin_land,Fid_AOT_correc_land,
     *             Fid_AOT_small_land,Fid_AOT_Large_land,
     *             Fid_mass_con_land,
     *             Fid_angs_coff_land,Fid_cldfrac_land,
     *             Fid_dust_wght_land,Fid_numpix_land,
     *             Fid_ref_land,Fid_refsd_land,Fid_qa_land,
     *             Fid_qa_extra_land,Fid_Byproducts_Land,
     *             Fid_mean_ref_all,Fid_sd_ref_all,
     *             Fid_path_radiance,
     *             Fid_Critical_Reflectance,
     *             Fid_error_Critical_Reflectance,
     *             Fid_error_path_radiance,
     *             Fid_QWeight_Path_Radiance,
     *             Fid_QWeight_Crit_Reflectance,
     *             Fid_Surface_Reflectance,Fid_Fitting_error,
     *             Fid_AOT_correc_land213 

 
C Define Land SDS arrays
C

      PARAMETER(
     *Fid_aer_land          = 'Aerosol_Type_Land',
     *Fid_scatt_angle_land  = 'Scattering_Angle_Land',
     *Fid_AOT_Contin_land   = 'Continental_Optical_Depth_Land',
     *Fid_AOT_correc_land   = 'Corrected_Optical_Depth_Land',
     *Fid_AOT_small_land    = 'Optical_Depth_Small_Land',
     *Fid_AOT_large_land    = 'Optical_Depth_Large_Mode_Land',
     *Fid_mass_con_land     = 'Mass_Concentration_Land',
     *Fid_angs_coff_land    = 'Angstrom_Exponent_Land', 
     *Fid_cldfrac_land      = 'Cloud_Fraction_Land', 
     *Fid_dust_wght_land    = 'Optical_Depth_Ratio_Small_Land',
     *Fid_numpix_land       = 'Number_Pixels_Used_Land',
     *Fid_ref_land          = 'Mean_Reflectance_Land',
     *Fid_refsd_land        = 'STD_Reflectance_Land',
     *Fid_qa_land           = 'Quality_Assurance_Land',
     *Fid_Byproducts_Land = 'Aerosol_Cldmask_Byproducts_Land',
     *Fid_qa_extra_land     = 'Quality_Assurance_Crit_Ref_Land',
     *Fid_Surface_Reflectance='Surface_Reflectance_Land',
     *Fid_Fitting_error      ='Fitting_Error_Land')
c 3/2001 New arrays
       PARAMETER(
     *Fid_mean_ref_all ='Mean_Reflectance_Land_All',
     *Fid_sd_ref_all   ='Standard_Deviation_Reflectance_Land_All',
     *Fid_path_radiance='Path_Radiance_Land',
     *Fid_Critical_Reflectance='Critical_Reflectance_Land', 
     *Fid_error_Critical_Reflectance='Error_Critical_Reflectance_Land', 
     *Fid_error_path_radiance='Error_Path_Radiance_Land',
     *Fid_QWeight_Path_Radiance=
     *'QualityWeight_Path_Radiance_Land',
     *Fid_QWeight_Crit_Reflectance=
     *'QualityWeight_Critical_Reflectance_Land',
     *Fid_AOT_correc_land213 = 
     *'Corrected_Optical_Depth_Land_wav2p1')
C
C Define solutions and Bands used for Land
C
      INTEGER Land_Sol1,Land_Sol2,Land_Sol3,Band_Land,Land_Sol4
      PARAMETER(Land_Sol1=2,Land_Sol2=3,
     *Land_Sol3=3,Band_Land=7,Land_Sol4=4)
C
C Define qa land
C
      INTEGER QA_ocean,QA_Land
      PARAMETER(QA_ocean=5,QA_Land=5)
c Define surface ratio for wavelength 0.47 and 0.66 um to compute
c critical reflactance
      Real Surface_wav47,Surface_wav66
      PARAMETER(Surface_wav47=0.25,Surface_wav66=0.5)

C
C Define SDS data scales & offsets
C
      DOUBLE PRECISION SCALE1,SCALE2,SCALE3,OFFSET1,OFFSET2,OFFSET3,
     &                 SCALE4,OFFSET4
      PARAMETER (SCALE1=1.0,SCALE2=100.0,SCALE3=1000.0,
     &           SCALE4=10000.0,OFFSET1=0.0,OFFSET2=0.0,OFFSET3=0.0,
     &           OFFSET4=0.0)
C
C Define dimension names
C
      CHARACTER*(*) dim_1st,dim_2nd,dim_3rd,dim_ref
      PARAMETER     (dim_1st='Cells Across Swath')
      PARAMETER     (dim_2nd='Cells Along Swath')
      PARAMETER     (dim_3rd='Solutions')
      PARAMETER     (dim_ref='Number Of Bands')
C
C PCF file registration number
C
      INTEGER  S1,S2,S3,L1,L2,L3,INSCI,
     &          LRN_Geo,LRN_L1B_1km,LRN_L1B_500,LRN_L1B_250,
     &          LRN_CldMsk,LRN_MOD04,LRN_MOD05,LRN_MOD07,
     &          LRN_WISC_ANC_met,LRN_WISC_ANC_ozn,
     &          LRN_WISC_ANC_ice,LRN_WISC_ANC_sst,
     &          OUTSCI,LRN_QC
      INTEGER LRN_MCF,LRN_MCFQC,LRN_QCMET
      INTEGER LRN_LUT466,LRN_LUT553,LRN_LUT644,LRN_LUT213
      INTEGER LRN_LUTMAP
      PARAMETER (S1=407520,S2=407530,S3=407540)
      PARAMETER (L1=407580,L2=407590,L3=407600)
      PARAMETER (INSCI=407640)
      PARAMETER (LRN_LUT466=405100,LRN_LUT553=405110,
     &           LRN_LUT644=405120,LRN_LUT213=405130)
      PARAMETER (LRN_LUTMAP=405140)
      PARAMETER (LRN_L1B_1km=700002,LRN_L1B_500=700001,
     &           LRN_L1B_250=700000,LRN_Geo=600000,LRN_CldMsk=422500,
     &           LRN_MOD04=405000,LRN_MOD05=410000,LRN_MOD07=420000,
     &           LRN_WISC_ANC_met=900000,LRN_WISC_ANC_ozn=900020,
     &           LRN_WISC_ANC_ice=900040,LRN_WISC_ANC_sst=900030,
     &           LRN_QC=405001)
      PARAMETER (LRN_MCF=405500,LRN_MCFQC=405501,LRN_QCMET=405601)

C
C File Handle Logical Unit Numbers
C

      INTEGER   handle_LUT466,handle_LUT553,
     &          handle_LUT644,handle_LUT213
      INTEGER   handle_LUTMAP
      INTEGER   handle_INSCI,handle_OUTSCI,handle_QC
      INTEGER   handle_S(3),handle_L(3)
C
C Obsolete handles
C
      INTEGER   handle_LUT486,handle_LUT663,handle_LUTPHS

C
C Declarations of PARAMETERS to hold Geolocation array Fill Values
C
      INTEGER*2 FV_SolAz,FV_SenAz,FV_SolZen,FV_SenZen,
     &          FV_Height,FV_Range
      REAL FV_Lat, FV_Lon
      PARAMETER ( FV_SolAz = -32767,  FV_SenAz = -32767,
     &            FV_SolZen = -32767, FV_SenZen = -32767,
     &            FV_Height = -32767, FV_Range = 0,
     &            FV_Lat = -999.0,    FV_Lon = -999.0 )

C
C Declaration of PARAMETERS to hold Fill Values of L1B and
C Geolocation parameter data in science code.
C
      REAL FV_L1B, FV_GEO
      PARAMETER ( FV_L1B = -999.0, FV_GEO = -999.0 )




