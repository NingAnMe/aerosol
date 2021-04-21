        SUBROUTINE PROCESS_Land(HANDLE_LUT466,HANDLE_LUT553,HANDLE_LUT644,
     &  HANDLE_LUT213,HANDLE_LUTMAP,IMONTH,ISCAN,IDATA,NUMSQ,MTHET0,
     &  MTHET,MDPHI,MHGHT,Lat_center,Lon_center,START_500,END_500,
     &  START_250,END_250,START_1KM,END_1KM,W470_syn,W550_SYN,
     &  W659_syn,W865_syn,W124_SYN,W164_SYN,W213_syn,
     &  CldMsk_250,Set_Counter_Land,QA_Flag_Land,Success_Ret_Land,
     &  Fail_Ret_Land,SDSLAT,SDSLON,SDS_MTHET0,SDS_MTHET,SDS_MPHI,
     &  SDS_Tau_Land_Ocean,CLDMSK_500,SDS_Tau_Land_Ocean_img,
     &  SDS_Aerosol_Type,SDS_SCAT_ANGLE_land,SDS_mass_conc_land,
     &  SDS_angs_coeff_land,SDS_CLDFRC_land,SDS_dust_weighting,
     &  SDS_est_uncer,SDS_RefF_land,
     &  SDS_TranF_land,SDS_NUMPIXELS_land,SDSTAU_corrected,
     &  SDS_ref_land,SDS_ref_STD_land,SDS_QCONTROL_land,
     &  SDS_Mean_Reflectance_Land_All, SDS_SDev_Reflectance_Land_All,
     &  SDS_Path_Radiance_Land, SDS_Critical_Reflectance_Land,
     &  SDS_Error_Crit_Reflectance_Land,SDS_Error_Path_Radiance_Land,
     &  SDS_QFlag_Critical_Ref_Land,SDS_QFlag_Path_Radiance_Land,SDS_QCONTROL_CritRef_land,
     &  G_factor,quality_land,Ret_Quality_cirrus,cloud_num_land,
     &  SDS_Surface_Reflectance_Land,SDS_Fitting_Error_Land,Qcontrol_special_land,
     &  SDSTAU_corrected_213,Quality_flag_forJoint,SDSTAU_small_land)


C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C
C   This program derives aerosol optical depth (AOD) at 10km x 10km
C   from cloud and water screened 500 meter and 250 meter MODIS measured
C   radiances over land. AOD are derived at 0.553 micron, by inverting
C   radiance data at 0.466,0.644 and 2.113 micron channels. Dark pixels
C   are identified by the 2.113 mid-shortwave-IR (SWIR) channel, and
C   sorted so that the brightest (50%) and darkest (20%) pixels are discarded
C   to reduce cloud and other contaminations. Surface reflectance in the 
C   the visible channels (0.466 and 0.644 micron) are estimated from
C   the 2.113 channel, using relationships parameterized from vegetation
C   indices in the mid-IR (MVI) channels of 1.24 and 2.113 microns), and
C   also as a function of the solar/surface/satellite scattering angles. 
C   Note, that the 2.113 SWIR is not assumed transparent to aerosol, so that
C   the inversion of the two visible and one SWIR channels results in
C   three parameters: 1) spectral AOD normalized to 0.553 micron
C   2) The spectral fine mode AOD (AODfine = AOD * non-dust weighting)
C   and 3) Surface reflectance at 2.113 micron.
C 
C   The aerosol models have been derived from the AERONET data base and
C   are considered "dynamic" models of the optical depth. 
C   and are fixed as a function of location and season. 
C 
c   The program assumes that the input data are in reflectance units
C   of PI*L/(F0*cos(theta0)) for all visible and mid-IR channels. 
C   The input data are on MODIS spatial resolutions where the pixel size 
C   of the 0.66 micron is double the resolution of the 0.466, 1.24 and 2.11 
C   micron channels
C
C   Note: the program uses input cloud mask file (cloudy=0,clear=1),
C   which is determined by the thresholds of visible reflectance and
C   the visible reflectance ratios.  It requires that for 0.47 micron
C   channel 75% of the subpixels (250 m resolution) to be cloud free
C   and 100% of the subpixels to be water free. For 0.66 micron channel
C   it requires 100% subpixels to be cloud and water free. The selection
C   of 20th to 50th percentile will eliminate the residual clouds.
C   A valid retrieval needs at least 12 remaining pixels.
C
C!INPUT PARAMETERS:
C
C   HANDLE_LUTs          Logical unit numbers to open multi-wavelength lookup tables
C   IMONTH               Calendar month
C   ISACN                Scan number
C   IDATA                Index of 10x10 km box
C   NUMSQ                Total number of 10x10 km boxes
C   MTHET0               Solar zenith angle
C   MTHET                viewing angle
C   MDPHI                Relative azimuth angle
C   LAT_CENTER           Center latitude of 10x10 km box
C   LON_CENTER           Center longitude of 10x10 km box
C   START_500            Starting index for 500 m resolution array
C   END_500              Ending index for 500 m resolution array
C   START_250            Starting index for 250 m resolution array
C   END_250              Ending index for 250 m resolution array
C   START_1KM            Starting index for 1 km resolution array
C   END_1KM              Ending index for 1 km resolution array
C   W470_SYN             L1B data at 0.466 micron
C   W213_SYN             L1B data at 2.113 micron
C   W124_SYN             L1B data at 1.24 micron
C   W164_SYN             L1B data at 1.64 micron
C   W553_SYN             L1B data at 0.553 micron
C   W659_SYN             L1B data at 0.644 micron
C   W865_SYN             L1B data at 0.865 micron
C   CLDMSK_250           Cloud mask in 250 m resolution
C   SET_COUNTER_LAND     Counter of land
C
C!OUTPUT PARAMETERS:
C
C   QA_Flag_Land         QA flag araay for aerosol over land
C   Success_Ret_Land     Number of successfully retrieved value
C   Fail_Ret_Land        Number of failed retrieved value
C   SDSLAT               HDF SDS array for latitude
C   SDSLON               HDF SDS array for longitude
C   SDS_MTHET0           HDF SDS array for solar zenith angle
C   SDS_MTHET            HDF SDS array for viewing angle
C   SDS_MPHI             HDF SDS array for relative azimuth
C   SDS_Aerosol_Type     HDF SDS array for aerosol type
C   SDS_SCAT_ANGLE_land  HDF SDS array for scattering angle
C   SDS_mass_conc_lan    HDF SDS array for mass concentration
C   SDS_angs_coeff_land  HDF SDS array for angstrom coefficient
C   SDS_CLDFRC_land      HDF SDS array for number of cloudy pixels
C   SDS_dust_weighting   HDF SDS array for dust weighting factor
C   SDS_est_uncer        HDF SDS array for estimated uncertainties
C   SDS_NUMPIXELS_land   HDF SDS array for number of pixels used in retrieval
C   SDSTAU_corrected     HDF SDS array for corrected aerosol optical depth
C   SDSTAU_small_land          HDF SDS array for fine mode aerosol optical depth
C   SDS_ref_land         HDF SDS array for mean apparent reflectance
C   SDS_ref_STD_land     HDF SDS array for std of apparent reflectance
C   SDS_QCONTROL_land    HDF SDS array for quality control
C
C!REVISION HISTORY:
C $Log: Process_land_V6.f,v $
c 02/15/2006 levy
c Initial revision
c
c!TEAM-UNIQUE HEADER:
c
c Developed by MODIS Aerosol/Solar Water Vapor Retrieval Team
c GSFC, Greenbelt, MD
c
C
C!DESIGN NOTES:
C
C The following ERROR CODE indicates no retrieval possibly done:
C   Shana!!
C      1     Solar, satellite zenith angle and relative azimuth angles
C            are out of bound of look-up tables
C      2     Threshold for 2.13 um is not met in the entire grid box
C      3     The thresold for number of cloudfree points is not met.
C            (NUMCLDFREE see the parameter) or there is water.
C      4     The computed reflectance for 0.47 or 0.66 channels is out
C            of bounds from look-up table
C      5     Threshold for 2.13 um is not met in option 2 and 3
C      6     Aerosol type can not be determined
C
C The values of aerosol optical thickness corresponding to error code
C Error code       Value of aerosol optical thickness
C
C      1              -3.0
C      2              -4.0
C      3              -5.0
C      4              -6.0
C      5              -7.0
C      6              -8.0
C
C  Aerosol types (FTABLE)
C      1              Continental
C      2              Generic : SSA ~ 0.9
C      3              Smoke   : SSA ~ 0.85
C      4              Urban   : SSA ~ 0.95
C      5              Dust
C
C Procedure used in retrieving aerosol optical thickness:
C
C      0 = no dark targets possibly met by the following thresholds
C      1 = threshold used for wave 2.13 um 0.01 - 0.25
C      2 = threshold used for wave 2.13 um 0.25 - 0.40
C
C INTERNALS:
C
C   Subroutines:
C      RNLOOKUP      -  READS THE LOOK-UP TABLES
C      ERROR         -  SETS THE ERROR CODES
C      OUTPUT        -  WRITES THE OUTPUT TO HDF FILE
C      AEROSOL_MAP   -  Reads map of aerosol model as function of season and place
C      INTANGLE_NL_RAY  -  Interpolates LUT Rayleigh parameters to measured geometry
C      Process_land_Rob -  The main code for doing the radiance to aerosol inversion
C      COMPUTE_SCATTANGLE_LAND - Computes scattering angle
C      Statistics_land  -  Statistics for Path Radiance and Critical Reflectance
C      FILLVALUE_LAND - Inserts fill values for parameters if errors
C
C
C   Variables:
C     W470_SYN(2*IGRIDX,2*IGRIDY)    Reflectance for wav=0.47um
C     W550_SYN(2*IGRIDX,2*IGRIDY)    Reflectance for wav=0.55um
C     W213_SYN(2*IGRIDX,2*IGRIDY)    Reflectance for wav=2.13um
C     W124_SYN(2*IGRIDX,2*IGRIDY)    Reflectance for wav=1.24um
C     W164_SYN(2*IGRIDX,2*IGRIDY)    Reflectance for wav=1.64um
C     W659_SYN(4*IGRIDX,4*IGRIDY)    Reflectance for wav=0.66um
C     W865_SYN(4*IGRIDX,4*IGRIDY)    Reflectance for wav=0.86um
C     CLDMSK_250(4*ISWATH,4*ILINE)   Cloud mask of 250 m resolution
C                                    (0=cloudy,1=clear)
C     MTHET0                         solar zenith angle (degree)
C     MTHET                          satellite viewangle angle (degree)
C     MDPHI                          relative azimuth in angle (degree)
C     MHGHT                          Topographic altitude (km)
C     yint644,yint466,slope466,slope644  
C                 characteristics of VIS/IR surface reflectance relationship
C
C LAND SDS_ARRAYS.........
C
C    SDS_QCONTROL_land      Quality control SDS array
C    SDS_Aerosol_Type       Index of Aerosol type
C    SDS_SCAT_ANGLE_land    Scattering Angle
C    SDS_angs_coeff_land    Angstrom exponent for 0.47 and 0.67 micron
C    SDS_CLDFRC_land        Cloud fraction (%)
C    SDS_dust_weighting     Dust aerosol weighting factor 
C    SDS_est_uncer          Uncertainty of optical thickness at 0.47 and 0.66 micron
C    SDS_NUMPIXELS_land     Number of pixels with desired percentile
C    SDSTAU_corrected       Corrected optical thickness at 0.47 0.55 and 0.66 micron
C    SDS_ref_land           Mean reflectance at five bands
C    SDS_ref_STD_land       Standard deviation of reflectance at five bands
C    SDS_mass_conc_land     Mass concentration
C
C!END
C----------------------------------------------------------------------
C  Parameters for the resolution of modis data

      IMPLICIT NONE
      SAVE

      INCLUDE 'mod04.inc'
      INCLUDE 'mod04_land.inc'
C
C Define SDS array for hdf output
C
      INTEGER *2   SDS_MTHET0(NUMCELLS),SDS_MTHET(NUMCELLS),
     &             SDS_MPHI(NUMCELLS)
            REAL   SDSLAT(NUMCELLS),SDSLON(NUMCELLS)

C
C Combined Land/Ocean SDS Arrays
C

      INTEGER *2   SDS_Tau_Land_Ocean(NUMCELLS),
     &             SDS_Tau_Land_Ocean_img(NUMCELLS)

C
C  LAND SDS_ARRAYS........
C
      BYTE         SDS_QCONTROL_land(QA_LAND,NUMCELLS)
      BYTE         SDS_QCONTROL_CritRef_land(QA_LAND,NUMCELLS)
      INTEGER *2   SDS_Aerosol_Type(NUMCELLS),
     &             SDS_SCAT_ANGLE_land(NUMCELLS),
     &             SDS_angs_coeff_land(NUMCELLS),
     &             SDS_CLDFRC_land(NUMCELLS),
     &             SDS_dust_weighting(NUMCELLS),
     &             SDS_NUMPIXELS_land(NUMCELLS,Land_Sol1),
     &             SDSTAU_corrected(NUMCELLS,Land_Sol3),
     &             SDS_ref_land(NUMCELLS,Band_land),
     &             SDS_ref_STD_land(NUMCELLS,Band_land),
     &             SDSTAU_small_land(NUMCELLS,Land_Sol4)
     
       INTEGER *2 SDS_Surface_Reflectance_Land(NUMCELLS,Land_Sol3),
     &            SDS_Fitting_Error_Land(NUMCELLS),
     &            SDSTAU_corrected_213(NUMCELLS)

       REAL       SDS_mass_conc_land(NUMCELLS)

C
C EXTRA LAND SDS_ARRAYS..........FOR LAND Statistics ONLY
C
       INTEGER *2  SDS_Mean_Reflectance_Land_All(NUMCELLS,Land_Sol3),
     &  SDS_SDev_Reflectance_Land_All(NUMCELLS,Land_Sol3),
     &  SDS_Path_Radiance_Land(NUMCELLS,Land_Sol1),
     &  SDS_Critical_Reflectance_Land(NUMCELLS,Land_Sol1),
     &  SDS_Error_Crit_Reflectance_Land(NUMCELLS,Land_Sol1),
     &  SDS_Error_Path_Radiance_Land(NUMCELLS,Land_Sol1),
     &  SDS_QFlag_Critical_Ref_Land(NUMCELLS,Land_Sol1),
     &  SDS_QFlag_Path_Radiance_Land(NUMCELLS,Land_Sol1)

C
C Obsolete (02/2006) Land SDS Arrays
C
      INTEGER *2   
     &            SDS_est_uncer(NUMCELLS,Land_Sol1),
     &            SDS_RefF_land(NUMCELLS,Land_Sol2),
     &            SDS_TranF_land(NUMCELLS,Land_Sol1)

      INTEGER QA_Flag_Land(19),Success_Ret_Land,Fail_Ret_Land
      INTEGER NUMSQDIM,NUMXBOX,NUMYBOX,Quality_flag_forJoint
      PARAMETER (NUMSQDIM=400)
      INTEGER RTN,START_DATA,END_DATA,IAER
      CHARACTER*100 fname
      CHARACTER*20 att_n,dtype
      INTEGER NUMMODEL,NUMTAU,NUMSCATT,LINES
      PARAMETER (NUMMODEL=4,NUMTAU=12,NUMSCATT=12,LINES=100)
      INTEGER IDATA,ISCAN,IT,IERROR,IRD,NUMDATA,NUMSCAN,IWEIGHT,nms
      INTEGER Ret_Quality_cirrus 
      INTEGER IFINISH,IMONTH,IPR,IPROCE,IRPHSOM,ISMK,ISULP,LINE,NUMSQ,ISMK_H,ISMK_M
      INTEGER Sea_landFSQ(NUMSQDIM,IGRIDX,IGRIDY),cloud_num_land
      INTEGER Sea_landF(IGRIDX,IGRIDY),Set_Counter_Land
      INTEGER START_500,END_500,START_250,END_250,START_1KM,END_1KM
      INTEGER CLDMSKD3(NUMSQDIM,4*IGRIDX,4*IGRIDY)
      INTEGER CLDMSK_250(4*ISWATH,4*ILINE)
      INTEGER CLDMSK_500(2*ISWATH,2*ILINE)
      REAL WEIGHT,SCAT_ANGLE_LAND

c  L1B Reflectance data
      REAL W470_SYN(2*ISWATH,2*ILINE)
      REAL W550_SYN(2*ISWATH,2*ILINE)
      REAL W213_SYN(2*ISWATH,2*ILINE)
      REAL W124_SYN(2*ISWATH,2*ILINE)
      REAL W164_SYN(2*ISWATH,2*ILINE)
      REAL w865_SYN(4*ISWATH,4*ILINE)
      REAL W659_SYN(4*ISWATH,4*ILINE)

c L2 Reflectance data
      REAL REFW466_L,REFW550_L 
      REAL REFW644_L,REFW124_L,REFW164_L,REFW866_L,REFW212_L
      REAL SDW466_L,SDW550_L
      REAL SDW644_L,SDW124_L,SDW164_L,SDW866_L,SDW212_L

      INTEGER Num_Pixels_Used, quality_land
      REAL SAVERATIO
      REAL THR213MIN, THR213MAX

      REAL MTHET0,MTHET,MDPHI,MHGHT
      REAL LATM,LONM,LAT_CENTER,LON_CENTER
      REAL WATER_VAPOR(NUMSQDIM),OZONE_COL(NUMSQDIM)
      CHARACTER * 5 IOFLAG

      integer ij,ik ,Qcontrol_special_land
      INTEGER NUMRED(2*ISWATH,2*ILINE),NUMBLUE(2*ISWATH,2*ILINE)

C
C The followings scalars are defined in mod04.inc
C
C MINMTHET0,MAXMTHET0,MINMTHET,MAXMTHET,MINMPHI,MAXMPHI,MAXTAU
C
       REAL G_factor, DEGRAD

       INTEGER NUMCLDFREE,NUMPERBLUE,NUMPERRED,NumHandles
       PARAMETER (NUMCLDFREE=20,NUMPERBLUE=7,NUMPERRED=7)
       REAL FLUXDNBLUE,FLUXDNRED,FLUXUPBLUE,FLUXUPRED

C
C Percentiles used for the radiances averaged 20th and 50th percentile
C

      INTEGER NLOWPERC,NUPPERC
      PARAMETER (NLOWPERC=20,NUPPERC=50)

c  Parameters Direct from the new lookup table (NL0 = New Land Initialize)

      REAL SBAR_NL0(NLTHET0,NLTAU,NLWAV,NLTABLE)
      REAL OPTH_NL0(NLTAU,NLWAV,NLTABLE)
      REAL INT_NL0(NLPHI,NLTHE,NLTHET0,NLTAU,NLWAV,NLTABLE)
      REAL FdT_NL0(NLTHE,NLTHET0,NLTAU,NLWAV,NLTABLE)
      REAL Fd_NL0(NLTHET0,NLTAU,NLWAV,NLTABLE)
      REAL T_NL0(NLTHE,NLTHET0,NLTAU,NLWAV,NLTABLE)
      REAL MASSCOEF_NL0(NLTAU,NLWAV,NLTABLE)
      REAL EXTNORM_NL0(NLTAU,NLWAV,NLTABLE)

C  For Determining aerosol type from aerosol map
      INTEGER nlon, nlat
      PARAMETER (nlon = 360, nlat = 180)
      INTEGER AEROSOL(nlon,nlat)

c retrieved products
      REAL ERR644
      REAL TAULAND55,RHOSFC213
      REAL MASSCON,ANGSTROM
      REAL RHOSFC(NLWAV)
      REAL ETA
      REAL AOD(NLWAV),AODF(NLWAV), AODC(NLWAV)
      INTEGER AVE_COUNT
      INTEGER FTABLE_NL
      INTEGER ETA_FLAG

c dummy
      INTEGER IWAV 

c Rayleigh reflectance interpolated for angle

      REAL REF_RAY_NL(NLWAV)
      REAL Rayleigh_look(2)


C     End of Declarations
C ------------------------------------------------------- 
C
C Execution only for the first time
C

       IF(Set_Counter_Land.EQ.1) THEN

        NUMDATA=NUMSQ

C Subroutine RNLOOKUP reads the new look-up tables for land

      CALL RNLOOKUP(
     +    HANDLE_LUT466,HANDLE_LUT553,HANDLE_LUT644,HANDLE_LUT213,
     +    INT_NL0,Fd_NL0,T_NL0,OPTH_NL0,
     +    SBAR_NL0,MASSCOEF_NL0,EXTNORM_NL0)
C
C  Determine fine mode aeorsol location map
C
       CALL AEROSOL_MAP(HANDLE_LUTMAP,IMONTH, AEROSOL)

      ENDIF

c     Initialize retrievals

      ETA = -9999
      ERR644 = -9999
      MASSCON = -9999
      ANGSTROM = -9999
      DO IWAV = 1, NLWAV
        RHOSFC(NLWAV) = -9999
        AOD(NLWAV) = -9999
        AODF(NLWAV) = -9999
        AODC(NLWAV) = -9999
      ENDDO
      AVE_COUNT = -9999
      FTABLE_NL = -9999       
      IAER = -9999

      START_DATA=1
      END_DATA=NUMDATA
      IFINISH=0
      Qcontrol_special_land=0
 

C
C Checking the angle bounds
C

       IF(MTHET0 .GE. MINMTHET0 .AND. MTHET0 .LE. MAXMTHET0.AND.
     *    MTHET  .GE. MINMTHET  .AND. MTHET  .LE. MAXMTHET .AND.
     *    MDPHI  .GE. MINMPHI   .AND. MDPHI  .LE. MAXMPHI) THEN
c
        CALL  COMPUTE_SCATTANGLE_LAND(MTHET0,MTHET,MDPHI,IDATA,
     *  SCAT_ANGLE_LAND)

C  
C Do path radiance and critical radiance statistics
C 

        CALL INTANGLE_RAY_NL(MTHET0,MTHET,MDPHI,
     *	  INT_NL0,REF_RAY_NL)

        Rayleigh_look(1) = REF_RAY_NL(iwave_466)  
        Rayleigh_look(2) = REF_RAY_NL(iwave_644)  
          
        CALL statistics_land(W470_SYN,W659_SYN,W213_SYN,CLDMSK_500,
     *      SDS_Mean_Reflectance_Land_All,
     *      SDS_SDev_Reflectance_Land_All,
     *      SDS_Path_Radiance_Land,
     *      SDS_Critical_Reflectance_Land,
     *      SDS_Error_Crit_Reflectance_Land,
     *    SDS_Error_Path_Radiance_Land,
     *    SDS_QFlag_Critical_Ref_Land,SDS_QFlag_Path_Radiance_Land,
     *    START_1KM,END_1KM,Idata,iscan,MTHET0,MTHET,
     *    SDS_QCONTROL_CritRef_land,SCAT_ANGLE_LAND,
     *    Rayleigh_look,W865_SYN)
    
   
       LATM=Lat_center
       LONM=Lon_center
       IPROCE = 0
 
C
C  Select criterion for detecting dark targets using 2.1 micron channel
C  (reflectance at 2.1 micron between 0.01 and THR213MAX) if the second
C  criterion fails (i.e., IFINISH=0)
C
             
             THR213MIN = 0.01
             THR213MAX = 0.25 
        Call Average_land(ISWATH,ILINE,CLDMSK_250,W470_SYN,W550_SYN,
     * W659_SYN,W865_SYN,W124_SYN,W164_SYN,W213_SYN,START_500,END_500,
     * START_250,END_250,START_1KM,END_1KM,REFW466_L,REFW550_L,REFW644_L,
     * REFW124_L,REFW164_L,REFW866_L,REFW212_L,SDW466_L,SDW550_L,
     * SDW644_L,SDW124_L,SDW164_L,SDW866_L,SDW212_L,IFINISH,IGRIDX,IGRIDY,
     * THR213MIN,THR213MAX,Num_Pixels_Used,IERROR,NUMRED,NUMBLUE)
     
             If(IFINISH.EQ.1)  IPROCE = 1
 

C 12/15/2001
C only continental model Procedure =2
            
            IF(IFINISH.EQ.0) THEN  
                G_factor=1./COS(DTR*MTHET)
     *                +1./SQRT(COS(DTR*MTHET0))
                G_factor=0.5*G_factor
               
               THR213MIN= 0.25  
               THR213MAX= 0.25*G_factor 
               
         IF(THR213MAX.LE.40.0) THEN
         Call Average_land(ISWATH,ILINE,CLDMSK_250,W470_SYN,W550_SYN,
     *  W659_SYN,W865_SYN,W124_SYN,W164_SYN,W213_SYN,START_500,END_500,
     * START_250,END_250,START_1KM,END_1KM,REFW466_L,REFW550_L,REFW644_L,
     *  REFW124_L,REFW164_L,REFW866_L,REFW212_L,SDW466_L,SDW550_L,
     *  SDW644_L,SDW124_L,SDW164_L,SDW866_L,SDW212_L,IFINISH,IGRIDX,IGRIDY,
     *  THR213MIN,THR213MAX,Num_Pixels_Used,IERROR,NUMRED,NUMBLUE) 
                   If(IFINISH.EQ.1)  IPROCE = 2
                     
          ENDIF
C 1c Endif for  IPROCE=2
            ENDIF


C Now do Rob's new inversion routine
         If(IFINISH.EQ.1) Then
              IERROR=0
         CALL PROCESS_Land_Inver(IPROCE,
     *   MTHET0,MTHET,MDPHI,SCAT_ANGLE_LAND,IMONTH,LATM,LONM,MHGHT,
     *   REFW466_L,REFW550_L,REFW644_L,REFW866_L,
     *   REFW124_L,REFW164_L,REFW212_L,
     *   SDW466_L,SDW550_L,SDW644_L,SDW866_L,
     *   SDW124_L,SDW164_L,SDW212_L,
     *   INT_NL0,Fd_NL0,T_NL0,OPTH_NL0,SBAR_NL0,AEROSOL,REF_RAY_NL,
     *   ETA,ETA_FLAG,AOD,ERR644,RHOSFC,AVE_COUNT,FTABLE_NL,
     *   MASSCOEF_NL0,MASSCON,
     *   EXTNORM_NL0,AODF,AODC,ANGSTROM)
     
C Set IAER     
            IAER = FTABLE_NL
            
     
C     If 2.13 Um not met procedure and error is set     
         ELSE
           IERROR=4
           IPROCE=0
           IAER =-9999
C ENDIF for Ifinish     
           ENDIF
        

          
 
C
C Output parameters to HDF file
C

      CALL OUTPUT(LATM,LONM,IAER,IDATA,IERROR,
     *  REFW466_L,REFW550_L,REFW644_L,REFW866_L,
     *  REFW124_L,REFW164_L,REFW212_L,SDW466_L,SDW550_L,
     *  SDW644_L,SDW124_L,SDW164_L,SDW866_L,SDW212_L,IPROCE,
     *  SCAT_ANGLE_LAND,ANGSTROM,MTHET0,MTHET,MDPHI,
     *  QA_Flag_Land,Success_Ret_Land,Fail_Ret_Land,
     *  FLUXDNBLUE,FLUXDNRED,FLUXUPBLUE,FLUXUPRED,MASSCON,
     *  SDSLAT,SDSLON,SDS_MTHET0,SDS_MTHET,SDS_MPHI,
     *  SDS_Tau_Land_Ocean_img,
     *  SDS_Aerosol_Type,SDS_SCAT_ANGLE_land,SDS_mass_conc_land,
     *  SDS_angs_coeff_land,SDS_CLDFRC_land,SDS_dust_weighting,
     * SDS_est_uncer,SDS_RefF_land,SDS_TranF_land,SDS_NUMPIXELS_land,
     * SDSTAU_corrected,SDS_ref_land,SDS_ref_STD_land,
     * SDS_QCONTROL_land,quality_land,Ret_Quality_cirrus,cloud_num_land,
     * SDS_Surface_Reflectance_Land,SDS_Fitting_Error_Land,
     * ETA,ETA_FLAG,AOD,AODF,RHOSFC,
     *  SDSTAU_corrected_213,SDSTAU_small_land,
     * ERR644,Num_Pixels_Used,Qcontrol_special_land,Quality_flag_forJoint)
       

 
 

C
C                        **Following else for 'ifang'

      ELSE
C                        **Write the output if the angles are out of
C                          bounds from the lookup table for different
C                          options with error indicators.
C
        IERROR=1
       
            
        CALL OUTPUT(LATM,LONM,IAER,IDATA,IERROR,
     *  REFW466_L,REFW550_L,REFW644_L,REFW866_L,
     *  REFW124_L,REFW164_L,REFW212_L,SDW466_L,SDW550_L,
     *  SDW644_L,SDW124_L,SDW164_L,SDW866_L,SDW212_L,IPROCE,
     *  SCAT_ANGLE_LAND,ANGSTROM,MTHET0,MTHET,MDPHI,
     *  QA_Flag_Land,Success_Ret_Land,Fail_Ret_Land,
     *  FLUXDNBLUE,FLUXDNRED,FLUXUPBLUE,FLUXUPRED,MASSCON,
     * SDSLAT,SDSLON,SDS_MTHET0,SDS_MTHET,SDS_MPHI,
     * SDS_Tau_Land_Ocean_img,
     *  SDS_Aerosol_Type,SDS_SCAT_ANGLE_land,SDS_mass_conc_land,
     *  SDS_angs_coeff_land,SDS_CLDFRC_land,SDS_dust_weighting,
     * SDS_est_uncer,SDS_RefF_land,SDS_TranF_land,SDS_NUMPIXELS_land,
     * SDSTAU_corrected,SDS_ref_land,SDS_ref_STD_land,
     * SDS_QCONTROL_land,quality_land,Ret_Quality_cirrus,cloud_num_land,
     * SDS_Surface_Reflectance_Land,SDS_Fitting_Error_Land,
     * ETA,ETA_FLAG,AOD,AODF,RHOSFC,
     *  SDSTAU_corrected_213,SDSTAU_small_land,
     * ERR644,Num_Pixels_Used,Qcontrol_special_land,Quality_flag_forJoint)
c       write(37,*)Iscan,idata,IPROCE,ETA,Qcontrol_special_land

 
 
  
C
C Endif for checking the angle bounds
C
      ENDIF
  
           
   1  FORMAT(132A1)
      RETURN
      END



C***********************************************************************
      SUBROUTINE OUTPUT(LATM,LONM,IAER,IDATA,IERROR,
     *  REFW466_L,REFW550_L,REFW644_L,REFW866_L,
     *  REFW124_L,REFW164_L,REFW212_L,SDW466_L,SDW550_L,
     *  SDW644_L,SDW124_L,SDW164_L,SDW866_L,SDW212_L,IPROCE,
     *  SCAT_ANGLE_LAND,ANGSTROM,MTHET0,MTHET,MDPHI,
     *  QA_Flag_Land,Success_Ret_Land,Fail_Ret_Land,
     *  FLUXDNBLUE,FLUXDNRED,FLUXUPBLUE,FLUXUPRED,MASSCON,
     * SDSLAT,SDSLON,SDS_MTHET0,SDS_MTHET,SDS_MPHI,
     * SDS_Tau_Land_Ocean_img,
     *  SDS_Aerosol_Type,SDS_SCAT_ANGLE_land,SDS_mass_conc_land,
     *  SDS_angs_coeff_land,SDS_CLDFRC_land,SDS_dust_weighting,
     * SDS_est_uncer,SDS_RefF_land,SDS_TranF_land,SDS_NUMPIXELS_land,
     * SDSTAU_corrected,SDS_ref_land,SDS_ref_STD_land,
     * SDS_QCONTROL_land,quality_land,Ret_Quality_cirrus,cloud_num_land,
     * SDS_Surface_Reflectance_Land,SDS_Fitting_Error_Land,
     * ETA,ETA_FLAG,AOD,AODF,RHOSFC,
     * SDSTAU_corrected_213,SDSTAU_small_land,
     * ERR644,Num_Pixels_Used,Qcontrol_special_land,Quality_flag_forJoint)
C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C                 This subroutine stores all SDSe arrays to HDF file
C
C!INPUT PARAMETERS: all varaiables to be wrriten as output
C
C   LATM          Latitude of data cell
C   LONM          Longitude of data cell
C   IAER          Aerosol type
C   BARLBLUE      Average reflectance for blue channel
C   IDATA         Data cell number from 1 to NUMDATA
C   NUMDATA       Number of retrieval cells across orbit swath
C   TAUABLUE      Blue channel aerosol optical thickness (Continental)
C   BARLRED       Average reflectance for red channel
C   TAUARED       Red channel aerosol optical thickness (Continental)
C   SDBLUE        STD of blue channel reflectances
C   SDRED         STD of red channel reflectances
C   TAUABLUEC     Blue channel aerosol optical thickness (Corrected)
C   TAUAREDC      Red channel aerosol optical thickness (Corrected)
C   IBLUE         Number of blue channel observations
C   IRED          Number of red channel observations
C   IERROR        Error flag (0-4)
C   WEIGHT        Relative contribution of smoke/sulfate particles to
C                 dust in the computation of the aerosol optical depth
C   ISULP         Sulfate (1)/no sulfate flag (0)
C   ISMK          Smoke (1)/no smoke flag (0)
C   IPROCE        Aerosol retrieval procedure ID (0-4)
C   SAVERATIO     Aerosol path radiance ration (red to blue channel
C                 for continental model)
C   PIXELS        Number of cells across swath (same as NUMDATA)
C   LINES         Number of cells along swath (same as NUMSCAN)
C   NUMXBOX       Not used
C   NUMYBOX       Not used
C
C!OUTPUT PARAMETERS: 13 variables for output to HDF FILE
C
C  SDS1           HDF array of cell latitudes
C  SDS2           HDF array of cell longitudes
C  SDS3           HDF array of spectral reflectances
C  SDS4           HDF array of aerosol optical thicknesses for
C                     continental model
C  SDS5           HDF array of the STD of spectral reflectances
C  SDS6           HDF array of aerosol optical thicknesses for
C                     corrected model
C  SDS7           HDF array of STD for corrected optical thickness
C                     blue channel for continental model)
C  SDS8           HDF array of aerosol path radiance ratio (red to
C                     blue channel for continental model)
C  SDS9           HDF array of relative aerosol optical depth
C                     (smoke/sulfate particles to dust)
C  SDS10          HDF array of the number of blue channel clear pixels
C  SDS11          HDF array of the number of red channel clear pixels
C  SDS12          HDF array of retrieval procedure ID (0-4)
C  SDS13          HDF array of aerosol type (0-3)
C  SDS14          HDF array of Error flag (0-4)
C
C!REVISION HISTORY:
C 01/28/98 fhliang
C fixed prolog.
C
C Updated code to comply with most MODIS software standards.
C
C!TEAM-UNIQUE HEADER:
C
C Developed by MODIS Aerosol Team at NASA GSFC, Greenbelt, MD
C
C!REFERENCES AND CREDITS
C     WRITTEN BY: Shana Mattoo
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'mod04.inc'
      INCLUDE 'mod04_land.inc'
C

c rhucek 01/15/98:  Changed type declaration of QA_Temp from INTEGER
c                   to BYTE.
      BYTE QA_Temp
      INTEGER I,QA_Flag_Land(19),Success_Ret_Land,Fail_Ret_Land,cloud_num_land
      integer quality_land, Ret_Quality_cirrus,IWAV,Num_Pixels_Used
      INTEGER IERROR,ICLDBLUE,ICLDRED,IAER,IPROCE,IPR,ISULP,ISMK,IDATA
      INTEGER Qcontrol_special_land,Quality_flag_forJoint
      REAL  TAUABLUE,TAUARED,TAUAREDC,TAUABLUEC,TAUAREDC2,TAUABLUEC2,SDBLUE,SDRED,SD0P86
      REAL  LATM,LONM,SAVERATIO,BARLBLUE,BARLRED,BARL0P86,SDS_SCAT_ANGLE
      REAL  BARRBLUE,BARRRED,TAUABLUENN,TAUAREDNN,SDTAUCBLUE,SDTAUCRED
      REAL  MTHET0,MTHET,MDPHI,WEIGHT,SCAT_ANGLE_LAND,ANGSTROM
      REAL  TAUARAT,TAUAGREENC,TAUAGREENC2
      REAL  FLUXDNBLUE,FLUXDNRED,FLUXUPBLUE,FLUXUPRED,MASSCON
      Real New_cloud_num,Quality_Flag_for_retr
      REAL REFW466_L,REFW550_L 
      REAL REFW644_L,REFW124_L,REFW164_L,REFW866_L,REFW212_L
      REAL SDW466_L,SDW550_L
      REAL SDW644_L,SDW124_L,SDW164_L,SDW866_L,SDW212_L
C
      INTEGER*2   SDS_MTHET0(NUMCELLS),
     &            SDS_MTHET(NUMCELLS),
     &            SDS_MPHI(NUMCELLS),
     &            SDS_Tau_Land_Ocean(NUMCELLS),
     &            SDS_Tau_Land_Ocean_img(NUMCELLS),
     &            SDS_Aerosol_Type(NUMCELLS),
     &            SDS_SCAT_ANGLE_land(NUMCELLS),
     &            SDS_angs_coeff_land(NUMCELLS),
     &            SDS_CLDFRC_land(NUMCELLS),
     &            SDS_dust_weighting(NUMCELLS),
     &            SDS_NUMPIXELS_land(NUMCELLS,Land_Sol1),
     &            SDSTAU_corrected(NUMCELLS,Land_Sol3),
     &            SDS_ref_land(NUMCELLS,Band_land),
     &            SDS_ref_STD_land(NUMCELLS,Band_land),
     &            SDSTAU_small_land(NUMCELLS,Land_Sol4)
     
       INTEGER *2 SDS_Surface_Reflectance_Land(NUMCELLS,Land_Sol3),
     &            SDS_Fitting_Error_Land(NUMCELLS),
     &            SDSTAU_corrected_213(NUMCELLS)
      REAL  SDSLAT(NUMCELLS),SDSLON(NUMCELLS),
     *      SDS_mass_conc_land(NUMCELLS)
      BYTE  SDS_QCONTROL_land(QA_LAND,NUMCELLS)
C
C Obsolete (02/2006) Land SDS Arrays
C
      INTEGER *2  
     &            SDS_est_uncer(NUMCELLS,Land_Sol1),
     &            SDS_RefF_land(NUMCELLS,Land_Sol2),
     &            SDS_TranF_land(NUMCELLS,Land_Sol1)

      REAL ETA
      REAL RHOSFC(NLWAV) 
      REAL AOD(NLWAV),AODF(NLWAV),AODC(NLWAV)
      REAL ERR644
      INTEGER ETA_FLAG
      SAVE
      SDS_CLDFRC_land(IDATA)=-9999
      
c compute fraction instead of % for outpt

      IF (cloud_num_land .ge.0) THEN
        New_cloud_num=real(cloud_num_land/100.)  
      ENDIF
        
      
      IF (AOD(iwave_553) .lt. -0.10) IERROR=5
      IF (AOD(iwave_553) .gt. 5.00)  IERROR=6
      IF (IPROCE.eq.0) IERROR=4

C If Errors  
        
      IF (IERROR.GT.0) THEN
        Quality_Flag_for_retr=11
        Fail_Ret_Land=Fail_Ret_Land+1
        QA_Flag_Land(7)=0
        QA_Flag_Land(8)=0
        QA_Flag_Land(9)=0
        QA_Flag_Land(10)=0   
C set it to 12 to indicate No retrivals        
        QA_Flag_Land(11)=Quality_Flag_for_retr
        QA_Flag_Land(12)=IERROR
        QA_Flag_Land(15)=0 
        Qcontrol_special_land=0
        Quality_flag_forJoint=QA_Flag_Land(8)
        IF ( New_cloud_num .ge.0) THEN
          SDS_CLDFRC_land(IDATA)=(New_cloud_num*SCALE3+OFFSET3)
        ENDIF
C Call for Fill values
     
        CALL FILLVALUE_LAND(IDATA,SDS_Tau_Land_Ocean_img,
     *SDS_Aerosol_Type,SDSTAU_corrected_213,
     *SDS_SCAT_ANGLE_land,SDS_mass_conc_land,SDS_angs_coeff_land,
     *SDS_CLDFRC_land,SDS_dust_weighting,
     *SDS_est_uncer,SDS_RefF_land,SDS_TranF_land,SDS_NUMPIXELS_land,
     *SDSTAU_corrected,SDS_ref_land,SDS_ref_STD_land,
     *SDS_QCONTROL_land,SDSTAU_small_land, 
     *SDS_Surface_Reflectance_Land,SDS_Fitting_Error_Land,
     *Qcontrol_special_land)
     

     
C    IF   retrivals Fill up the SDS's  
   
      ELSE IF (IERROR.EQ.0) THEN 
      
C   Intilize the Quality_Flag_for_retr =0  to indicate good quality  

      Quality_Flag_for_retr=0
  
C Store SDS arrays for population to HDF file

     
        Success_Ret_Land = Success_Ret_Land+1
       
        QA_Flag_Land(7)=1
        QA_Flag_Land(8)=3 
  
        

C quality_land .eq.0 is if there is one single pixel of water 

        IF (quality_land .eq.0)then
         QA_Flag_Land(8)=0 
         Quality_Flag_for_retr=2
         ENDIF
         
C Ret_Quality_cirrus =0 if thin cirrus detection 
      
        IF (Ret_Quality_cirrus .eq.0)then
         Quality_Flag_for_retr=3
         QA_Flag_Land(8)=0  
         ENDIF
      
C If Fitting error is greater than =0.25 quality is bad  
    
        IF (ERR644  .gt. 0.25) then
         QA_Flag_Land(8)=0   
          Quality_Flag_for_retr=4
         ENDIF 
        
C Set Mass concentration & Fine optical depth to zero if tau is -ve.         
       IF (AOD(iwave_553) .LT. 0.0) then  
          Qcontrol_special_land=2 
          Quality_Flag_for_retr=5
            MASSCON=0.0 
              DO IWAV = 1, NLWAV 
                AODF(IWAV) = 0.00 
              ENDDO
        ENDIF                 
         
C If -0.10 <= AOD <= -0.05 then set AOD(all waves) = -0.05 and Quality = zero

        IF (AOD(iwave_553) .GE. -0.10 
     +    .AND. AOD(iwave_553) .LE. -0.05) THEN 

c  
            IF (IPROCE .GT. 1) THEN
              AOD(iwave_553) = -0.05
              AOD(iwave_466) = -0.05 
            ELSE
c Set AOD to -0.05, AODFine to 0.00         
              DO IWAV = 1, NLWAV
                AOD(IWAV) = -0.05   
              ENDDO
            ENDIF
         ENDIF
                                       
       
        
      
        
C set quality for the number of pixels......   
       if( Num_Pixels_Used .ge. 12 .and. Num_Pixels_Used .le.20)then   
       QA_Flag_Land(8)=0 
       Quality_Flag_for_retr=6
       Endif
       if( Num_Pixels_Used .ge. 21 .and. Num_Pixels_Used .le.30) then  
       QA_Flag_Land(8)=1
       Quality_Flag_for_retr=7
       ENdif
        if( Num_Pixels_Used .ge.31 .and. Num_Pixels_Used .le.50)then   
        QA_Flag_Land(8)=2
        Quality_Flag_for_retr=8
       ENDIF
        if( Num_Pixels_Used .ge.51)QA_Flag_Land(8)=3 
        
        SDS_Aerosol_Type(IDATA)=IAER
        SDS_SCAT_ANGLE_land(IDATA)=SCAT_ANGLE_LAND*SCALE2+OFFSET2 
        SDSTAU_corrected(IDATA,1)=(AOD(iwave_466)*SCALE3)+OFFSET3
        SDSTAU_corrected(IDATA,2)=(AOD(iwave_553)*SCALE3)+OFFSET3
        SDSTAU_corrected(IDATA,3)=(AOD(iwave_644)*SCALE3)+OFFSET3
        SDSTAU_corrected_213(IDATA)=(AOD(iwave_212)*SCALE3)+OFFSET3  
c  New........        
        SDSTAU_small_land(IDATA,1)=(AODF(iwave_466)*SCALE3)+OFFSET3
        SDSTAU_small_land(IDATA,2)=(AODF(iwave_553)*SCALE3)+OFFSET3
        SDSTAU_small_land(IDATA,3)=(AODF(iwave_644)*SCALE3)+OFFSET3
        SDSTAU_small_land(IDATA,4)=(AODF(iwave_212)*SCALE3)+OFFSET3
        
        SDS_CLDFRC_land(IDATA)= (New_cloud_num*SCALE3+OFFSET3)
        SDS_dust_weighting(IDATA)=NINT((ETA) * SCALE3 + OFFSET3) 
        SDS_NUMPIXELS_land(IDATA,1)=Num_Pixels_Used
        SDS_NUMPIXELS_land(IDATA,2)=Num_Pixels_Used
        SDS_Fitting_Error_Land(Idata)= ((ERR644*SCALE3) + OFFSET3) 
        SDS_mass_conc_land(IDATA)=MASSCON*SCALE1+OFFSET1
        
! Check  ANGSTROM   
C If optical depth negative then set angs_coeff_land to fill value 
C do not set the flag to zero    
    
       if(Qcontrol_special_land .eq.2)then
         SDS_angs_coeff_land(IDATA)=-9999
       ELSE
        IF (ANGSTROM.LE.5.0 .AND. ANGSTROM.GT.-1.00) THEN
          SDS_angs_coeff_land(IDATA)=ANGSTROM*SCALE3+OFFSET3 
        ELSE  
          SDS_angs_coeff_land(IDATA)=-9999
             QA_Flag_Land(8)=0  
             Quality_Flag_for_retr=9
         ENDIF
         ENDIF
         
C If Procedure is 2 set quality to report optical depths at 0.47 & 55 only

        IF (IPROCE.GT.1)then
           Qcontrol_special_land=1
           Quality_Flag_for_retr=1
C If Procedure is 2  Quality is zero
           QA_Flag_Land(8)=0
        ENDIF       
        

         Quality_flag_forJoint=QA_Flag_Land(8)
C set quality flag 9 & 10 so that Level 3 uses the qulity flag to average        
          QA_Flag_Land(9)=QA_Flag_Land(7)
          QA_Flag_Land(10)=QA_Flag_Land(8)  
      
C   report eta only when optical depth is < 0.2  

        IF (AOD(iwave_553) .lt. 0.2) THEN 
         Quality_Flag_for_retr=10
          SDS_dust_weighting(IDATA)=-9999 
        ENDIF
        
        QA_Flag_Land(11)=Quality_Flag_for_retr
        QA_Flag_Land(12)=IERROR
C
        SDS_ref_land(IDATA,1) = NINT(REFW466_L * SCALE4 + OFFSET4)
        SDS_ref_land(IDATA,2) = NINT(REFW550_L * SCALE4 + OFFSET4)
        SDS_ref_land(IDATA,3) = NINT(REFW644_L * SCALE4 + OFFSET4)
        SDS_ref_land(IDATA,4) = NINT(REFW866_L * SCALE4 + OFFSET4)
        SDS_ref_land(IDATA,5) = NINT(REFW124_L * SCALE4 + OFFSET4)
        SDS_ref_land(IDATA,6) = NINT(REFW164_L * SCALE4 + OFFSET4)
        SDS_ref_land(IDATA,7) = NINT(REFW212_L * SCALE4 + OFFSET4)
c
        SDS_ref_STD_land(IDATA,1) =NINT(SDW466_L * SCALE4 + OFFSET4)
        SDS_ref_STD_land(IDATA,2) =NINT(SDW550_L * SCALE4 + OFFSET4)
        SDS_ref_STD_land(IDATA,3) =NINT(SDW644_L * SCALE4 + OFFSET4)
        SDS_ref_STD_land(IDATA,4) =NINT(SDW866_L * SCALE4 + OFFSET4)
        SDS_ref_STD_land(IDATA,5) =NINT(SDW124_L * SCALE4 + OFFSET4)
        SDS_ref_STD_land(IDATA,6) =NINT(SDW164_L * SCALE4 + OFFSET4)
        SDS_ref_STD_land(IDATA,7) =NINT(SDW212_L * SCALE4 + OFFSET4)
      
        SDS_Surface_Reflectance_Land(IDATA,1) =
     *    (RHOSFC(iwave_466)*SCALE3)+OFFSET3
        SDS_Surface_Reflectance_Land(IDATA,2) = 
     *    (RHOSFC(iwave_644)*SCALE3)+OFFSET3
        SDS_Surface_Reflectance_Land(IDATA,3)  = 
     *    (RHOSFC(iwave_212)*SCALE3)+OFFSET3
     
        IF ( Qcontrol_special_land .eq.1 ) 
     *  CALL FILLVALUE_LAND(IDATA,SDS_Tau_Land_Ocean_img,
     *SDS_Aerosol_Type,SDSTAU_corrected_213,
     *SDS_SCAT_ANGLE_land,SDS_mass_conc_land,SDS_angs_coeff_land,
     *SDS_CLDFRC_land,SDS_dust_weighting,
     *SDS_est_uncer,SDS_RefF_land,SDS_TranF_land,SDS_NUMPIXELS_land,
     *SDSTAU_corrected,SDS_ref_land,SDS_ref_STD_land,
     *SDS_QCONTROL_land,SDSTAU_small_land,
     *SDS_Surface_Reflectance_Land,SDS_Fitting_Error_Land,
     *Qcontrol_special_land)
     
c ENDIF FOR ERROR =0 or error > 0
      ENDIF
       
   
C
C Store QA flags into Quality_Assurance_Land array according to the order
C of bits in MODIS atmosphere QA plan
C
      QA_Temp=0
      CALL BYTE_SET(QA_Flag_Land(7),0,QA_Temp)
      CALL BYTE_SET(QA_Flag_Land(8),1,QA_Temp)
       CALL BYTE_SET(QA_Flag_Land(9),4,QA_Temp)
       CALL BYTE_SET(QA_Flag_Land(10),5,QA_Temp)

      SDS_QCONTROL_land(1,IDATA)=QA_Temp

      QA_Temp=0
      QA_Flag_Land(13)=0
      QA_Flag_Land(14)=0
      CALL BYTE_SET(QA_Flag_Land(11),0,QA_Temp)
      CALL BYTE_SET(QA_Flag_Land(12),4,QA_Temp)
c      CALL BYTE_SET(QA_Flag_Land(13),6,QA_Temp)
c      CALL BYTE_SET(QA_Flag_Land(14),7,QA_Temp)

      SDS_QCONTROL_land(2,IDATA)=QA_Temp

      QA_Temp=0
      QA_Flag_Land(17)=3
      QA_Flag_Land(18)=3

      CALL BYTE_SET(QA_Flag_Land(15),0,QA_Temp)
      CALL BYTE_SET(QA_Flag_Land(16),2,QA_Temp)
      CALL BYTE_SET(QA_Flag_Land(17),4,QA_Temp)
      CALL BYTE_SET(QA_Flag_Land(18),6,QA_Temp)

      SDS_QCONTROL_land(3,IDATA)=QA_Temp

      QA_Temp=0
      QA_Flag_Land(19)=1
      CALL BYTE_SET(QA_Flag_Land(19),0,QA_Temp)
 
      SDS_QCONTROL_land(4,IDATA)=QA_Temp

      SDS_QCONTROL_land(5,IDATA)=0
      
      
        
      
c         write(37,*)idata,Ierror,IPROCE,IAER,Quality_flag_forJoint, 
c     *  QA_Flag_Land(8),quality_land, Ret_Quality_cirrus, ERR644,ETA,
c     *  RHOSFC(iwave_212),AOD(iwave_553),SDS_dust_weighting(IDATA) 
   
C
C   Re-initialized working variables
C
 

      DO I=1,19 
        QA_Flag_Land(I)=0
      ENDDO
     
      IAER=-9999
      ICLDBLUE=-9999
      ICLDRED=-9999
      IPROCE=-9999 
      IERROR=-9999
      REFW466_L=-9999
      REFW550_L=-9999
      REFW644_L=-9999
      REFW124_L=-9999
      REFW164_L=-9999
      REFW866_L=-9999
      REFW212_L=-9999
      SDW466_L=-9999
      SDW550_L =-9999
      SDW644_L=-9999
      SDW124_L=-9999
      SDW164_L=-9999
      SDW866_L=-9999
      SDW212_L=-9999 
C      Re-Initialize for next run
      ETA = -9999
      ERR644 = -9999
      MASSCON = -9999
      ANGSTROM = -9999
      DO IWAV = 1, NLWAV
         AOD(IWAV) = -9999
         AODF(IWAV) = -9999
         AODC(IWAV) = -9999
         RHOSFC(IWAV) = -9999
      ENDDO
C
100   FORMAT(i4,10(10f10.4,/))
     
      RETURN
      END

C***********************************************************************
      SUBROUTINE COMPUTE_SCATTANGLE_LAND(MTHET0,MTHET,MDPHI,IDATA,
     *SCAT_ANGLE_LAND)
C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C              This subroutine computes scattering angle from MODIS
C              geometry.
C
C!INPUT PARAMETERS:
C
C       MEHET0            Solar zenith angle
C       MTHET             Satellite Viewing angle
C       MDPHI             Relative azimuth angle
C       IDATA             Index of 10x10 km box
C
C!OUTPUT PARAMETERS:
C
C       SCAT_ANGLE_LAND   Scattering angle
C
C!REVISION HISTORY:
C 02/02/98 achu
C identified input/output arguments in INPUT / OUTPUT PARAMETERS sections
C in prolog.
C
C 01/28/98 fhliang
C fixed prolog.
C
C!TEAM-UNIQUE HEADER:
C
C Developed by MODIS Aerosol Team at NASA GSFC, Greenbelt, MD
C
C!DESIGN NOTES:
C
C!END
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'mod04.inc'
      REAL SCAT_ANGLE_LAND
      REAL MTHET0,MTHET,MDPHI
      INTEGER IDATA
      SAVE

C      DTR=ACOS(-1.)/180.
C      RTD=180./ACOS(-1.)
      SCAT_ANGLE_LAND=0.0

C
C Scattering angle of MODIS geometry
C

       SCAT_ANGLE_LAND= -COS(MTHET0*DTR)*COS(MTHET*DTR)
     *                  +SIN(MTHET0*DTR)*SIN(MTHET*DTR)
     *                  *COS(MDPHI*DTR)

       SCAT_ANGLE_LAND= ACOS(SCAT_ANGLE_LAND)*RTD

       RETURN
       END

C*****************************************************************

      SUBROUTINE  statistics_land(W470_SYN,W659_SYN,W213_SYN,CLDMSK_500,
     *      SDS_Mean_Reflectance_Land_All,
     *      SDS_SDev_Reflectance_Land_All,
     *      SDS_Path_Radiance_Land,
     *      SDS_Critical_Reflectance_Land,
     *      SDS_Error_Crit_Reflectance_Land,
     *      SDS_Error_Path_Radiance_Land,
     *      SDS_QFlag_Critical_Ref_Land,SDS_QFlag_Path_Radiance_Land,
     *  START_1KM,END_1KM,Idata,iscan, MTHET0,MTHET,
     *  SDS_QCONTROL_CritRef_land,SCAT_ANGLE_LAND,Rayleigh_look,W865_SYN)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C              This subroutine computes different Statistics like path radiance,slopes and
C error for land
C
C!INPUT PARAMETERS:
C
C        W470_SYN        Reflectance for 0.470 um 
C        W659_SYN        Reflectance for 0.659 um   
C       W213_SYN         Reflectance for 2.13 um            
C       CLDMSK_500    Cloud mask   
C       MEHET0             Solar zenith angle
C       MTHET               Satellite Viewing angle
C       IDATA                Index of 10x10 km box
C
C!OUTPUT PARAMETERS:
C
C      SDS_Mean_Reflectance_Land_All 
C      SDS_SDev_Reflectance_Land_All 
C      SDS_Path_Radiance_Land 
C      SDS_Critical_Reflectance_Land 
C      SDS_Error_Crit_Reflectance_Land 
C      SDS_Error_Path_Radiance_Land 
C      SDS_QFlag_Critical_Ref_Land
C     SDS_QFlag_Path_Radiance_Land
C!REVISION HISTORY:
CDeveloped 4/2001
C
C!TEAM-UNIQUE HEADER:
C
C Developed by Shana Mattoo (Aerosol Team at NASA GSFC, Greenbelt, MD
C
C!DESIGN NOTES:
C
C!END
C-----------------------------------------------------------------------
         IMPLICIT NONE
         SAVE
       INCLUDE 'mod04.inc'
       REAL W659_SYN(4*ISWATH,4*ILINE),W865_SYN(4*ISWATH,4*ILINE),
     *     W470_SYN(2*ISWATH,2*ILINE),W550_SYN(2*ISWATH,2*ILINE),
     *     W124_SYN(2*ISWATH,2*ILINE),W164_SYN(2*ISWATH,2*ILINE),
     *     W213_SYN(2*ISWATH,2*ILINE),
     *     W443o_SYN(ISWATH,ILINE),W551o_SYN(ISWATH,ILINE),
     *     W667o_SYN(ISWATH,ILINE),W869o_SYN(ISWATH,ILINE)
      REAL arefw659,arefw865,AVE,SDEV,array(2*IGRIDX*2*IGRIDY)
      REAL ref_interm(3,2*IGRIDX*2*IGRIDY),SCAT_ANGLE_LAND
      REAL diff_full_half,final_diff_full_half(Land_Sol1)
      INTEGER CLDMSK_500(2*ISWATH,2*ILINE),Flag_full
      INTEGER Final_Flag_full(Land_Sol1)
      INTEGER START_1KM,END_1KM,JJ,II
      INTEGER  QCONTROL_land_wav1,QCONTROL_land_wav2
      INTEGER *2  SDS_Mean_Reflectance_Land_All(NUMCELLS,Land_Sol3),
     &SDS_SDev_Reflectance_Land_All(NUMCELLS,Land_Sol3),
     &SDS_Path_Radiance_Land(NUMCELLS,Land_Sol1),
     &  SDS_Critical_Reflectance_Land(NUMCELLS,Land_Sol1),
     &  SDS_Error_Crit_Reflectance_Land(NUMCELLS,Land_Sol1),
     &  SDS_Error_Path_Radiance_Land(NUMCELLS,Land_Sol1),
     &  SDS_QFlag_Critical_Ref_Land(NUMCELLS,Land_Sol1),
     &  SDS_QFlag_Path_Radiance_Land(NUMCELLS,Land_Sol1)
          BYTE  SDS_QCONTROL_CritRef_land(QA_LAND,NUMCELLS)
      INTEGER NUMDATA_659,NUMDATA_865,NUMDATA_470,ij
      integer index_error_flags(Land_Sol1,12),
     * NUMDATA_550,NUMDATA_124,NUMDATA_164,NUMDATA_213,NUMDATA_443o,
     * NUMDATA_551o,NUMDATA_667o, NUMDATA_869o
      INTEGER JMASK,IMASK,IBLUE,JBLUE,IXX,IYY,IX,IY,IIJ,IIK,No_water
      INTEGER IDUMMY,kdata,iwav,iscan,idata,numdata,inum,ik,index_wave
      Real array_47(2*ISWATH*2*ILINE),array_67(2*ISWATH*2*ILINE),SIG
      real A,B,SIGA,SIGB,CHI2,Q,array_213(2*ISWATH*2*ILINE),abdev
      real Average_allwaves(Land_Sol3),standard_allwaves(Land_Sol3)
      real log_standard,MTHET0,MTHET 
      integer MWT,INDX(2*ISWATH*2*ILINE),numaver1
      real Rayleigh_look(Land_Sol1),co_variance,Surface
      real THRSHOLD1,THRSHOLD2,SCATMODIS,Diff_path,Path_rad(Land_Sol1)
      real Error_Pathrad(Land_Sol1),critic_ref(Land_Sol1)
      real error_critic(Land_Sol1),slope(Land_Sol1)
      integer weighting_critical(Land_Sol1),weighting_path(Land_Sol1),
     *     Qa_Flag_final(Land_Sol1)
      real chisq_for_flag,diff_critic_ref
      real final_chisq_for_flag(Land_Sol1) 
      real final_diff_critic_ref(Land_Sol1)
      integer index_outlayer(2*ISWATH*2*ILINE),index_inlayer(2*ISWATH*2*ILINE)
c intialize
        No_water=0
        aREFW659=0.0
        NUMDATA_470=0
        NUMDATA_659=0
        NUMDATA_213=0
         numaver1=0
         numdata=0
c

c loop around 1 km

        DO   IYY = 1,IGRIDY
         IMASK=4*IYY-3
         IBLUE=2*IYY-1
        DO  IXX=START_1KM,END_1KM
          JMASK=4*IXX-3
          JBLUE=2*IXX-1
c loop around 500 meters
       DO IY =IBLUE,2*IYY
           IIJ=2*IY-1
         DO IX= JBLUE,2*IXX
            IIK=2*IX-1
         IF(CLDMSK_500(IX,IY) .GT. 0) THEN
c   For each clear pixel find product of
c   Reflectance for 0.659 um
             numaver1=0
             aREFW659=0.0
c loop around 250 meters
            DO JJ= IIJ,2*IY
            DO II= IIK,2*IX
       IF(W659_syn(II,JJ).GT. 0.0 .AND. W659_syn(II,JJ).LE. 1.0) THEN
       numaver1=numaver1+ CLDMSK_500(IX,IY)
       aREFW659=aREFW659+(W659_syn(II,JJ)*CLDMSK_500(IX,IY))
        ENDIF
C ENDDO FOR 250METER RESOLUTION
            ENDDO
            ENDDO
c loop around 250 meters to find water pixels....
            No_water=0
            DO JJ= IIJ,2*IY
            DO II= IIK,2*IX
       IF(W865_SYN(IX,IY)-W659_syn(II,JJ).GT.0.1) THEN
       No_water=No_water+1 
        ENDIF
C ENDDO FOR 250METER RESOLUTION
            ENDDO
            ENDDO
c All pixels for 250 resolution have to be clear
      IF( numaver1 .EQ.4 .and. No_water .eq.4) then
c
       NUMDATA_659=NUMDATA_659+1

c    Foe clear pixel find product all Reflectances for
c    channel 3-channel 7 and cloud mask
C
C     WAVE 0.470 UM
C
      IF(W470_SYN(IX,IY).GT. 0.0 .AND.W470_SYN(IX,IY).LE. 1.0) THEN
          NUMDATA_470= NUMDATA_470+1

         ENDIF



C     WAVE 2.13 UM
C

        IF(W213_syn(IX,IY).GT. 0.0 .AND. W213_syn(IX,IY).LE. 1.0) THEN
         NUMDATA_213= NUMDATA_213+1

         ENDIF
c endif or all good  pixels in 250 meter resolution
         endif
      
c  register same pixels.........
       IF( NUMDATA_470 .eq.NUMDATA_659 .and.
     *   NUMDATA_659 .eq.NUMDATA_213 .and.numaver1 .EQ.4 ) then
c eliminate water pixels
        if( NUMDATA_659 .gt. 0 .and.W213_syn(IX,IY) .gt.0.01 ) then
        numdata=numdata+1
        ref_interm(1, numdata)=(W470_syn(IX,IY)*  CLDMSK_500(IX,IY))
        ref_interm(2, numdata)=aREFW659/numaver1
        ref_interm(3, numdata)=(W213_syn(IX,IY)*CLDMSK_500(IX,IY))
               endif
c  endif for equal pixels
        ENDif
c endif for all cloudy pixels
       ENDIF
c    ENDDO for 500 * 500 meter 20*20 box
          ENDDO
          ENDDO
C      ENDDO's for 1km pixels for 10*10 box
         ENDDO
         ENDDO
c Compute average and standard deviation.....
        AVE=0.0
        SDEV=0.0

c   fine average and standard deviation for data if cloud free and good pixels
c   meet the criteria

      IF( numdata .gt. 30) then
c order the 2.13 data
       Do kdata=1,numdata
            array(kdata)=ref_interm(3,kdata)
       Enddo
         CALL INDEXX(numdata,array,INDX)
c
         Do Iwav =1 ,Land_Sol3
            Do kdata=1,numdata
            array(kdata)=ref_interm(iwav,INDX(kdata))
           enddo
         call ave_std(array,numdata,AVE,SDEV)
        Average_allwaves(iwav)=ave
        standard_allwaves(iwav)=SDEV

c Save SDS for HDF file.......

       SDS_Mean_Reflectance_Land_All(IDATA,iwav)=
     *nint(AVE * SCALE4 + OFFSET4)
       SDS_SDev_Reflectance_Land_All(IDATA,iwav)=
     * nint(SDEV * SCALE4 + OFFSET4)
c calculate log deviation for only 2.13 to be used in tests
         Do kdata=1,numdata
            array(kdata)=alog(ref_interm(3,INDX(kdata)))
           enddo
         call ave_std(array,numdata,AVE,SDEV)
         log_standard=SDEV
c Enddo for  Wavelengths
      enddo

c  call fitting routine only when numdata meets criteria...
            Do kdata=1,numdata
            array_47(kdata)=ref_interm(1,INDX(kdata))
            array_67(kdata)=ref_interm(2,INDX(kdata))
            array_213(kdata)=ref_interm(3,INDX(kdata))
           enddo 

cfit for wav 47 * 2.13 um
       do Iwav =1,2
         Path_rad(iwav)=-99
         slope(iwav)=-99
          kdata=0
         if( Iwav .eq.1)then 
         Surface=Surface_wav47
        call medfit(array_213,array_47,numdata,A,B,Siga,Sigb,abdev,
     *Average_allwaves,standard_allwaves,log_standard,iscan,idata,
     *SDS_QCONTROL_CritRef_land,SCAT_ANGLE_LAND,co_variance,
     *Flag_full,diff_full_half,surface,chisq_for_flag,diff_critic_ref,
     * index_outlayer,index_inlayer)
        
c fit for wav 67 * 2.13 um
       Else 
        Surface=Surface_wav66
       call medfit(array_213,array_67,numdata,A,B,Siga,Sigb,abdev,
     *Average_allwaves,standard_allwaves,log_standard,iscan,idata,
     *SDS_QCONTROL_CritRef_land,SCAT_ANGLE_LAND,co_variance,
     *Flag_full,diff_full_half,surface,chisq_for_flag,diff_critic_ref,
     *index_outlayer,index_inlayer) 
        endif
c
c A is path radiance B is slope sigma error in path radiance sigb is error in slope
c index wave sets the index in the fillvalue routine
c write only if slope and path radiance are positive
        index_wave=iwav
      if( A .gt. 0 .and. B .gt.0) then
        Path_rad(iwav)=A
        slope(iwav)=B
        Error_Pathrad(iwav)=SIGA
        critic_ref(iwav)=A/(1.-B/Surface)
         error_critic(iwav)=sqrt((SIGA**2)+(2.*critic_ref(iwav)*Co_variance)+
     *                        (SIGB**2)*(critic_ref(iwav)**2))
         Final_Flag_full(iwav)=Flag_full
         Final_diff_full_half(iwav)= diff_full_half
         final_chisq_for_flag(iwav)=chisq_for_flag
         final_diff_critic_ref(iwav)=diff_critic_ref
      Else
           kdata=kdata+1
         CALL FILLVALUE_LAND_extra(IDATA,index_wave,
     *      SDS_Mean_Reflectance_Land_All,
     *      SDS_SDev_Reflectance_Land_All,
     *      SDS_Path_Radiance_Land,
     *      SDS_Critical_Reflectance_Land,
     *      SDS_Error_Crit_Reflectance_Land,
     *      SDS_Error_Path_Radiance_Land,
     *      SDS_QFlag_Critical_Ref_Land,
     *      SDS_QFlag_Path_Radiance_Land)
              QCONTROL_land_wav1=0
              QCONTROL_land_wav2=0
              call  Fill_QAflag_CritRef_land(QCONTROL_land_wav1,
     *QCONTROL_land_wav2, SDS_QCONTROL_CritRef_land,Idata)
       Endif
c ENDDO for wavelength
       Enddo
        CAll Flag_analysis(Rayleigh_look,SCAT_ANGLE_LAND,Path_rad,
     *   Error_Pathrad,critic_ref,error_critic,index_error_flags,
     *   Final_Flag_full,Final_diff_full_half,weighting_critical,
     *  weighting_path,Qa_Flag_final,final_chisq_for_flag,
     *final_diff_critic_ref)
c  Quality flag for final qa flag
        QCONTROL_land_wav1=Qa_Flag_final(1)
        QCONTROL_land_wav2=Qa_Flag_final(2)
        call  Fill_QAflag_CritRef_land(QCONTROL_land_wav1,
     *QCONTROL_land_wav2,
     * SDS_QCONTROL_CritRef_land,Idata)
        Do  iwav=1,2
         index_wave=iwav
      if( Path_rad(iwav) .gt. 0 .and. slope(iwav) .gt. 0) then
c path radiance is corrected for the view angle...........
       SDS_Path_Radiance_Land(IDATA,iwav)=
     $  nint((Path_rad(Iwav)*(COS(MTHET*DTR)))*SCALE4 + OFFSET4)
c Error in path radiance is corrected for the view angle...........
      SDS_Error_Path_Radiance_Land(IDATA,iwav)=
     $ nint((Error_Pathrad(iwav)*(COS(MTHET*DTR)))*SCALE4 + OFFSET4)
c Critical Reflactance
        SDS_Critical_Reflectance_Land(IDATA,iwav)=
     *          nint(critic_ref(iwav)* SCALE4 + OFFSET4)
c Error Critical Reflactance
          SDS_Error_Crit_Reflectance_Land(IDATA,Iwav)=
     *nint(error_critic(iwav)* SCALE4 + OFFSET4)
c Quality flag for PAth radiance
      SDS_QFlag_Critical_Ref_Land(IDATA,Iwav)=
     *               weighting_critical(iwav)
c Quality flag for PAth radiance
      SDS_QFlag_Path_Radiance_Land(IDATA,Iwav)=
     *           weighting_path(iwav)
       ELSE
      CALL FILLVALUE_LAND_extra(IDATA,index_wave,
     *      SDS_Mean_Reflectance_Land_All,
     *      SDS_SDev_Reflectance_Land_All,
     *      SDS_Path_Radiance_Land,
     *      SDS_Critical_Reflectance_Land,
     *      SDS_Error_Crit_Reflectance_Land,
     *      SDS_Error_Path_Radiance_Land,
     *      SDS_QFlag_Critical_Ref_Land,
     *      SDS_QFlag_Path_Radiance_Land)
              QCONTROL_land_wav1=0
              QCONTROL_land_wav2=0
              call  Fill_QAflag_CritRef_land(QCONTROL_land_wav1,
     *QCONTROL_land_wav2, SDS_QCONTROL_CritRef_land,Idata)
       Endif
C ENDDO FOR WAVELENGTHS
       ENDDO

         ELSE
c  call fill values
             index_wave=3
            CALL FILLVALUE_LAND_extra(IDATA,index_wave,
     *      SDS_Mean_Reflectance_Land_All,
     *      SDS_SDev_Reflectance_Land_All,
     *      SDS_Path_Radiance_Land,
     *      SDS_Critical_Reflectance_Land,
     *      SDS_Error_Crit_Reflectance_Land,
     *      SDS_Error_Path_Radiance_Land,
     *      SDS_QFlag_Critical_Ref_Land,SDS_QFlag_Path_Radiance_Land)
         QCONTROL_land_wav1=0
         QCONTROL_land_wav2=0
        call  Fill_QAflag_CritRef_land(QCONTROL_land_wav1,
     *QCONTROL_land_wav2,
     * SDS_QCONTROL_CritRef_land,Idata)
c endif  for criteria for number of points
        ENDIF
100     format( 2i7,f10.3,10i15)
         RETURN
         END

c
         SUBROUTINE MEDFIT(X,Y,NDATA,A,B,Siga,Sigb,ABDEV,
     *Average_allwaves,standard_allwaves,log_standard, iscan,idata,
     *SDS_QCONTROL_CritRef_land,SCAT_ANGLE_LAND,co_variance,
     *Flag_full,diff_full_half,surface,chisq_for_flag,diff_critic_ref,
     *index_outlayer,index_inlayer)
C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C              This subroutine was taken from numrical recepies but was extensivily
C modified. The routine is basic fitting routine ands calculates slopes etc.

C!INPUT PARAMETERS:
C
C!INPUT PARAMETERS:
C     REAL             X                   elements of array X 
C     REAL             X                   elements of array Y 
C    INTEGER      NDATA          Number of data pointa
C!OUTPUT PARAMETERS:
C
C    A                 Intercept
C    B                 Slope
C    Siga             Error in Intercept
C    Sigb             Error in Slope
C    ABDEV       Absolute Deviation
C
C!REVISION HISTORY:
CDeveloped 4/2001
C
C!TEAM-UNIQUE HEADER:
C
C Developed by Shana Mattoo (Aerosol Team at NASA GSFC, Greenbelt, MD
C
C!DESIGN NOTES:
C
C!END
C-----------------------------------------------------------------------
         IMPLICIT NONE
         SAVE
         INCLUDE 'mod04.inc'
         INTEGER NMAX,NDATA,j,ik,NDATAT,numpoints_outerlayer
         integer  numpoints_half,iscan,idata
        integer index_outlayer(ndata),index_inlayer(ndata)
       integer  numpoints_inlayer,numpoints_half_inlayer,im
       integer numpoints_outerlayer_half,Flag_full
      PARAMETER (NMAX=1000)
      EXTERNAL ROFUNC
c rhucek 02/14/06
       real       Rel_Equality_EPS,          Zero_Eps
       parameter (Rel_Equality_Eps=0.000001, Zero_Eps = 0.000001)
       
       real Average_allwaves(3),a,b,abdev,aa,ABDEVT,arr,del,CHISQ
       real standard_allwaves(3),log_standard,Co_variance
       real st_dev,b1,F1,b2,f2,f,xt,yt,bb,ROFUNC
      COMMON /ARRAYS/ NDATAT,XT(NMAX),YT(NMAX),ARR(NMAX),AA,ABDEVT
      REAL X(NDATA),Y(NDATA) 
       real Siga,Sigb,diff_full_half
       real SX,SY,SXY,SXX,SX_half,sy_half,SXY_half,SXX_half
       real SX_outlayer,SY_outlayer,SXY_outlayer,SXX_outlayer
      real SX_outlayer_half,SY_outlayer_half
      real SXY_outlayer_half,SXX_outlayer_half
       real  SX_noutlayer,SY_noutlayer,SXY_noutlayer,SXX_noutlayer
       real  SX_half_noutlayer,SY_half_noutlayer,SXY_half_noutlayer,
     *SXX_half_noutlayer,SCAT_ANGLE_LAND
       real AA_noutlayer,BB_noutlayer,CHISQ_noutlayer
       real AA_half_noutlayer,BB_half_noutlayer,CHISQ_half_noutlayer
       real  Sigma_all_one,Sigma_all_two,Sigma_half_one,Sigma_half_two
       BYTE  SDS_QCONTROL_CritRef_land(QA_LAND,NUMCELLS)
       real Co_variance_noutlayer,Co_variance_noutlayer_half,surface
       real chisq_for_flag,diff_critic_ref
c intialize
      DO  J=1,NDATA
               index_outlayer(j)=0
               index_inlayer(j)=0
      ENDDO
      Siga=0.
      Sigb=0.
      SX=0.
      SY=0.
      SXY=0.
      SXX=0.
      SX_half=0.
      sy_half=0.
      SXY_half=0.
      SXX_half=0.
      numpoints_half=0
c  compute sums for whole array and array till Average value
      DO 11 J=1,NDATA
        XT(J)=X(J)
        YT(J)=Y(J)
        SX=SX+X(J)
        SY=SY+Y(J)
        SXY=SXY+X(J)*Y(J)
        SXX=SXX+X(J)**2
      if(X(J).ge.X(1).and.X(J).le.Average_allwaves(3))then
       numpoints_half= numpoints_half+1
      SX_half=SX
      SY_half=SY
      SXY_half=SXY
      SXX_half=SXX
      endif
11    CONTINUE
c  compute  slope,intecept and chisqare
      NDATAT=NDATA
      DEL=NDATA*SXX-SX**2
      AA=(SXX*SY-SX*SXY)/DEL
      BB=(NDATA*SXY-SX*SY)/DEL
      CHISQ=0.
      DO 12 J=1,NDATA
        CHISQ=CHISQ+(Y(J)-(AA+BB*X(J)))**2
12    CONTINUE
      SIGB=SQRT(CHISQ/DEL)
      B1=BB
      F1=ROFUNC(B1)
      B2=BB+SIGN(3.*SIGB,F1)
      F2=ROFUNC(B2)
1     IF(F1*F2.GT.0.)THEN
        BB=2.*B2-B1
        B1=B2
        F1=F2
        B2=BB
        F2=ROFUNC(B2)
        GOTO 1
      ENDIF
      SIGB=0.01*SIGB
2     IF(ABS(B2-B1).GT.SIGB)THEN
        BB=0.5*(B1+B2)

        IF(BB.EQ.B1 .OR. BB.EQ.B2) GOTO 3

        F=ROFUNC(BB)
        IF(F*F1.GE.0.)THEN
          F1=F
          B1=BB
        ELSE
          F2=F
          B2=BB
        ENDIF
        GOTO 2
      ENDIF
3     A=AA
      B=BB
      ABDEV=ABDEVT/NDATA
c compute indexs for outer layers to elminate them
      ik=0
      im=0
      numpoints_outerlayer=0
      numpoints_outerlayer_half=0
      st_dev=2*SQRT(CHISQ/(NDATA-1))
      DO  J=1,NDATA
         if (Y(J)-(A+B*X(J)) .gt. st_dev)then
               ik=ik+1
               index_outlayer(ik)=j
       else
               im=im+1
               index_inlayer(im)=j
       endif
      ENDDO
       numpoints_outerlayer=ik

c check the points in outer layer till average 

       do J=1,numpoints_half
       if(index_outlayer(j) .gt.0 .and.
     *   index_outlayer(j) .le.numpoints_half) then
      numpoints_outerlayer_half=numpoints_outerlayer_half+1

       endif
      enddo
c compute sums for outlayers
c
      SX_outlayer=0.
      SY_outlayer=0.
      SXY_outlayer=0.
      SXX_outlayer=0.
      SX_outlayer_half=0.
      SY_outlayer_half=0.
      SXY_outlayer_half=0.
      SXX_outlayer_half=0.
      SX_noutlayer=0
      SY_noutlayer=0.
      SXY_noutlayer=0.
      SXX_noutlayer=0.
      SX_half_noutlayer=0
      SY_half_noutlayer=0.
      SXY_half_noutlayer=0.
      SXX_half_noutlayer=0.

         DO  J=1,numpoints_outerlayer
        SX_outlayer=SX_outlayer+X(index_outlayer(j))
        SY_outlayer=SY_outlayer+Y(index_outlayer(j))
        SXY_outlayer=SXY_outlayer+
     *X(index_outlayer(j))*Y(index_outlayer(j))
        SXX_outlayer=SXX_outlayer+X(index_outlayer(j))**2
        ENDDO

c compute sum without outer_layers
c
c     for all points
      SX_noutlayer=SX - SX_outlayer
      SY_noutlayer=SY - SY_outlayer
      SXY_noutlayer=SXY - SXY_outlayer
      SXX_noutlayer=SXX -  SXX_outlayer

c     for half of points till average
       DO  J=1,numpoints_outerlayer_half
        SX_outlayer_half=SX_outlayer_half+X(index_outlayer(j))
        SY_outlayer_half=SY_outlayer_half+Y(index_outlayer(j))
        SXY_outlayer_half=SXY_outlayer_half+
     *X(index_outlayer(j))*Y(index_outlayer(j))
        SXX_outlayer_half=SXX_outlayer_half+X(index_outlayer(j))**2
        ENDDO
      SX_half_noutlayer=SX_half - SX_outlayer_half
      SY_half_noutlayer=SY_half - SY_outlayer_half
      SXY_half_noutlayer=SXY_half - SXY_outlayer_half
      SXX_half_noutlayer=SXX_half -  SXX_outlayer_half

c  compute new A and B for half of points and all points without outer layers

c    for all points without outer layer

       numpoints_inlayer  = NDATA - numpoints_outerlayer

c  compute chisq for half and all points without outerlayer


        AA_noutlayer=0.
        BB_noutlayer=0.
        CHISQ_noutlayer=0.
        Sigma_all_one=0.
         Sigma_all_two=0.
        Co_variance_noutlayer=0.0

         DEL=numpoints_inlayer*SXX_noutlayer-SX_noutlayer**2
        if( DEL .gt.0.0) then
         AA_noutlayer=
     * (SXX_noutlayer*SY_noutlayer - SX_noutlayer * SXY_noutlayer)/DEL
         BB_noutlayer=
     *( numpoints_inlayer*SXY_noutlayer-SX_noutlayer*SY_noutlayer)/DEL
c calculate     CHISQ
          DO   J=1,numpoints_inlayer
          CHISQ_noutlayer=CHISQ_noutlayer+
     *             (Y( index_inlayer(j))-
     *      (AA_noutlayer+BB_noutlayer*X( index_inlayer(j))))**2
           ENDDO
       endif

      
      
c calculate Sigma all points without outer layers
         numpoints_half_inlayer  =numpoints_half -
     *             numpoints_outerlayer_half
c   calculate    Co_variance
            if( DEL .gt.0.0 .and.numpoints_inlayer .gt.0 ) then
          Co_variance_noutlayer=
     * -(1./DEL)*(SX_noutlayer*(CHISQ_noutlayer/numpoints_inlayer))
        Sigma_all_one=((1./DEL)*(SXX_noutlayer*CHISQ_noutlayer))
        Sigma_all_two=((numpoints_inlayer/DEL)*CHISQ_noutlayer)
        Sigma_all_one=SQRT(Sigma_all_one)
     *                          /SQRT(real(numpoints_inlayer))  
        Sigma_all_two=SQRT(Sigma_all_two)
     *                           /SQRT(real(numpoints_inlayer))
         endif
    
       AA_half_noutlayer=0.        
       BB_half_noutlayer=0.
      CHISQ_half_noutlayer=0.
       Sigma_half_one=0.
       Sigma_half_two=0.
      Co_variance_noutlayer_half=0.0
c
      DEL=numpoints_half_inlayer*SXX_half_noutlayer-SX_half_noutlayer**2
         if( DEL .gt.0.0) then
        AA_half_noutlayer=
     * (SXX_half_noutlayer*SY_half_noutlayer -
     *SX_half_noutlayer * SXY_half_noutlayer)/DEL
       BB_half_noutlayer=( numpoints_half_inlayer*
     *SXY_half_noutlayer-SX_half_noutlayer*SY_half_noutlayer)/DEL
c calculate Sigma half points without outer layers

       DO   J=1, numpoints_half_inlayer
          CHISQ_half_noutlayer=CHISQ_half_noutlayer+
     * (Y( index_inlayer(j))-
     *(AA_half_noutlayer+BB_half_noutlayer*X( index_inlayer(j))))**2
        ENDDO
        endif
c   calculate    Co_variance
          if(DEL .gt.0.0 .and.  numpoints_half_inlayer .gt.0) then
          Co_variance_noutlayer_half=
     * -(1./DEL)*(SX_half_noutlayer*
     *  (CHISQ_half_noutlayer/numpoints_half_inlayer))
         Sigma_half_one=((1./DEL)*(SXX_half_noutlayer
     $         *CHISQ_half_noutlayer))
        Sigma_half_two=((numpoints_half_inlayer/DEL)
     $   *   CHISQ_half_noutlayer) 
          Sigma_half_one=SQRT(Sigma_half_one)
     $     /SQRT(real(numpoints_half_inlayer))
          Sigma_half_two=SQRT(Sigma_half_two)
     $                             /SQRT(real(numpoints_half_inlayer))

           endif
c  consistancy checks for positivity of intercept and slopes

         if((AA_half_noutlayer .gt. 0.0 .and.AA_half_noutlayer .le. 1.0)
     * .and.(BB_half_noutlayer .gt. 0.0 .and .
     *       BB_half_noutlayer .le. 1.0) .or.
     * (AA_noutlayer .gt.0.0 .and.AA_noutlayer .le.1.0) .and.
     * (BB_noutlayer .gt. 0.0 .and. BB_noutlayer.le. 1.0)) then
c
c   call subroutine tests to test if full or half of the points are to be used 
c   for fit
       Call tests( AA_half_noutlayer,BB_half_noutlayer,AA_noutlayer,
     *BB_noutlayer,Sigma_half_one,Sigma_half_two,Sigma_all_one,
     *Sigma_all_two,Average_allwaves,standard_allwaves,log_standard,
     *iscan,idata,A,B,Siga,Sigb,
     *SCAT_ANGLE_LAND,numpoints_inlayer,numpoints_half_inlayer,
     *CHISQ_noutlayer,CHISQ_half_noutlayer,CHISQ,Flag_full,diff_full_half,
     *Co_variance_noutlayer,Co_variance_noutlayer_half,Co_variance,
     *chisq_for_flag,diff_critic_ref,surface)
       else
        A=-99.0
        B=-99.0
        siga=-99.0
        sigb=-99.0
        endif
      RETURN
      END
c
      FUNCTION ROFUNC(B)
C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C              This function is taken from numrical recepies 
C
C!INPUT PARAMETERS:
C     REAL             B                   
C
C!OUTPUT PARAMETERS:
C
C   ROFUN
C
C!REVISION HISTORY:
CDeveloped 4/2001
C
C!TEAM-UNIQUE HEADER:
C
C 
C
C!DESIGN NOTES:
C
C!END
C 
C-----------------------------------------------------------------------
         SAVE
      INTEGER NMAX
      PARAMETER (NMAX=1000)
      COMMON /ARRAYS/ NDATA,X(NMAX),Y(NMAX),ARR(NMAX),AA,ABDEV
      N1=NDATA+1
      NML=N1/2
      NMH=N1-NML
      DO 11 J=1,NDATA
        ARR(J)=Y(J)-B*X(J)
11    CONTINUE
      CALL SORT_data(NDATA,ARR)
      AA=0.5*(ARR(NML)+ARR(NMH))
      SUM=0.
      ABDEV=0.
      DO 12 J=1,NDATA
        D=Y(J)-(B*X(J)+AA)
        ABDEV=ABDEV+ABS(D)
        SUM=SUM+X(J)*SIGN(1.0,D)
12    CONTINUE
      ROFUNC=SUM
      RETURN
      END
      SUBROUTINE SORT_data(N,RA)
      DIMENSION RA(N)
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
        ELSE
          RRA=RA(IR)
          RA(IR)=RA(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(RA(J).LT.RA(J+1))J=J+1
          ENDIF
          IF(RRA.LT.RA(J))THEN
            RA(I)=RA(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
      GO TO 10
       RETURN
      END
      Subroutine tests ( AA_half_noutlayer,BB_half_noutlayer,
     *AA_noutlayer,
     *BB_noutlayer,Sigma_half_one,Sigma_half_two,Sigma_all_one,
     *Sigma_all_two,Average_allwaves,standard_allwaves,log_standard,
     *iscan,idata,A,B,Siga,Sigb,
     *SCAT_ANGLE_LAND,numpoints_inlayer,numpoints_half_inlayer,
     *CHISQ_noutlayer,CHISQ_half_noutlayer,CHISQ,Flag_full,
     *diff_full_half,Co_variance_noutlayer,Co_variance_noutlayer_half,
     *Co_variance,chisq_for_flag,diff_critic_ref,surface)
C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C              This subroutine tests the array that should be used for fitting
C!INPUT PARAMETERS:
C     AA_half_noutlayer        Intercept till average
C    BB_half_noutlayer         Slope till average
C    AA_noutlayer                Intercept for whole array
C    BB_noutlayer                 Slope for whole array
C   Sigma_half_one              Error in Intercept till average
C   Sigma_half_two              Error in Slope till average
C    Sigma_all_one               Error in Intercept for whole array
C    Sigma_all_two               Error in Slope for whole array
C    Average_allwaves          Average values for whole array
C   standard_allwaves           Standard devaition for whole array
C   log_standard                   Standard devaition for log of whole array
C    idata                               Index for 10*10 array
C!OUTPUT PARAMETERS:
C
C                       A                   Final Intercept used after tests
c                        B                   Final Slope used after tests
C                  Siga                   Final error in Intercept used after tests
C                  Sigb                   Final error in Slope used after tests
C                  CHISQ
C
C!REVISION HISTORY:
CDeveloped 4/2001
C
C!TEAM-UNIQUE HEADER:
C
C 
C
C!DESIGN NOTES:
C
C!END
C-----------------------------------------------------------------------

        IMPLICIT NONE
         SAVE
         INCLUDE 'mod04.inc'
         Integer iscan, idata,QCONTROL_land,Flag_full
         Real   AA_half_noutlayer,BB_half_noutlayer,AA_noutlayer,
     *BB_noutlayer,Sigma_half_one,Sigma_half_two,Sigma_all_one,
     *Sigma_all_two,Average_allwaves(3),standard_allwaves(3)
        Real  log_standard,A,B,Siga,Sigb,SCATMODIS,SCAT_ANGLE_LAND
        Real THRSHOLD1,THRSHOLD2,diff_full_half
        Integer numpoints_inlayer,numpoints_half_inlayer
        Real CHISQ_noutlayer,CHISQ_half_noutlayer,CHISQ
        real Co_variance_noutlayer,Co_variance_noutlayer_half
        real Co_variance,chisq_for_flag,surface,diff_critic_ref
         A=-99.0
         B=-99.0
         Siga=-99.0
         Sigb=-99.0
         Co_variance=-99.0
         CHISQ=-99.0
         Flag_full=0
         chisq_for_flag=-99.0
        diff_critic_ref=-99.0
        diff_full_half=ABS(AA_half_noutlayer - AA_noutlayer)
        
        diff_critic_ref=
     *   ABS((AA_noutlayer/(1.-BB_noutlayer/Surface))-
     *     (AA_half_noutlayer/(1.-BB_half_noutlayer/Surface)))
              
c   test 0
       if(( 2*log_standard)/Average_allwaves(3) .ge. 0.5) then
c   test 1
       if(standard_allwaves(3) .ge. 0.01) then
c   test 2
       IF(AA_half_noutlayer  .lt.  AA_noutlayer .and.
     *     numpoints_inlayer .gt. 25 ) then
c   test 4
        if(Sigma_all_one .lt. 0.01) then
          if( AA_noutlayer .gt.0 .and. BB_noutlayer .gt.0)then
               A=AA_noutlayer
               B=BB_noutlayer
               siga=Sigma_all_one
               sigb=Sigma_all_two
               CHISQ=CHISQ_noutlayer
               Flag_full=1
            chisq_for_flag=sqrt(CHISQ_noutlayer/(numpoints_inlayer-1))
              Co_variance=Co_variance_noutlayer
         endif
      endif
c else for test2
       ELSE
c test 3
c        
         if (ABS(AA_half_noutlayer - AA_noutlayer) .lt. 0.003 .and.
     *     numpoints_inlayer .gt. 25 )  then
c test 4
           if(Sigma_all_one .lt. 0.01) then
               if( AA_noutlayer .gt.0 .and. BB_noutlayer .gt.0)then
               A=AA_noutlayer
               B=BB_noutlayer
               siga=Sigma_all_one
               sigb=Sigma_all_two
               CHISQ=CHISQ_noutlayer
               Flag_full=1
               Co_variance=Co_variance_noutlayer
           chisq_for_flag=sqrt(CHISQ_noutlayer/(numpoints_inlayer-1))
            endif
         endif
      
c else for test3 to use half layer
      else
       if(Sigma_half_one .lt. 0.01) then
        if( AA_noutlayer .gt.0 .and. BB_noutlayer .gt.0 .and.
     *    numpoints_half_inlayer.gt. 25 )  then
               A= AA_half_noutlayer
               B= BB_half_noutlayer
               siga=Sigma_half_one
               sigb=Sigma_half_two
               CHISQ=CHISQ_half_noutlayer 
               Co_variance=Co_variance_noutlayer_half
               Flag_full=0
            chisq_for_flag=
     *sqrt(CHISQ_half_noutlayer/(numpoints_half_inlayer-1))
           endif
        endif
c   Endif for test3
       endif
c   endif test 2
      endif
cc   else for test 1
      endif
cc   else for test 0
      else
       if (ABS(AA_half_noutlayer - AA_noutlayer) .lt. 0.003.and.
     *     numpoints_inlayer .gt. 25 ) then
          if( AA_noutlayer .gt.0 .and. BB_noutlayer .gt.0)then
               A=AA_noutlayer
               B=BB_noutlayer
               siga=Sigma_all_one
               sigb=Sigma_all_two
               CHISQ=CHISQ_noutlayer
               Flag_full=1
               Co_variance=Co_variance_noutlayer
             chisq_for_flag=sqrt(CHISQ_noutlayer/(numpoints_inlayer-1))
             endif
         endif
       endif
   

      Return
      end
C*****************************************************************

      Subroutine Fill_QAflag_CritRef_land(QCONTROL_land_wav1,
     *QCONTROL_land_wav2, SDS_QCONTROL_CritRef_land,Idata)
C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine sets the array for quality control flags
C
C!INPUT PARAMETERS:
C              QA_Flag_CritRef_land         Quality flag
C               Idata                                     Index of 10*10 box
C
C!OUTPUT PARAMETERS:
C              SDS_QCONTROL_CritRef_land      HDF array for quality control
C
C!REVISION HISTORY:
C $Log: Process_land_V2.f,v $
c 04/2001
c 
C
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS Shana Mattoo
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------

       IMPLICIT NONE
       SAVE

       INCLUDE 'mod04.inc'

       Integer QA_Flag_CritRef_land(12),idata,ii 
       Integer QCONTROL_land_wav1,QCONTROL_land_wav2
       BYTE   SDS_QCONTROL_CritRef_land(QA_Land,NUMCELLS),QA_Temp


c Product Quality & Retrieval Processing QA flags over ocean
C Quality of 3 and 4 ( Average solution)are repeated tion have
c as best and average solution have same quality.
c

c        QA_Flag_CritRef_land(1)=0     NO retrieval ( Not useful)
c        QA_Flag_CritRef_land(1)=1     retrieval    (  useful)
c
c    For all non retrieval boxes QA_Flag_CritRef_land is set to zero to indicate
c    not useful and QA_Flag_CritRef_land(5) is set to values of QCONTROL_land_land to
c    indicate the cause of Retrieval. QA_Flag_CritRef_land(6) is set to 11 for
c    no retrieval
         If ( QCONTROL_land_wav1.le. 0) then
           QA_Flag_CritRef_land(1)=0
           QA_Flag_CritRef_land(2)=0
         Else
            QA_Flag_CritRef_land(1)=1
         endif
         If ( QCONTROL_land_wav2.le. 0) then
           QA_Flag_CritRef_land(3)=0
           QA_Flag_CritRef_land(4)=0
         Else
            QA_Flag_CritRef_land(3)=1 
         endif
c  All retrieval boxes
c Estimated Quality of aerosol parameters
c        QA_Flag_CritRef_land(2)=3     very Good
c        QA_Flag_CritRef_land(2)=2     Good
c        QA_Flag_CritRef_land(2)=1     Marginal
c        QA_Flag_CritRef_land(2)=0     Bad
c        QA_Flag_CritRef_land(4)=3     very Good
c        QA_Flag_CritRef_land(4)=2     Good
c        QA_Flag_CritRef_land(4)=1     Marginal
c        QA_Flag_CritRef_land(4)=0     Bad

c
cIF  QCONTROL_landis 0( see doc.)quality of retrieval  is very good set control for
cboth wave .46 and 66
c
       QA_Flag_CritRef_land(2)= QCONTROL_land_wav1
       QA_Flag_CritRef_land(4)= QCONTROL_land_wav2
          

      QA_Temp=0
      CALL BYTE_SET(QA_Flag_CritRef_land(1),0,QA_Temp)
      CALL BYTE_SET(QA_Flag_CritRef_land(2),1,QA_Temp)
      CALL BYTE_SET(QA_Flag_CritRef_land(3),4,QA_Temp)
      CALL BYTE_SET(QA_Flag_CritRef_land(4),5,QA_Temp)
C  End of 1 byte

        SDS_QCONTROL_CritRef_land(1,IDATA)=QA_Temp

       Return
       end

         Subroutine Flag_analysis(Rayleigh_look,SCAT_ANGLE_LAND,Path_rad,
     *      Error_Pathrad,critic_ref,error_critic,index_error_flags,
     *     Flag_full,diff_full_half,weighting_critical,weighting_path,
     *     Qa_Flag_final,chisq_for_flag,diff_critic_ref)
C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C
C          This subroutine  Uses combination of tests to derive the weighting and
C          final Qa flag for the Quality Assurance
C
C!INPUT PARAMETERS:
C
C          Rayleigh_look    Rayleigh from Lookup table
C          SCAT_ANGLE_LAND  Scattering angle
C          Path_rad         Path Radiance derived
C          Error_Pathrad    Error in path radiance
C          critic_ref       Critical Reflactance
C          error_critic     Error in Critical Ref
C          Flag_full       Flag if Full or half data used
C          diff_full_half  Difference between half and full fit
C 
C
C!OUTPUT PARAMETERS:
C      index_error_flags     Index of different errors
C      weighting_critical    Weighting critic_ref based on the Errors
C      weighting_path        Weighting path radiance based on the Errors
C      Qa_Flag_final         Final flag for QA to determine Quality of data
C!REVISION HISTORY:
C 
C 4/23/01  
C
C Updated code to comply with most MODIS software standards.
C
C!TEAM-UNIQUE HEADER:
C
C Developed by Shana Mattoo
C modified 4/23/01 GSFC, Greenbelt, MD
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------
         IMPLICIT NONE
         SAVE
         INCLUDE 'mod04.inc'
       real Rayleigh_look(Land_Sol1)
       real THRSHOLD1,THRSHOLD2,SCATMODIS,Diff_path,Path_rad(Land_Sol1)
       real Error_Pathrad(Land_Sol1),critic_ref(Land_Sol1)
       real error_critic(Land_Sol1)
       real diff_full_half(Land_Sol1),SCAT_ANGLE_LAND
       integer iwav,ierr_index,num_ierr_index
       parameter(num_ierr_index=12)
       integer index_error_flags(Land_Sol1,num_ierr_index)
       integer Flag_full(Land_Sol1)
       Integer sum_index(Land_Sol1),weighting_critical(Land_Sol1)
       integer Qcontrol_crit_land(Land_Sol1) 
       integer weighting_path(Land_Sol1)
       integer Qcontrol_path_land(Land_Sol1)
       integer Qa_Flag_final(Land_Sol1)
       real chisq_for_flag(Land_Sol1),diff_critic_ref(Land_Sol1)
       
c  TEST for Particle Size
         SCATMODIS=SCAT_ANGLE_LAND
       IF(SCATMODIS .LT.60.0)SCATMODIS=60.0
       IF(SCATMODIS .GT.170.0)SCATMODIS=170.0
       IF(SCATMODIS.GE.60.0.AND.SCATMODIS.LT.150.0) THEN
        THRSHOLD1=1.1+0.0011*(SCATMODIS-60.0)
        THRSHOLD2=0.9+0.0011*(SCATMODIS-60.0)
      ELSE IF(SCATMODIS.GE.150.0.AND.SCATMODIS.LE.180.0) THEN
        THRSHOLD1=1.2-0.0033*(SCATMODIS-150.0)
        THRSHOLD2=1.0-0.0033*(SCATMODIS-150.0)
      ENDIF
           Diff_path=(Path_rad(2)-Rayleigh_look(2))/
     *     (Path_rad(1)-Rayleigh_look(1))

         Do iwav=1,Land_Sol1
         Do ierr_index=1,num_ierr_index
         index_error_flags(iwav,ierr_index)=0
         enddo
         enddo
         Do iwav =1,2
c   ERRRORS>>>>>>>>>>
         If(critic_ref(iwav) .gt.0.0 .and.critic_ref(iwav) .le.1.)
     *   index_error_flags(iwav,1)= (2**(0))
         If(Error_Pathrad(iwav) .lt.0.005)
     * index_error_flags(iwav,2)=(2**(1))
c one of the wavelength has no values
         If(Diff_path.lt.0) index_error_flags(iwav,3)=
     *                           (2**(2))
c Large Mode
        if(Diff_path.gt.THRSHOLD1) index_error_flags(iwav,4)= 
     *                                 (2**(3))
c Mixed Mode
         if(Diff_path.lt.THRSHOLD1 .and.Diff_path.GT.THRSHOLD2)
     *       index_error_flags(iwav,5)= (2**(4))
c Small Mode
         if(Diff_path.lt.THRSHOLD2) index_error_flags(iwav,6)= 
     *                                  (2**(5))
c
        If( error_critic(iwav) .lt.0.02)
     * index_error_flags(iwav,7)=(2**(6))

      If(  error_critic(iwav) .lt.0.01)
     * index_error_flags(iwav,8)=(2**(7))

        If(Error_Pathrad(iwav) .lt.0.0005)
     * index_error_flags(iwav,9)=(2**(8))
  

      If(diff_full_half(iwav). lt. 0.003)index_error_flags(iwav,10)= 
     *                                  (2**(9))

      If(Flag_full(iwav) .eq.1)index_error_flags(iwav,11)= 
     *                                  (2**(10))
c Enddo for wavelength
       enddo
c For critical reflactance flags sum first 8 tests  
        Do iwav=1,2
         weighting_critical(iwav)=0
         sum_index(iwav)=0
        Do ierr_index=1,num_ierr_index-3
          sum_index(iwav)=sum_index(iwav)+
     *   index_error_flags(iwav,ierr_index)
        Enddo
         IF(sum_index(iwav) .EQ. 227) THEN
          weighting_critical(iwav)=12
          Qcontrol_crit_land(iwav)=3
         Elseif(sum_index(iwav) .EQ.225) THEN
          weighting_critical(iwav)=10
          Qcontrol_crit_land(iwav)=3
          Elseif(sum_index(iwav) .EQ.99) THEN
          weighting_critical(iwav)=8
          Qcontrol_crit_land(iwav)=2
          Elseif(sum_index(iwav) .EQ.97) THEN
          weighting_critical(iwav)=6
          Qcontrol_crit_land(iwav)=2
          Elseif(sum_index(iwav) .EQ.81) THEN
          weighting_critical(iwav)=4
          Qcontrol_crit_land(iwav)=1
          Elseif(sum_index(iwav) .EQ.211) THEN
          weighting_critical(iwav)=4
          Qcontrol_crit_land(iwav)=1
          elseif(sum_index(iwav) .EQ.83) THEN
          weighting_critical(iwav)=4
          Qcontrol_crit_land(iwav)=1
          elseif(sum_index(iwav) .EQ.209) THEN
          weighting_critical(iwav)=4
          Qcontrol_crit_land(iwav)=1
          elseif(sum_index(iwav) .EQ.73) THEN
          weighting_critical(iwav)=3
          Qcontrol_crit_land(iwav)=0
          elseif(sum_index(iwav) .EQ.203) THEN
          weighting_critical(iwav)=3
          Qcontrol_crit_land(iwav)=0
          elseif(sum_index(iwav) .EQ.75) THEN
          weighting_critical(iwav)=3
          Qcontrol_crit_land(iwav)=0
          elseif(sum_index(iwav) .EQ.201) THEN
          weighting_critical(iwav)=3
          Qcontrol_crit_land(iwav)=0
          elseif(sum_index(iwav) .EQ.69) THEN
          weighting_critical(iwav)=2
          Qcontrol_crit_land(iwav)=0
          elseif(sum_index(iwav) .EQ.199) THEN
          weighting_critical(iwav)=2
          Qcontrol_crit_land(iwav)=0
          elseif(sum_index(iwav) .EQ.71) THEN
          weighting_critical(iwav)=2
          Qcontrol_crit_land(iwav)=0
          elseif(sum_index(iwav) .EQ.197) THEN
          weighting_critical(iwav)=2
          Qcontrol_crit_land(iwav)=0
          elseif(mod(sum_index(iwav),2).EQ.0) THEN
          weighting_critical(iwav)=1
          Qcontrol_crit_land(iwav)=0
          elseif(mod(sum_index(iwav),2).NE.0) THEN
             IF(sum_index(iwav) .le.63) then
              weighting_critical(iwav)=1
              Qcontrol_crit_land(iwav)=0
             Endif
          Endif
           weighting_critical(iwav)= weighting_critical(iwav)*10
          if( diff_critic_ref(iwav) .le.0.01)weighting_critical(iwav)=
     *     weighting_critical(iwav)+3
          if( chisq_for_flag(iwav) .le.0.005)weighting_critical(iwav)=
     *     weighting_critical(iwav)+5
c  change to -ve for test11 ,-weighting means half fit used else full
          if(index_error_flags(iwav,11) .eq.0)
     *     weighting_critical(iwav)=-weighting_critical(iwav)
c enddo for wave
        enddo
c For Path radiance 
        Do iwav=1,2
         weighting_path(iwav)=0
         sum_index(iwav)=0
        Do ierr_index=2,6
          sum_index(iwav)=sum_index(iwav)+
     *   index_error_flags(iwav,ierr_index)
        Enddo
          Do ierr_index=9,10
          sum_index(iwav)=sum_index(iwav)+
     *   index_error_flags(iwav,ierr_index)
          enddo
          
         IF(sum_index(iwav) .EQ. 802) THEN
          weighting_path(iwav)=12
         Qcontrol_path_land(iwav)=3
         Elseif(sum_index(iwav) .EQ.546) THEN
          weighting_path(iwav)=10
         Qcontrol_path_land(iwav)=3
          Elseif(sum_index(iwav) .EQ.290) THEN
          weighting_path(iwav)=8
         Qcontrol_path_land(iwav)=3
          Elseif(sum_index(iwav) .EQ.34) THEN
          weighting_path(iwav)=7
         Qcontrol_path_land(iwav)=2
          Elseif(sum_index(iwav) .EQ.544) THEN
          weighting_path(iwav)=6
         Qcontrol_path_land(iwav)=2
          Elseif(sum_index(iwav) .EQ.32) THEN
          weighting_path(iwav)=5
         Qcontrol_path_land(iwav)=1
          elseif(sum_index(iwav) .EQ.18) THEN
          weighting_path(iwav)=4
         Qcontrol_path_land(iwav)=1
          elseif(sum_index(iwav) .EQ.530) THEN
          weighting_path(iwav)=4
         Qcontrol_path_land(iwav)=1
          elseif(sum_index(iwav) .EQ.786) THEN
          weighting_path(iwav)=4
         Qcontrol_path_land(iwav)=1
          elseif(sum_index(iwav) .EQ.274) THEN
          weighting_path(iwav)=4
         Qcontrol_path_land(iwav)=1
          elseif(sum_index(iwav) .EQ.10) THEN
          weighting_path(iwav)=2
         Qcontrol_path_land(iwav)=0
          elseif(sum_index(iwav) .EQ.522) THEN
          weighting_path(iwav)=2
         Qcontrol_path_land(iwav)=0
          elseif(sum_index(iwav) .EQ.778) THEN
          weighting_path(iwav)=2
         Qcontrol_path_land(iwav)=0
          elseif(sum_index(iwav) .EQ.266) THEN
          weighting_path(iwav)=2
         Qcontrol_path_land(iwav)=0
          elseif(sum_index(iwav) .EQ.6) THEN
          weighting_path(iwav)=1
         Qcontrol_path_land(iwav)=0
          elseif(sum_index(iwav) .EQ.518) THEN
          weighting_path(iwav)=1
         Qcontrol_path_land(iwav)=0
          elseif(sum_index(iwav) .EQ.774) THEN
          weighting_path(iwav)=1
         Qcontrol_path_land(iwav)=0
          elseif(sum_index(iwav) .EQ.262) THEN
          weighting_path(iwav)=1
         Qcontrol_path_land(iwav)=0
          elseif(sum_index(iwav) .EQ.0) THEN
          weighting_path(iwav)=0
         Qcontrol_path_land(iwav)=0
          elseif(sum_index(iwav) .EQ.16) THEN
          weighting_path(iwav)=0
         Qcontrol_path_land(iwav)=0
          elseif(sum_index(iwav) .EQ.8) THEN
          weighting_path(iwav)=0
         Qcontrol_path_land(iwav)=0
          elseif(sum_index(iwav) .EQ.4) THEN
          weighting_path(iwav)=0
         Qcontrol_path_land(iwav)=0
          Endif
            weighting_path(iwav)= weighting_path(iwav)*10
         if( diff_critic_ref(iwav) .le.0.01)weighting_path(iwav)=
     *    weighting_path(iwav)+3
          if( chisq_for_flag(iwav) .le.0.005)weighting_path(iwav)=
     *     weighting_path(iwav)+5
c  change to -ve for test11 ,-weighting means half fit used else full
          if(index_error_flags(iwav,11) .eq.0)
     *     weighting_path(iwav)=-weighting_path(iwav)
c enddo for wave
        enddo
        do iwav=1,2
        Qa_Flag_final(iwav)=(Qcontrol_path_land(iwav)+
     *                       Qcontrol_crit_land(iwav))/2
        enddo

       Return
       end



C ROBS NEW ROUTINES!
C*********************************************************************
       SUBROUTINE PROCESS_Land_Inver(IPROCE,
     *    MTHET0,MTHET,MDPHI,SCAT_ANGLE_LAND,IMONTH,LATM,LONM,MHGHT,
     *    REFW466,REFW553,REFW644,REFW866,REFW123,REFW163,REFW212,
     *    SDW466,SDW553,SDW644,SDW866,SDW123,SDW163,SDW212,
     *    INT_NL0,Fd_NL0,T_NL0,OPTH_NL0,SBAR_NL0,AEROSOL,REF_RAY_NL,
     *    ETA,ETA_FLAG,AOD,ERR644,RHOSFC,AVE_COUNT,FTABLE_NL,
     *    MASSCOEF_NL0,MASSCON,
     *    EXTNORM_NL0,AODF,AODC,ANGSTROM)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C   The subroutine derives non-cloudy aerosol optical thickness from MODIS
C   measured radiances over land, using the 0.47 (blue), 0.66 (red) 
C   and 2.13 (IR) micron channels.
C   Input data are cloud screened and averaged reflectance at at 10 x 10 km. 
C   Inversion:
C   A) Select fine mode model based on geography (dynamic aerosol models)
C   B) Mix fine mode and dust mode with selected discrete mixing ratios
C      best matches the observed spectral reflectance.
C
C   The program assumes that the input data are in reflectance units
C   of PI*L/(F0*cos(theta0)) for all channels.
C   It also assumes that the surface reflectance in the two visible channels
C   are functions of the surface reflectance in the IR. 
C   Nominally, red is one half of IR, and blue is one-half of red, respectively. 
C!INPUT PARAMETERS:
C
C   MTHET0                         solar zenith angle (degree)
C   MTHET                          satellite viewangle angle (degree)
C   MDPHI                          relative azimuth in angle (degree)
C   MHGHT                          Topographic altitude (km)
C   IMONTH                         Calendar month
C   LATM                           latitude of 10x10 km box
C   LONM                           longitude of 10x10 km box
C   REFW466,REFW644,REFW866,REFW212   averaged cloud screened reflectance
C   SDW470,SDW659,SDW866,SDW212       stddev cloud screened reflectance
C   INT_NL0			      reflectance from lookup table
C   Fd_NL0                           flux down from lookup table
C   T_NL0                           transmission from lookup table
C   OPTH_NL0                        optical thickness from lookup table
C   SBAR_NL0                        sbar from lookup table
C   MASSCOEF_NL0                    Mass Concentration from lookup table
C   EXTNORM_NL0                     Normalized Extinction Coeficients from LUT
C
C!OUTPUT PARAMETERS:
C
C  ETA                Fine mode (non dust) fraction
C  AOD                AOD at 4 wavelengths 
C  AODF/C             Fine and Coarse mode AOD at 4 wavelengths 
C  RHOSFC             Surface reflectance at 4 wavelengths
C  AVE_COUNT          Number of observations with error < 3%
C  FTABLE_NL          Which Fine model
C  MASSCON            Mass Concentration
C  ETA_FLAG           Whether ETA between 0.0 and 1.0
C  ANGSTROM           Angstrom Exponenent 0.47/0.66
C   
C  Aerosol types (FTABLE)
C      1              Continental
C      2              Generic : SSA ~ 0.9
C      3              Smoke   : SSA ~ 0.85
C      4              Urban   : SSA ~ 0.95
C      5              Dust
C
C INTERNALS:
C
C   Subroutines:
C      RNLOOKUP       -  READS THE LOOK-UP TABLES
C      SELECT_NLOOKUP -  SELECT LOOK-UP TABLE
C      INTANGLE_NL   -  Interpolates LUT parameters to measured geometry
C      TOA_SIMULATE  -  Computes simulated TOA reflectance for each TAU index
C      RETRIEVAL1    -  Derives AOT, ETA and 2.1 Surface Reflectance for
C                       IPROCE=1 (0.01 < rho2p1 < 0.25)
C      RETRIEVAL2    -  Derives AOT for Continental model, IPROCE=2(0.25 < rho2p1)
C      SHLSRT_NL     -  Sorts an array into ascending order
C      INTERP_EXTRAP -  Linear interpolation/extrapolation
C      PERCENTILE    -  Computes average value/standard deviation of selected
C                       pixels given percentiles
C      ERROR         -  SETS THE ERROR CODES
C      OUTPUT        -  WRITES THE OUTPUT TO HDF FILE
C      AEROSOL_MAP   -  Reads map of aerosol model as function of season and place
C      INT_ELEV      -  Interpolates lookup table at different wavelengths to 
C                       simulate elevation 
C
C
C!END
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'mod04.inc'
      INCLUDE 'mod04_land.inc'

      INTEGER NUMSQDIM,NUMXBOX,NUMYBOX
      PARAMETER (NUMSQDIM=400)
      INTEGER RTN,START_DATA,END_DATA
      INTEGER IDATA,ISCAN

      INTEGER IFINISH,IMONTH,IPR,IPROCE
      REAL SCAT_ANGLE_LAND

      REAL MTHET0,MTHET,MPHI0, MPHI, MDPHI,MHGHT
      REAL LATM,LONM,LAT_CENTER,LON_CENTER
C      REAL WATER_VAPOR(NUMSQDIM),OZONE_COL(NUMSQDIM)
      CHARACTER * 5 IOFLAG

C*********************************************************************
C   Parameters used from look-up tables
C ********************************************************************
C   wav_nl=wavelengths(4)(.466,.533,.644,2.13 micrometers)       4
C   nlthet0=number of theta0(solar zenith angle)              9
C   thet0_nl=(0,12,24,36,48,54,60,66,72   degrees)
C   nlthe =number of the(observation zenith angle)           13
C   the_nl=(0,6,.....72 degrees every 6 degrees)
C   nlphi =number of phi(observation azimuth angle)          16
C   phi_nl=(0,12,24,36,48,60,72,84,96,
C        108,120,132,144,156,168,180)
C   nltau =number of opth(optical thickness)                 7 
C   opth_nl=(0.0,0.25,0.50,0.75,1.0,2.0,3.0,5.0)
C   lhght = number of hght(observation heights)              1
C   hght=(80.0 km)
C**********************************************************************

C  
C  For Determining aerosol type from aerosol map
      INTEGER nlon, nlat
      PARAMETER (nlon = 360, nlat = 180)
      INTEGER AEROSOL(nlon,nlat)
      INTEGER ilat, ilon
      INTEGER FTABLE,FTABLE_NL
      REAL LONM1
      REAL dlat, dlon
      PARAMETER (dlat = 0.5, dlon = 0.5)
      CHARACTER*5 AEROSOL_TYPEs(5), AEROSOL_TYPE




C     Data directly read from the entire set of LUTs

      REAL SBAR_NL0(NLTHET0,NLTAU,NLWAV,NLTABLE)
      REAL OPTH_NL0(NLTAU,NLWAV,NLTABLE)
      REAL INT_NL0(NLPHI,NLTHE,NLTHET0,NLTAU,NLWAV,NLTABLE)
      REAL Fd_NL0(NLTHET0,NLTAU,NLWAV,NLTABLE)
      REAL T_NL0(NLTHE,NLTHET0,NLTAU,NLWAV,NLTABLE)
      REAL MASSCOEF_NL0(NLTAU,NLWAV,NLTABLE)
      REAL EXTNORM_NL0(NLTAU,NLWAV,NLTABLE)

      integer ij,ik,set_counter_land,qcontrol_special 

C     Data for specific LUTs (specified by aerosol model)

      REAL INT_NL1(NLPHI,NLTHE,NLTHET0,NLTAU,NLWAV,NLSIZE)
      REAL Fd_NL1(NLTHET0,NLTAU,NLWAV,NLSIZE)
      REAL SBAR_NL1(NLTHET0,NLTAU,NLWAV,NLSIZE)
      REAL T_NL1(NLTHE,NLTHET0,NLTAU,NLWAV,NLSIZE)

C     Interpolated lookup table data

      REAL INT_NL(NLTAU,NLWAV,NLSIZE)
      REAL Fd_NL(NLTAU,NLWAV,NLSIZE)
      REAL T_NL(NLTAU,NLWAV,NLSIZE)
      REAL FdT_NL(NLTAU,NLWAV,NLSIZE)
      REAL SBAR_NL(NLTAU,NLWAV,NLSIZE)
      REAL OPTH_NL(NLTAU,NLWAV,NLSIZE)
      REAL EQWAV_NL(NLWAV)
      REAL MASSCOEF_NL(NLTAU,NLWAV,NLSIZE)
      REAL EXTNORM_NL(NLTAU,NLWAV,NLSIZE)

c     Rayleigh data
      REAL REF_RAY_NL(NLWAV)


C     Simulated Reflectancea
      REAL  RHO_S212(NLTAU,NLSIZE), RHOSTAR(NLTAU,NLWAV,NLSIZE)
      REAL yint_466,slope_466,yint_644,slope_644

C     Results

      REAL ERR644
      REAL TAULAND55,RHOSFC212
      REAL RHOSFC(NLWAV)
      REAL ETA
      INTEGER ETA_FLAG
      REAL AOD(NLWAV),AODF(NLWAV),AODC(NLWAV)
      INTEGER AVE_COUNT
      REAL MASSCON
      REAL ANGSTROM


C     Dummy Indices
     
      INTEGER NSOLUTION,ISIZE,IETA,JETA,IWAV
      INTEGER IPHI,ITHE,ITAU,ITHET0,J
      INTEGER NUMDATA

C     Geometry Indices

      INTEGER KSZAM1,KSZAP1,KTHEM1,KTHEP1,KPHIM1,KPHIP1
      REAL REFW466,REFW553,REFW644,REFW866,REFW123,REFW163,REFW212
      REAL SDW466,SDW553,SDW644,SDW866,SDW123,SDW163,SDW212

c     INPUT Surface reflectance and TAU
c     Match corresponding aerosol model with aerosol map index

      SAVE

      DATA AEROSOL_TYPEs/'CONTN','BACKG','SMOKE','URBAN','DUST'/
     

      

c     Dummy set some stuff

C Pre-processing

      REFW466L = REFW466
      REFW644L = REFW644
      REFW212L = REFW212
      REFW124L = REFW123
      REFW866L = REFW866
      REFW164L = REFW163
      REFW553L = REFW553

C   Following If statement checks for missing data in a box 10*10
C  in any one of wavelengths.
C  If there is missing data in any one wavelength
C  arrays are filled with _fillValues.

       IF (REFW466L.LT.99999 .AND. REFW644L .LT. 99999 .AND.
     *    REFW866L.LT.99999 .AND. REFW212L .LT. 99999) THEN


C Determine expected fine model (from date and location)

      ilon = 181 + NINT(LONM + dlon)
      ilat = 91 - NINT(LATM + dlat)

      FTABLE = AEROSOL(ilon,ilat) + 2
      AEROSOL_TYPE = AEROSOL_TYPEs(FTABLE)

C  Select LUT corresponding to season and location
C  Obtain fine mode LUT (choice of tables #2, #3 or #4)
C  Coarse mode LUT = #5
C  Continental mode LUT = #1

      IF (IPROCE .EQ. 1) THEN
        MTABLE_NL(1) = FTABLE
        MTABLE_NL(2) = DTABLE_NL
      ELSE IF (IPROCE .EQ. 2) THEN
        MTABLE_NL(1) = 1
        MTABLE_NL(2) = 1
      ENDIF
      FTABLE_NL = MTABLE_NL(1)


      DO iSIZE = 1, NLSIZE

         CALL SELECT_NLOOKUP(INT_NL0,Fd_NL0,T_NL0,OPTH_NL0,
     *      SBAR_NL0,MASSCOEF_NL0,EXTNORM_NL0,
     *      ISIZE,INT_NL1,Fd_NL1,T_NL1,SBAR_NL1,
     *      OPTH_NL,MASSCOEF_NL,EXTNORM_NL,MTABLE_NL(ISIZE))

      ENDDO


C Interpolate lookup tables to measured angles. 

	CALL  SET_index_inter_NL(MTHET0,MTHET,MDPHI,
     *        KSZAM1,KSZAP1,KTHEM1,KTHEP1,KPHIM1,KPHIP1)
 
C Interpolate lookup table to measured geometry. 
     
        CALL INTANGLE_NL(MTHET0,MTHET,MDPHI,
     *	  INT_NL1,Fd_NL1,T_NL1,SBAR_NL1,
     *    INT_NL,Fd_NL,T_NL,FdT_NL,SBAR_NL,
     *    KSZAM1,KSZAP1,KTHEM1,KTHEP1,KPHIM1,KPHIP1)

C Interpolate lookup table to target elevation by interpolating wavelengths

        CALL INT_ELEV(EQWAV_NL,INT_NL,Fd_NL,T_NL,FdT_NL,OPTH_NL,
     *        SBAR_NL,REF_RAY_NL,MHGHT)

C
C  Simulate TOA reflectance
C

           CALL TOA_SIMULATE(
     *        MTHET0,SCAT_ANGLE_LAND,
     *        INT_NL,FdT_NL,OPTH_NL,SBAR_NL,RHO_S212,RHOSTAR,
     *        yint_466,slope_466,yint_644,slope_644)

C	 
C DO RETRIEVAL!!!!!!!!!!!
C     Retrieval#1 for IPROCE=1, Retrieval#2 for IPROCE=2	 
C

           ETA_FLAG = 0
           IF (IPROCE .EQ. 1) THEN	
             CALL RETRIEVAL1 (
     *       yint_466,slope_466,yint_644,slope_644,
     *       OPTH_NL,MASSCOEF_NL,EXTNORM_NL,
     *       RHO_S212,RHOSTAR,
     *       AVE_COUNT,ETA,ETA_FLAG,AOD,AODF,AODC,
     *       RHOSFC212,ERR644,RHOSFC,MASSCON)
           ELSE IF (IPROCE .EQ. 2) THEN
            CALL RETRIEVAL2 (
     *       yint_466,slope_466,yint_644,slope_644,
     *       OPTH_NL,MASSCOEF_NL,RHO_S212,RHOSTAR,
     *       AVE_COUNT,ETA,AOD,
     *       RHOSFC212,ERR644,RHOSFC,MASSCON)
           ENDIF

C Calculate Angstrom Exponent

           IF (AOD(iwave_644) .GT. 0) THEN
             ANGSTROM = -1.0 * ALOG(AOD(iwave_466)/AOD(iwave_644))/
     *           ALOG(0.466/0.644)
           ENDIF


          ENDIF
       
      RETURN		
 
      END

C*****************************************************************
      SUBROUTINE SELECT_NLOOKUP(
     *      INT_NL0,Fd_NL0,T_NL0,OPTH_NL0,SBAR_NL0,MASSCOEF_NL0,
     *      EXTNORM_NL0,
     *      ISIZE,INT_NL1,Fd_NL1,T_NL1,SBAR_NL1,
     *      OPTH_NL,MASSCOEF_NL,EXTNORM_NL,ITAB)
C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C              This subroutine selects look-up table (using ITABLE)
C	       Used to select both the "fine" and the "coarse/dust" land model
C
C!INPUT PARAMETERS:
C
C          INT_NL0      Reflectance
C          Fd_NL0       Flux Down
C          T_NL0        Transmssion factor
C          OPTH_NL0     Optical depth
C          SBAR_NL0     Sbar
C          MASSCOEF_NL0  MASSCON coeficient
C          EXTNORM_NL0  Normalized extinction coeficient
C          ITABLE     Index for desired lookup table
C          ISIZE      Index indicating "fine" or "coarse"
C
C!OUTPUT PARAMETERS:
C
C          INT_NL1        Reflectance
C          Fd_NL1       Flux Down
C          T_NL1        Transmssion factor
C          SBAR_NL1       Sbar
C          OPTH_NL       Optical depth
C          MASSCOEF_NL     Mass Conencentration coeficient
C          EXTNORM_NL     Normalized extinction coeficient
C
C!REVISION HISTORY:
C 5/1/98 DAC created
C
C!TEAM-UNIQUE HEADER:
C
C Developed by MODIS aerosol team at Goddard Space Flight Center,
C Greenbelt, Maryland.
C
C!END
C-----------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'mod04_land.inc'

      INTEGER IWAV,ITAB,ITAU,ITHET0,ITHE,IPHI
      INTEGER ISIZE

      REAL OPTH_NL0(NLTAU,NLWAV,NLTABLE)
      REAL SBAR_NL0(NLTHET0,NLTAU,NLWAV,NLTABLE)
      REAL INT_NL0(NLPHI,NLTHE,NLTHET0,NLTAU,NLWAV,NLTABLE)
      REAL Fd_NL0(NLTHET0,NLTAU,NLWAV,NLTABLE)
      REAL T_NL0(NLTHE,NLTHET0,NLTAU,NLWAV,NLTABLE)
      REAL MASSCOEF_NL0(NLTAU,NLWAV,NLTABLE)
      REAL EXTNORM_NL0(NLTAU,NLWAV,NLTABLE)

      REAL INT_NL1(NLPHI,NLTHE,NLTHET0,NLTAU,NLWAV,NLSIZE)
      REAL Fd_NL1(NLTHET0,NLTAU,NLWAV,NLSIZE)
      REAL SBAR_NL1(NLTHET0,NLTAU,NLWAV,NLSIZE)
      REAL T_NL1(NLTHE,NLTHET0,NLTAU,NLWAV,NLSIZE)
      REAL OPTH_NL(NLTAU,NLWAV,NLSIZE)
      REAL MASSCOEF_NL(NLTAU,NLWAV,NLSIZE)
      REAL EXTNORM_NL(NLTAU,NLWAV,NLSIZE)
      SAVE

C
C Select look-up table variable for given ITABLE
C

      DO 10 IWAV=1,NLWAV
        DO 20 ITAU=1,NLTAU
          OPTH_NL(ITAU,IWAV,ISIZE) = OPTH_NL0(ITAU,IWAV,ITAB)
          MASSCOEF_NL(ITAU,IWAV,ISIZE) = MASSCOEF_NL0(ITAU,IWAV,ITAB)
          EXTNORM_NL(ITAU,IWAV,ISIZE) = EXTNORM_NL0(ITAU,IWAV,ITAB)
          DO 30 ITHET0=1,NLTHET0
            Fd_NL1(ITHET0,ITAU,IWAV,ISIZE) =
     +         Fd_NL0(ITHET0,ITAU,IWAV,ITAB) 
            SBAR_NL1(ITHET0,ITAU,IWAV,ISIZE) =
     +         SBAR_NL0(ITHET0,ITAU,IWAV,ITAB) 
            DO 40 ITHE=1,NLTHE
               T_NL1(ITHE,ITHET0,ITAU,IWAV,ISIZE) = 
     +            T_NL0(ITHE,ITHET0,ITAU,IWAV,ITAB) 
              DO 50 IPHI=1,NLPHI
                  INT_NL1(IPHI,ITHE,ITHET0,ITAU,IWAV,ISIZE) = 
     +              INT_NL0(IPHI,ITHE,ITHET0,ITAU,IWAV,ITAB) 
50            CONTINUE
40          CONTINUE
30        CONTINUE
20      CONTINUE
10     CONTINUE

      RETURN
      END


C*****************************************************************
       SUBROUTINE SET_index_inter_NL(MTHET0,MTHET,MDPHI,
     *            KSZAM1,KSZAP1,KTHEM1,KTHEP1,KPHIM1,KPHIP1)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine sets the index for ending and starting
C             positions from the lookup table (based on input geometry)
C
C!INPUT PARAMETERS:
C              MTHET0      Measured solar Zenith angle
C              MTHET      Measured view  Zenith angle
C              MDPHI      Measured Azimuthal Angle
C
C              THET0      array of  solar Zenith angle in look_up table
C              THE        array of  satllite Zenith angle in look_up table
C              PHI        array of azimuth angle in look_up table
C
C!OUTPUT PARAMETERS:
C               KSZAM1     Starting Index for solar zenith angle
C               KSZAP1     Ending Index for solar zenith angle
C               KTHEM1     Starting Index for view angle
C               KTHEP1     Ending   Index for  view angle
C               KPHIM1     Starting Index for  azimuth angle
C               KPHIP1     Ending   Index for  azimuth angle
C
C!REVISION HISTORY:
C
C!TEAM-UNIQUE HEADER: - NOT YET DEFINED
C
C!REFERENCES AND CREDITS Shana Mattoo
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------

       IMPLICIT NONE
       SAVE

       INCLUDE 'mod04_land.inc'

       REAL MTHET0,MTHET,MDPHI,DEL
       INTEGER KSZAM1,KSZAP1,KTHEM1,KTHEP1,KPHIM1,KPHIP1
       INTEGER I

C      Initialize
 
       KSZAM1=0
       KSZAP1=0
       KTHEM1=0
       KTHEP1=0
       KPHIM1=0
       KPHIP1=0

C      Find THET0 indices
       DO I = 1,NLTHET0-1
          IF ((MTHET0 .GE. THET0_NL(I)) .AND. 
     *       (MTHET0 .LE. THET0_NL(I+1))) THEN
	    KSZAM1=I
	    KSZAP1=I+1
          ENDIF
       ENDDO

C      Find THE indices
       DO I = 1,NLTHE-1
          IF ((MTHET .GE. THE_NL(I)) .AND. 
     *       (MTHET .LE. THE_NL(I+1))) THEN
	    KTHEM1=I
	    KTHEP1=I+1
          ENDIF
       ENDDO

C      Find PHI indices
       DO I = 1,NLPHI-1
          IF ((MDPHI .GE. PHI_NL(I)) .AND. 
     *       (MDPHI .LE. PHI_NL(I+1))) THEN
	    KPHIM1=I
	    KPHIP1=I+1
          ENDIF
       ENDDO


       RETURN
       END


			
C*********************************************************************
      SUBROUTINE   INTANGLE_NL(MTHET0,MTHET,MDPHI,
     *	  INT_NL1,Fd_NL1,T_NL1,SBAR_NL1,
     *    INT_NL,Fd_NL,T_NL,FdT_NL,SBAR_NL,
     *    KSZAM1,KSZAP1,KTHEM1,KTHEP1,KPHIM1,KPHIP1)
C----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:  Subroutine INTANGLE_NL interpolates the lookup
C               reflectances to the measured geometry.
C
C!INPUT PARAMETERS:
C	  INT_NL1		  radiance
C	  Fd_NL1		  flux down
C	  T_NL1		  transmission
C	  SBAR_NL1		  sbar
C         KSZAM1      Starting Index for solar zenith angle
C         KSZAP1      Ending Index for solar zenith angle
C         KTHEM1      Starting Index for view angle
C         KTHEP1      Ending   Index for  view angle
C         KPHIM1      Starting Index for  azimuth angle
C         KPHIP1      Ending   Index for  azimuth angle
C         MTHET0        Solar zenith angle.
C         MTHET        View angle.
C         MDPHI        Azimuth angle.
C
C         THET0      array of  solar Zenith angle in look_up table
C         THE        array of  satllite Zenith angle in look_up table
C         PHI        array of azimuth angle in look_up table
C
C!OUTPUT PARAMETERS 
C	  INT_NL		  interpolated radiance
C	  Fd_NL		  interpolated flux down
C	  T_NL		  interpolated transmission
C	  SBAR_NL         interpolated  sbar
C	  FdT_NL	  two way transmission transmission
C         REF_RAY        Reflectance for rayleigh only
C
C!REVISION HISTORY:
C
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS:
C
C!DESIGN NOTES:
C
C!END
C-----------------------------------------------------------------------

      IMPLICIT NONE
      SAVE

      INCLUDE 'mod04_land.inc'

c     Inputs
      REAL  MTHET0,MTHET,MDPHI
      INTEGER KSZAM1,KSZAP1,KTHEM1,KTHEP1,KPHIM1,KPHIP1
      
      REAL INT_NL1(NLPHI,NLTHE,NLTHET0,NLTAU,NLWAV,NLSIZE)
      REAL Fd_NL1(NLTHET0,NLTAU,NLWAV,NLSIZE)
      REAL SBAR_NL1(NLTHET0,NLTAU,NLWAV,NLSIZE)
      REAL T_NL1(NLTHE,NLTHET0,NLTAU,NLWAV,NLSIZE)

c     Outputs
      REAL INT_NL(NLTAU,NLWAV,NLSIZE),Fd_NL(NLTAU,NLWAV,NLSIZE)
      REAL T_NL(NLTAU,NLWAV,NLSIZE),FdT_NL(NLTAU,NLWAV,NLSIZE)
      REAL SBAR_NL(NLTAU,NLWAV,NLSIZE)

c     Dummy
      CHARACTER*132 LINE
      CHARACTER*45  LINE1
      INTEGER ICASE,IJ,IPHI,ISIZE,ITAU,ITH,ITH0,IWAV,IETA,LOPT
      REAL  X(2),Y(2),XX1(2),YY1(2),XX2(2),YY2(2),Y1
      REAL  WW2(2),WW1(2),W1
      REAL  VV2(2),V1
      REAL  UU2(2),U1
      INTEGER LL,MM,NN

C LOOP IS AROUND THET0,NEW FILE FOR EACH THETA0
C

        Y1=0.0
        DO IJ = 1,2
           X(IJ)=0.0
           Y(IJ)=0.0
           XX1(IJ)=0.0
           XX2(IJ)=0.0
           YY1(IJ)=0.0
           YY2(IJ)=0.0
           WW1(IJ)=0.0
           WW2(IJ)=0.0
           VV2(IJ)=0.0
           UU2(IJ)=0.0
        ENDDO
        DO 5 ISIZE= 1,NLSIZE
          DO 15 IWAV=1,NLWAV
            DO 25  ITAU  = 1,NLTAU
              LL=0
              y1 = 0.0
              DO  40 ITH0  = KSZAM1,KSZAP1
                MM=0
                DO  50  ITH  = KTHEM1,KTHEP1
                  NN=0
                  DO 60  IPHI  = KPHIM1,KPHIP1
                    NN=NN+1
                    X(NN)=PHI_NL(IPHI)
                    Y(NN)=INT_NL1(IPHI,ITH,ITH0,ITAU,IWAV,ISIZE)
60                CONTINUE

                  CALL INTERP_EXTRAP(NN,MDPHI,X,Y,Y1,1)
                  MM=MM+1
                  XX1(MM)=THE_NL(ITH)
                  YY1(MM)=Y1
                  WW1(MM)=T_NL1(ITH,ITH0,ITAU,IWAV,ISIZE)

50              CONTINUE
                y1=0.0
                w1=0.0 
                CALL INTERP_EXTRAP(MM,MTHET,XX1,YY1,Y1,1)
                CALL INTERP_EXTRAP(MM,MTHET,XX1,WW1,W1,1)
                LL=LL+1
                XX2(LL)=THET0_NL(ITH0)
                YY2(LL)=Y1
                WW2(LL)=W1
                VV2(LL)=Fd_NL1(ITH0,ITAU,IWAV,ISIZE)
                UU2(LL)=SBAR_NL1(ITH0,ITAU,IWAV,ISIZE)

40            CONTINUE
              y1=0.0
              w1=0.0
              v1=0.0
              u1=0.0
              CALL INTERP_EXTRAP(LL,MTHET0,XX2,YY2,Y1,1)
              CALL INTERP_EXTRAP(LL,MTHET0,XX2,WW2,W1,1)
              CALL INTERP_EXTRAP(LL,MTHET0,XX2,VV2,V1,1)
              CALL INTERP_EXTRAP(LL,MTHET0,XX2,UU2,U1,1)

              INT_NL(ITAU,IWAV,ISIZE) = Y1
              T_NL(ITAU,IWAV,ISIZE) = W1
              Fd_NL(ITAU,IWAV,ISIZE) = V1
              SBAR_NL(ITAU,IWAV,ISIZE) = U1
              FdT_NL(ITAU,IWAV,ISIZE) = W1*V1

25          CONTINUE
15        CONTINUE
 5      CONTINUE


        RETURN
        END



C*********************************************************************
      SUBROUTINE  TOA_SIMULATE(
     *    MTHET0,SCAT_ANGLE_LAND,
     *    INT_NL,FdT_NL,OPTH_NL,SBAR_NL,RHO_S212,RHOSTAR,
     *    yint_466,slope_466,yint_644,slope_644)

C-----------------------------------------------------------------------
C!F77
C
C
C!DESCRIPTION:
C               This subroutine simulates TOA reflectance
C
C
C! INPUTS
C!OUTPUTS (INTO INCLUDE FILE)
C	       RHOSTAR	  array of TOA reflectance
C-----------------------------------------------------------------------

        IMPLICIT NONE
        Save
        INCLUDE 'mod04_land.inc'
       
c     Inputs

      REAL MTHET0,SCAT_ANGLE_LAND
      REAL INT_NL(NLTAU,NLWAV,NLSIZE),FdT_NL(NLTAU,NLWAV,NLSIZE),
     *    SBAR_NL(NLTAU,NLWAV,NLSIZE),OPTH_NL(NLTAU,NLWAV,NLSIZE)
      REAL REF_RAY_NL(NLWAV)

c     for estimating surface reflectance
      REAL yint_466, slope_466
      REAL yint_644, slope_644
      REAL RHO_S466, RHO_S644
      REAL MVI

c     Outputs

      REAL  RHO_S212(NLTAU,NLSIZE), RHOSTAR(NLTAU,NLWAV,NLSIZE)

c     Dummy

        INTEGER IETA,ITAU,IWAV,ISIZE,I


C 	At each tau index, calculate surface reflectance at 2.1 and
C           apparent reflectance at all wavelengths
       DO ISIZE = 1, NLSIZE
          DO ITAU = 1,NLTAU
	    RHO_S212(ITAU,ISIZE) = 0.0
	    RHO_S212(ITAU,ISIZE) = 
     *         (INT_NL(ITAU,iwave_212,ISIZE)-REFW212L) / 
     *         ((SBAR_NL(ITAU,iwave_212,ISIZE) * 
     *          (INT_NL(ITAU,iwave_212,ISIZE)-REFW212L)) -  
     *          (FdT_NL(ITAU,iwave_212,ISIZE))) 

            IF (RHO_S212(ITAU,ISIZE) .GT. 1.0) THEN
              RHO_S212(ITAU,ISIZE) = -9.999
            ENDIF 


c         Estiamte surface reflectance
c         Use Red/IR and Blue/Red surface correlations

c         Use MVI (function of 1.24 and 2.1)

c MVI dependence
            MVI = (REFW124L - REFW212L)/(REFW124L + REFW212L)
            IF (MVI .GT. 0.75) THEN
               yint_644 = 0.0
               slope_644 = 0.58
               yint_466 = 0.005
               slope_466 = 0.49
            ELSE IF (MVI .LT. 0.25) THEN
               yint_644 = 0.0
               slope_644 = 0.48
               yint_466 = 0.005
               slope_466 = 0.49
            ELSE !(MVI around 0.5)
               yint_644 = 0.0
               slope_644 = 0.48 + (0.10/0.50)*(MVI-0.25)
               yint_466 = 0.005
               slope_466 = 0.49
            ENDIF

c Scattering angle dependence
            yint_644 = yint_644 - 0.00025*SCAT_ANGLE_LAND + 0.033
            slope_644 = slope_644 +  0.002*SCAT_ANGLE_LAND - 0.27

            RHO_S644 = slope_644*RHO_S212(ITAU,ISIZE) + yint_644
            RHO_S466 = slope_466*RHO_S644 + yint_466
 

C          Compute model differentiated apparent reflectance
           RHOSTAR(ITAU,iwave_466,ISIZE) = 0.0
           RHOSTAR(ITAU,iwave_466,ISIZE) = 
     *       INT_NL(ITAU,iwave_466,ISIZE) +
     *       ((FdT_NL(ITAU,iwave_466,ISIZE) * RHO_S466) / 
     *       (1 - (SBAR_NL(ITAU,iwave_466,ISIZE) * RHO_S466)))

           RHOSTAR(ITAU,iwave_553,ISIZE) = 0.0

           RHOSTAR(ITAU,iwave_644,ISIZE) = 0.0
           RHOSTAR(ITAU,iwave_644,ISIZE) = 
     *       INT_NL(ITAU,iwave_644,ISIZE) +
     *       ((FdT_NL(ITAU,iwave_644,ISIZE) * RHO_S644) / 
     *       (1 - (SBAR_NL(ITAU,iwave_644,ISIZE) * RHO_S644)))

           RHOSTAR(ITAU,iwave_212,ISIZE) = 0.0
           RHOSTAR(ITAU,iwave_212,ISIZE) = 
     *          INT_NL(ITAU,iwave_212,ISIZE) +
     *          ((FdT_NL(ITAU,iwave_212,ISIZE) *
     *           (RHO_S212(ITAU,ISIZE))) / 
     *           (1 - (SBAR_NL(ITAU,iwave_212,ISIZE) *
     *            (RHO_S212(ITAU,ISIZE)))))

          ENDDO

        ENDDO
        RETURN

      END


C*********************************************************************

      SUBROUTINE RETRIEVAL1(
     *   yint_466,slope_466,yint_644,slope_644,
     *   OPTH_NL,MASSCOEF_NL,EXTNORM_NL,RHO_S212,RHOSTAR,
     *   AVE_COUNT,ETA,ETA_FLAG,AOD,AODF,AODC,
     *   RHOSFC212,ERR644,RHOSFC,MASSCON)
C-----------------------------------------------------------------------
C!F77
C
C
C!DESCRIPTION:
C               This subroutine retrieves optical thickness, surface
C	 	reflectance and error parameters by comparing
C	        MODIS observations and LUT data
C
C
C! INPUTS
C              REFW466L,REFW644L,REFW212L   reflectance
C              OPTH_NL      LUT based optical thickness
C              MASSCOEF_NL  LUT based mass concentration coeficients
C              EXTNORM_NL   LUT based normalized extinction coeficients
C              RHO_S212   simulated surface reflectance
C              RHOSTAR    simulated TOA reflectance
C              yint644,yint466,slope466,slope644  
C                 characteristics of VIS/IR surface reflectance relationship
C! OUTPUTS
C	       ERR644	  retrieval fitting errors 
C	       AOD        retrieved Optical depths 
C	       AODF/C     Fine and coarse mode optical depth 
C	       RHOSFC     retrieved surface reflectance 
C	       ETA        retrieved fine mode weighting
C	       MASSCON    retrieved mass concentration coeficient
C              ETA_FLAG   0 if eta within 0.0 - 1.0
C
C-----------------------------------------------------------------------

        IMPLICIT NONE
        INCLUDE 'mod04_land.inc'

c     INPUTS
      REAL OPTH_NL(NLTAU,NLWAV,NLSIZE)
      REAL MASSCOEF_NL(NLTAU,NLWAV,NLSIZE)
      REAL EXTNORM_NL(NLTAU,NLWAV,NLSIZE)
      REAL RHO_S212(NLTAU,NLSIZE), RHOSTAR(NLTAU,NLWAV,NLSIZE)
      REAL yint_466,slope_466,yint_644,slope_644

c     Intermediate
      REAL ETA_TEMP
      REAL AOD553_TEMP(NLETA)
      REAL RHOSFC212_TEMP(NLETA),ERR644_TEMP(NLETA)
      REAL ERR644_SORT(NLETA)
      REAL NLETA_SORT(NLETA)
      REAL RHOSFC212_SORT(NLETA)
      REAL AOD553_SORT(NLETA)


c     OUTput TAU, ETA and SFCReflectance
      REAL SFC212OUT,ETAOUT,ERR644OUT,AOD553OUT
      REAL SFC212BEST,ETABEST,ERR644BEST
      REAL SFC212TOT,ETATOT,ERR644TOT
      REAL SFC212AVE,ETAAVE,ERR644AVE
      REAL AOD553AVE,AOD553BEST,AOD553TOT
      INTEGER AVE_COUNT


      REAL AODF553, AODC553, AOD553, EXTNORM
      REAL RHOSTAR_TOT(NLTAU,NLWAV)
      REAL ERRWAVE(NLWAV)
      REAL RHO_S212_TOT(NLTAU)
      REAL RHOSTAR_TOT1(NLWAV,NLETA)

c     OUTPUT
      REAL ETA,ERR644,RHOSFC(NLWAV),AOD(NLWAV)
      REAL RHOSFC212
      REAL MASSCON,MASSCONF,MASSCONC
      REAL AODF(NLWAV), AODC(NLWAV)
      INTEGER ETA_FLAG

c     Dummy

      REAL XMIN,XS(100,100),YS(100,100),X(100),Y(100),Denom
      REAL y1
      INTEGER IETA,JETA,ITAU,IWAV,ISIZE,I,NSOLUTION

      SAVE	

c     Initialize

      ETA = -9999
      ERR644 = -9999
      MASSCON = -9999
      DO IWAV = 1, NLWAV
        RHOSFC(NLWAV) = -9999
        AOD(NLWAV) = -9999
      ENDDO
   

C Now loop through ETA values (kind of like ocean)
      NSOLUTION = 0
      DO 199 IETA = 1, NLETA

C Average, best, QCONTROL, FILLVALUE, OUTPUT, etc
        ETA_TEMP = -0.2 + (IETA * 0.1)

C 	Compute total apparent reflectance

        DO ITAU = 1, NLTAU
          DO IWAV = 1, NLWAV
	     RHOSTAR_TOT(ITAU,IWAV) = 
     *          ETA_TEMP * RHOSTAR(ITAU,IWAV,1) + 
     *          ((1-ETA_TEMP) * RHOSTAR(ITAU,IWAV,2))
          ENDDO
C 	Compute total surface reflectance
          RHO_S212_TOT(ITAU) = 
     *          ETA_TEMP * RHO_S212(ITAU,1) + 
     *          ((1-ETA_TEMP) * RHO_S212(ITAU,2))


        ENDDO


C       Interpolate everything based on measured reflectance at 466.


C                   ********COMPUTE FOR TAU FOR  USING
C                           RADAINCE VALUE OF 0.466 UM AND
C                           TAU VALUES OF WAV553
C

         DO ITAU = 1, NLTAU
             X(ITAU) = RHOSTAR_TOT(ITAU,iwave_466)
             Y(ITAU) = OPTH_NL(ITAU,iwave_553,1)
          ENDDO
          Y1=0
          CALL INTERP_EXTRAP(NLTAU,REFW466L,X,Y,Y1,1)
          AOD553_TEMP(IETA) = y1

C
C                 ******* FOR TAUX55 COMPUTE appararent Reflectance FOR
C                     all wavelengths
C
         DO IWAV = 1,NLWAV
            DO ITAU = 1,NLTAU
               X(ITAU)=OPTH_NL(ITAU,iwave_553,1)
               Y(ITAU)=RHOSTAR_TOT(ITAU,IWAV)
            ENDDO
            y1=0
            CALL INTERP_EXTRAP(NLTAU,AOD553_TEMP(IETA),X,Y,Y1,1)
            RHOSTAR_TOT1(IWAV,IETA)=Y1
         ENDDO

C
C                 ******* FOR TAUX55 COMPUTE Surface Reflectance at 2.1
C
          DO ITAU = 1,NLTAU
               X(ITAU)=OPTH_NL(ITAU,iwave_553,1)
               Y(ITAU)=RHO_S212_TOT(ITAU)
          ENDDO
          y1=0
          CALL INTERP_EXTRAP(NLTAU,AOD553_TEMP(IETA),X,Y,Y1,1)
          RHOSFC212_TEMP(IETA)=Y1

C       Errors

         ERRWAVE(1) = ABS(REFW466L - RHOSTAR_TOT1(1,IETA)) / REFW466L
         ERRWAVE(3) = ABS(REFW644L - RHOSTAR_TOT1(3,IETA)) / REFW644L
         ERRWAVE(4) = ABS(REFW212L - RHOSTAR_TOT1(4,IETA)) / REFW212L

C
C                 ******* Determine error at 644nm
C
C

         ERR644_TEMP(IETA) = ERRWAVE(iwave_644)


 199      ENDDO

C Sort results in ascending error order
          DO IETA = 1, NLETA
             NLETA_SORT(IETA) = IETA
             ERR644_SORT(IETA) = ERR644_TEMP(IETA)
             RHOSFC212_SORT(IETA) = RHOSFC212_TEMP(IETA)
             AOD553_SORT(IETA) = AOD553_TEMP(IETA)
          ENDDO
          CALL SHLSRT_NL(NLETA,ERR644_SORT,NLETA_SORT)
          DO IETA = 1, NLETA
             JETA = NLETA_SORT(IETA)
             RHOSFC212_SORT(IETA) = RHOSFC212_SORT(JETA)
             AOD553_SORT(IETA) = AOD553_SORT(JETA)
          ENDDO

C Best solution


          ETABEST = (-0.2 + (NLETA_SORT(1) * 0.1))
          ERR644BEST = ERR644_SORT(1)
          SFC212BEST = RHOSFC212_SORT(1)
          AOD553BEST = AOD553_SORT(1)

C Average solution

          AOD553TOT = 0.0
          AOD553AVE = -9999
          ETATOT = 0.0
          ERR644TOT = 0.0
          SFC212TOT = 0.0
          ETAAVE = -9999
          ERR644AVE = -9999
          SFC212AVE = -9999
          AVE_COUNT = 0

          DO IETA = 1, NLETA
             IF (ERR644_SORT(IETA) .LE. 0.03
     *            .AND. AVE_COUNT .LT. 3) THEN
                ERR644OUT = ERR644_SORT(IETA)
                ETAOUT = (-0.2 + (NLETA_SORT(IETA) * 0.1))
                SFC212OUT = RHOSFC212_SORT(IETA)
                AOD553OUT = AOD553_SORT(IETA)
                ETATOT = ETATOT + ETAOUT
                ERR644TOT = ERR644TOT + ERR644OUT
                SFC212TOT = SFC212TOT + SFC212OUT
                AOD553TOT = AOD553TOT + AOD553OUT
                AVE_COUNT = AVE_COUNT + 1
             ENDIF
          ENDDO

          IF (AVE_COUNT .GT. 0) THEN
             ETAAVE = ETATOT / AVE_COUNT
             ERR644AVE = ERR644TOT / AVE_COUNT
             SFC212AVE = SFC212TOT / AVE_COUNT
             AOD553AVE = AOD553TOT / AVE_COUNT
          ENDIF

c          IF (ETAAVE .NE. -9999) THEN
c             ETA = ETAAVE
c             ERR644 = ERR644AVE
c             RHOSFC212 = SFC212AVE
c             AOD553 = AOD553AVE
c             RHOSFC(iwave_212) = RHOSFC212
c             RHOSFC(iwave_466) = yint_466 + slope_466*RHOSFC212
c             RHOSFC(iwave_644) = yint_644 + slope_644*RHOSFC212
c          ELSE
          AVE_COUNT = -1
          ETA = ETABEST
          ERR644 = ERR644BEST
          RHOSFC212 = SFC212BEST
          AOD553 = AOD553BEST
          RHOSFC(iwave_212) = RHOSFC212
          RHOSFC(iwave_644) = yint_644 + slope_644*RHOSFC212
          RHOSFC(iwave_466) = yint_466 + slope_466*RHOSFC(iwave_644)
c          ENDIF

c set  out of bounds Eta to 0 or 1

       IF( ETA .LT. 0) THEN
           ETA=0.0
           ETA_FLAG = -1
       ENDIF
       IF( ETA .GT. 1) THEN
           ETA=1.0
           ETA_FLAG = -1
       ENDIF

c   Compute fine and coarse AOD for mass concentration
           AODF553 = AOD553 * ETA
           AODC553 = AOD553 * (1.-ETA)
C
C                 ******* FOR TAUX55 COMPUTE Mass Concentration
C

C   Fine
          DO ITAU = 1,NLTAU
               X(ITAU)=OPTH_NL(ITAU,iwave_553,1)
               Y(ITAU)=MASSCOEF_NL(ITAU,iwave_553,1)
          ENDDO
          y1=0
          CALL INTERP_EXTRAP(NLTAU,AODF553,X,Y,Y1,1)
          MASSCONF=Y1*AODF553

C  Coarse
          DO ITAU = 1,NLTAU
               X(ITAU)=OPTH_NL(ITAU,iwave_553,2)
               Y(ITAU)=MASSCOEF_NL(ITAU,iwave_553,2)
          ENDDO
          y1=0.0
          CALL INTERP_EXTRAP(NLTAU,AODC553,X,Y,Y1,1)
          MASSCONC=Y1*AODC553

          MASSCON = MASSCONF + MASSCONC

C Compute AOD for fine mode at all wavelengths
         DO IWAV = 1,NLWAV
            DO ITAU = 1,NLTAU
               X(ITAU)=OPTH_NL(ITAU,iwave_553,1)
               Y(ITAU)=EXTNORM_NL(ITAU,IWAV,1)
            ENDDO
            y1=0.0
            CALL INTERP_EXTRAP(NLTAU,AODF553,X,Y,Y1,1)
            EXTNORM=Y1
            AODF(IWAV) = AODF553 * EXTNORM
         ENDDO

C Compute AOD for coarse mode at all wavelengths
C Compute total AOD at all wavelengths
         DO IWAV = 1,NLWAV
            DO ITAU = 1,NLTAU
               X(ITAU)=OPTH_NL(ITAU,iwave_553,2)
               Y(ITAU)=EXTNORM_NL(ITAU,IWAV,2)
            ENDDO
            y1=0.0
            CALL INTERP_EXTRAP(NLTAU,AODC553,X,Y,Y1,1)
            EXTNORM=Y1
            AODC(IWAV) = AODC553 * EXTNORM
            AOD(IWAV) = AODF(IWAV) + AODC(IWAV)
         ENDDO


      RETURN
      END

C*********************************************************************

      SUBROUTINE RETRIEVAL2(
     *   yint_466,slope_466,yint_644,slope_644,
     *   OPTH_NL,MASSCOEF_NL,RHO_S212,RHOSTAR,
     *   AVE_COUNT,ETA,AOD,RHOSFC212,ERR644,RHOSFC,MASSCON)
C-----------------------------------------------------------------------
C!F77
C
C
C!DESCRIPTION:
C               This subroutine retrieves optical thickness, surface
C	 	reflectance and error parameters by comparing
C	        MODIS observations and LUT data. The "continental"
C               aerosol model is assumed.
C
C
C! INPUTS
C              REFW466L,REFW644L,REFW212L   reflectance
C              OPTH       array of LUT optical thickness
C              RHO_S212   simulated surface reflectance
C              RHOSTAR    simulated TOA reflectance
C              yint644,yint466,slope466,slope644  
C                 characteristics of VIS/IR surface reflectance relationship
C! OUTPUTS
C	       ERR644	  retrieval fitting errors 
C	       AOD        retrieved optical depths 
C	       RHOSFC     retrieved surface reflectance 
C	       ETA        retrieved fine mode weighting
C
C-----------------------------------------------------------------------

        IMPLICIT NONE
        INCLUDE 'mod04_land.inc'

c     INPUTS
      REAL OPTH_NL(NLTAU,NLWAV,NLSIZE)
      REAL MASSCOEF_NL(NLTAU,NLWAV,NLSIZE)
      REAL RHO_S212(NLTAU,NLSIZE), RHOSTAR(NLTAU,NLWAV,NLSIZE)
      REAL yint_466,slope_466,yint_644,slope_644

c     Intermediate

      INTEGER AVE_COUNT

     
      REAL RHOSTAR_TOT(NLTAU,NLWAV)
      REAL ERRWAVE(NLWAV)
      REAL RHO_S212_TOT(NLTAU)
      REAL RHOSTAR_TOT1(NLWAV)

c     OUTPUT
      REAL ETA,ERR644,RHOSFC(NLWAV),AOD(NLWAV)
      REAL RHOSFC212
      REAL MASSCON

c     Dummy

      REAL XMIN,XS(100,100),YS(100,100),X(100),Y(100),Denom
      REAL y1
      INTEGER JETA,ITAU,IWAV,ISIZE,I,NSOLUTION

      SAVE	

c     Initialize

      ETA = -9999
      ERR644 = -9999
      MASSCON = -9999
      DO IWAV = 1, NLWAV
        RHOSFC(NLWAV) = -9999
        AOD(NLWAV) = -9999
      ENDDO

C Now loop through ETA values (kind of like ocean)
      NSOLUTION = 0
C Average, best, QCONTROL, FILLVALUE, OUTPUT, etc

C 	Compute total apparent reflectance

        DO ITAU = 1, NLTAU
          DO IWAV = 1, NLWAV
	     RHOSTAR_TOT(ITAU,IWAV) = RHOSTAR(ITAU,IWAV,1)
          ENDDO
C 	Compute total surface reflectance
          RHO_S212_TOT(ITAU) =  RHO_S212(ITAU,1)
        ENDDO


C       Interpolate everything based on measured reflectance at 466.

C                   ********COMPUTE FOR TAU FOR  USING
C                           RADAINCE VALUE OF 0.466 UM AND
C                           TAU VALUES OF WAV553
C

        DO IWAV = 1, NLWAV
	  DO ITAU = 1, NLTAU
	     X(ITAU) = RHOSTAR_TOT(ITAU,iwave_466)
	     Y(ITAU) = OPTH_NL(ITAU,IWAV,1)
	  ENDDO
          Y1=0
          CALL INTERP_EXTRAP(NLTAU,REFW466L,X,Y,Y1,1)
          AOD(IWAV)=y1
        ENDDO

C
C                 ******* FOR TAUX55 COMPUTE appararent Reflectance FOR
C                     all wavelengths
C
         DO IWAV = 1,NLWAV
            DO ITAU = 1,NLTAU
               X(ITAU)=OPTH_NL(ITAU,iwave_553,1)
               Y(ITAU)=RHOSTAR_TOT(ITAU,IWAV)
	    ENDDO
            y1=0
            CALL INTERP_EXTRAP(NLTAU,AOD(iwave_553),X,Y,Y1,1)
            RHOSTAR_TOT1(IWAV)=Y1
         ENDDO

C
C                 ******* FOR TAUX55 COMPUTE Surface Reflectance at 2.1
C
          DO ITAU = 1,NLTAU
               X(ITAU)=OPTH_NL(ITAU,iwave_553,1)
               Y(ITAU)=RHO_S212_TOT(ITAU)
	  ENDDO
          y1=0
          CALL INTERP_EXTRAP(NLTAU,AOD(iwave_553),X,Y,Y1,1)
          RHOSFC212=Y1

C   	Errors

	 ERRWAVE(1) = ABS(REFW466L - RHOSTAR_TOT1(1)) / REFW466L
	 ERRWAVE(3) = ABS(REFW644L - RHOSTAR_TOT1(3)) / REFW644L
	 ERRWAVE(4) = ABS(REFW212L - RHOSTAR_TOT1(4)) / REFW212L

C
C                 ******* Determine error at 644nm  
C
C

 	 ERR644 = ERRWAVE(iwave_644)

         AVE_COUNT = -1
  
         RHOSFC(iwave_212) = RHOSFC212
         RHOSFC(iwave_553) = -9999
         RHOSFC(iwave_644) = yint_644 + slope_644*RHOSFC212 
         RHOSFC(iwave_466) = yint_466 + slope_466*RHOSFC(iwave_644)

C
C                 ******* FOR TAUX55 COMPUTE Mass Concentration  
C

C   Continental only 
          DO ITAU = 1,NLTAU
               X(ITAU)=OPTH_NL(ITAU,iwave_553,1)
               Y(ITAU)=MASSCOEF_NL(ITAU,iwave_553,1)
          ENDDO
          y1=0
          CALL INTERP_EXTRAP(NLTAU,AOD(iwave_553),X,Y,Y1,1)
          MASSCON=Y1*AOD(iwave_553)


      RETURN
      END

C*********************************************************************

       subroutine SHLSRT_NL(n,arr1,arr2)

C-----------------------------------------------------------------------
C !F77
C
C !DESCRIPTION: From numerical recepies Sorts arr1ay 'arr1' of
C              length 'n' into ascending order ('arr1' and arr2
C              is replaced on OUTPUT by its sorted rearr1angement)
C
C !INPUT PARAMETERS:
C                 N         length of ARR1
C                ARR1       Unsorted array ARR1
C                ARR2       Unsorted array ARR2
C
C !OUTPUT PARAMETERS:
C                ARR1       Sorted array ARR1
C                ARR2       Sorted array ARR2
C
C
C !TEAM-UNIQUE HEADER:
C
C !END
C-----------------------------------------------------------------------

      implicit none

      integer i,j,k,l,lognb2,m,n,nn
      real aln2i,tiny
      parameter (aln2i = 1.4426950, tiny = 1.e-5)
      real  t,q,arr1(n),arr2(n)

*/  Modified by JC Guu  01/14/97
*/  One logical variable declared.

      logical flag

      lognb2 = int(alog(float(n))*aln2i+tiny)
      m = n
      do 12 nn = 1,lognb2
        m = m/2
        k = n-m
        do 11 j = 1,k
          i = j

*/  Modified by JC Guu  01/14/97
*/  Change a "continue ... if() go to" construct
*/  to a "DO WHILE" construct.

C  3       continue

          flag=.true.

          do while(flag.eqv..true.)
          l = i+m
          flag=.false.
          if(arr1(l).lt.arr1(i)) then
            t = arr1(i)
            arr1(i) = arr1(l)
            arr1(l) = t
            q = arr2(i)
            arr2(i) = arr2(l)
            arr2(l) = q
c            write(6,*)'nn,m,k,j,i,l,t,arr1(i),arr1(l)',
c     * nn,m,k,j,i,l,t,arr1(i),arr1(l)
            i = i-m
            if(i.ge.1) flag=.true.
          endif
          end do

C            if(i.ge.1) go to 3
C          endif

11      continue
12    continue

      return

      end

C*********************************************************************

      SUBROUTINE INTERP_EXTRAP(M,X1,X,Y,Y1,INDEX)

C----------------------------------------------------------------------
C !F77
C
C !DESCRIPTION: This subroutine is a general purpose routine and
C              interpolates linearly.  Value of y1 is interpolated
C              or extrapolated for x1
C
C !INPUT PARAMETERS:I,M,X1,X,Y
C
C !OUTPUT PARAMETERS:Y1,LOPT
C
C !REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c
C
C !TEAM-UNIQUE HEADER:
C
C !END
C----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER IL,LL,LOPT,M,INDEX
      REAL PINTEN,PPHI,SINTEN,SPHI
      REAL  X(M),Y(M),Y1,X1,Diff

      Y1=0.0
      LOPT=0
      LL=M-1
        DO 230 IL=1,LL
C        Extrapolation on lower bound
       IF(X1 .LE.X(1))THEN
          PPHI=X(1)
          SPHI=X(2)
          PINTEN=Y(1)
          SINTEN=Y(2)
           Diff=(SPHI-PPHI)
           if(Diff . Le. 0.0) Diff=1
          Y1=PINTEN+((SINTEN-PINTEN)*((X1-PPHI)/Diff))
               LOPT=1

*/  Modified by JC Guu  01/09/97
*/  "GO TO 290" is changed to RETURN

           RETURN

C        Extrapolation on upper bound
       ELSEIF(X1 .GE.X(LL+1)) THEN
           PPHI=X(LL)
           SPHI=X(LL+1)
         PINTEN=Y(LL)
         SINTEN=Y(LL+1)
         Diff=(SPHI-PPHI)
           if(Diff .Le. 0.0) Diff=1
          Y1=PINTEN+((SINTEN-PINTEN)*((X1-PPHI)/Diff))
              LOPT=1

*/  Modified by JC Guu  01/09/97
*/  "GO TO 290" is changed to RETURN

           RETURN

C       interpolation
       ELSEIF (X1 .GE.X(IL) .AND.X1.LE. X(IL+1)) THEN
         PPHI=X(IL)
         SPHI=X(IL+1)
         PINTEN=Y(IL)
         SINTEN=Y(IL+1)
         Diff=(SPHI-PPHI)
           if(Diff .Le. 0.0) Diff=1
          Y1=PINTEN+((SINTEN-PINTEN)*((X1-PPHI)/Diff))
          LOPT=1

*/  Modified by JC Guu  01/09/97
*/  "GO TO 290" is changed to RETURN
*/  and two lines are commented out.

           RETURN
C        ELSE
C        GO TO 230

         ENDIF
  230    CONTINUE

  290    RETURN
          END



C***********************************************************************
      SUBROUTINE RNLOOKUP(
     +    HANDLE_LUT466,HANDLE_LUT553,HANDLE_LUT644,HANDLE_LUT213,
     +    INT_NL0,Fd_NL0,T_NL0,OPTH_NL0,
     +    SBAR_NL0,MASSCOEF_NL0,EXTNORM_NL0)

C-----------------------------------------------------------------------
C!F77
C
C
C!DESCRIPTION:
C               This subroutine reads 4 lookup tables
C
C!INPUT PARAMETERS:
C
C!OUTPUT PARAMETERS:
C
C          INT_NL0           Reflectance
C          Fd_NL0           Flux down
C          T_NL0           Transmission factor
C          OPTH_NL0          Optical depth
C          SBAR_NL0          Sbar
C          MASSCOEF_NL0      Mass Concentration coeficient
C          EXTNORM_NL0       Normalized extinction coeficient
C
C!OUTPUT PARAMETERS (INTO INCLUDE FILE)
C          THET0           Array of LUT Solar Zenith Angles
C          THE             Array of LUT Satellite View angles
C          PHI             Array of LUT Azimuth angle differences
C          WAV             Array of LUT wavelengths
C
C!DESIGN NOTES:
C
C  Read from look-up table.
C   wav=wavelengths(2)(.466,.533,.644 and 2.13 micrometers)  4
C   lthet0=number of theta0(solar zenith angle)              9
C   thet0=(0,12,24,36,48,54,60,66,72   degrees)
C   lthe =number of the(observation zenith angle)           16
C   the=(0,6,.....72 degrees every 6 degrees)
C   lphi =number of phi(observation azimuth angle)          31
C   phi=(0,6,12,18,24,30,36,42,48,54,60,66,72,78,84,90,96,
C        102,108,114,120,126,132,138,144,150,156,162,168,
C        174,180)
C   ltau =number of opth(optical thickness)                  7
C   opth=(0.0,0.25,0.50,1.0,2.0,3.0,5.0)
C   lhght = number of hght(observation heights)              1
C   hght=(80.0 km)
C
C!REVISION HISTORY:
C 02/08/2006 Rob Levy/Shana Mattoo
C!TEAM-UNIQUE HEADER:
C
C Developed by MODIS aerosol team at NASA Goddard Space Flight
C Center, Greenbelt, Maryland
C
C!END
C-------------------------------------------------------------------------
C
        IMPLICIT NONE 
        INCLUDE 'mod04_land.inc'
        INTEGER HANDLE_LUT466,HANDLE_LUT553
        INTEGER HANDLE_LUT644,HANDLE_LUT213
        INTEGER I,IPHI,ITAU,ITAB,NFILE
        INTEGER ITHE,ITHET0,IWAV
        REAL OPTH_NL0(NLTAU,NLWAV,NLTABLE)
        REAL MASSCOEF_NL0(NLTAU,NLWAV,NLTABLE)
        REAL EXTNORM_NL0(NLTAU,NLWAV,NLTABLE)
        REAL SSA_NL0(NLTAU,NLWAV,NLTABLE)
        REAL QEXT_NL0(NLTAU,NLWAV,NLTABLE)
        REAL BEXT_NL0(NLTAU,NLWAV,NLTABLE)
        REAL VEXT_NL0(NLTAU,NLWAV,NLTABLE)
        REAL MEXT_NL0(NLTAU,NLWAV,NLTABLE)
        REAL SBAR_NL0(NLTHET0,NLTAU,NLWAV,NLTABLE)
        REAL INT_NL0(NLPHI,NLTHE,NLTHET0,NLTAU,NLWAV,NLTABLE)
        REAL Fd_NL0(NLTHET0,NLTAU,NLWAV,NLTABLE)
        REAL T_NL0(NLTHE,NLTHET0,NLTAU,NLWAV,NLTABLE)
        CHARACTER*1 LINE(132)
        REAL  Omega0(NLTAU,NLWAV,NLTABLE),ROD(NLWAV),GOD(NLWAV)
        SAVE

C Loops through each wavelength and aerosol model

      DO 10 IWAV = 1,NLWAV
      DO 20 ITAB=1,NLTABLE

        IF (IWAV .EQ. 1) NFILE = HANDLE_LUT466        
        IF (IWAV .EQ. 2) NFILE = HANDLE_LUT553
        IF (IWAV .EQ. 3) NFILE = HANDLE_LUT644
        IF (IWAV .EQ. 4) NFILE = HANDLE_LUT213
C
C Reads observation zenith angles(the),and observation azimuth angles
C from look-up tables
C
        READ(NFILE,1000)(THE_NL(I),I=1,NLTHE)
        READ(NFILE,1000)(PHI_NL(I),I=1,NLPHI)

C
C Reads wavelength(wav),optical thickness(opth),solar zenith angle(thet0),
C reflectance ofatmosphere(sbarw) from look-up tables.
C

        DO 100 ITAU=1,NLTAU
          READ(NFILE,*)
          READ(NFILE,1001) SSA_NL0(ITAU,IWAV,ITAB),
     +        QEXT_NL0(ITAU,IWAV,ITAB),
     +        BEXT_NL0(ITAU,IWAV,ITAB),
     +        VEXT_NL0(ITAU,IWAV,ITAB),
     +        MEXT_NL0(ITAU,IWAV,ITAB),
     +        MASSCOEF_NL0(ITAU,IWAV,ITAB)
          READ(NFILE,1002) WAV_NL(IWAV),OPTH_NL0(ITAU,IWAV,ITAB),
     +        ROD(IWAV),GOD(IWAV)
 
          DO 101 ITHET0=1,NLTHET0
             READ(NFILE,1003) 
     +         THET0_NL(ITHET0), SBAR_NL0(ITHET0,ITAU,IWAV,ITAB), 
     +         Fd_NL0(ITHET0,ITAU,IWAV,ITAB)

C
C Reads transmission as a function of observation zenith angle,
C optical thickness
C

             READ(NFILE,1004)
     +          (T_NL0(ITHE,ITHET0,ITAU,IWAV,ITAB),ITHE=1,NLTHE)       
             READ(NFILE,*)
C
C Reads atmospheric radiance (int) as a function of solar zenith angle,
C optical thickness, height, observation zenith angle and azimuth angle
C from look-up table.
C
              DO 103 ITHE=1,NLTHE
                 READ(NFILE,1005)
     +      (INT_NL0(IPHI,ITHE,ITHET0,ITAU,IWAV,ITAB),IPHI=1,NLPHI)
103           CONTINUE
101       CONTINUE
100     CONTINUE

c  Set extinction parameters for "AOD = 0.0" case

        QEXT_NL0(1,IWAV,ITAB) = QEXT_NL0(2,IWAV,ITAB)
        BEXT_NL0(1,IWAV,ITAB) = BEXT_NL0(2,IWAV,ITAB)
        VEXT_NL0(1,IWAV,ITAB) = VEXT_NL0(2,IWAV,ITAB)
        MEXT_NL0(1,IWAV,ITAB) = MEXT_NL0(2,IWAV,ITAB)
        MASSCOEF_NL0(1,IWAV,ITAB) = MASSCOEF_NL0(2,IWAV,ITAB)

20    CONTINUE
10    CONTINUE

       DO ITAB=1,NLTABLE
       DO ITAU=1,NLTAU
       DO IWAV=1,NLWAV
       EXTNORM_NL0(ITAU,IWAV,ITAB) =
     *     QEXT_NL0(ITAU,IWAV,ITAB) / QEXT_NL0(ITAU,iwave_553,ITAB)
       ENDDO
       ENDDO
       ENDDO

1000  FORMAT(5X,16F8.3)
1001  FORMAT(6(5x,f8.3))
1002  FORMAT(10x,F8.3,3(4x,f8.3))
1003  FORMAT(3(5x,f11.4))
1004  FORMAT(3X,16f9.5)
1005  FORMAT(16f10.5)

      RETURN
      END


c  formats from IDL file
c FORMAT = '(A10,2f8.3,5(A7,f8.3))'
c FORMAT = '(6(A5,f8.3))'
c FORMAT = '(A10,f8.3,3(A4,f8.3))'
c FORMAT = '(3(A5,f11.4))'
c FORMAT = '(A3,16f9.5)'
c FORMAT = '(16f10.5)'


C*********************************************************************
      SUBROUTINE INT_ELEV(EQWAV_NL,INT_NL,Fd_NL,T_NL,FdT_NL,OPTH_NL,
     *        SBAR_NL,REF_RAY_NL,MHGHT)
C----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:  Subroutine INTELEV interpolates the lookup
C               reflectances to the target elevation.
C               Basically it is fudge:
C               Interpolate between wavelengths to simulate
C               elevation by a longer wavelength
C
C!INPUT PARAMETERS:
C         WAV_NL           wavelengths
C	  INT_NL		  radiance
C	  Fd_NL		  flux down
C         T_NL            transmission
C         FdT_NL          2 way transmission factor
C	  SBAR_NL	  sbar
C         OPTH_NL          optical depth
C         MGHTH           elevation
C
C!OUTPUT PARAMETERS 
C	  INT_NL		  interpolated radiance
C	  Fd_NL		          interpolated flux down
C         T_NL                    interpolated transmission
C	  FdT_NL		  interpolated transmission
C	  SBAR_NL	  interpolated sbar
C         OPTH_NL          interpolated optical depth
C         REF_RAY        Reflectance for rayleigh only
C         EQWAV_NL        Interpolated wavelengths
C
C!REVISION HISTORY:
C 02/08/2006 Shana Mattoo/Rob Levy
C Initial revision
C
C!TEAM-UNIQUE HEADER:
C
C Developed by MODIS Aerosol Retrieval Team at NASA GSFC, Greenbelt, MD
C
C!REFERENCES AND CREDITS
C
C!DESIGN NOTES:
C
C!END
C-----------------------------------------------------------------------

      IMPLICIT NONE
      SAVE

      INCLUDE 'mod04_land.inc'

c     Inputs
      REAL  MHGHT
      
c     Inputs and Outputs
      REAL INT_NL(NLTAU,NLWAV,NLSIZE),FdT_NL(NLTAU,NLWAV,NLSIZE),
     *    Fd_NL(NLTAU,NLWAV,NLSIZE), T_NL(NLTAU,NLWAV,NLSIZE),
     *    SBAR_NL(NLTAU,NLWAV,NLSIZE),OPTH_NL(NLTAU,NLWAV,NLSIZE)
      REAL REF_RAY_NL(NLWAV)
      REAL INT_NL9(NLTAU,NLWAV,NLSIZE),FdT_NL9(NLTAU,NLWAV,NLSIZE),
     *    Fd_NL9(NLTAU,NLWAV,NLSIZE), T_NL9(NLTAU,NLWAV,NLSIZE),
     *    SBAR_NL9(NLTAU,NLWAV,NLSIZE),OPTH_NL9(NLTAU,NLWAV,NLSIZE)

      REAL ROD_1013(NLWAV)
      REAL p, p0, expfactor
      PARAMETER (p0 = 1013.0)
      REAL ROD_PRES(NLWAV)
      REAL EQWAV_NL(NLWAV)

      REAL lambda0, lambda1, lambda2, diff0, exp0, exp1, exp2
      REAL tau0, tau1, tau2

c     Dummy
      INTEGER ICASE,IJ,ISIZE,ITAU,IWAV,IWAV1,IWAV2,JWAV
      REAL  X(2),Y(2),W(2),V(2),Z(2),T(2),U(2)
      REAL  Y1,W1,V1,Z1,T1,U1
      INTEGER LL


C     Estimate surface pressure (hypsometric EQ)
      p = p0 * exp(-(MHGHT/7.5))

C     Estimate ROD at nominal wavelenghts at p0 and at pres
      DO IWAV = 1, 3
         expfactor = -4.15 + (0.2 * WAV_NL(IWAV))
         ROD_1013(IWAV) = 0.0088 * WAV_NL(IWAV)**expfactor
         ROD_PRES(IWAV) = 0.0088*(p/p0) * WAV_NL(IWAV)**expfactor

C    Estimate equivalent wavelengths for ROD at pressure
         lambda1 = 0.1
         lambda2 = 2.0
         diff0 = 99.
         DO WHILE (diff0 .GT. 0.00001)
           lambda0 = (lambda1 + lambda2) / 2.
           exp0 = -4.15 + 0.2*lambda0
           exp1 = -4.15 + 0.2*lambda1
           exp2 = -4.15 + 0.2*lambda2
           tau0 = 0.0088*lambda0**exp0
           tau1 = 0.0088*lambda1**exp1
           tau2 = 0.0088*lambda2**exp2
           IF (tau1 .GT. ROD_PRES(IWAV) .AND. 
     *          tau2 .LT. ROD_PRES(IWAV)) THEN
              IF (tau0 .GT. ROD_PRES(IWAV)) THEN
                lambda1 = (lambda1 + lambda2)/2.
              ELSE 
                lambda2 = (lambda1 + lambda2)/2.
              ENDIF
           ENDIF
           diff0 = ABS(tau0 - ROD_PRES(IWAV))
         ENDDO
         EQWAV_NL(IWAV) = lambda0 
       ENDDO

 
C     INterpolate lookup tables to equiv Waves (let's start only with
C      1st two wavelengths until we derive 0.86 table)


        DO IJ = 1,2
           X(IJ)=0.0
           Y(IJ)=0.0
           V(IJ)=0.0
           W(IJ)=0.0
           Z(IJ)=0.0
           U(IJ)=0.0
           T(IJ)=0.0
        ENDDO
        DO 5 ISIZE= 1,NLSIZE
          DO 15 IWAV=1,3
            DO 25  ITAU  = 1,NLTAU
              LL=0
              Y1=0.0
              W1=0.0
              Z1=0.0
              V1=0.0
              IF (IWAV .EQ. 3) THEN
               IWAV1 = IWAV-1
               IWAV2 = IWAV
              ELSE
               IWAV1 = IWAV
               IWAV2 = IWAV+1
              ENDIF
              DO 60  JWAV = IWAV1, IWAV2
                LL=LL+1

c    Interpolate on log log scale
 
                X(LL)=ALOG(WAV_NL(JWAV))
                Y(LL)=ALOG(INT_NL(ITAU,JWAV,ISIZE))
                W(LL)=ALOG(FdT_NL(ITAU,JWAV,ISIZE))
                Z(LL)=ALOG(SBAR_NL(ITAU,JWAV,ISIZE))
                V(LL)=OPTH_NL(ITAU,JWAV,ISIZE)
                U(LL)=ALOG(Fd_NL(ITAU,JWAV,ISIZE))
                T(LL)=ALOG(T_NL(ITAU,JWAV,ISIZE))
                IF (OPTH_NL(ITAU,JWAV,ISIZE) .GT. 0.) THEN
                  V(LL)=ALOG(OPTH_NL(ITAU,JWAV,ISIZE))
                ENDIF
60            CONTINUE

              CALL INTERP_EXTRAP(LL,ALOG(EQWAV_NL(IWAV)),X,Y,Y1,1)
              CALL INTERP_EXTRAP(LL,ALOG(EQWAV_NL(IWAV)),X,W,W1,1)
              CALL INTERP_EXTRAP(LL,ALOG(EQWAV_NL(IWAV)),X,Z,Z1,1)
              CALL INTERP_EXTRAP(LL,ALOG(EQWAV_NL(IWAV)),X,V,V1,1)
              CALL INTERP_EXTRAP(LL,ALOG(EQWAV_NL(IWAV)),X,U,U1,1)
              CALL INTERP_EXTRAP(LL,ALOG(EQWAV_NL(IWAV)),X,T,T1,1)

              INT_NL9(ITAU,IWAV,ISIZE) = EXP(Y1)
              FdT_NL9(ITAU,IWAV,ISIZE) = EXP(W1)
              Fd_NL9(ITAU,IWAV,ISIZE) = EXP(U1)
              T_NL9(ITAU,IWAV,ISIZE) = EXP(T1)
              SBAR_NL9(ITAU,IWAV,ISIZE) = EXP(Z1)
              OPTH_NL9(ITAU,IWAV,ISIZE) = EXP(V1)
              IF (V1 .EQ. 0.) THEN
                OPTH_NL9(ITAU,IWAV,ISIZE) = V1
              ENDIF


25          CONTINUE
15        CONTINUE
 5      CONTINUE


        DO ISIZE= 1,NLSIZE
          DO IWAV=1,3
            DO ITAU  = 1,NLTAU
              INT_NL(ITAU,IWAV,ISIZE) = INT_NL9(ITAU,IWAV,ISIZE)
              FdT_NL(ITAU,IWAV,ISIZE) = FdT_NL9(ITAU,IWAV,ISIZE)
              Fd_NL(ITAU,IWAV,ISIZE) = Fd_NL9(ITAU,IWAV,ISIZE)
              T_NL(ITAU,IWAV,ISIZE) = T_NL9(ITAU,IWAV,ISIZE)
              SBAR_NL(ITAU,IWAV,ISIZE) = SBAR_NL9(ITAU,IWAV,ISIZE)
              OPTH_NL(ITAU,IWAV,ISIZE) = OPTH_NL9(ITAU,IWAV,ISIZE)
            ENDDO
            REF_RAY_NL(IWAV) = INT_NL(1,IWAV,ISIZE)
          ENDDO
        ENDDO


        RETURN
        END


c*************************************************
        SUBROUTINE AEROSOL_MAP(HANDLE_LUTMAP,IMONTH,AEROSOL)
C----------------------------------------------------
C!F77
C
C!DESCRIPTION:  Subroutine AEROSOL_MAP reads
C               the aerosol map (1 degree resolution)
C               to determine expected aerosol type at
C               a given location and season
C
C!INPUT PARAMETERS:
C         HANDLE_LUTMAP   Logical Unit # for file
C	  IMONTH          Integer month 1-12
C
C!OUTPUT PARAMETERS 
C	  AEROSOL	  360x180 map of aerosol type for appropriate season
C
C!REVISION HISTORY:
C 02/08/2006 Shana Mattoo/Rob Levy
C Initial revision
C
C!TEAM-UNIQUE HEADER:
C
C Developed by MODIS Aerosol Retrieval Team at NASA GSFC, Greenbelt, MD
C
C!REFERENCES AND CREDITS
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------

        IMPLICIT none

        INTEGER IMONTH, IMONTH1,iseason
        INTEGER HANDLE_LUTMAP
	INTEGER nlon, nlat
	PARAMETER (nlon = 360, nlat = 180)
 	INTEGER AEROSOL(nlon,nlat)
 	INTEGER AEROSOL_all(nlon,nlat,4)
        INTEGER ilat, ilon
        CHARACTER*3 MMMs(4), MMM
        CHARACTER*1 ss
        DATA MMMs/'DJF','MAM','JJA','SON'/
        ss=","

        DO iseason = 1, 4
         READ(HANDLE_LUTMAP,*) 
         DO ilat = 1, nlat
            READ(HANDLE_LUTMAP,99) 
     &      (AEROSOL_all(ilon,ilat,iseason), ilon=1, nlon)
         ENDDO
        ENDDO

        IMONTH1 = IMONTH
        IF (IMONTH .EQ. 12) THEN 
          IMONTH1 = 0
        ENDIF
        iseason = IMONTH1/3 + 1
        MMM = MMMs(iseason)
        DO ilon = 1, nlon
          DO ilat = 1, nlat
            AEROSOL(ilon,ilat) = AEROSOL_all(ilon,ilat,iseason)
          ENDDO
        ENDDO
        
        
!99     FORMAT (i1,359(",",i1))
 99     FORMAT (i1,359(ss,i1))

        RETURN 
        END
C********************************************************************
       SUBROUTINE Average_land(ISWATH,ILINE,CLDMSK_250,W470_SYN,W550_SYN,
     * W659_SYN,W865_SYN,W124_SYN,W164_SYN,W213_SYN,START_500,END_500,
     * START_250,END_250,START_1KM,END_1KM,REFW466_L,REFW550_L,REFW644_L,
     * REFW124_L,REFW164_L,REFW866_L,REFW212_L,SDW466_L,SDW550_L,
     * SDW644_L,SDW124_L,SDW164_L,SDW866_L,SDW212_L,IFINISH,IGRIDX,IGRIDY,
     * THR213MIN,THR213MAX,Num_Pixels_Used,IERROR,NUMRED,NUMBLUE)
C----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C
C   This subroutine processes 10*10 pixel box for cloud detection and
C   finds the average reflectance for red and blue channels. Surface
C   Reflectance from wavelength 2.13 . This surface Reflectance and
C   average Reflectance for red and blue channel are send to lookup
C   table and optical thickness is derived.
C
C!INPUT PARAMETERS:
C
C      IGRIDX        Number of pixels in x-direction
C      IGRIDY        Number of pixels in y-direction
C      W470_SYN      Reflectance for wav=0.47um
C      W213_SYN      Reflectance for wav=2.13um
C      W659_SYN      Reflectance for wav=0.66um
C      CLDMSK_250    Cloud mask at 250 m resolution (1=cloudy,o=clear)
C      MTHET0        Measured solar zenith angle in deg.
C      MTHET         Measured viewangle from ground in deg.
C      MDPHI         Measured relative azimuth angle deg.
C      THR213MIN     Minumum threshold for 2.13 micro wavelength.
C      THR213MAX     Maximum threshold for 2.13 micro wavelength.
C
C!OUTPUT PARAMETERS:
C     REFWL???      REFLECTANCE in 7 wavelengths
C 
C!REVISION HISTORY:
C 01/28/2006 Shana Mattoo
C Initial revision
C
C!TEAM-UNIQUE HEADER:
C
C Developed by MODIS Aerosol Retrieval Team at NASA GSFC, Greenbelt, MD
C
C!REFERENCES AND CREDITS
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IFINISH,IGRIDX,IGRIDY,ISWATH,ILINE,IX,IY
      INTEGER  IERROR,IOPT,INUM
      INTEGER START_500,END_500,START_250,END_250,START_1KM,END_1KM
      INTEGER ICLDBLUE,ICLDRED,I213,indX(400),N20,N50,INN
      REAL W470_SYN(2*ISWATH,2*ILINE)
      REAL W550_SYN(2*ISWATH,2*ILINE)
      REAL W213_SYN(2*ISWATH,2*ILINE)
      REAL W124_SYN(2*ISWATH,2*ILINE)
      REAL W164_SYN(2*ISWATH,2*ILINE)
      REAL w865_SYN(4*ISWATH,4*ILINE)
      REAL W659_SYN(4*ISWATH,4*ILINE)
      INTEGER NUMRED(2*ISWATH,2*ILINE),NUMBLUE(2*ISWATH,2*ILINE)
      INTEGER CLDMSK_250(4*ISWATH,4*ILINE),I 
      REAL THR213MIN,THR213MAX
      REAL   Ref_Inter_Land(7,400),Array(400),Array_new(400)
      INTEGER  Good_pixels(7) 
      REAL     array_interm(7,400),ref_allwav(7),sd_allwav(7)
      REAL REFW466_L,REFW550_L
      REAL REFW644_L,REFW124_L,REFW164_L,REFW866_L,REFW212_L
      REAL SDW466_L,SDW550_L
      REAL SDW644_L,SDW124_L,SDW164_L,SDW866_L,SDW212_L
      REAL ref_val,sd_val
      INTEGER Num_Pixels_Used
C
C Initialization
C

         Num_Pixels_Used=0
         IFINISH=0
         IERROR=4
         REFW466_L=-9999
         REFW550_L=-9999
          REFW644_L=-9999
          REFW124_L=-9999
          REFW164_L=-9999
          REFW866_L=-9999
          REFW212_L=-9999
          SDW466_L=-9999
          SDW550_L =-9999
          SDW644_L=-9999
          SDW124_L=-9999
          SDW164_L=-9999
          SDW866_L=-9999
          SDW212_L=-9999 

      CALL Cldmask_NDVI(ISWATH,ILINE,CLDMSK_250,W470_SYN,W550_SYN,
     *  W659_SYN,W865_SYN,W124_SYN,W164_SYN,W213_SYN,IGRIDX,IGRIDY,
     *  START_500,END_500,THR213MIN,THR213MAX,Ref_Inter_Land,
     *  ICLDBLUE,ICLDRED,I213,NUMRED,NUMBLUE)

c        write(36,*)'THR213MIN,THR213MAX,ICLDBLUE,ICLDRED,I213',
c     *THR213MIN,THR213MAX,ICLDBLUE,ICLDRED,I213
     

C
C I213>0 and Icld>0 indicate that the threshold for 2.13 micron
C is met and the data set is cloud free as defined.
C

      IF(I213 .GT. 0 .AND.ICLDBLUE .GT.0)THEN
c
c Sort in asending order and get the index for Reflactances at 0.66 um
         do ix=1,ICLDBLUE
         array(ix)=Ref_Inter_Land(3,ix)
c          array(ix)=Ref_Inter_Land(1,ix)
         enddo

       CALL INDEXX(ICLDBLUE,array,INDX)
       
       
C Set the arrays to reject 1/4 of all bright and dark pixels based on
c Wavelength of 865 nm

          inum=ICLDBLUE
    
           
c Reject 20% of brightest and 50% of darkest cloudfree pixels to eliminate
C the residual shadows and sub_cloud pixels
C
        N20=INT(REAL(inum)/REAL(5))
        N50=INT(REAL(inum)/REAL(2))
c   Throw away dark & bright pixels
c
       IF( (N50-N20) .gt.12) then
          IFINISH =1
            DO IY=1,7
                inn=0
                Good_pixels(IY)=0
                DO ix=N20,N50
                     inn=inn+1
                     array_new(inn)=Ref_Inter_Land(IY,indx(ix)) 
                ENDDO
                
CThrow away bad pixles in all wavelengths
             DO IX=1,inn
           if   ( array_new(IX) .GT.0. .AND.  array_new(IX) .LE.1)then
                Good_pixels(IY)= Good_pixels(IY)+1 
                array_interm(IY,Good_pixels(IY))= array_new(ix) 
              endif
          ENDDO 
c  ENDDO for wavelengths
          ENDDO
c Report  valid pixels used for Blue channel at 500 meter resolution

           Num_Pixels_Used=Good_pixels(3)
      
            DO iy=1,7
c Call to subroutine ave_std computes average and standard deviation for
c Reflectances
          IF(Good_pixels(IY) .gt.0) then
              DO IX=1,Good_pixels(IY)
              array(IX)= array_interm(IY,ix)
              ENDDO
            call ave_std(array, Good_pixels(IY),ref_val,sd_val)
             ref_allwav(iy)=ref_val
             sd_allwav(iy)=sd_val 
         ELSE
              ref_allwav(iy)=-99999.0
              sd_allwav(iy)=-9999.0
         ENDIF
c  ENDDO for number of wavelengths
        ENDDO
        
        
c Check if  3 wavelengths have valid range          
       if(  ref_allwav(1) .gt.0 .and.
     * ref_allwav(3) .gt. 0 .and. ref_allwav(7)  .gt. 0) then
     
          do iy = 1,7
   
C
C   WAVE 0.470 UM
C
          if(iy .eq.1)then
          REFW466_L=ref_allwav(iy)
          SDW466_L=sd_allwav(iy)
C
C   WAVE 0.550 UM
C
          elseif(iy .eq.2)then
          REFW550_L=ref_allwav(iy)
          SDW550_L=sd_allwav(iy)
C
C   WAVE 0.659 UM
C
         elseif (iy .eq.3)then
         REFW644_L=ref_allwav(iy)
         SDW644_L=sd_allwav(iy) 
C
C   WAVE 0.865 UM
C
         elseif(iy .eq.4)then
        REFW866_L=ref_allwav(iy)
        SDW866_L=sd_allwav(iy)

C
C   WAVE 1.24 UM
C
         elseif(iy .eq.5)then
         REFW124_L=ref_allwav(iy)
         SDW124_L=sd_allwav(iy)
 
C   WAVE 1.64 UM
C
      elseif(iy .eq. 6)then
        REFW164_L=ref_allwav(iy)
        SDW164_L=sd_allwav(iy)
 
C
C   WAVE 2.13  UM
C
         elseif(iy .eq. 7)then
        REFW212_L=ref_allwav(iy)
        SDW212_L=sd_allwav(iy) 
         endif
         
c enddo for wavelengths
        ENDDO
C Else and endif for to Check if  3 wavelengths have valid range else put fill values        
      Else
          
         IERROR = 2
         REFW466_L=-9999
         REFW550_L=-9999
         REFW644_L=-9999
         REFW124_L=-9999
          REFW164_L=-9999
          REFW866_L=-9999
          REFW212_L=-9999
          SDW466_L=-9999
          SDW550_L =-9999
          SDW644_L=-9999
          SDW124_L=-9999
          SDW164_L=-9999
          SDW866_L=-9999
          SDW212_L=-9999 
      ENDIF
        
c  ENDIF for N50-N20
         IERROR = 3 
         ENDIF
        
C ENDIF for  I213>0 and Icld>0 indicate that the threshold for 2.13 micron
C is met and the data set is cloud free as defined.
       
        ENDIF
          
    
           RETURN
           END

C***********************************************************************
      SUBROUTINE Cldmask_NDVI(ISWATH,ILINE,CLDMSK_250,W470_SYN,W550_SYN,
     *  W659_SYN,W865_SYN,W124_SYN,W164_SYN,W213_SYN,IGRIDX,IGRIDY,
     *  START_500,END_500,THR213MIN,THR213MAX,Ref_Inter_Land,
     *   ICLDBLUE,ICLDRED,I213,NUMRED,NUMBLUE)
C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C              The following subroutine finds the dark targets using a
C              threshold in the 2.13 micron channel.it averages the
C              cloud free pixels in red channel to the 0.5 km resolution.
C              If too cloudy or too bright at 2.13 it leaves the value
C              as zero.
C
C!INPUT PARAMETERS:
C
C           CLDMSK_250      Cloud mask at 250 m resolution (0=cloudy,1=clear)
C             W213_SYN      Reflectance for at 2.13 um
C             W470_SYN      Reflectance for at 0.47 um
C             W659_SYN      Reflectance for at 0.66 um
C               THR213      Threshold to detect dark pixels for 2.13 um
C               IGRIDX      Number of pixels in 1x1 km resolution
C               IGRIDY      Number of pixels in 1x1 km resolution
C
C!OUTPUT PARAMETERS:
C
C                 I213      number of pixels indicating cloudfree
C              ICLDRED      Number of pixels in red channel(cloudfree)
C             ICLDBLUE      Number of pixels in blue channel(cloudfree)
C             Ref_Inter_land Reflectance in 7 channels
C
C!REVISION HISTORY:
C 01/28/98 fhliang
C fixed prolog.
C
C Updated code to comply with most MODIS software standards.
C
C!TEAM-UNIQUE HEADER:
C
C Developed by MODIS Aerosol/Solar Water Vapor Retrieval Team
C GSFC, Greenbelt, MD
C
C!REFERENCES AND CREDITS
C     WRITTEN BY:Shana Mattoo
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------
C
        IMPLICIT NONE
      INTEGER IFINISH,IGRIDX,IGRIDY,ISWATH,ILINE 
      INTEGER  IERROR,IOPT,Iwave,I213
      INTEGER START_500,END_500,START_250,END_250,START_1KM,END_1KM
      REAL W470_SYN(2*ISWATH,2*ILINE)
      REAL W550_SYN(2*ISWATH,2*ILINE)
      REAL W213_SYN(2*ISWATH,2*ILINE)
      REAL W124_SYN(2*ISWATH,2*ILINE)
      REAL W164_SYN(2*ISWATH,2*ILINE)
      REAL w865_SYN(4*ISWATH,4*ILINE)
      REAL W659_SYN(4*ISWATH,4*ILINE)
      INTEGER CLDMSK_250(4*ISWATH,4*ILINE)
      REAL  W865_SYN_ForAve 
      REAL  W659_SYN_ForAve
      REAL   Ref_Inter_Land(7,400)
      real THR213MIN,THR213MAX
      INTEGER NUMRED(2*ISWATH,2*ILINE),NUMBLUE(2*ISWATH,2*ILINE)
      integer ICLDBLUE,ICLDRED,NUM_indx_x,J,I,IWAV
      Integer  NUM_indx_y
      INTEGER IMASK,IRED,JMASK,JRED,NUMCFR,NOWATER,II,JJ,IJ,IK
              
       SAVE

C
C  Initialization of variables and arrays
C

       I213 =0
       ICLDBLUE =0
       ICLDRED  =0
       NUM_indx_x=0
       NUM_indx_y=0
       
      
         DO J = 1,ILINE *2
          DO I = 1,ISWATH*2
          NUMBLUE(I,J)=0 
         ENDDO
         ENDDO
         do iwav=1,7
         do j=1,400
       Ref_Inter_Land(Iwav,j)=0
         ENDDO
         ENDDO

      
C
C Loop for 500x500 meter resolution based upon 2.13 um channel
C

       DO 200 I = START_500,END_500
           IMASK=2*I-1
         DO 201 J = 1,2*IGRIDY
           W865_SYN_ForAve=0 
           W659_SYN_ForAve=0 
           ICLDRED=0
           NUMBLUE(I,J)=0
           JMASK=2*J-1
           NUMCFR=0
           NOWATER=0
C
C Check if therhold for 2.13 um is met
C (missing pixels or noisy detectors are ignored)
C

          IF(W213_SYN(I,J) .LE. THR213MAX .AND.
     *       W213_SYN(I,J) .GT. THR213MIN) THEN

             I213 = I213+ 1

C
C Count the cloud-free 250x250 m subpixels in 500x500 m range
C

          DO 202 II = IMASK , 2*I
            DO 203 JJ = JMASK , 2*J

              IF(CLDMSK_250(II,JJ) .EQ. 1) THEN

               NUMCFR = NUMCFR + 1

              ENDIF

203         CONTINUE
202       CONTINUE
       
C
C Treats missing or noisy detector as water pixels to be ignored
C

C
C Compute apparant Reflectance and surface reflectance for 0.66 um channel
C for cloud and water free pixels in 250 m resolution and treats missing
C or noisy pixels as water pixels
C

          DO 204 II = IMASK , 2*I
            DO 205  JJ = JMASK , 2*J

              IF(W865_SYN(II,JJ) .GT. 0.0 .AND.
     *           W659_SYN(II,JJ) .GT. 0.0) THEN

C NDVI test according to Eric Vermote
C
               IF((W865_SYN(II,JJ)-W659_SYN(II,JJ))/
     *            (W865_SYN(II,JJ)+W659_SYN(II,JJ)).GT.0.1) THEN

                NOWATER=NOWATER+1

          IF(CLDMSK_250(II,JJ) .EQ. 1) THEN
           W865_SYN_ForAve= W865_SYN_ForAve+W865_SYN(II,JJ)
           W659_SYN_ForAve= W659_SYN_ForAve+W659_SYN(II,JJ) 
                  ICLDRED=ICLDRED+1
c          write(36,*)I,j,ii,jj,W865_SYN_ForAve,W659_SYN_ForAve,
c     *    ICLDRED
                ENDIF

              ENDIF

            ENDIF
      

205         CONTINUE
204       CONTINUE
          


C
C Check if all 4 out of 4 pixels are cloud free and all pixels
C are no water pixels in 250 m resolution
C Derive surface reflectance for 0.47 um channel (ignore missing or noisy pixels)
C

           IF(NUMCFR .GE. 4 .AND. NOWATER .EQ. 4) THEN
            IF(W470_SYN(I,J ) .GT. 0.0) THEN
               NUM_indx_x=NUM_indx_x+1
              Ref_Inter_Land(1,NUM_indx_x)=W470_SYN(I,J ) 
              Ref_Inter_Land(2,NUM_indx_x)=W550_SYN(I,J ) 
              Ref_Inter_Land(5,NUM_indx_x)= W124_SYN(I,J ) 
              Ref_Inter_Land(6,NUM_indx_x)=W164_SYN (I,J )
              Ref_Inter_Land(7,NUM_indx_x)=W213_SYN (I,J ) 
               NUMBLUE(NUM_indx_x,J)=NUMBLUE(NUM_indx_x,J)+1
               ICLDBLUE = ICLDBLUE + 1
         
             ENDIF
               ENDIF
c             write(36,*)'one',i,j,NUM_indx_x,ICLDBLUE,ICLDRED
C
C Derive average apparent reflectance for 0.66 and 0.86 um channels
C

        IF(ICLDRED .GT. 0) THEN
            NUM_indx_y= NUM_indx_y+1
         Ref_Inter_Land(3,NUM_indx_y)= W659_SYN_ForAve/ICLDRED
         Ref_Inter_Land(4,NUM_indx_y)= W865_SYN_ForAve/ICLDRED
        ENDIF
c            write(36,*)'two',i,j,NUM_indx_x,ICLDBLUE,ICLDRED
            
C If threshold for 2.13 is met
            
          ENDIF
     


201      CONTINUE
200    CONTINUE

       RETURN
       END
       
C*********************************************************************
      SUBROUTINE INTANGLE_RAY_NL(MTHET0,MTHET,MDPHI,
     *	     INT_NL0,REF_RAY_NL)		
C----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:  Subroutine INTANGLE_NL interpolates the lookup
C               Rayleigh reflectances to the measured geometry.
C
C!INPUT PARAMETERS:
C	  INT_NL0	radiance from LUT
C         MTHET0        Solar zenith angle.
C         MTHET        View angle.
C         MDPHI        Azimuth angle.
C
C         THET0      array of  solar Zenith angle in look_up table
C         THE        array of  satllite Zenith angle in look_up table
C         PHI        array of azimuth angle in look_up table
C
C!OUTPUT PARAMETERS 
C         REF_RAY_NL0      Reflectance for rayleigh only
C
C!REVISION HISTORY:
C
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS:
C
C!DESIGN NOTES:
C
C!END
C-----------------------------------------------------------------------

      IMPLICIT NONE
      SAVE

      INCLUDE 'mod04_land.inc'

c     Inputs
      REAL  MTHET0,MTHET,MDPHI
      INTEGER KSZAM1,KSZAP1,KTHEM1,KTHEP1,KPHIM1,KPHIP1
      
      REAL INT_NL0(NLPHI,NLTHE,NLTHET0,NLTAU,NLWAV,NLTABLE)

c     Outputs
      REAL REF_RAY_NL(NLWAV)

c     Dummy
      CHARACTER*132 LINE
      CHARACTER*45  LINE1
      INTEGER ICASE,IJ,IPHI,ITABLE,ITAU,ITH,ITH0,IWAV,IETA,LOPT
      REAL  X(2),Y(2),XX1(2),YY1(2),XX2(2),YY2(2),Y1
      INTEGER LL,MM,NN

C      Initialize
 
       KSZAM1=0
       KSZAP1=0
       KTHEM1=0
       KTHEP1=0
       KPHIM1=0
       KPHIP1=0

C      Find THET0 indices
       DO ITH0 = 1,NLTHET0-1
          IF ((MTHET0 .GE. THET0_NL(ITH0)) .AND. 
     *       (MTHET0 .LE. THET0_NL(ITH0+1))) THEN
	    KSZAM1=ITH0
	    KSZAP1=ITH0+1
          ENDIF
       ENDDO

C      Find THE indices
       DO ITH = 1,NLTHE-1
          IF ((MTHET .GE. THE_NL(ITH)) .AND. 
     *       (MTHET .LE. THE_NL(ITH+1))) THEN
	    KTHEM1=ITH
	    KTHEP1=ITH+1
          ENDIF
       ENDDO

C      Find PHI indices
       DO IPHI = 1,NLPHI-1
          IF ((MDPHI .GE. PHI_NL(IPHI)) .AND. 
     *       (MDPHI .LE. PHI_NL(IPHI+1))) THEN
	    KPHIM1=IPHI
	    KPHIP1=IPHI+1
          ENDIF
       ENDDO

C
C Initialize
C
        Y1=0.0
        DO IJ = 1,2
           X(IJ)=0.0
           Y(IJ)=0.0
           XX1(IJ)=0.0
           XX2(IJ)=0.0
           YY1(IJ)=0.0
           YY2(IJ)=0.0
        ENDDO

C
C Interpolate Rayleigh
C


          ITABLE = 1
          ITAU = 1
          DO 15 IWAV=1,NLWAV
              LL=0
              y1 = 0.0
              DO  40 ITH0  = KSZAM1,KSZAP1
                MM=0
                DO  50  ITH  = KTHEM1,KTHEP1
                  NN=0
                  DO 60  IPHI  = KPHIM1,KPHIP1
                    NN=NN+1
                    X(NN)=PHI_NL(IPHI)
                    Y(NN)=INT_NL0(IPHI,ITH,ITH0,ITAU,IWAV,ITABLE)
60                CONTINUE

                  CALL INTERP_EXTRAP(NN,MDPHI,X,Y,Y1,1)
                  MM=MM+1
                  XX1(MM)=THE_NL(ITH)
                  YY1(MM)=Y1

50              CONTINUE
                y1=0.0
                CALL INTERP_EXTRAP(MM,MTHET,XX1,YY1,Y1,1)
                LL=LL+1
                XX2(LL)=THET0_NL(ITH0)
                YY2(LL)=Y1

40            CONTINUE
              y1=0.0
              CALL INTERP_EXTRAP(LL,MTHET0,XX2,YY2,Y1,1)

            REF_RAY_NL(IWAV) = Y1
15        CONTINUE


        RETURN
        END


