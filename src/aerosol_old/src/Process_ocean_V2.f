C--------------------------------------------------------------------
C  Copyright (C) 2002,  Space Science and Engineering Center, 
C  University C  of Wisconsin-Madison, Madison WI.
C      
C  This program is free software; you can redistribute it 
C  and/or modify it under the terms of the GNU General 
C  Public License as published by the Free Software Foundation; 
C  either version 2 of the License, or (at your option) any 
C  later version.
C
C  This program is distributed in the hope that it will be 
C  useful, but WITHOUT ANY WARRANTY; without even the implied 
C  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
C  See the  GNU General Public License for more details.
C
C  You should have received a copy of the GNU General Public 
C  License along with this program; if not, write to the Free 
C  Software Foundation, Inc., 59 Temple Place, Suite 330, 
C  Boston, MA  02111-1307 USA
C--------------------------------------------------------------------
C
       SUBROUTINE PROCESS_ocean(HANDLE_S,HANDLE_L,
     &        ISCAN,IDATA,NUMSQ,MTHET0,MTHET,MPHI,START_500,
     &        END_500,START_250,END_250,START_1KM,END_1KM,
     &        W659_SYN,W865_SYN,W470_SYN,W550_SYN,W124_SYN,
     &        W164_SYN,W213_SYN,W551o_SYN,W667o_SYN,W869o_SYN,
     &        Sunglint_Flag,CldMsk_500,Set_Counter_Ocean,
     &        SDSTAU_best,SDSTAUS_best,SDSTAUB_best,
     &        SDSTAU_average,SDSTAUS_average,SDSTAUB_average,
     &        SDS_Least_error,SDS_small_weighting,SDS_sol_INDX_small,
     &        SDS_sol_INDX_large,SDS_ref,SDS_ref_STD,SDSASSY_best,
     &        SDSASSY_average,SDSBACK_best,SDSBACK_average,SDS_effrad,
     &        SDS_RefF_best,SDS_ccn,SDS_mass_conc,
     &        SDS_RefF_average,SDS_TranF_best,SDS_TranF_average,
     &        SDS_QCONTROL_ocean,SDS_NUMPIXELS,SDS_SCAT_ANGLE_OCEAN,
     &        SDS_AOT_model,SDS_CLDFRC_ocean,
     &        Set_Counter_Ocean_cloud,QA_Flag_ocean,GLINT_ANGLE,
     &        SDS_angs_coeff1,SDS_angs_coeff2,SDS_Tau_Land_Ocean,Refl_4,
     &        Qcontrol_special,SDS_correc_small_weighting,
     &        SDS_Tau_Land_Ocean_img,High_Cloud_Flag_500,
     &        W138_SYN,Refl_3,Refl_1,data_size)

C----------------------------------------------------------------------
C !F77
C
C !DESCRIPTION:
C  The MODIS ocean aerosol product consists of aerosol optical thickness
C  and size parameters estimates derived on a 10x10 (1-km) pixel spatial
C  array.  The measured radiances in a wide spectral range (0.47-2.13
C  microns) are inverted assuming a bi-lognormal size distribution.
C  The volume and the mean particle size for each log-normal mode are
C  determined.  When fully developed, the aerosol ocean algorithm with
C  use the seven MODIS bands: 1, 2, 3, 4, 5, 6, and 7.
C
C !INPUT PARAMETERS:   NONE
C
C !OUTPUT PARAMETERS:  NONE
C SDS_ref_STD     Standard deviation of reflectances at 7 bands
C SDSTAU_best     Optical thickness for best solution
C SDSTAUS_best    Optical thickness contribution small particles for best solution
C SDSTAUB_best    Optical thickness contribution large particles for best solution
C SDSTAU_average  Optical thickness for best solution
C SDSTAUS_average Optical thickness contribution small particles for best solution
C SDSTAUB_average Optical thickness contribution large particles for best solution
C SDS_Least_error         Least square error estimated
C SDS_small_weighting     Small mode weighting factor
C SDS_sol_INDX_small      Solution Number index small particles
C SDS_sol_INDX_large      Solution Number index large particles
C SDSASSY_best      Asymmetry_Factor at 7 bands for best solution
C SDSASSY_average   Asymmetry_Factor at 7 bands for average solution
C SDSBACK_best      Backscattering ratio at 7 bands of best solution
C SDSBACK_average   Backscattering ratio at 7 bands of average solution
C SDS_effrad         Effective_Radius at 0.55 micron of both solutions
C SDS_AOT_model Ratio of optical depth of small mode vs effective optical depth at 550
C SDS_RefF_best     Normalized reflected_flux at 7 bands of best solution
C SDS_RefF_average  Normalized reflected_flux at 7 bands of average solution
C SDS_TranF_best    Normalized Transmitted_flux at 7 bands of best solution
C SDS_TranF_average Normalized Transmitted_flux at 7 bands of average solution
C SDS_SCAT_ANGLE_OCEAN Scattering angle ocean
C SDS_QCONTROL         Quality control SDS array
C SDS_NUMPIXELS        Number of Pixels used for 0.55 micron
C SDS_ccn              Cloud_Fraction in percentage
C SDS_mass_conc        Mass concentration
C SDS_angs_coeff1      Angstrom Exponent for 0.550 and 0.865 miron
C SDS_angs_coeff2      Angstrom exponent for 0.865 and 2.130 micron
C
C !REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c Revision 2.1  1996/05/28  15:34:05  vlin
c Updated to write out one scancube at a time
c
C
C !TEAM-UNIQUE HEADER:
C
C !REFERENCES AND CREDITS:
C
C  WRITTEN BY:
C  SHANA MATTOO                E-MAIL:mattoo@climate.gsfc.nasa.gov
C  APPLIED RESEARCH CORPORATION          PHONE:  (301) 614-6214
C  NASA/GODDARD SPACE FLIGHT CENTER      FAX:    (301) 614-6307
C  CODE 913
C  GREENBELT, MD 20771
C
C !DESIGN NOTES:
C
C Internals Variables:
c
C    Reflectances of Wavelengths used modis,ch1-ch7  identified by REFW*
C
C       REFW659
C       REFW865
C       REFW470
C       REFW550
C       REFW124
C       REFW164
C       REFW213
C      CLDMSK(4*IGRIDX,4*IGRIDY)    Cloud mask 0=cloudy,1=clear
C      MTHET0     Measured solar zenith angle in deg.
C      MTHET      Measured viewangle from ground in deg.
C      MPHI       Measured azimuth  in deg.
C      NWAV       Number of wavelengths.
C      NTAU       Number of optical thicknesses.
C      NTH0       Number of solar zenith angles.
C      NTHET      Number of view angles.
C      NPHI       Number of azimuth angles.
C      PHC,JPHI   Azimuth angles.
C      THET       View angles.
C      THET0      Solar zenith angles.
C      TVALUE     Transmission factor.
C      AINT       Reflectance from look-up table.
C      TAUA       optical thicknesses.
C      Numdata    Number of input data sets.
C
C !END
C-----------------------------------------------------------------------

      IMPLICIT  NONE
      SAVE

      INCLUDE 'mod04.inc'


C     HDF array names..........
      BYTE         SDS_QCONTROL_ocean(QA_ocean,NUMCELLS)
      REAL SDS_ccn(NUMCELLS,NUM_solutions),
     &     SDS_mass_conc(NUMCELLS,NUM_solutions)
      INTEGER*2 SDS_ref(NUMCELLS,NWAV)
      INTEGER*2 SDS_ref_STD(NUMCELLS,NWAV)
              INTEGER*2 SDSTAU_best(NUMCELLS,NWAV),
     &            SDSTAU_average(NUMCELLS,NWAV),
     &            SDSTAUB_best(NUMCELLS,NWAV),
     &            SDSTAUB_average(NUMCELLS,NWAV),
     &            SDSTAUS_best(NUMCELLS,NWAV),
     &            SDSTAUS_average(NUMCELLS,NWAV),
     &            SDSASSY_best(NUMCELLS,NWAV),
     &            SDSASSY_average(NUMCELLS,NWAV),
     &            SDSBACK_best(NUMCELLS,NWAV),
     &            SDSBACK_average(NUMCELLS,NWAV),
     &            SDS_RefF_best(NUMCELLS,NWAV),
     &            SDS_RefF_average(NUMCELLS,NWAV),
     &            SDS_TranF_best(NUMCELLS,NWAV),
     &            SDS_TranF_average(NUMCELLS,NWAV)
       INTEGER*2
     &            SDS_small_weighting(NUMCELLS,NUM_solutions),
     &            SDS_correc_small_weighting(NUMCELLS),
     &            SDS_Least_error(NUMCELLS,NUM_solutions),
     &            SDS_effrad(NUMCELLS,NUM_solutions),
     &            SDS_sol_INDX_small(NUMCELLS,NUM_solutions),
     &            SDS_sol_INDX_large(NUMCELLS,NUM_solutions),
     &            SDS_angs_coeff1(NUMCELLS,NUM_solutions),
     &            SDS_angs_coeff2(NUMCELLS,NUM_solutions),
     &            SDS_AOT_model(NUMCELLS,num_model_index),
     &            SDS_CLDFRC_ocean(NUMCELLS)
        INTEGER*2 SDS_NUMPIXELS(NUMCELLS),
     & SDS_SCAT_ANGLE_OCEAN(NUMCELLS),SDS_Tau_Land_Ocean(NUMCELLS),
     & SDS_Tau_Land_Ocean_img(NUMCELLS)

c
c    Extra computed quantities from Look-up table
c
      REAL RGSS(NUMCASES),SIGMAS(NUMCASES),RGSB(NUMCASEB)
      REAL SIGMAB(NUMCASEB)
      REAL EXTSMALL(NUMCASES,NWAV),EXTBIG(NUMCASEB,NWAV)
      REAL MOMENTSSMALL(NUMCASES,4),MOMENTSBIG(NUMCASEB,4)
      REAL CCNSMALL(NUMCASES)
      REAL EXTNORSMALL(NUMCASES,NWAV),EXTNORBIG(NUMCASEB,NWAV)
      REAL BACKSCTTSMALL(NUMCASES,NWAV),BACKSCTTBIG(NUMCASEB,NWAV)
      REAL ASSYMSMALL(NUMCASES,NWAV),ASSYMBIG(NUMCASEB,NWAV)
      REAL NSMALL,NBIG,CCN,ASSYM(NWAV),BACKSCATT(NWAV)
      REAL TAUSMALL(NWAV),TAUBIG(NWAV)
      REAL ALBEDOSMALL(NUMCASES,NWAV),ALBEDOBIG(NUMCASEB,NWAV)
      REAL EFFRADIUS,EFFVARIANCE,REF_FLUX(NWAV),TRANS_FLUX(NWAV)
C
      INTEGER IDATA,ISMALL,IBIG
      INTEGER NUMDATA,NSOLUTION,NUMSQ
       REAL FUNMIN,CLD_FRAC,MASS_CON_OCEAN
      INTEGER START_500,END_500,START_250,END_250,START_1KM,END_1KM
      INTEGER ISCAN,NUMDATA_550,Qcontrol_special
       REAL sd_W659,sd_W865, sd_W470,sd_W550,sd_W124,sd_W164,
     *     sd_W213,sd_W443o,sd_W551o,sd_W667o,sd_W869o
c
      INTEGER NUM_ARRAY_ELEMENTS,data_size(2)
      PARAMETER(NUM_ARRAY_ELEMENTS=77)
      REAL MTHET0,MTHET,MPHI
      REAL ARRAY(NUM_ARRAY_ELEMENTS,NUMCASES*NUMCASEB)
      REAL NEW_ARRAY(NUM_ARRAY_ELEMENTS,NUMCASEB*NUMCASES)
      REAL AVE_ARRAY(NUM_ARRAY_ELEMENTS,NUM_solutions)
      real SCAT_ANGLE_OCEAN
      REAL  PHC(NPHI),THET(NTHET),THET0(NTH0)
      REAL  AINTS( NPHI, NTHET, NTH0, NTAU, NWAV, NUMCASES)
      REAL  AINTB( NPHI, NTHET, NTH0, NTAU, NWAV, NUMCASEB)
      REAL  TAUAS(NUMCASES,NWAV,NTAU)
      REAL  TAUAB(NUMCASEB,NWAV,NTAU),WAVE(NWAV)
      REAL  Ref_ray(NWAV)
      REAL ALBEDO_R_SMALL(NTH0,NTAU,NWAV,NUMCASES)
      REAL ALBEDO_R_BIG(NTH0,NTAU,NWAV,NUMCASEB)
      REAL ALBEDO_T_SMALL(NTH0,NTAU,NWAV,NUMCASES)
      REAL ALBEDO_T_BIG(NTH0,NTAU,NWAV,NUMCASEB)
      REAL ALBEDO_R_SMALL_tau(NTAU,NWAV,NUMCASES)
      REAL ALBEDO_R_BIG_tau(NTAU,NWAV,NUMCASEB)
      REAL ALBEDO_T_SMALL_tau(NTAU,NWAV,NUMCASES)
      REAL ALBEDO_T_BIG_tau(NTAU,NWAV,NUMCASEB)
      REAL ALBEDO_R_SMALL_final(NWAV,NUMCASES)
      REAL ALBEDO_R_BIG_final(NWAV,NUMCASEB)
      REAL ALBEDO_T_SMALL_final(NWAV,NUMCASES)
      REAL ALBEDO_T_BIG_final(NWAV,NUMCASEB)
      INTEGER JPHI(NPHI),ITAU,IWAV,TOT_cldfree_number
      REAL REFSMALL(NUMCASES,NWAV,NTAU),REFBIG(NUMCASEB,NWAV,NTAU)
      REAL ref_rayall(NPHI,NTHET,NTH0,NWAV)
      INTEGER KSZAM1,KSZAP1,KTHEM1,KTHEP1,KPHIM1,KPHIP1
      REAL W659_SYN(4*ISWATH,4*ILINE),W865_SYN(4*ISWATH,4*ILINE),
     *     W470_SYN(2*ISWATH,2*ILINE),W550_SYN(2*ISWATH,2*ILINE),
     *     W124_SYN(2*ISWATH,2*ILINE),W164_SYN(2*ISWATH,2*ILINE),
     *     W213_SYN(2*ISWATH,2*ILINE),
     *     W443o_SYN(ISWATH,ILINE),W551o_SYN(ISWATH,ILINE),
     *     W667o_SYN(ISWATH,ILINE),W869o_SYN(ISWATH,ILINE)
       INTEGER CLDMSK_syn(4*ISWATH,4*ILINE),Set_Counter_Ocean
       INTEGER CLDMSK_500(2*ISWATH,2*ILINE),Set_Counter_Ocean_cloud
       INTEGER NCLDMSK_SYN(2*ISWATH,2*ILINE+1)
       INTEGER Sunglint_Flag(ISWATH,ILINE),QA_Flag_Ocean(12)
       real W138_SYN(ISWATH,ILINE) 
       real Refl_4(2*ISWATH,2*ILINE) 
       INTEGER   High_Cloud_Flag_500(2*ISWATH,2*ILINE)
       Real Refl_3(2*ISWATH,2*ILINE) ,Refl_1(4*ISWATH,4*ILINE)

       INTEGER maskoption,num_resol
       Real   GLINT_ANGLE
       SDS_CLDFRC_ocean (IDATA)=-9999
c start with bad QCONTROL
c
c   First time in scan cube reads the lookup table.

       If (Set_Counter_Ocean .EQ.1) then
            NUMDATA=NUMSQ
C          Read look-up table

      CALL READ_LOOK(RGSS,SIGMAS,EXTSMALL,MOMENTSSMALL,
     *    CCNSMALL,EXTNORSMALL,BACKSCTTSMALL,ASSYMSMALL,
     *    RGSB,SIGMAB,EXTBIG,MOMENTSBIG,EXTNORBIG,BACKSCTTBIG,
     *    ASSYMBIG,ALBEDOSMALL,ALBEDOBIG,
     *    ALBEDO_R_SMALL,ALBEDO_R_BIG,ALBEDO_T_SMALL,ALBEDO_T_BIG,
     *    PHC,THET,THET0,AINTS,TAUAS,WAVE,
     *    AINTB,TAUAB,JPHI,ref_rayall,HANDLE_S,HANDLE_L)
        ENDIF
c
c First time into ocean set new cloud mask
c
       if(Set_Counter_Ocean_cloud.EQ.1)then
            maskoption=1
            num_resol=0
        CALL MAKE_CLDMSK(Refl_3 ,Refl_4,Refl_1,NCLDMSK_SYN,data_size)
       endif
       
C
C   Followig If statements checks if measured modis angles are
C   out of bounds from lookup table.
C
         IF(MTHET0 .GE. MINMTHET0 .AND. MTHET0 .LE. MAXMTHET0.AND.
     *     MTHET   .GE. MINMTHET  .AND. MTHET  .LE. MAXMTHET .AND.
     *     MPHI    .GE. MINMPHI   .AND. MPHI   .LE. MAXMPHI)  THEN
c
c Call SET_index_inter to set the indexes for measured geometry to use for
c  interpolation
C
        CALL SET_index_inter(MTHET0,MTHET,MPHI,THET0,
     *        KSZAM1,KSZAP1,KTHEM1,KTHEP1,KPHIM1,KPHIP1)
c
c Call INTANGLE_ray to interpolate the rayleigh Reflectance for measured geo.
c
        CALL INTANGLE_ray(PHC,THET,THET0,ref_rayall,
     *  MTHET0,MTHET,MPHI,Ref_ray,KSZAM1,KSZAP1,KTHEM1,KTHEP1,
     *  KPHIM1,KPHIP1)
c
C                **** subroutine AVERAGE averages Reflectances for 10*10 box
c
          CALL AVERAGE(W659_SYN,W865_SYN,W470_SYN,W550_SYN,
     *  W124_SYN,W164_SYN,W213_SYN,W443o_SYN,W551o_SYN,W667o_SYN,
     *  W869o_SYN,CLDMSK_syn,Sunglint_Flag,NCLDMSK_syn,
     *  START_1KM,END_1KM,NUMDATA_550,sd_W659,sd_W865,sd_W470,sd_W550,
     *  sd_W124,sd_W164, sd_W213,sd_W443o,sd_W551o,sd_W667o,sd_W869o,
     *  Ref_ray,MTHET0,WAVE,TOT_cldfree_number,GLINT_ANGLE,iscan,idata,
     *  Qcontrol_special, High_Cloud_Flag_500,W138_SYN,CLD_FRAC)
 
c       if(CLD_FRAC .GE.0)SDS_CLDFRC_ocean (IDATA)=CLD_FRAC*100.
c Change from percent to fraction for output

          if(CLD_FRAC .GE.0)SDS_CLDFRC_ocean (IDATA)=
     *             (CLD_FRAC*SCALE3+OFFSET3)
        
C
C   Following If statement checks for missing data in a box 10*10
C  in any one of wavelengths.
c  If there is missing data in any one wavelength
C  arrays are filled with _fillValues.
         IF(REFW550.LT.99999 .AND. REFW659 .LT. 99999 .AND.
     *    REFW865.LT.99999 .AND. REFW124 .LT. 99999 .AND.
     *    REFW164.LT.99999 .AND. REFW213 .LT. 99999 
     *     .and.Qcontrol_special .eq . 0  ) THEN

c
C Call INTsolarzenith_albedo_tran interpolates to measured solar Zenith angle
c
              CALL INTsolarzenith_albedo_tran(THET0,ALBEDO_R_SMALL,
     *           ALBEDO_R_BIG,ALBEDO_T_SMALL,ALBEDO_T_BIG,MTHET0,
     *           ALBEDO_R_SMALL_tau,ALBEDO_R_BIG_tau,ALBEDO_T_SMALL_tau,
     *           ALBEDO_T_BIG_tau,KSZAM1,KSZAP1)
c
c Call to subroutine INTANGLE interpolates the lookup reflectances to the
C measured geometry.
c
          CALL INTANGLE(PHC,THET,THET0,AINTS,TAUAS,
     *           AINTB,TAUAB,MTHET0,MTHET,MPHI,REFSMALL,REFBIG,
     *           Ref_ray,KSZAM1,KSZAP1,KTHEM1,KTHEP1,KPHIM1,KPHIP1)
c
CCall to subroutine COMPUTE_SCATTANGLE_OCEAN  computes scattering angle for
CMODIS measured Geometry
C
c          CALL COMPUTE_SCATTANGLE_OCEAN(MTHET0,MTHET,MPHI,IDATA,
c     *                                    SCAT_ANGLE_OCEAN)
c
c Loop around the cases of small and large size-distribution.
C
         NSOLUTION=0
         DO 199 ISMALL = 1,NUMCASES
        DO ITAU = 1,NTAU
          TAUAL(ITAU)=TAUAS(1,Index_wave_550,ITAU)
         DO IWAV=1,NWAV
            REFSMALLL(IWAV,ITAU)=REFSMALL(ISMALL,IWAV,ITAU)
         ENDDO
      ENDDO
         DO 201 IBIG   = 1,NUMCASEB
            DO ITAU = 1,NTAU
               DO IWAV=1,NWAV
                REFBIGL(IWAV,ITAU)=REFBIG(IBIG,IWAV,ITAU)
               ENDDO
            ENDDO
c
c             **** subroutine SUBMIN computes the minimum of the
c                     function derived from the weighting of small and
c                     large modes. It returns optical thicknesses values.

             CALL SUBMIN(FUNMIN,Ref_ray)
c
c  Call to subroutine COMPUTE_alltau computes optical thickness for
C  all wavelengths for small and coarse distributions.
c
             CALL  COMPUTE_alltau(EXTSMALL,EXTBIG,EXTNORSMALL,
     *        EXTNORBIG,ISMALL,IBIG)
c
C Call INTtau_albedo_tran interpolates to computed tau
c
              CALL INTtau_albedo_tran(ALBEDO_R_SMALL_tau,
     *       ALBEDO_R_BIG_tau,ALBEDO_T_SMALL_tau,ALBEDO_T_BIG_tau,
     *       ALBEDO_R_SMALL_final,ALBEDO_R_BIG_final,
     *       ALBEDO_T_SMALL_final,ALBEDO_T_BIG_final,TAUAS,TAUAB,
     *       ISMALL,IBIG)

C
c Call to subroutine COMPUTE_INVERVAR computes the secondary  varaibles
C from Look-up table and computed variables
C
            CALL COMPUTE_INVERVAR(EXTSMALL,
     *         EXTBIG,CCNSMALL,MOMENTSSMALL,
     *         MOMENTSBIG,NSMALL,NBIG,EFFRADIUS,EFFVARIANCE,
     *         ISMALL,IBIG,ALBEDOSMALL,ALBEDOBIG,
     *         CCN,BACKSCTTSMALL,BACKSCTTBIG,ASSYMSMALL,ASSYMBIG,
     *      BACKSCATT,ASSYM,REF_FLUX,TRANS_FLUX,ALBEDO_R_SMALL_final,
     *      ALBEDO_R_BIG_final,ALBEDO_T_SMALL_final,ALBEDO_T_BIG_final,
     *      MASS_CON_OCEAN,ALBEDO_R_SMALL_tau,MTHET0)


               NSOLUTION=NSOLUTION+1
c
C Call to store_other stores the output parameters for HDF output file
C   for each 10*10 boxes on a swath * 10 lines
c
       SCAT_ANGLE_OCEAN = -999.0
         CALL store_other(REF_FLUX,TRANS_FLUX,FUNMIN,EFFRADIUS,
     * BACKSCATT, ASSYM,NSOLUTION,CCN,SCAT_ANGLE_OCEAN,
     * ARRAY,NUM_ARRAY_ELEMENTS,NUMDATA_550,ISMALL,IBIG,
     *sd_W470,sd_W550,sd_W659,sd_w865,sd_W124,sd_W164,Sd_W213,CLD_FRAC,
     * MASS_CON_OCEAN)
  201 CONTINUE
  199 CONTINUE
c
C    Call to AVERAGE_output finds the best solution and average solution.
c
         CALL AVERAGE_output(ARRAY,NUM_ARRAY_ELEMENTS,AVE_ARRAY,
     *              NEW_ARRAY)

c

C
C  IF TAU55 IS IN GREATER THAN RANGE OF MAXTAU, FILL VALUES
C  Set 
      IF(AVE_ARRAY(4,2) .LE.MAXTAU )THEN
C
C    IF TAU55 IS LESS THAN -0.01 FILL WITH FILL VALUES
C
       IF(AVE_ARRAY(4,2) .GT. -0.01 )THEN
C
C  IF TAU55 IS WITHIN RANGE OF -0.01 AND 0 SET QCONTROL AND WRITE THE
C  VALUES
C
      IF( AVE_ARRAY(4,2) .GT. -0.01 .AND. AVE_ARRAY(4,2) .LT.0 )
     *       QCONTROL=9
C     CALL to Fill_QAflag_ocean  uses quality control to fill the
C      quality qontrol array
C
       CALL Fill_QAflag_ocean( QA_Flag_Ocean,SDS_QCONTROL_Ocean,Idata)

C     CALL to storeref_data   Stores Reflectance data
C     and Geolocation for HDF file.
C
        CALL storeref_data(IDATA,SDS_ref,SDS_ref_STD,NUMDATA_550,
     *  sd_W470,sd_W550,sd_W659,sd_w865,sd_W124,sd_W164,Sd_W213,
     *  SDS_CLDFRC_ocean,SDS_NUMPIXELS,
     *  SDS_SCAT_ANGLE_OCEAN,CLD_FRAC,SCAT_ANGLE_OCEAN)

c
C     Stores all varaibles into HDF files
c
       CALL  set_output(IDATA,AVE_ARRAY,NUM_ARRAY_ELEMENTS,
     *SDSTAU_best,SDSTAU_average,SDSTAUB_best,
     &SDSTAUB_average,SDSTAUS_best,SDSTAUS_average,
     &SDS_small_weighting,SDS_Least_error,
     &SDSASSY_best,SDSASSY_average,SDSBACK_best,SDSBACK_average,
     &SDS_effrad,SDS_RefF_best,SDS_RefF_average,SDS_TranF_best,
     &SDS_TranF_average,SDS_sol_INDX_small,SDS_sol_INDX_large,
     &SDS_ccn,SDS_mass_conc,SDS_angs_coeff1,
     &SDS_angs_coeff2,SDS_AOT_model)
C
C  ELSE AND ENDIF FOR IF TAU55 IS LESS THAN -0.01 FILL WITH FILL VALUES
C
          ELSE
        QCONTROL=-8
       Call Fill_QAflag_ocean(QA_Flag_Ocean,SDS_QCONTROL_Ocean,
     *              Idata)
      CALL FILLVALUE_Ocean(IDATA,SDS_ref,SDS_ref_STD,
     *SDSTAU_best,SDSTAUS_best,SDSTAUB_best,SDSTAU_average,
     *SDSTAUS_average,SDSTAUB_average,SDS_Least_error,
     *SDS_small_weighting,SDS_sol_INDX_small,SDS_sol_INDX_large,
     *SDSASSY_best,SDSASSY_average,SDS_ccn,sds_mass_conc,
     *SDSBACK_best,SDSBACK_average,SDS_effrad,SDS_AOT_model,
     *SDS_RefF_best,SDS_RefF_average,SDS_TranF_best,SDS_TranF_average,
     *SDS_angs_coeff1,SDS_angs_coeff2,SDS_SCAT_ANGLE_OCEAN,
     *SDS_QCONTROL_ocean,SDS_NUMPIXELS,SDS_CLDFRC_ocean,
     *SDS_Tau_Land_Ocean,Qcontrol_special,SDS_correc_small_weighting)
       ENDIF
C
C  ELSE AND ENDIF IF TAU55 IS IN GREATER THAN RANGE OF MAXTAU, FILL VALUES
C

       ELSE
        QCONTROL=-9
        Call Fill_QAflag_ocean(QA_Flag_Ocean,SDS_QCONTROL_Ocean,
     *              Idata)
         CALL FILLVALUE_Ocean(IDATA,SDS_ref,SDS_ref_STD,
     *SDSTAU_best,SDSTAUS_best,SDSTAUB_best,SDSTAU_average,
     *SDSTAUS_average,SDSTAUB_average,SDS_Least_error,
     *SDS_small_weighting,SDS_sol_INDX_small,SDS_sol_INDX_large,
     *SDSASSY_best,SDSASSY_average,SDS_ccn,sds_mass_conc,
     *SDSBACK_best,SDSBACK_average,SDS_effrad,SDS_AOT_model,
     *SDS_RefF_best,SDS_RefF_average,SDS_TranF_best,SDS_TranF_average,
     *SDS_angs_coeff1,SDS_angs_coeff2,SDS_SCAT_ANGLE_OCEAN,
     *SDS_QCONTROL_ocean,SDS_NUMPIXELS,SDS_CLDFRC_ocean,
     *SDS_Tau_Land_Ocean_img,Qcontrol_special,
     *SDS_correc_small_weighting)
        ENDIF

        Else
          If( qcontrol_special .eq.3) then
        CALL storeref_data(IDATA,SDS_ref,SDS_ref_STD,NUMDATA_550,
     *  sd_W470,sd_W550,sd_W659,sd_w865,sd_W124,sd_W164,Sd_W213,
     *  SDS_CLDFRC_ocean,SDS_NUMPIXELS,
     *  SDS_SCAT_ANGLE_OCEAN,CLD_FRAC,SCAT_ANGLE_OCEAN)
        QCONTROL=-1
       Call Fill_QAflag_ocean(QA_Flag_Ocean,SDS_QCONTROL_Ocean,
     *              Idata)
       CALL FILLVALUE_Ocean(IDATA,SDS_ref,SDS_ref_STD,
     *SDSTAU_best,SDSTAUS_best,SDSTAUB_best,SDSTAU_average,
     *SDSTAUS_average,SDSTAUB_average,SDS_Least_error,
     *SDS_small_weighting,SDS_sol_INDX_small,SDS_sol_INDX_large,
     *SDSASSY_best,SDSASSY_average,SDS_ccn,sds_mass_conc,
     *SDSBACK_best,SDSBACK_average,SDS_effrad,SDS_AOT_model,
     *SDS_RefF_best,SDS_RefF_average,SDS_TranF_best,SDS_TranF_average,
     *SDS_angs_coeff1,SDS_angs_coeff2,SDS_SCAT_ANGLE_OCEAN,
     *SDS_QCONTROL_ocean,SDS_NUMPIXELS,SDS_CLDFRC_ocean,
     *SDS_Tau_Land_Ocean_img,Qcontrol_special,
     *SDS_correc_small_weighting)
         ELSE
        Call Fill_QAflag_ocean(QA_Flag_Ocean,SDS_QCONTROL_Ocean,
     *              Idata)
       CALL FILLVALUE_Ocean(IDATA,SDS_ref,SDS_ref_STD,
     *SDSTAU_best,SDSTAUS_best,SDSTAUB_best,SDSTAU_average,
     *SDSTAUS_average,SDSTAUB_average,SDS_Least_error,
     *SDS_small_weighting,SDS_sol_INDX_small,SDS_sol_INDX_large,
     *SDSASSY_best,SDSASSY_average,SDS_ccn,sds_mass_conc,
     *SDSBACK_best,SDSBACK_average,SDS_effrad,SDS_AOT_model,
     *SDS_RefF_best,SDS_RefF_average,SDS_TranF_best,SDS_TranF_average,
     *SDS_angs_coeff1,SDS_angs_coeff2,SDS_SCAT_ANGLE_OCEAN,
     *SDS_QCONTROL_ocean,SDS_NUMPIXELS,SDS_CLDFRC_ocean,
     *SDS_Tau_Land_Ocean_img,Qcontrol_special,
     *SDS_correc_small_weighting)
         Endif
      ENDIF

c                **** Else and endif If angles are out of bounds.
c                     fill the arrays with _FillValue
      ELSE
c  angles are out of bounds
      QCONTROL=-6
       Call Fill_QAflag_ocean(QA_Flag_Ocean,SDS_QCONTROL_Ocean,
     *              Idata)
       CALL FILLVALUE_Ocean(IDATA,SDS_ref,SDS_ref_STD,
     *SDSTAU_best,SDSTAUS_best,SDSTAUB_best,SDSTAU_average,
     *SDSTAUS_average,SDSTAUB_average,SDS_Least_error,
     *SDS_small_weighting,SDS_sol_INDX_small,SDS_sol_INDX_large,
     *SDSASSY_best,SDSASSY_average,SDS_ccn,sds_mass_conc,
     *SDSBACK_best,SDSBACK_average,SDS_effrad,SDS_AOT_model,
     *SDS_RefF_best,SDS_RefF_average,SDS_TranF_best,SDS_TranF_average,
     *SDS_angs_coeff1,SDS_angs_coeff2,SDS_SCAT_ANGLE_OCEAN,
     *SDS_QCONTROL_ocean,SDS_NUMPIXELS,SDS_CLDFRC_ocean,
     *SDS_Tau_Land_Ocean_img,Qcontrol_special,
     *SDS_correc_small_weighting)
      ENDIF

      
       RETURN
      END

    

C*********************************************************************

       SUBROUTINE READ_LOOK(RGSS,SIGMAS,EXTSMALL,MOMENTSSMALL,
     *    CCNSMALL,EXTNORSMALL,BACKSCTTSMALL,ASSYMSMALL,
     *    RGSB,SIGMAB,EXTBIG,MOMENTSBIG,EXTNORBIG,BACKSCTTBIG,
     *    ASSYMBIG,ALBEDOSMALL,ALBEDOBIG,
     *    ALBEDO_R_SMALL,ALBEDO_R_BIG,ALBEDO_T_SMALL,ALBEDO_T_BIG,
     *    PHC,THET,THET0,AINTS,TAUAS,WAVE,
     *    AINTB,TAUAB,JPHI,ref_rayall,HANDLE_S,HANDLE_L)

C----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:   This subroutine reads the lookup table which is
C                generated from Radiative Transfer code RATRAN for
C                all wavelenghths.
C
C!INPUT PARAMETERS:
C          HANDLE_S        Input read for small particles(lookup).
C          HANDLE_L        Input read for large particles(lookup).
C
C!OUTPUT PARAMETERS:
C      SMALL MODE.........
c              RGSS        RADIUS
C            SIGMAS        SIGMA
C           EXTSMALL       EXTINCTION COEFF
C       MOMENTSSMALL       MOMENTS SMALL MODE
C           CCNSMALL       CLOUD CONDENSATION NUCLII
C        EXTNORSMALL       EXTINCTION COEFF NORMALIZED
C      BACKSCTTSMALL       BACKSCATTERING RATIO
C         ASSYMSMALL       ASSYMETRY FACTOR
C        ALBEDOSMALL       ALBEDO SINGLE SCATTERING
C     ALBEDO_R_SMALL       REFLECTED ALBEDO
C      ALBEDO_T_SMALL      TRANSMITTED ALBEDO
C      LARGE MODE..........
C               RGSB       RADIUS
C             SIGMAB       SIGMA
C             EXTBIG       EXTINCTION COEFF
C         MOMENTSBIG       MOMENTS SMALL MODE
C          EXTNORBIG       EXTINCTION COEFF NORMALIZED
C        BACKSCTTBIG       BACKSCATTERING RATIO
C           ASSYMBIG       ASSYMETRY FACTOR
C          ALBEDOBIG       ALBEDO SINGLE SCATTERING
C       ALBEDO_R_BIG       REFLECTED ALBEDO
C       ALBEDO_T_BIG       TRANSMITTED ALBEDO
C                PHC       Azimuth angle.
C               THET       view angle.
C              THET0       Solar zenith angle.
C              TFLUX       dowanward flux
C               AINTS       radiance(l/fo),fo=1 small mode
C               AINTB       radiance(l/fo),fo=1 large mode
C               TAUAS       optical thickness SMALL MODE
C               TAUAB       optical thickness LARGE MODE
C               NWAV       number of wavelength
C               NTAU       number of opticl thickness.
C               NTH0       number of solar zenith angle.
C              NTHET       number of view angle.
C               NPHI       number of azimuth
C               JPHI       azimuth
C         ref_rayall      radiance(l/fo),fo=1 FOR RAYLEIGH TAUA=0.0
C
C
C !REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c fixed prolog;
C
C !TEAM-UNIQUE HEADER:
C
C !REFERENCES AND CREDITS:
C
C !DESIGN NOTES:
C
C !END
C-----------------------------------------------------------------------

      IMPLICIT NONE
      SAVE

      INCLUDE 'mod04.inc'

      CHARACTER*132 LINE
      REAL EXTSMALL(NUMCASES,NWAV),EXTBIG(NUMCASEB,NWAV)
      REAL RGSS(NUMCASES),SIGMAS(NUMCASES)
      REAL RGSB(NUMCASEB),SIGMAB(NUMCASEB)
      REAL MOMENTSSMALL(NUMCASES,4),MOMENTSBIG(NUMCASEB,4)
      REAL CCNSMALL(NUMCASES),TR
      REAL EXTNORSMALL(NUMCASES,NWAV),EXTNORBIG(NUMCASEB,NWAV)
      REAL BACKSCTTSMALL(NUMCASES,NWAV),BACKSCTTBIG(NUMCASEB,NWAV)
      REAL ASSYMSMALL(NUMCASES,NWAV),ASSYMBIG(NUMCASEB,NWAV)
      REAL ALBEDOSMALL(NUMCASES,NWAV),ALBEDOBIG(NUMCASEB,NWAV)
      REAL ALBEDO_R_SMALL(NTH0,NTAU,NWAV,NUMCASES)
      REAL ALBEDO_R_BIG(NTH0,NTAU,NWAV,NUMCASEB)
      REAL ALBEDO_T_SMALL(NTH0,NTAU,NWAV,NUMCASES)
      REAL ALBEDO_T_BIG(NTH0,NTAU,NWAV,NUMCASEB)
      REAL ALBEDO_R_RAY(NWAV,NTH0)
      REAL ALBEDO_T_RAY(NWAV,NTH0)
      INTEGER ICASE,IFILE,IPHI,ITAU,ITH,ITH0,
     2        IWAV,IJ
      INTEGER JPHI(NPHI)
      REAL  PHC(NPHI),THET(NTHET),THET0(NTH0),WAVE(NWAV)
      REAL  AINTS( NPHI, NTHET, NTH0, NTAU, NWAV, NUMCASES)
      REAL  TAUAS(NUMCASES,NWAV,NTAU)
      REAL  AINTB( NPHI, NTHET, NTH0, NTAU, NWAV, NUMCASEB)
      REAL  TAUAB(NUMCASEB,NWAV,NTAU),DUMMY(NTH0)
      REAL  CCNdummy,ref_rayall(NPHI,NTHET,NTH0,NWAV)
      REAL EFFRADSMALL(NUMCASES),EFFRADBIG(NUMCASEB),QSCT


C read rayleigh data

              IFILE = HANDLE_S(1)
         DO 200 IWAV=1,NWAV
      
c
C Read aerosol information

                  read(IFILE,500)LINE
                  read(IFILE,505)WAVE(IWAV)

c Read ocean information
         DO IJ =1,4
          READ(IFILE,500)LINE
        ENDDO
c Read Atmosphere information

                  READ(IFILE,500)LINE
                  READ(IFILE,515)TR,TAUAS(1,IWAV,1)
        DO IJ = 1,2
         READ(IFILE,500)LINE
        ENDDO
C
c Read all other information
        DO IJ = 1,2
          READ(IFILE,500)LINE
        ENDDO

           READ(IFILE,525)(THET0(IJ),IJ=1,NTH0)

c
                  READ(IFILE,530)(DUMMY(IJ),IJ=1,NTH0)
                  READ(IFILE,530)(DUMMY(IJ),IJ=1,NTH0)
                  READ(IFILE,530)(DUMMY(IJ),IJ=1,NTH0)
                  READ(IFILE,530)(DUMMY(IJ),IJ=1,NTH0)
                  READ(IFILE,530)(ALBEDO_R_RAY(IWAV,IJ),IJ=1,NTH0)
                  READ(IFILE,530)(ALBEDO_T_RAY(IWAV,IJ),IJ=1,NTH0)
c
                  READ(IFILE,500)LINE


            DO 300 ITH0=1,NTH0
                     READ(IFILE,500)LINE
                     IF(ITH0.EQ.1)THEN
                       READ(IFILE,535)(THET(ITH),ITH=1,NTHET)
                     ELSE
                       DO IJ=1,3
                       READ(IFILE,500)LINE
                       ENDDO
                   ENDIF
c
             DO  400 IPHI=1,NPHI
               READ(IFILE,540)JPHI(IPHI),
     *         (ref_rayall(IPHI,ITH,ITH0,IWAV),ITH=1,NTHET)
  400       CONTINUE
c    enddo for ith0
 300       CONTINUE
c    enddo for wav
 200       CONTINUE

C LOOP IS AROUND THET0,NEW FILE FOR EACH SMALL CASE
         DO 10 ICASE =1,NUMCASES
         DO 20 IWAV=1,NWAV
c Leave itau=1 to fill up with taua=0.0 later in subroutine INTANGLE
         DO 30 ITAU = 2,NTAU
C Read aerosol information

                  read(IFILE,500)LINE
                  read(IFILE,505)WAVE(IWAV)
        DO IJ = 1,3
          READ(IFILE,500)LINE
        ENDDO

       READ(IFILE,515)RGSS(ICASE),SIGMAS(ICASE)
       READ(IFILE,500)LINE
       READ(IFILE,515)EFFRADSMALL(ICASE)
       READ(IFILE,520)MOMENTSSMALL(ICASE,1),MOMENTSSMALL(ICASE,2)
       READ(IFILE,520)MOMENTSSMALL(ICASE,3),MOMENTSSMALL(ICASE,4)
       READ(IFILE,515)ALBEDOSMALL(ICASE,IWAV),ASSYMSMALL(ICASE,IWAV)
       READ(IFILE,515)CCNSMALL(ICASE),BACKSCTTSMALL(ICASE,IWAV)
       READ(IFILE,520)QSCT,EXTSMALL(ICASE,IWAV)

c Read ocean information
         DO IJ =1,4
          READ(IFILE,500)LINE
        ENDDO
c Read Atmosphere information

                  READ(IFILE,500)LINE
                  READ(IFILE,515)TR,TAUAS(ICASE,IWAV,ITAU)
        DO IJ = 1,2
         READ(IFILE,500)LINE
        ENDDO
C
c Read all other information
        DO IJ = 1,2
          READ(IFILE,500)LINE
        ENDDO

           READ(IFILE,525)(THET0(IJ),IJ=1,NTH0)

c
                  READ(IFILE,530)(DUMMY(IJ),IJ=1,NTH0)
                  READ(IFILE,530)(DUMMY(IJ),IJ=1,NTH0)
                  READ(IFILE,530)(DUMMY(IJ),IJ=1,NTH0)
                  READ(IFILE,530)(DUMMY(IJ),IJ=1,NTH0)
        READ(IFILE,530)(ALBEDO_R_SMALL(IJ,ITAU,IWAV,ICASE),IJ=1,NTH0)
        READ(IFILE,530)(ALBEDO_T_SMALL(IJ,ITAU,IWAV,ICASE),IJ=1,NTH0)
c
                  READ(IFILE,500)LINE


            DO 40 ITH0=1,NTH0
                     READ(IFILE,500)LINE
                     IF(ITH0.EQ.1)THEN
                       READ(IFILE,535)(THET(ITH),ITH=1,NTHET)
                     ELSE
                       DO IJ=1,3
                       READ(IFILE,500)LINE
                       ENDDO
                   ENDIF
c
             DO  50 IPHI=1,NPHI
               READ(IFILE,540)JPHI(IPHI),
     *         (AINTS(IPHI,ITH,ITH0,ITAU,IWAV,ICASE),ITH=1,NTHET)
   50       continue
c    enddo for ith0
   40       CONTINUE
c    enddo for tau
   30       CONTINUE
c enddo for nwav
C Fill the array for albedo and transmission for all cases tau=0
      TAUAS(ICASE,IWAV,1)=TAUAS(1,IWAV,1)
      DO IJ=1,NTH0
      ALBEDO_R_SMALL(IJ,1,IWAV,ICASE)=ALBEDO_R_RAY(IWAV,IJ)
      ALBEDO_T_SMALL(IJ,1,IWAV,ICASE)=ALBEDO_T_RAY(IWAV,IJ)
      ENDDO

   20       CONTINUE
c    enddo for num of size distribution for small
   10       CONTINUE
                  DO IPHI=1,NPHI
                     PHC(IPHI)=FLOAT(JPHI(IPHI))
                  ENDDO

C READ LARGE CASES OF SIZEDISTRIBUTION



              IFILE = HANDLE_L(1)
         DO 60 ICASE =1,NUMCASEB
         DO 70 IWAV=1,NWAV
c Leave itau=1 to fill up with taua=0.0 later in subroutine INTANGLE
         DO 80 ITAU = 2,NTAU
C Read aerosol information

                  read(IFILE,500)LINE
                  read(IFILE,505)WAVE(IWAV)
        DO IJ = 1,3
          READ(IFILE,500)LINE
        ENDDO

       READ(IFILE,515)RGSB(ICASE),SIGMAB(ICASE)
       READ(IFILE,500)LINE
       READ(IFILE,515)EFFRADBIG(ICASE)
       READ(IFILE,520)MOMENTSBIG(ICASE,1),MOMENTSBIG(ICASE,2)
       READ(IFILE,520)MOMENTSBIG(ICASE,3),MOMENTSBIG(ICASE,4)
       READ(IFILE,515)ALBEDOBIG(ICASE,IWAV),ASSYMBIG(ICASE,IWAV)
       READ(IFILE,515)CCNdummy,BACKSCTTBIG(ICASE,IWAV)
       READ(IFILE,520)QSCT,EXTBIG(ICASE,IWAV)

c Read ocean information
         DO IJ =1,4
          READ(IFILE,500)LINE
        ENDDO
c Read Atmosphere information

                  READ(IFILE,500)LINE
                  READ(IFILE,515)TR,TAUAB(ICASE,IWAV,ITAU)
        DO IJ = 1,2
         READ(IFILE,500)LINE
        ENDDO
C
c Read all other information
        DO IJ = 1,2
          READ(IFILE,500)LINE
        ENDDO

          READ(IFILE,525)(THET0(IJ),IJ=1,NTH0)

c
                  READ(IFILE,530)(DUMMY(IJ),IJ=1,NTH0)
                  READ(IFILE,530)(DUMMY(IJ),IJ=1,NTH0)
                  READ(IFILE,530)(DUMMY(IJ),IJ=1,NTH0)
                  READ(IFILE,530)(DUMMY(IJ),IJ=1,NTH0)
       READ(IFILE,530)(ALBEDO_R_BIG(IJ,ITAU,IWAV,ICASE),IJ=1,NTH0)
       READ(IFILE,530)(ALBEDO_T_BIG(IJ,ITAU,IWAV,ICASE),IJ=1,NTH0)
c
                  READ(IFILE,500)LINE


            DO 90 ITH0=1,NTH0
                     READ(IFILE,500)LINE
                     IF(ITH0.EQ.1)THEN
                       READ(IFILE,535)(THET(ITH),ITH=1,NTHET)
                     ELSE
                       DO IJ=1,3
                       READ(IFILE,500)LINE
                       ENDDO
                   ENDIF
c

             DO  100 IPHI=1,NPHI
               READ(IFILE,540)JPHI(IPHI),
     *         (AINTB(IPHI,ITH,ITH0,ITAU,IWAV,ICASE),ITH=1,NTHET)
 100       continue
c    enddo for ith0
  90      CONTINUE
c    enddo for tau
  80       CONTINUE
c    enddo for wav
C Fill the array for albedo and transmission for all cases tau=0
      TAUAB(ICASE,IWAV,1)=TAUAS(1,IWAV,1)
      DO IJ=1,NTH0
      ALBEDO_R_BIG(IJ,1,IWAV,ICASE)=ALBEDO_R_RAY(IWAV,IJ)
      ALBEDO_T_BIG(IJ,1,IWAV,ICASE)=ALBEDO_T_RAY(IWAV,IJ)
      ENDDO
  70      CONTINUE
c    enddo for num of size distribution for large
  60       CONTINUE
C
c     format statements
c
500               format(132a1)
505               format(t32,f6.4)
510               format(t32,f6.4,t65,e11.4)
515               format(t32,f6.4,t70,f6.4)
520               format(t26,e12.4,t64,e12.4)
525               format(t12,f6.1,3x,6(2x,f5.1,3x)/
     1                   t12,f6.1,3x,6(2x,f5.1,3x))
530               format(t10,7e10.3/t10,7e10.3)
535               format(t10,f4.1,6(5x,f5.1)/t9,7(f5.1,5x)/
     1                   t9,7(f5.1,5x))
540               format(i4,1x,7e10.3/5x,7e10.3/5x,7e11.3)

      RETURN
      END



C*********************************************************************

       SUBROUTINE MAKE_CLDMSK(W470_SYN,W550_SYN,W670_SYN,NCLDMSK_SYN,
     &data_size)

C----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:   This subroutine generates newcloud mask for 250*250
C                pixel resolution.
C
C!INPUT PARAMETERS:
C          CLDMSK_500     cldmask for 500 meter resolution
C          num_resol
C
C!OUTPUT PARAMETERS:
C NCLDMSK_SYN new cloud mask at 250 pixel resolution
C
C
C !REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c fixed prolog;
C
C !TEAM-UNIQUE HEADER:
C
C !REFERENCES AND CREDITS:
C
C !DESIGN NOTES:
C
C !END
C-----------------------------------------------------------------------

        IMPLICIT  NONE
        SAVE

        INCLUDE   'mod04.inc'

        REAL W550_SYN(ISWATH*2,ILINE*2),THRSH
        Real W470_SYN(ISWATH*2,ILINE*2)
        Real W550_SYN_1km(ISWATH,ILINE)

c rhucek 08/09/02: resized arrays RMED and RMEDSQ to ISWATH*2 
c       REAL RMED(ISWATH),RMEDSQ(ISWATH)
        REAL RMED(ISWATH*2),RMEDSQ(ISWATH*2)

c rhucek 08/09/02: declared NROWS array of size ISWATH*2
        INTEGER  NROWS(ISWATH*2),NCLDMSK_SYN(2*ISWATH,2*ILINE+1)
        REAL     W670_SYN(4*ISWATH,4*ILINE)
        INTEGER NUMSQ,I,J,N,NUM,ISTART,IEND
        INTEGER SQNUM,M,IX,IXX,IY,IYY
        integer CldMsk_1km(ISWATH,ILINE)
        integer  IBLUE,JBLUE,numaver,data_size(2)
        real AREFW550,AREFW670 ,W670_SYN_500m(2*ISWATH,2*ILINE)
        integer X2_offset,Y2_offset,l,p,LL,PP,add_clear,JX,JY

C     INTIALIZE THE ARRAY FOR NEW CLOUD MASK

           DO IYY=1,ILINE*2
               DO  IXX=1,data_size(1)*2
                NCLDMSK_SYN(IXX,IYY)=0
               ENDDO
           ENDDO
c
   

           THRSH=.0025  
            CALL CldMsk_3by3_pixel(ISWATH,ILINE,W550_SYN,THRSH,NROWS,
     *                             RMED,RMEDSQ,NCLDMSK_SYN,data_size)
       
c change 670 into 500 meter resolution

         DO   IYY = 1,ILINE*2  
         IBLUE=2*IYY-1
          DO  IXX=1,data_size(1)*2
          JBLUE=2*IXX-1
           numaver=0
           AREFW670=0
          DO IY = IBLUE,2*IYY 
          DO IX= JBLUE,2*IXX 
         if( W670_SYN( IX,IY) .gt.0 .and. W670_SYN( IX,IY) .le.1.0) then
         numaver=numaver+1
        AREFW670=AREFW670+ W670_SYN( IX,IY) 
        endif
        enddo
        enddo
          IF( numaver .eq.4)THEN
          W670_SYN_500m(ixx,iyy)=AREFW670/numaver
          else
          W670_SYN_500m(ixx,iyy)=-9999.99
           ENDIF
           enddo
           enddo 
c recover  overwrite cldmask with clear if ratio condition is satsified to recall thick dust
            DO JY=1,ILINE*2
            DO JX=1,data_size(1)*2
          if( W470_SYN(JX,JY) .gt.0 .and.W670_SYN_500m(JX,JY) .gt.0) then
              if((W470_SYN(JX,JY)/W670_SYN_500m(JX,JY)) .le.0.75) THEN
                  NCLDMSK_SYN(jx,jy)=1  
              ENDIF
         ENDIF
          ENDDO
        ENDDO
C overwrite cldmask  with cloudy data if reflectance at 0.47 gt than the threhold
          DO JY=1,ILINE*2
          DO JX=1,data_size(1)*2
           IF( W470_SYN(JX,JY).gt.0.40) THEN
           NCLDMSK_SYN(jx,jy)=0  
             ENDIF
         ENDDO
       ENDDO
         return
         end
C***************************************************************************
        SUBROUTINE CldMsk_3by3_pixel( ISWATH,ILINE,REF1KM,THRHLD,
     *                                NROWS,RMED,RMEDSQ,CLDMSK,data_size )
C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C              This subroutine derive cloud mask using 3 x 3 spatial 
C              variability based upon MODIS 500 m resolution data
C
C!INPUT PARAMETERS:
C
C             Data_Size   Number of pixels in along and against scan
C                ISWATH   Number of pixels at 1 km resolution along scan
C                 ILINE   Number of pixels at 1 km resolution against scan
C                REF1KM   Reflectance at 500 meter resolution
C               THRHLD1   Threshold1 of 3x3 spatial variability
C               THRHLD2   Threshold2 of pixel reflectance value
C
C!OUTPUT PARAMETERS:
C
C                CLDMSK  Cloud mask
C                  RMED  Mean
C                RMEDSQ  Standard deviation
C
C!REVISION HISTORY:
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
      INTEGER IY,JY,JX,NY,ISWATH,ILINE,Data_Size(2),ISTART
      integer IEND
      INTEGER NROWS(ISWATH*2),CLDMSK(ISWATH*2,ILINE*2),add_numcld
      REAL REF1KM(ISWATH*2,ILINE*2),THRHLD
      REAL RMED(ISWATH*2),RMEDSQ(ISWATH*2),STD,VAR
      SAVE

C 
C Initialize work arrays 
      DO JX = 1, data_size(1)*2
        NROWS(JX) = 0
        RMED(JX) = 0
        RMEDSQ(JX) = 0
      ENDDO

C
C Checking 500 meter resolution 

      NY=0
       DO IY=2,ILINE*2-1
        ISTART=IY-1
        IEND=ISTART+2

        DO JY=ISTART,IEND
          NY=NY+1 

          DO JX=2,data_size(1)*2-1

c             IF(REF1KM(JX,JY).GT.0.0.AND.REF1KM(JX+1,JY).GT.0.0.AND.REF1KM(JX-1,JY).GT.0.0) THEN
              NROWS(JX)=NROWS(JX) + 1
              RMED(JX)=RMED(JX)+(REF1KM(JX,JY)+REF1KM(JX+1,JY)+REF1KM(JX-1,JY))
              RMEDSQ(JX)=RMEDSQ(JX)+(REF1KM(JX,JY)*REF1KM(JX,JY)
     1                             + REF1KM(JX+1,JY)*REF1KM(JX+1,JY)
     2                             + REF1KM(JX-1,JY)*REF1KM(JX-1,JY))
c            ENDIF
          
C...........make clear determination where possible and re-initialize work array elements
            IF(NY.EQ.3) THEN
              
C..............make clear/cloud determination only when all 9 pixels in 3x3 array are valid 
               IF(NROWS(JX) .EQ. 3) THEN
                  VAR=9.0/8.0*(RMEDSQ(JX)/9.0-RMED(JX)*RMED(JX)/81.0)

                  IF(VAR.GT.0.0) THEN
                     STD=SQRT(VAR)

                     IF(STD.LT.THRHLD) THEN
                        CLDMSK(JX,IY)=1
                     ENDIF
                  ENDIF
               ENDIF

               NROWS(JX)=0
               RMED(JX)=0.0
               RMEDSQ(JX)=0.0
             ENDIF

          ENDDO
        ENDDO

        NY=0

      ENDDO 

      DO JX=1,data_size(1)*2
        CLDMSK(JX,1)=CLDMSK(JX,2)
        CLDMSK(JX,ILINE*2)=CLDMSK(JX,ILINE*2-1)
      ENDDO
   
      DO JY=1,ILINE*2
        CLDMSK(1,JY)=CLDMSK(2,JY)
        CLDMSK(data_size(1)*2,JY)=CLDMSK(data_size(1)*2-1,JY)
      ENDDO
      RETURN
      END

C*********************************************************************

        SUBROUTINE AVERAGE(W659_SYN,W865_SYN,W470_SYN,W550_SYN,
     *  W124_SYN,W164_SYN,W213_SYN,W443o_SYN,W551o_SYN,W667o_SYN,
     *  W869o_SYN,CLDMSK_syn,Sunglint_Flag,NCLDMSK_syn,
     *  START_1KM,END_1KM,NUMDATA_550,sd_W659,sd_W865,sd_W470,sd_W550,
     *  sd_W124,sd_W164, sd_W213,sd_W443o,sd_W551o,sd_W667o,sd_W869o,
     *  Ref_ray,MTHET0,WAVE,TOT_cldfree_number,GLINT_ANGLE,iscan,idata,
     * Qcontrol_special, High_Cloud_Flag_500,W138_SYN,CLD_FRAC)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine averages the reflectances for each
C              10*10 pixel  square and finds standdard deviation for
C               averaged Reflectances
C
C!INPUT PARAMETERS:
C         IDATA        Index of bin number in a swath
C    Reflectances of wavelengths used modis,ch1-ch7  identified by W*_SYN
C    Extra wavelengths from ocean color ch9,ch12,ch13 and ch16 will be
C    used for substituting if Ch1 - ch7 do not have valid data.
C    These channels are identified by W*o_SYN.
C
C       W659_SYN
C       W865_SYN
C       W470_SYN
C       W550_SYN
C       W124_SYN
C       W164_SYN
C       W213_SYN
C       W443o_SYN
C       W551o_SYN
C       W667o_SYN
C       W869o_SYN
C       CLDMSK_syn   40*40 array reflectance for cloud mask.
C        Sunglint_Flag  Sunglint Flag from cloud mask
C        START_500     Starting Index for 500 meter resolution
C        END_500       Ending  Index for 500 meter resolution
C        START_250     Starting Index for 250 meter resolution
C        END_250       ending Index for 500 meter resolution
C        START_1KM     Starting Index for 1km resolution
C        END_1KM       ending Index for 1km resolution
c        QCONTROL      quality control index
C        Ref_ray       Rayleigh Reflectance
C        MTHET0        Measured Soalr zenith angle
c
c  Input from the subroutine AVERAGE_to_500meter
c
C     Reflectances averaged for 500 meter resolution
C     wavelengths used modis,ch1-ch7  identified by ref_interm_w*
C    Extra wavelengths from ocean color ch9,ch12,ch13 and ch16 will be
C    used for substituting if Ch1 - ch7 do not have valid data.
C    These channels are identified by ref_interm_w*o
C       ref_interm_w659
C       ref_interm_w865
C       ref_interm_w470
C       ref_interm_w550
C       ref_interm_w124
C       ref_interm_w164
C      ref_interm_w213
C      ref_interm_w443o
C      ref_interm_w551o
C      ref_interm_w667o
C      ref_interm_w869o
C
C      NCLDMSK_syn
c Number of data points in each 500 meter box for all wavelengths used
C      NUMDATA_659
C      NUMDATA_865
C      NUMDATA_470
C      NUMDATA_550
C      NUMDATA_124
C      NUMDATA_164
C      NUMDATA_213
C      NUMDATA_443o
C      NUMDATA_551o
C      NUMDATA_667o
C      NUMDATA_869o
c
C     Total number of valid data  500 meter resolution
C     wavelengths used modis,ch1-ch7  identified by VFlag_w*
C    Extra wavelengths from ocean color ch9,ch12,ch13 and ch16 will be
C    used for substituting if Ch1 - ch7 do not have valid data.
C    These channels are identified by VFlag_w*o
C     VFlag_w659
C     VFlag_w865
C     VFlag_w470
C     VFlag_w550
C     VFlag_w124
C     VFlag_w164
C     VFlag_w213
C     VFlag_w443o
C     VFlag_w551o
C     VFlag_w667o
C     VFlag_w869o
C!OUTPUT PARAMETERS:
C   Reflectances of Wavelengths used modis,ch1-ch7  identified by REFW*
C
C       REFW659
C       REFW865
C       REFW470
C       REFW550
C       REFW124
C       REFW164
C       REFW213
C   STANDARD DEVIATION of Wavelengths used modis,ch1-ch7  identified by sd_w*
C
C       SD_W659
C       SD_W865
C       SD_W470
C       SD_W550
C       SD_W124
C       SD_W164
C       SD_W213
C     NUMDATA_550  Number of pixels used for processing
C
C!REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c fixed prolog;
C
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS:
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'mod04.inc'

      INTEGER START_1KM,END_1KM
      INTEGER IX,IY,WATER,CLDFREE
      INTEGER NUMDATA_659,NUMDATA_865,NUMDATA_470,
     * NUMDATA_550,NUMDATA_124,NUMDATA_164,NUMDATA_213,NUMDATA_443o,
     * NUMDATA_551o,NUMDATA_667o,NUMDATA_869o
      INTEGER NOGLINT,TOT_cldfree_number,cloudy
      REAL Ref_ray(NWAV),MTHET0,CLD_FRAC
      REAL W659_SYN(4*ISWATH,4*ILINE),W865_SYN(4*ISWATH,4*ILINE),
     *     W470_SYN(2*ISWATH,2*ILINE),W550_SYN(2*ISWATH,2*ILINE),
     *     W124_SYN(2*ISWATH,2*ILINE),W164_SYN(2*ISWATH,2*ILINE),
     *     W213_SYN(2*ISWATH,2*ILINE),W138_SYN(ISWATH,ILINE),
     *     W443o_SYN(ISWATH,ILINE),W551o_SYN(ISWATH,ILINE),
     *     W667o_SYN(ISWATH,ILINE),W869o_SYN(ISWATH,ILINE)
      real array(2*IGRIDX*2*IGRIDY),newarray(2*IGRIDX*2*IGRIDY)
       REAL  ref_interm(NWAV,2*IGRIDX*2*IGRIDY)
       REAL  array_interm(NWAV,2*IGRIDX*2*IGRIDY)
       REAL sd_W659,sd_W865, sd_W470,sd_W550,sd_W124,sd_W164,
     *     sd_W213,sd_W443o,sd_W551o,sd_W667o,sd_W869o
      REAL  sd_allwav(NWAV),var_allwav(NWAV)
      REAL del_var_wav659,del_var_wav164,del_var_waV213,del_var_wav124
      real WAVE(NWAV),array_new(2*IGRIDX*2*IGRIDY)
      real ref_val,sd_val
      INTEGER CLDMSK_syn(4*ISWATH,4*ILINE)
      INTEGER NCLDMSK_SYN(2*ISWATH,2*ILINE+1)
      integer indx(4*IGRIDX*4*IGRIDY)
      INTEGER  inum,index_set,inn,ij
      INTEGER vFlag_w659,vFlag_w865,vFlag_w470,
     * vFlag_w550,vFlag_w124,vFlag_w164,vFlag_w213,vFlag_w443o,
     * vFlag_w551o,vFlag_w667o, vFlag_w869o
      INTEGER Sunglint_Flag(ISWATH,ILINE)
      INTEGER n10,n40,iscan,idata,iwav,Qcontrol_special
      integer quality_wav(NWAV), Total_good_points
      real GLINT_ANGLE
      INTEGER  High_Cloud_Flag_500(2*ISWATH,2*ILINE)
         SAVE
        WATER=0
        cldfree=0
        Qcontrol1=0
        Qcontrol_special=0 
       Quality_dust_flag_glint  =0
       Quality_dust_flag_off_glint=0
        DO IWAV=1,NWAV
       Good_pixels(IWAV)=0
       ENDDO
c
c     Apply glint mask

c       CALL  GLINT_IKM(START_1KM,END_1KM,Sunglint_Flag,NOGLINT)
c         IF(NOGLINT.GE.100) THEN

        
         IF(GLINT_ANGLE.GT.30 .AND.GLINT_ANGLE.LE.40 )QCONTROL=10
c
c Call to AVERAGE_to_500meter averages all resolutions to 500 meter resolution
c
       
        CALL   AVERAGE_to_500meter(W659_SYN,W865_SYN,W470_SYN,
     *  W550_SYN,W124_SYN,W164_SYN,W213_SYN,W443o_SYN,W551o_SYN,
     *  W667o_SYN,W869o_SYN,ref_interm,NCLDMSK_syn,
     * NUMDATA_659,NUMDATA_865,NUMDATA_470,NUMDATA_550,
     * NUMDATA_124,NUMDATA_164,NUMDATA_213,NUMDATA_443o,NUMDATA_551o,
     * NUMDATA_667o, NUMDATA_869o,START_1KM,
     * END_1KM,VFlag_w659,VFlag_w865,VFlag_w470,VFlag_w550,VFlag_w124,
     *VFlag_w164,VFlag_w213,VFlag_w443o,VFlag_w551o,VFlag_w667o,
     *VFlag_w869o, cloudy,High_Cloud_Flag_500,W138_SYN,Ref_ray,Wave,
     *  CLD_FRAC)

     
c
c Sort in asending order and get the index for wavelength at 0.865
         do ix=1,NUMDATA_865
         array(ix)=ref_interm(Index_wave_865,ix)
         enddo

       CALL INDEXX(NUMDATA_865,array,INDX)

C Set the arrays to reject 1/4 of all bright and dark pixels based on
c Wavelength of 865 nm

          inum=NUMDATA_865
    

c Reject 1/4 of brightest and darkest cloudfree pixels to eliminate
C the residual shadows and sub_cloud pixels
C
        n10=inum/4
        n40=(inum-n10)


c THrow away 1/4 of brightest & darkest pixels in all wavelengths
c
         IF( ( n40-n10) .gt.10) then
            DO iy=1,NWAV
                inn=0
                DO ix=n10,n40
                     inn=inn+1
                     array_new(inn)=ref_interm(IY,indx(ix))
                ENDDO

cThrow away bad pixles in all wavelengths
          DO IX=1,inn
              if   ( array_new(IX) .GT.0. .AND. array_new(IX) .LE.1)then
                Good_pixels(IY)= Good_pixels(IY)+1
                array_interm(IY,Good_pixels(IY))= array_new(ix)
              endif
          ENDDO 
c  ENDDO for wavelengths
          ENDDO


      
            DO iy=1,NWAV
c Call to subroutine ave_std computes average and standard deviation for
c Reflectances
          IF(Good_pixels(IY) .gt.0) then
              DO IX=1,Good_pixels(IY)
              array(IX)= array_interm(IY,ix)
              ENDDO
            call ave_std(array, Good_pixels(IY),ref_val,sd_val)
             ref_allwav(iy)=ref_val
             sd_allwav(iy)=sd_val
             var_allwav(iy)=sd_val/ref_val
         ELSE
              ref_allwav(iy)=0.0
              var_allwav(iy)=0.0
         ENDIF
c  ENDDO for number of wavelengths
        ENDDO
c                    write(38,1)iscan,idata,total_good_points,
c     *         (Good_pixels(iy), ref_allwav(iy),iy=1,NWAV)
c 1        format(3i5,7(i5,f10.4))
c   
c
c


          do iy = 1,NWAV
C
C   WAVE 0.470 UM
C
          if(iy .eq.1)then
          REFW470=ref_allwav(iy)
          sd_w470=sd_allwav(iy)
C
C   WAVE 0.550 UM
C
          elseif(iy .eq.2)then
          REFW550=ref_allwav(iy)
          sd_w550=sd_allwav(iy)
C
C   WAVE 0.659 UM
C
         elseif (iy .eq.3)then
         REFW659=ref_allwav(iy)
         sd_w659=sd_allwav(iy)
         del_var_wav659=
     *   -(1./(alog(WAVE(iy)/WAVE(Index_wave_865))))*
     *   alog((1.+ var_allwav(iy))/
     *       (1.+var_allwav(Index_wave_865)))

C
C   WAVE 0.865 UM
C
         elseif(iy .eq.4)then
        REFW865=ref_allwav(iy)
         sd_w865=sd_allwav(iy)

C
C   WAVE 1.24 UM
C
         elseif(iy .eq.5)then
         REFW124=ref_allwav(iy)
         sd_w124=sd_allwav(iy)
        del_var_wav124=
     *   -(1./(alog(WAVE(iy)/WAVE(Index_wave_865))))*
     *   alog((1.+ var_allwav(iy))/
     *       (1.+var_allwav(Index_wave_865)))
C
C   WAVE 1.64 UM
C
      elseif(iy .eq. 6)then
        REFW164=ref_allwav(iy)
         sd_w164=sd_allwav(iy)
         del_var_wav164=
     *   -(1./(alog(WAVE(iy)/WAVE(Index_wave_865))))*
     *   alog((1.+ var_allwav(iy))/
     *       (1.+var_allwav(Index_wave_865)))
C
C   WAVE 2.13  UM
C
         elseif(iy .eq. 7)then
        REFW213=ref_allwav(iy)
         sd_w213=sd_allwav(iy)
         del_var_wav213=
     *   -(1./(alog(WAVE(iy)/WAVE(Index_wave_865))))*
     *   alog((1.+ var_allwav(iy))/
     *       (1.+var_allwav(Index_wave_865)))
         endif
c enddo for wavelengths
        enddo
c If GLINT_ANGLE.LE.lt.glintthreshold store the Reflectances standard deviation and number of pixelsonly.
c  
cOFF GLINT..........
         If( GLINT_ANGLE.Gt.GLINT_THRESHOLD  .and.
     &       ((REFW470 .gt.0  .and. REFW659 .gt.0) .and.
     &          ( REFW470/ REFW659  .le. 0.75)))then 
                 Quality_dust_flag_off_glint=1
           endif
c    GLINT...........
         If( GLINT_ANGLE.le.GLINT_THRESHOLD  .and.
     &       ((REFW470 .gt.0  .and. REFW659 .gt.0) .and.
     &          ( REFW470/ REFW659  .le. 0.95)))then 
                 Quality_dust_flag_glint=1   
          endif


        If( GLINT_ANGLE.GT.GLINT_THRESHOLD  .or.
     &     Quality_dust_flag_off_glint .eq.1 .or.
     &     Quality_dust_flag_glint .eq.1) then  

c If there is valid data in 865 nm channel and at least one channel has valid data
      Total_good_points=Good_pixels(2)+Good_pixels(3)+Good_pixels(5)+
     * Good_pixels(6)+Good_pixels(7)
         if(Good_pixels(Index_wave_865) .gt.10 .and. 
     *     Total_good_points.gt.30) then

C   If  Reflectance at wave 865 nm is gt than rhomin1
C   the aerosol content is  enough to process
c
             IF( REFW865 .GT. (Ref_ray(Index_wave_865)+
     *       Ref_ray(Index_wave_865)/10.))THEN

c  Quality control flags are set for the number of cloud free pixels used
c  QCONTROL=1 means that box is less than 10% cloudfree.

c
          NUMDATA_550=good_pixels(index_wave_865)
          inn=good_pixels(index_wave_865)
         if( inn .lt.20)QCONTROL=1
         if( inn .gt.20)QCONTROL=0
c
C   If  Reflectance at wave 865 nm is less than   rhomin2
C   the aerosol content is not enough to derive size-distribution, but
C   enough to derive optical thickness...process
c
           IF( REFW865 .LT.(1.5*Ref_ray(Index_wave_865)))then
           QCONTROL=2
c           Qcontrol_special=2
           endif

             
C
c Test if channel 2.13 and 1.64 um are good to process
C 
           if(Good_pixels(6) .ge. 10) then
           IF(REFW164 * COS(MTHET0*DTR) .LT. 3.600E-04) QCONTROL1=3
            endif
         
           if(Good_pixels(7) .ge. 10) then
           IF(REFW213 * COS(MTHET0*DTR) .LT. 3.110E-04) QCONTROL1=4
            endif
          
            if(Good_pixels(6) .ge. 10 .and. Good_pixels(7) .ge. 10)then
            IF(REFW213 * COS(MTHET0*DTR) .LT. 3.110E-04 .AND.
     *      REFW164 * COS(MTHET0*DTR)  .LT. 3.600E-04)QCONTROL1=5
            endif


C Process only for valid data
            QCONTROL=QCONTROL1
         IF( QCONTROL .GT.0) THEN
C The aerosol type and content are variable
        IF(var_allwav(Index_wave_865) .GT.0.05 .AND.
     *     ABS(del_var_wav659) .GT. 0.15 .OR.
     *     ABS(del_var_wav164) .GT. 0.15 .OR.
     *     ABS(del_var_wav213) .GT. 0.15) QCONTROL=6
cC The aerosolcontent are variable not the aerosol content
        IF(var_allwav(Index_wave_865) .GT.0.05 .AND.
     *      ABS(del_var_wav659) .LT. 0.15 .OR.
     *      ABS(del_var_wav164) .LT. 0.15 .OR.
     *      ABS(del_var_wav213) .LT. 0.15) QCONTROL=7
         ENDIF

C   If  Reflectance at wave 865 nm is lt than rhomin1
C   the aerosol content is  not enough to process
         ELSE
              QCONTROL=-3
             Qcontrol_special=1
        ENDIF
c If there is no valid data in all channel s  
                 ELSE
                  QCONTROL=-10
                 Qcontrol_special=-2
                 ENDIF
c Glint and only Reflectances ,sd and number of pixels will bestored
                 ELSE
               Qcontrol_special=3
            NUMDATA_550=good_pixels(index_wave_865)
            QCONTROL=11
                 ENDIF
c  ELSE for number of cloudfree pixels inn box is too cloudy
        ELSE
c If cloud free pixels are lt 10 no processing is performed .
              QCONTROL=-2
              Qcontrol_special=-2
        ENDIF
 
        IF (QCONTROL .LT.0) THEN
             REFW470=99999
             REFW550=99999
             REFW659=99999
             REFW865=99999
             REFW124=99999
             REFW164=99999
             REFW213=99999
        ENDIF 
           RETURN
           END



C*********************************************************************

       SUBROUTINE storeref_data(IDATA,SDS_ref,SDS_ref_STD,NUMDATA_550,
     *  sd_W470,sd_W550,sd_W659,sd_w865,sd_W124,sd_W164,Sd_W213,
     *  SDS_CLDFRC_ocean,SDS_NUMPIXELS,
     *  SDS_SCAT_ANGLE_OCEAN,CLD_FRAC,SCAT_ANGLE_OCEAN)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine stores the average Reflectance for all wavelengths
C             into arrays to be written to HDF file.
C
C!INPUT PARAMETERS:
C         IDATA          Data point Number
C
CC   STANDARD DEVIATION of Wavelengths used modis,ch1-ch7  identified by sd_w
C
C       SD_W659
C       SD_W865
C       SD_W470
C       SD_W550
C       SD_W124
C       SD_W164
C       SD_W213
C       QCONTROL       quality control
CSCAT_ANGLE_OCEAN      Scattering angle
C        CLD_FRAC      cloud Fraction
C       NUMDATA_550  Number of pixels used for processing
C
C    Reflectances of Wavelengths used modis,ch1-ch7  identified by REFW*
c    passed through mod04.inc
C       REFW659
C       REFW865
C       REFW470
C       REFW550
C       REFW124
C       REFW164
C       REFW213
C!OUTPUT PARAMETERS:for HDF write
c        SDS_ref               Array for Reflectance data
C        SDS_CLDFRC_ocean             Array for Cloud Fraction
C       SDS_QCONTROL           Array for quality control
C       SDS_NUMPIXELS          Array for number of Pixels used
C       SDS_SCAT_ANGLE_OCEAN   Array for Scattering angle
C
C
C!REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c fixed prolog;
C
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS:
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------

          IMPLICIT  NONE
          SAVE

          INCLUDE 'mod04.inc'

          INTEGER I,IDATA,NUMDATA_550
          REAL sd_W659,sd_W865,sd_W470,sd_W550,sd_W124,sd_W164,
     *     sd_W213,CLD_FRAC
           REAL SCAT_ANGLE_OCEAN
          INTEGER*2 SDS_ref(NUMCELLS,NWAV)
          INTEGER*2 SDS_ref_STD(NUMCELLS,NWAV)
          INTEGER * 2 SDS_NUMPIXELS(NUMCELLS),
     &   SDS_CLDFRC_ocean (NUMCELLS),SDS_SCAT_ANGLE_OCEAN(NUMCELLS)

c                **** Store input reflectance data to be written for
c                     HDF output file

         SDS_ref(IDATA,1) =  nint(REFW470*SCALE4+OFFSET4)
         SDS_ref(IDATA,2) =  nint(REFW550*SCALE4+OFFSET4)
         SDS_ref(IDATA,3) =  nint(REFW659*SCALE4+OFFSET4)
         SDS_ref(IDATA,4) =  nint(REFW865*SCALE4+OFFSET4)
         SDS_ref(IDATA,5) =  nint(REFW124*SCALE4+OFFSET4)
         SDS_ref(IDATA,6) =  nint(REFW164*SCALE4+OFFSET4)
         SDS_ref(IDATA,7) =  nint(REFW213*SCALE4+OFFSET4)
         SDS_ref_STD(IDATA,1) = nint(sd_W470*SCALE4+OFFSET4)
         SDS_ref_STD(IDATA,2) = nint(sd_W550*SCALE4+OFFSET4)
         SDS_ref_STD(IDATA,3) = nint(sd_w659*SCALE4+OFFSET4)
         SDS_ref_STD(IDATA,4) = nint(sd_W865*SCALE4+OFFSET4)
         SDS_ref_STD(IDATA,5) = nint(sd_W124*SCALE4+OFFSET4)
         SDS_ref_STD(IDATA,6) = nint(sd_W164*SCALE4+OFFSET4)
         SDS_ref_STD(IDATA,7) = nint(sd_W213*SCALE4+OFFSET4)
         SDS_NUMPIXELS(IDATA)=NUMDATA_550
 
         RETURN
         END



C*********************************************************************

      SUBROUTINE INTANGLE_ray(PHC,THET,THET0,ref_rayall,
     *  MTHET0,MTHET,MPHI,Ref_ray,KSZAM1,KSZAP1,KTHEM1,KTHEP1,
     *  KPHIM1,KPHIP1)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine interpolates rayleigh Reflectance for
C             measured solar zenith, view and azimuthal angle.
C!INPUT PARAMETERS:
C                PHC        Azimuth angle.
C               THET        View angle.
C               THET0       Solar zenith angle.
C          Ref_rayall      Rayleigh Reflectance
C              MTHET0      Measured solar Zenith angle
C               MTHET      Measured view  Zenith angle
C                MPHI      Measured Azimuthal Angle
C               KSZAM1     Starting Index for solar zenith angle
C               KSZAP1     Ending Index for solar zenith angle
C               KTHEM1     Starting Index for view angle
C               KTHEP1     Ending   Index for  view angle
c               KPHIM1     Starting Index for  azimuth angle
C               KPHIP1     Ending   Index for  azimuth angle
C!OUTPUT PARAMETERS: interpolated Rayleigh
C             Ref_ray      interpolated Rayleigh Reflectance
C
C!REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c fixed prolog;
C
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS:
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------

      IMPLICIT NONE
      SAVE

      INCLUDE 'mod04.inc'

      INTEGER IJ,IPHI,ITH,ITH0,IWAV,LOPT
      REAL  PHC(NPHI),THET(NTHET),THET0(NTH0)
      REAL  Ref_rayall( NPHI, NTHET, NTH0,NWAV)
      REAL  Ref_ray(NWAV)
      REAL  MTHET0,MTHET,MPHI
      REAL  X(100),Y(100),XX1(100),YY1(100),XX2(100),YY2(100),Y1
      INTEGER KSZAM1,KSZAP1,KTHEM1,KTHEP1,KPHIM1,KPHIP1
      INTEGER LL,MM,NN


C LOOP IS AROUND THET0,NEW FILE FOR EACH THETA0
c
          Y1=0.0
          DO IJ = 1,100
          X(IJ)=0.0
          Y(IJ)=0.0
          XX1(IJ)=0.0
          YY1(IJ)=0.0
          XX2(IJ)=0.0
          YY2(IJ)=0.0
          ENDDO
          DO 20 IWAV=1,NWAV
          LL=0
          DO 30 ITH0 = KSZAM1,KSZAP1
          MM=0
          DO 40 ITH= KTHEM1,KTHEP1
          NN=0
          DO 50 IPHI =KPHIM1,KPHIP1
          NN=NN+1
          X(NN)=PHC(IPHI)
          Y(NN)=Ref_rayall(IPHI,ITH,ITH0,IWAV)
 50       CONTINUE
c
           CALL INTERP(NN,MPHI,X,Y,Y1)
            MM=MM+1
           XX1(MM)=THET(ITH)
           YY1(MM)=Y1
 40         CONTINUE
            y1=0.0
           CALL INTERP(MM,MTHET,XX1,YY1,Y1)
c
             LL=LL+1
            XX2(LL)=THET0(ITH0)
            YY2(LL)=Y1
 30        CONTINUE

             y1=0.0
           CALL INTERP(LL,MTHET0,XX2,YY2,Y1)
c                  ****** change to Reflectance units

        Ref_ray(IWAV)=(Y1*PI)/(COS(DTR*MTHET0))
  20       CONTINUE

            RETURN
            END



C*********************************************************************

      SUBROUTINE INTANGLE(PHC,THET,THET0,AINTS,TAUAS,
     *           AINTB,TAUAB,MTHET0,MTHET,MPHI,REFSMALL,REFBIG,
     *           Ref_ray,KSZAM1,KSZAP1,KTHEM1,KTHEP1,KPHIM1,KPHIP1)

C----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:  Subroutine INTANGLE interpolates the lookup
C               reflectances to the measured geometry.
C
C!INPUT PARAMETERS:
C          PHC        Azimuth angle.
C         THET        view angle.
C         THET0       Solar zenith angle.
C         AINTS       radiance(l/fo),fo=1 for small mode
C         AINTB       radiance(l/fo),fo=1 for large mode
C         TAUAS       optical thickness for small mode
C         TAUAb       optical thickness for large  mode
C         KSZAM1      Starting Index for solar zenith angle
C         KSZAP1      Ending Index for solar zenith angle
C         KTHEM1      Starting Index for view angle
C         KTHEP1      Ending   Index for  view angle
c         KPHIM1      Starting Index for  azimuth angle
C         KPHIP1      Ending   Index for  azimuth angle
C      MTHETA0        Solar zenith angle.
C        MTHET        View angle.
C         MPHI        Azimuth angle.
C
C!OUTPUT PARAMETERS:
C      REFSMALL       reflectance from input interpolated
C                    for MODIS geometry(small modes)
C      REFBIG        reflectance from input interpolated
C                    for MODIS geometry(large modes)
C     Ref_ray        Reflectance for rayleigh only
C
C!REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c fixed prolog;
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

      INCLUDE 'mod04.inc'

      CHARACTER*132 LINE
      CHARACTER*45  LINE1
      INTEGER ICASE,IJ,IPHI,ISIZE,ITAU,ITH,ITH0,IWAV,LOPT,NUMCASE
      REAL  PHC(NPHI),THET(NTHET),THET0(NTH0)
      REAL  AINTS( NPHI, NTHET, NTH0, NTAU, NWAV, NUMCASES)
      REAL  TAUAS(NUMCASES,NWAV,NTAU)
      REAL  AINTB( NPHI, NTHET, NTH0, NTAU, NWAV, NUMCASEB)
      REAL  TAUAB(NUMCASEB,NWAV,NTAU)
      REAL  Ref_ray(NWAV)
      REAL  REFSMALL(NUMCASES,NWAV,NTAU),REFBIG(NUMCASEB,NWAV,NTAU)
      REAL  MTHET0,MTHET,MPHI
      REAL  X(100),Y(100),XX1(100),YY1(100),XX2(100),YY2(100),Y1
      INTEGER KSZAM1,KSZAP1,KTHEM1,KTHEP1,KPHIM1,KPHIP1
      INTEGER LL,MM,NN

C LOOP IS AROUND THET0,NEW FILE FOR EACH THETA0
c

          Y1=0.0
          DO IJ = 1,100
          X(IJ)=0.0
          Y(IJ)=0.0
          XX1(IJ)=0.0
          YY1(IJ)=0.0
          XX2(IJ)=0.0
          YY2(IJ)=0.0
          ENDDO
          DO 5 ISIZE= 1,2
          IF(ISIZE .EQ.1)NUMCASE=NUMCASES
          IF(ISIZE .EQ.2)NUMCASE=NUMCASEB
          DO 10 ICASE =1,NUMCASE
          DO 20 IWAV=1,NWAV
c interpolate starting from optical thickess indexed 2.
C at the end of routine fill the array with interpoltaed ta=0.0
          DO 30  ITAU  = 2,NTAU
             NN=0
          DO  40 ITH0  = KSZAM1,KSZAP1
             MM=0
          DO  50  ITH  = KTHEM1,KTHEP1
              LL=0
          DO 60  IPHI  = KPHIM1,KPHIP1
           LL=LL+1
          X(LL)=PHC(IPHI)
         IF( ISIZE.EQ.1)Y(LL)=AINTS(IPHI,ITH,ITH0,ITAU,IWAV,ICASE)
         IF( ISIZE.EQ.2)Y(LL)=AINTB(IPHI,ITH,ITH0,ITAU,IWAV,ICASE)
 60       CONTINUE
           CALL INTERP(LL,MPHI,X,Y,Y1)
           MM=MM+1
           XX1(MM)=THET(ITH)
           YY1(MM)=Y1
 50         CONTINUE
            y1=0.0
           CALL INTERP(MM,MTHET,XX1,YY1,Y1)
             NN=NN+1
            XX2(NN)=THET0(ITH0)
            YY2(NN)=Y1
 40        CONTINUE
             y1=0.0
            CALL INTERP(NN,MTHET0,XX2,YY2,Y1)
c                  ****** change to Reflectance units

      IF(ISIZE.EQ.1)REFSMALL(ICASE,IWAV,ITAU)=(Y1*PI)/(COS(DTR*MTHET0))
      IF(ISIZE.EQ.2)REFBIG(ICASE,IWAV,ITAU)=(Y1*PI)/(COS(DTR*MTHET0))
c
  30       CONTINUE
c
c fill empty array of itau=1 with rayleigh (taua=0.0)
c
            IF(ISIZE.EQ.1)REFSMALL(ICASE,IWAV,1)=Ref_ray(IWAV)
            IF(ISIZE.EQ.2)REFBIG(ICASE,IWAV,1)=Ref_ray(IWAV)

  20       CONTINUE
  10       CONTINUE
  5        CONTINUE

            RETURN
            END


C*********************************************************************

      SUBROUTINE INTERP(M,XBAR,X,Y,Y1)

C----------------------------------------------------------------------
C !F77
C
C !DESCRIPTION: This subroutine is a general purpose routine and
C              interpolates linearly.  Value of y1 is interpolated
C              for x1.no extrapolation is allowed.
C
C !INPUT PARAMETERS:I,M,X1,X,Y
C
C !OUTPUT PARAMETERS:Y1,LOPT
C
C !REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c Revision 1.2  1997/01/27  19:56:07  jguu
c Size for a filename string buffer is changed to
c PGS_PC_FILE_PATH_MAX.
c The number of scans of the processed granule is passed in
c WIS_ANC and READ_AVERAGE_MODIS_DEPEN.
c All "GO TO"s are eliminated.
c Strict equality comparisions are changed to inequality comparisions.
c
C
C !TEAM-UNIQUE HEADER:
C
C !END
C----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER IL,LL,LOPT,M
      REAL PINTEN,PPHI,SINTEN,SPHI
      REAL  X(M),Y(M),Y1,XBAR,Diff

      Y1=0.0
      LOPT=0
      LL=M-1
        DO 230 IL=1,LL
C        Extrapolation on lower bound
       IF(XBAR .LE.X(1))THEN
          PPHI=X(1)
          SPHI=X(2)
          PINTEN=Y(1)
          SINTEN=Y(2)
           Diff=(SPHI-PPHI)
           if(Diff . Le. 0.0) Diff=1
          Y1=PINTEN+((SINTEN-PINTEN)*((XBAR-PPHI)/Diff))
               LOPT=1

*/  Modified by JC Guu  01/09/97
*/  "GO TO 290" is changed to RETURN

           RETURN

C        Extrapolation on upper bound
       ELSEIF(XBAR .GE.X(LL+1)) THEN
           PPHI=X(LL)
           SPHI=X(LL+1)
         PINTEN=Y(LL)
         SINTEN=Y(LL+1)
         Diff=(SPHI-PPHI)
           if(Diff . Le. 0.0) Diff=1
          Y1=PINTEN+((SINTEN-PINTEN)*((XBAR-PPHI)/Diff))
              LOPT=1

*/  Modified by JC Guu  01/09/97
*/  "GO TO 290" is changed to RETURN

           RETURN

C       interpolation
       ELSEIF (XBAR .GE.X(IL) .AND.XBAR.LE. X(IL+1)) THEN
         PPHI=X(IL)
         SPHI=X(IL+1)
         PINTEN=Y(IL)
         SINTEN=Y(IL+1)
         Diff=(SPHI-PPHI)
           if(Diff . Le. 0.0) Diff=1
          Y1=PINTEN+((SINTEN-PINTEN)*((XBAR-PPHI)/Diff))
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



C*********************************************************************

      SUBROUTINE SUBMIN(FUNMIN,Ref_ray)

C----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:  THIS SUBROUTINE IS A GENERAL PURPOSE ROUTINE
C               AND INTERPOLATES EXPONANTIALLY. IGNORE SENDS
C               VALUE OF 0 IF SUBROUTINE INTEXP IS USED AND
C               1 IF IT FINDS THE FUNCTION TO BE LINEAR.
C
C!INPUT PARAMETERS:
C          REFSMALL    reflectance from input interpolated
C                      for MODIS geometry(small modes)
C            REFBIG    reflectance from input interpolated
c                      for MODIS geometry(large modes)
C              TAUA    Optical thickness for lookup table.
C             NSIZE    Size,big small
C           NUMCASE    Number of cases
C              NWAV    Number of wavelengths
C              NTAU    Number of optical thicknesses
C
C!OUTPUT PARAMETERS:   NONE
C
C!DESIGN NOTE:
C AX IS LOWER END-POINT OF THE INTERVAL IN WHICH MINIMUM OF FMIN
C IS TO BE LOCATED.
C BX IS THE UPPER END-POINT OF THE INTERVAL.
C TOL IS LENGTH OF THE FINAL SUBINTERVAL CONTAINING THE MINIMUM.
C FMIN IS OUTPUT , THE APPROXIMATE MINIMUM OF THE FUNCTION FMIN ON THE
C ORGINAL INTERVAL(A,B)
C
C!REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c fixed prolog;
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

      INCLUDE 'mod04.inc'

C THIS SUBROUTINE IS A GENERAL PURPOSE ROUTINE  AND INTERPOLATES
C EXPONANTIALLY.IGNORE SENDS VALUE OF 0 IF SUBROUTINE INTEXP IS
C USED AND 1 IF IT FINDS THE FUNCTION TO BE LINEAR.
      INTEGER IWAV,NJTAU,NJWAV
      REAL   XMIN
      REAL   AX,BX,FMIN,funmin
      REAL  Ref_ray(NWAV)
      real Ref_ray_save(NWAV)
      EXTERNAL  FMIN
      COMMON/TWO/NJTAU,NJWAV,Ref_ray_save
      SAVE
      NJTAU=NTAU
      NJWAV=NWAV
      do Iwav=1,NWAV
      Ref_ray_save(iwav)=Ref_ray(iwav)
      enddo 
      AN=0.0
C TAUAS OR TAUAB CAN BE USED HERE, THEY CARRY SAME VALUES......


C
C  SUBROUTINE MNBRAK AND GOLDEN ARE USED TO FIND THE MINIMAL OF
C FUNCTION FMIN.  LOOK FOR NUMERICAL RECIPE FOR THE EXPLANATION
C
C AX IS LOWER END-POINT OF THE INTERVAL IN WHICH MINIMUM OF FMIN
C IS TO BE LOCATED.
C BX IS THE UPPER END-POINT OF THE INTERVAL.
C TOL IS LENGTH OF THE FINAL SUBINTERVAL CONTAINING THE MINIMUM.
C FMIN IS OUTPUT , THE APPROXIMATE MINIMUM OF THE FUNCTION FMIN ON THE
C ORGINAL INTERVAL(A,B)

         AX = 0.00
         BX = 1.00

c                  ***** call to halving routine findes the minimum
c                         function fmin
c
        CALL HALVING(AX,BX,FMIN,XMIN,FUNMIN)
c                  ***** call tocompute tausmall and big for xminetc
c
              AN=XMIN
        RETURN
        END



C*****************************************************************

      REAL FUNCTION FMIN(XMIN)

C----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:    FUNCTION FMIN IS FUNCTION TO BE MINIMIZED.
C
C!INPUT PARAMETERS:
C                XMIN    minimum value
C
C!OUTPUT PARAMETERS:
C                FMIN    value of function at minimum value
C
C
C!REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c fixed prolog;
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

      INCLUDE 'mod04.inc'

      INTEGER ITAU,IWAV,LOPT,NJTAU,NJWAV,NUMWAV,INDEX_wave
      REAL  RADWAV,Y1,asum1,sum_good_pixels,Ref_ray_save(NWAV)
      REAL XMIN,XS(100,100),YS(100,100),X(100),Y(100),Denom
      COMMON/TWO/NJTAU,NJWAV,Ref_ray_save

c If two long wavelengths are not available,force mode to small mode.
         IF( QCONTROL1 .EQ. 3 .or.QCONTROL1 .EQ. 4 .or.
     *      QCONTROL1 .eq.5)XMIN=1.0
        ASUM=0.0
        ASUM1=0.0
         sum_good_pixels=0
        FMIN=0.0
        TAUX55=0.0

C                   ******COMPUTE FUN TO BE MINIMIZED.
C

       DO 100 IWAV = 1,NJWAV
       DO 101 ITAU = 1,NJTAU
         XS(ITAU,IWAV)=TAUAL(ITAU)
         YS(ITAU,IWAV)=(REFSMALLL(IWAV,ITAU)*XMIN)
     *           +((1.-XMIN)*REFBIGL(IWAV,ITAU))
101    CONTINUE
100    CONTINUE
C
C                   ********COMPUTE FOR TAU FOR  USING
C                           RADAINCE VALUE OF 0.860 UM AND
C                           TAU VALUES OF WAV550
C
         DO 102 ITAU = 1,NJTAU
            X(ITAU)=YS(ITAU,Index_wave_865)
            Y(ITAU)=XS(ITAU,Index_wave_550)
102      CONTINUE
           Y1=0
           CALL INTERP(NJTAU,REFW865,X,Y,Y1)
            TAUX55=y1

C
C                 ******* FOR TAUX55 COMPUTE Reflectance FOR
C                     ALL WAVELENGTHS.
C
      DO 104 IWAV = 1,NJWAV
         DO 103 ITAU = 1,NJTAU
            X(ITAU)=TAUAL(ITAU)
            Y(ITAU)=YS(ITAU,IWAV)
103      CONTINUE
         y1=0
          CALL INTERP(NJTAU,TAUX55,X,Y,Y1)
            ALXWAV(IWAV)=Y1
104   CONTINUE
c For summation do not use 1st wavelength
          NUMWAV=NWAVN-1
c if wavelength 1.64 0r 2.13 is small  do not use for inversion
        IF(QCONTROL1.EQ.3 .OR. QCONTROL1.EQ.4) NUMWAV=NWAVN-2
        IF(QCONTROL1 .EQ.5)NUMWAV=NWAVN-3
       DO 105 IWAV = 1,NUMWAV
         INDEX_wave=IWAV+1
c if 1.64 is small ignore 1.64
          IF( IWAV .EQ.5 .AND.QCONTROL1.EQ.3)INDEX_wave=IWAV+2
c if 2.13 issmall ignore 2.13
          IF( IWAV .EQ.5 .AND.QCONTROL1.EQ.4)INDEX_wave=IWAV+1
c if 1.64 and 2.13 is small  ignore 1.64 and 2.13
        IF( IWAV .EQ.4 .AND.QCONTROL1.EQ.5)INDEX_wave=IWAV+1
C
        Denom=(ref_allwav(INDEX_wave)-Ref_ray_save(INDEX_wave))+0.01 

        if( Denom .lt. 0.01)Denom=0.01
          ASUM1= ASUM1+(((ref_allwav( INDEX_wave)-ALXWAV(INDEX_wave))
     *  /(Denom))**2)*
     *   Good_pixels(INDEX_wave)
        sum_good_pixels=sum_good_pixels+Good_pixels(INDEX_wave)
C
105   CONTINUE
               ASUM=SQRT(ASUM1/REAL(sum_good_pixels))
         FMIN=ASUM
      RETURN
      END



C********************************************************************

        SUBROUTINE COMPUTE_alltau(EXTSMALL,EXTBIG,EXTNORSMALL,
     *EXTNORBIG,ISMALL,IBIG)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine computes CCN, assymetry factor,
C              backscattering ratio, effective radius, effective
C              variance, and optical thicknesses for large and small
C              mode.
C
C!INPUT PARAMETERS: all the variables are for lookup table.
C       TAUX55SMALL Optical thickness contribution small particles
C       TAUX55BIG   Optical thickness contribution large particles
C        NWAV       Number of wavelengths
C        EXTSMALL   extinction coeff small particles
C            IBIG   Index for big mode
C           ISMALL  Index for small mode.
C
C!OUTPUT PARAMETERS:   all variables are computed for large and
C      TAU_COMPUTED Optical depth for all wavelengths
cTAU_SMALL_COMPUTED Optical depth for all wavelengths for Small particles
CTAU_BIG_COMPUTED   Optical depth for all wavelengths for large particles
C
C!REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c fixed prolog;
C
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS:
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------

      IMPLICIT NONE
      SAVE

      INCLUDE 'mod04.inc'

      INTEGER IBIG,ISMALL,IWAV
      REAL EXTSMALL(NUMCASES,NWAV),EXTBIG(NUMCASEB,NWAV)
      REAL EXTNORSMALL(NUMCASES,NWAV),EXTNORBIG(NUMCASEB,NWAV)
c       if(TAUX55. GT. MAXTAU)TAUX55=MAXTAU

c  Since we see the glint effect around tau of 0.5  no retrivals will be done
c less than tau of 0.5 in glint area. It is set to -ve value so that later on it is 
c filled with fill values

           if(TAUX55 .le.0.7 .and. Quality_dust_flag_glint  .eq.1)TAUX55= -0.02 

c   if tau at 0.55um is greater than maxtau(ie 5.00) taux55 is set to maxtau for
c  glint and off glint heavy dust episodes

        if((TAUX55. GT. MAXTAU .and. Quality_dust_flag_glint  .eq.1) 
     *    .or. (TAUX55. GT. MAXTAU .and. Quality_dust_flag_off_glint .eq.1))
     *     TAUX55=MAXTAU 

C      compute normalized extiction coeff
      DO IWAV = 1,NWAV
       EXTNORSMALL(ISMALL,IWAV)=
     *          EXTSMALL(ISMALL,IWAV)/EXTSMALL(ISMALL,Index_wave_550)
                EXTNORBIG(IBIG,IWAV)=
     *                EXTBIG(IBIG,IWAV)/EXTBIG(IBIG,Index_wave_550)
       ENDDO
c Changes 2/28
        DO IWAV = 1,NWAV
        TAUX55SMALL=TAUX55*AN
        TAUX55BIG=TAUX55-TAUX55SMALL
           TAU_COMPUTED(IWAV)=TAUX55SMALL*EXTNORSMALL(ISMALL,IWAV)
     *                        +TAUX55BIG*EXTNORBIG(IBIG,IWAV)
       TAU_SMALL_COMPUTED(IWAV)=TAUX55SMALL*EXTNORSMALL(ISMALL,IWAV)
       TAU_BIG_COMPUTED(IWAV)=   TAUX55BIG *EXTNORBIG(IBIG,IWAV)
       ENDDO
c end of  Changes 2/28
       RETURN
       END



C********************************************************************

      SUBROUTINE COMPUTE_INVERVAR(EXTSMALL,
     *         EXTBIG,CCNSMALL,MOMENTSSMALL,
     *         MOMENTSBIG,NSMALL,NBIG,EFFRADIUS,EFFVARIANCE,
     *         ISMALL,IBIG,ALBEDOSMALL,ALBEDOBIG,
     *         CCN,BACKSCTTSMALL,BACKSCTTBIG,ASSYMSMALL,ASSYMBIG,
     *      BACKSCATT,ASSYM,REF_FLUX,TRANS_FLUX,ALBEDO_R_SMALL_final,
     *      ALBEDO_R_BIG_final,ALBEDO_T_SMALL_final,ALBEDO_T_BIG_final,
     *      MASS_CON_OCEAN,ALBEDO_R_SMALL_tau,MTHET0)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine computes CCN, assymetry factor,
C              backscattering ratio, effective radius, effective
C              variance, and optical thicknesses for large and small
C              mode.
C
C!INPUT PARAMETERS: all the variables are for lookup table.
C       TAUX55SMALL Optical thickness contribution small particles
C       TAUX55BIG   Optical thickness contribution large particles
C        NUMCASE    Number of large and small cases
C        NWAV       Number of wavelengths
C        EXTSMALL   extinction coeff small particles
C    MOMENTSSMALL   Moments for small particles.
C        CCNSMALL   ccn for small particles.
C     EXTNORSMALL   Normalized extinction for small particles.
C   BACKSCTTSMALL   Back scattering for small particles.
C      ASSYMSMALL   Assymetry factor for small particles.
C     ALBEDOSMALL   single scattering albedo small particles
C          EXTBIG   extinction coeff large particles
C      MOMENTSBIG   Moments for large particles.
C       EXTNORBIG   ccn for big particles.
C     BACKSCTTBIG   Back scattering for large particles.
C         ASSYMBIG  Assymetry factor for large particles.
C       ALBEDOBIG   single scattering albedo large particles
CALBEDO_R_SMALL_final Albedo for monodirectionally incident radiation(SMALL)
CALBEDO_R_BIG_final  Albedo for monodirectionally incident radiation(LARGE)
CALBEDO_T_SMALL_final Trans for monodirectionally incident radiation(SMALL)
CALBEDO_T_BIG_final   Trans for monodirectionally incident radiation(LARGE)
C            IBIG   Index for big mode
C           ISMALL  Index for small mode.
C
C!OUTPUT PARAMETERS:   all variables are computed for large and
c                     small modes for the different solutions.
c           NSMALL  ratio of taux55small to extnor(small mode)
c           NBIG    ratio of taux55small to extnor(large mode)
c         EFFRADIUS effective radius weighed for large and small mode
c       EFFVARIANCE standard deviation for effective radius
c        TAUSMALL   ratio of TAUX55SMALL/ EXTNORSMALL
c        TAUBIG     ratio of TAUX55BIG  /EXTNORBIG
c        BACKSCATT  backscattering facror for large and small
c        ASSYM      assymetry factor for small and large
C        CCN        ccn for the solution
C      TAU_COMPUTED Optical depth for all wavelengths
cTAU_SMALL_COMPUTED Optical depth for all wavelengths for Small particles
CTAU_BIG_COMPUTED   Optical depth for all wavelengths for large particles
C     REF_FLUX      Albedo for monodirectionally incident radiation
C     TRANS_FLUX    Trans for monodirectionally incident radiation
C
C!REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c fixed prolog;
C
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS:
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------

      IMPLICIT NONE
      SAVE

      INCLUDE 'mod04.inc'

      INTEGER IBIG,ISMALL,IWAV
      REAL EXTSMALL(NUMCASES,NWAV),EXTBIG(NUMCASEB,NWAV)
      REAL MOMENTSSMALL(NUMCASES,4),MOMENTSBIG(NUMCASEB,4)
      REAL CCNSMALL(NUMCASES),NSMALL,NBIG
      REAL EFFRADIUS,EFFVARIANCE,CCN,BACKSCATT(NWAV),ASSYM(NWAV)
      REAL ALBEDOSMALL(NUMCASES,NWAV),ALBEDOBIG(NUMCASEB,NWAV)
      REAL BACKSCTTSMALL(NUMCASES,NWAV),BACKSCTTBIG(NUMCASEB,NWAV)
      REAL ASSYMSMALL(NUMCASES,NWAV),ASSYMBIG(NUMCASEB,NWAV)
      REAL REF_FLUX(NWAV),TRANS_FLUX(NWAV)
      REAL ALBEDO_R_SMALL_final(NWAV,NUMCASES)
      REAL ALBEDO_R_BIG_final(NWAV,NUMCASEB)
      REAL ALBEDO_T_SMALL_final(NWAV,NUMCASES)
      REAL ALBEDO_T_BIG_final(NWAV,NUMCASEB),MTHET0
      REAL MASS_CON_OCEAN,ALBEDO_R_SMALL_tau(NTAU,NWAV,NUMCASES)

      NBIG=0.0
      NSMALL=0.0
      CCN=0.0
      EFFRADIUS=0.0
      MASS_CON_OCEAN=0.0
C                             *** compute  ratio extsmall
c
          NSMALL=TAUX55SMALL/EXTSMALL(ISMALL,Index_wave_550)
           CCN=NSMALL*CCNSMALL(ISMALL)

          NBIG=TAUX55BIG/EXTBIG(IBIG,Index_wave_550)

C                             *** compute Effective radius
         IF(NSMALL .GT. 0.0 .OR.NBIG .GT.0.0 )then
        EFFRADIUS=(NSMALL*MOMENTSSMALL(ISMALL,3)+
     *  NBIG*MOMENTSBIG(IBIG,3)) /
     *  (NSMALL*MOMENTSSMALL(ISMALL,2)+
     *   NBIG*MOMENTSBIG(IBIG,2))
c        MASS_CON_OCEAN=(NSMALL*MOMENTSSMALL(ISMALL,3)+
c     *  NBIG*MOMENTSBIG(IBIG,3)) /(NSMALL+NBIG)
c NEW definition 9/24/2003.........
         MASS_CON_OCEAN=(NSMALL*MOMENTSSMALL(ISMALL,3)+
     *  NBIG*MOMENTSBIG(IBIG,3)) 
C                             *** compute Effective variance
        EFFVARIANCE=((NSMALL*MOMENTSSMALL(ISMALL,4)+
     *   NBIG*MOMENTSBIG(IBIG,4))/((EFFRADIUS**2)*
     *  (NSMALL*MOMENTSSMALL(ISMALL,2)+
     *  NBIG*MOMENTSBIG(IBIG,2)))-1.)
          ELSE
         EFFRADIUS=0.0
         EFFVARIANCE=0.0
        MASS_CON_OCEAN=0.0
         ENDIF
C                           **** compute spectral optical thickness
        DO IWAV = 1,NWAV



C                           **** compute backscattering ratio
          IF( TAU_SMALL_COMPUTED(IWAV) .GT.0.0 .OR.
     *          TAU_BIG_COMPUTED(IWAV) .GT. 0.0) THEN
        BACKSCATT(IWAV)=
     *      (ALBEDOSMALL(ISMALL,IWAV)* TAU_SMALL_COMPUTED(IWAV)*
     *                   BACKSCTTSMALL(ISMALL,IWAV)
     *    +  ALBEDOBIG(IBIG,IWAV)*TAU_BIG_COMPUTED(IWAV)*
     *           BACKSCTTBIG(IBIG,IWAV))/
     *  (ALBEDOSMALL(ISMALL,IWAV)*TAU_SMALL_COMPUTED(IWAV) +
     *   ALBEDOBIG(IBIG,IWAV)*TAU_BIG_COMPUTED(IWAV))
C                           **** compute Assymetry factor

       ASSYM(IWAV)=(ALBEDOSMALL(ISMALL,IWAV)*TAU_SMALL_COMPUTED(IWAV)*
     *            ASSYMSMALL(ISMALL,IWAV)
     *        +  ALBEDOBIG(IBIG,IWAV)*TAU_BIG_COMPUTED(IWAV)*
     *           ASSYMBIG(IBIG,IWAV))/
     *  (ALBEDOSMALL(ISMALL,IWAV)*TAU_SMALL_COMPUTED(IWAV)  +
     *   ALBEDOBIG(IBIG,IWAV)*TAU_BIG_COMPUTED(IWAV))

         REF_FLUX(IWAV)=( 
     *                  TAU_SMALL_COMPUTED(IWAV)*
     *            ALBEDO_R_SMALL_final(IWAV,ISMALL)
     *        +   TAU_BIG_COMPUTED(IWAV)*
     *            ALBEDO_R_BIG_final(IWAV,IBIG))/
     *  ( TAU_SMALL_COMPUTED(IWAV)  +
     *    TAU_BIG_COMPUTED(IWAV)) 
c subtract rayleigh to compute  and * mu0 because it is divided in LUT
c subtract rayleigh to compute  and * mu0 because it is divided in LUT
        REF_FLUX(IWAV)=(REF_FLUX(IWAV)-
     *         ALBEDO_R_SMALL_tau(1,IWAV,1))*(COS(MTHET0*DTR)) 

 
       
C                      **** compute transmittance
         TRANS_FLUX(IWAV)=( 
     *    TAU_SMALL_COMPUTED(IWAV)*
     *            ALBEDO_T_SMALL_final(IWAV,ISMALL)
     *        +  TAU_BIG_COMPUTED(IWAV)*
     *            ALBEDO_T_BIG_final(IWAV,IBIG))/
     *  ( TAU_SMALL_COMPUTED(IWAV)  +
     *    TAU_BIG_COMPUTED(IWAV))
       ELSE
            BACKSCATT(IWAV)=0.0
            ASSYM(IWAV)=0.0
            REF_FLUX(IWAV)=0.0
            TRANS_FLUX(IWAV)=0.0
       ENDIF
C
           ENDDO

       RETURN
       END



C*********************************************************************

      SUBROUTINE  HALVING(AX,BX,F,XMIN,FUNMIN)

C-----------------------------------------------------------------------
C !F77
C
C !DESCRIPTION: This subroutine findes the minimum of function by
C              NEWTON halving concept.
C
C !INPUT PARAMETERS:
C         AX      Intial condition for halving
c         BX      end condition for the intial
C         F       function whose minimum has to be determined.
C
C !OUTPUT PARAMETERS:
C          XMIN    minimum of function
C        FUNMIN    value of the minum function at xmin
C
C !REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c Revision 1.2  1997/01/27  19:56:07  jguu
c Size for a filename string buffer is changed to
c PGS_PC_FILE_PATH_MAX.
c The number of scans of the processed granule is passed in
c WIS_ANC and READ_AVERAGE_MODIS_DEPEN.
c All "GO TO"s are eliminated.
c Strict equality comparisions are changed to inequality comparisions.
c
C
C !TEAM-UNIQUE HEADER:
C
C !END
C----------------------------------------------------------------------

      IMPLICIT NONE
      SAVE

      REAL AX,BX,F,XMIN,X,X1,X2,FA,FB,FX,RMIN,FX1,FX2,ARR1(5),ARR2(5)
      real funmin
      integer iter,IFLAG
      EXTERNAL  F

       IFLAG=0
       XMIN=0.0
       FUNMIN=0.0
       X=(AX+BX)/2.
       FA=F(AX)
       FB=F(BX)
       FX=F(X)
c       if( taux55 .gt.2.0) then
c       XMIN=999.0
c       FUNMIN=9.99
c       return
c       endif
       DO 1 ITER = 1,10
       X1=(X+AX)/2.
       X2=(X+BX)/2.
       FX1=F(X1)
       FX2=F(X2)
c      if( taux55 .gt.2.0) then
c       XMIN=999.0
c       FUNMIN=9.99
c       return
c       endif
       ARR1(1)=AX
       ARR1(2)=BX
       ARR1(3)=X
       ARR1(4)=X1
       ARR1(5)=X2
       ARR2(1)=FA
       ARR2(2)=FB
       ARR2(3)=FX
       ARR2(4)=FX1
       ARR2(5)=FX2
       CALL shlsrt(5,ARR2,ARR1)
       RMIN=arr1(1)

*/  Modified by JC Guu  01/10/97
*/  Followings are 3 mutually exclusive 'if' blocks
*/  controlled by the 'go to 1' statement at the end
*/  of the 'if' blocks.  At label 1, the 'continue'
*/  statement is the terminal statement of a 'DO'
*/  loop and the 'endif' statement of the last 'if'
*/  block precedes the 'DO' loop terminal statement
*/  immediately. To eliminate the 'go to 1's, the
*/  conditions tested in the 2nd and 3rd 'if' blocks
*/  will include the conditions to prevent the flow
*/  of control from entering the 'if' blocks appeared
*/  later in the code when a 'if' block appeared
*/  earlier has been executed. In this case, the
*/  'go to 1's can be deleted.

       if( rmin .le. x1) then
        IFLAG=IFLAG+1
            ax=ax
            bx=x
           fb=fx
            x=x1
           fx=fx1
c           go to 1
       endif

*/  Modified by JC Guu  01/23/97
*/  Equality comparision 'rmin.eq.x' is changed to
*/  an inequality comparision.

       if(abs(rmin-x).lt.abs(x)*0.000001 .and. rmin.gt.x1) then
        IFLAG=IFLAG+1
         ax=x1
         fa=fx1
         bx=x2
         fb=fx2
         x=x
         fx=fx
c         go to 1
         endif

*/  Modified by JC Guu  01/23/97
*/  Equality comparision 'rmin.ne.x' is changed to
*/  an inequality comparision.

      if(rmin.ge.x2 .and.
     &   abs(rmin-x).gt.abs(x)*0.000001 .and. rmin.gt.x1) then
        IFLAG=IFLAG+1
         ax=x
         fa=fx
         bx=bx
         fb=fb
         x=x2
         fx=fx2
c         go to 1
         endif
1       continue
       IF( IFLAG .GT.1) THEN
       arr1(1)=ax
       arr1(2)=bx
       arr1(3)=x
       arr2(1)=fa
       arr2(2)=fb
       arr2(3)=fx
       CALL shlsrt(3,ARR2,ARR1)
       xmin=arr1(1)
       funmin=arr2(1)
       ELSE
       XMIN=999.0
       FUNMIN=9.99
       ENDIF

       return
       end



C********************************************************************

       SUBROUTINE store_other(REF_FLUX,TRANS_FLUX,FUNMIN,EFFRADIUS,
     * BACKSCATT, ASSYM,NSOLUTION,CCN,SCAT_ANGLE_OCEAN,
     * ARRAY,NUM_ARRAY_ELEMENTS,NUMDATA_550,ISMALL,IBIG,
     *sd_W470,sd_W550,sd_W659,sd_w865,sd_W124,sd_W164,Sd_W213,CLD_FRAC,
     * MASS_CON_OCEAN)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine stores all the output varaibles
C             to be wrriten into HDF format.
C
C!INPUT PARAMETERS:
C     IDATA        Index of box number
C     IBIG         Index for big mode
C     ISMALL       Index for small mode.
C     NUMCASE      Number of large and small cases
C     NWAV         Number of wavelengths
C     FUNMIN       Minimm Error function betwwen derived and
C                  computed radiances.
c     EFFRADIUS    Effective radius weighed for large and small mode.
c     EFFVARIANCE  standard deviation for effective radius.
C     CCNSMALL     CCN for small.
C     ASSYM        Assymetry factor for small and large.
c     NSMALL       Ratio of taux55small to extnor(small mode).
c     NBIG         Ratio of taux55small to extnor(large mode).
C     NSOLUTION    Index for the number of solution.
C     RGSS         Radius for small particle
C     CCN          ccn for the solution.
C     SIGMAS       Standard deviation for small mode.
C     RGSB         Radius for small particle
C     SIGMAB       Standard deviation for small mode.
c     REF_FLUX     Reflected Flux
C     TRANS_FLUX   Transmitted Flux
C NUM_ARRAY_ELEMENTS    Number of array elements.
C
C        Input through 'mod04.inc' from common
C       TAUX55SMALL Optical thickness contribution small particles
C       TAUX55BIG   Optical thickness contribution large particles
c       TAUX55      Optical thickness contribution large particles+small mode
c       AN           Weight factor for large and small mode
C!OUTPUT PARAMETERS:
C    ARRAY      Array containing all output variables for all small and
C               large solutions
C
C
C!REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c fixed prolog;
C
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS:
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE

      INCLUDE 'mod04.inc'

      INTEGER IWAV,NSOLUTION,IJ
      INTEGER NUM_ARRAY_ELEMENTS,NUMDATA_550,ISMALL,IBIG
       REAL    sd_W470,sd_W550,sd_W659,sd_w865,sd_W124,sd_W164,Sd_W213
      REAL REF_FLUX(NWAV),TRANS_FLUX(NWAV)
      REAL CCN,ASSYM(NWAV),BACKSCATT(NWAV),ALM
      REAL VAR
      REAL EFFRADIUS,FUNMIN,ERROR(10)
      REAL ARRAY(NUM_ARRAY_ELEMENTS,NUMCASES*NUMCASEB)
      REAL SCAT_ANGLE_OCEAN,CLD_FRAC,MASS_CON_OCEAN

c  Write ASCII file for scientific data only
            DO IWAV=1,NWAV
              IF( IWAV .EQ.1)ALM=REFW470
              IF( IWAV .EQ.2)ALM=REFW550
              IF( IWAV .EQ.3)ALM=REFW659
              IF( IWAV .EQ.4)ALM=REFW865
              IF( IWAV .EQ.5)ALM=REFW124
              IF( IWAV .EQ.6)ALM=REFW164
              IF( IWAV .EQ.7)ALM=REFW213
              ERROR(IWAV)=(ALM-ALXWAV(IWAV))
            ENDDO
            DO IJ =1 ,NUM_ARRAY_ELEMENTS
C  Store  INDEX FOR SMALL PARTICLES
               IF( IJ .EQ.1) VAR=ISMALL
C  Store  INDEX FOR BIG PARTICLES( NUMCASES is added to get the index given in Docu.
              IF (IJ .EQ.77)VAR=IBIG+NUMCASES

               IF( IJ .EQ.2) VAR=SCAT_ANGLE_OCEAN
c   Store values for optical thicknesses for 7 wavelengths
               IF (IJ .EQ.3)VAR=TAU_COMPUTED(1)
               IF (IJ .EQ.4)VAR=TAU_COMPUTED(2)
               IF( IJ .EQ.5)VAR=TAU_COMPUTED(3)
               IF( IJ .EQ.6)VAR=TAU_COMPUTED(4)
               IF( IJ .EQ.7)VAR=TAU_COMPUTED(5)
                IF( IJ .EQ.8)VAR=TAU_COMPUTED(6)
                IF( IJ .EQ.9)VAR=TAU_COMPUTED(7)
c Store values for optical thicknesses for 7 wavelengths for small mode
                IF( IJ .EQ.10) VAR=TAU_SMALL_COMPUTED(1)
                IF( IJ .EQ.11) VAR=TAU_SMALL_COMPUTED(2)
                IF( IJ .EQ.12) VAR=TAU_SMALL_COMPUTED(3)
                IF( IJ .EQ.13) VAR=TAU_SMALL_COMPUTED(4)
                IF( IJ .EQ.14) VAR=TAU_SMALL_COMPUTED(5)
                IF( IJ .EQ.15) VAR=TAU_SMALL_COMPUTED(6)
                IF( IJ .EQ.16) VAR=TAU_SMALL_COMPUTED(7)
c Store values for optical thicknesses for 7 wavelengths for large mode
                IF( IJ .EQ.17) VAR=TAU_BIG_COMPUTED(1)
                IF( IJ .EQ.18) VAR=TAU_BIG_COMPUTED(2)
                IF( IJ .EQ.19) VAR=TAU_BIG_COMPUTED(3)
                IF( IJ .EQ.20) VAR=TAU_BIG_COMPUTED(4)
                IF( IJ .EQ.21) VAR=TAU_BIG_COMPUTED(5)
                IF( IJ .EQ.22) VAR=TAU_BIG_COMPUTED(6)
                IF( IJ .EQ.23) VAR=TAU_BIG_COMPUTED(7)
c          1/2006 change in units  same as in Land
           IF( IJ .EQ.24)VAR=(MASS_CON_OCEAN /1.0E+6)*(4./3.* PI)
c       CCN at 0.55 um
              IF( IJ .EQ.25)VAR=CCN
c Assymetry factor for all 7 wavelengths
               IF( IJ .EQ.26) VAR= ASSYM(1)
               IF( IJ .EQ.27) VAR= ASSYM(2)
               IF( IJ .EQ.28) VAR= ASSYM(3)
               IF( IJ .EQ.29) VAR= ASSYM(4)
               IF( IJ .EQ.30) VAR= ASSYM(5)
               IF( IJ .EQ.31) VAR= ASSYM(6)
               IF( IJ .EQ.32) VAR= ASSYM(7)
c Backscattering ration for all 7 wavelengths

               IF( IJ .EQ.33) VAR=BACKSCATT(1)
               IF( IJ .EQ.34) VAR=BACKSCATT(2)
               IF( IJ .EQ.35) VAR=BACKSCATT(3)
               IF( IJ .EQ.36) VAR=BACKSCATT(4)
               IF( IJ .EQ.37) VAR=BACKSCATT(5)
               IF( IJ .EQ.38) VAR=BACKSCATT(6)
               IF( IJ .EQ.39) VAR=BACKSCATT(7)
c  save 39,40 for Angstorm coeff1 and coeff2
c Changes 2/28
         IF(TAU_COMPUTED(2) .GT.0.  . AND. TAU_COMPUTED(4) .GT.0.)THEN
              IF( IJ .EQ.40)VAR=(ALOG(TAU_COMPUTED(2)/TAU_COMPUTED(4)))/
     *                  (ALOG(0.867/0.550))
        ELSE
        VAR=-999.0
        ENDIF
        IF(TAU_COMPUTED(4) .GT.0.  . AND. TAU_COMPUTED(7) .GT.0.)THEN
           IF( IJ .EQ.41)VAR=(ALOG(TAU_COMPUTED(4)/TAU_COMPUTED(7)))/
     *                  (ALOG(2.13/0.867))
        ELSE
          VAR=-999.0
       ENDIF
C end of  Changes 2/28
c  Reflected flux for 7 wavelengths
               IF( IJ .EQ.42) VAR=REF_FLUX(1)
               IF( IJ .EQ.43) VAR=REF_FLUX(2)
               IF( IJ .EQ.44) VAR=REF_FLUX(3)
               IF( IJ .EQ.45) VAR=REF_FLUX(4)
               IF( IJ .EQ.46) VAR=REF_FLUX(5)
               IF( IJ .EQ.47) VAR=REF_FLUX(6)
               IF( IJ .EQ.48) VAR=REF_FLUX(7)
c  Transmitted  flux for 7 wavelengths
               IF( IJ .EQ.49) VAR=TRANS_FLUX(1)
               IF( IJ .EQ.50) VAR=TRANS_FLUX(2)
               IF( IJ .EQ.51) VAR=TRANS_FLUX(3)
               IF( IJ .EQ.52) VAR=TRANS_FLUX(4)
               IF( IJ .EQ.53) VAR=TRANS_FLUX(5)
               IF( IJ .EQ.54) VAR=TRANS_FLUX(6)
               IF( IJ .EQ.55) VAR=TRANS_FLUX(7)
c Least sqare error in percentage
              IF (IJ .EQ.56) VAR=FUNMIN*100.
c Small mode weighting factor
               IF (IJ .EQ.57)VAR=AN
c   save 58 for Optical_depth ratio-small
            IF(IJ .EQ.58)THEN
                 IF(TAU_COMPUTED(2) .GT. 0.0) THEN
                   VAR=TAU_SMALL_COMPUTED(2)/TAU_COMPUTED(2)
                 ELSE
                 VAR=0.0
                ENDIF
            ENDIF
c   save 59 for cloud fraction  
 
         IF (IJ .EQ.59) VAR=CLD_FRAC 

c   store number of pixels used
               IF (IJ .EQ.60)VAR=NUMDATA_550
c store all 7 wavelength reflectance
               IF (IJ .EQ.61)VAR=REFW470
               IF (IJ .EQ.62)VAR=REFW550
               IF (IJ .EQ.63)VAR=REFW659
               IF (IJ .EQ.64)VAR=REFW865
               IF (IJ .EQ.65)VAR=REFW124
               IF (IJ .EQ.66)VAR=REFW164
               IF (IJ .EQ.67)VAR=REFW213
c store all 7 wavelength standard deviation for Reflectance
               IF (IJ .EQ.68)VAR= sd_W470
               IF (IJ .EQ.69)VAR= sd_w550
               IF (IJ .EQ.70)VAR= sd_W659
               IF (IJ .EQ.71)VAR= sd_w865
               IF (IJ .EQ.72)VAR= sd_W124
               IF (IJ .EQ.73)VAR= Sd_W164
               IF (IJ .EQ.74)VAR= Sd_W213
C  Store quality conrol
              IF (IJ .EQ.75)VAR=QCONTROL
C  Store effective radius
              IF (IJ .EQ.76)VAR=EFFRADIUS
               ARRAY(IJ,NSOLUTION)=VAR
             ENDDO

          RETURN
          END



C********************************************************************

      SUBROUTINE AVERAGE_output(ARRAY,NUM_ARRAY_ELEMENTS,AVE_ARRAY,
     *              NEW_ARRAY)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine sorts according to minimum
C             error and averages all variables of output
C
C!INPUT PARAMETERS:
C     ARRAY              All output variables to be printed
C     NUM_ARRAY_ELEMENTS number of variables in output array
C     AVE_ARRAY          Array with all output varaibles for best
C                        and average solutions
c     NEW_ARRAY          Array used to hold the data
C!OUTPUT PARAMETERS:
C    AVE_ARRAY          Array with all output varaibles for best
C
C
C!REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c fixed prolog;
C
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS:
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------

        IMPLICIT NONE
        SAVE

        INCLUDE 'mod04.inc'

            integer NUM_ARRAY_ELEMENTS
            REAL ARRAY(NUM_ARRAY_ELEMENTS,NUMCASES*NUMCASEB)
            REAL ARR1(NUMCASES*NUMCASEB)
            REAL AVE_ARRAY(NUM_ARRAY_ELEMENTS,NUM_solutions),AVE,SDEV
            REAL NEW_ARRAY(NUM_ARRAY_ELEMENTS,NUMCASEB*NUMCASES)
            REAL DATA(NUMCASEB*NUMCASES)
           INTEGER IJ,INDX(NUMCASES*NUMCASEB)
           INTEGER ARRAY_ELEMENTS,NUMPOINTS,INUM,n_number
           REAL  Tot_epsilon,Tot_data,epsilon(3)
c    order according to smallest error in minimu function
c
           INUM=0

                 DO IJ=1,NUMCASES*NUMCASEB
                 ARR1(IJ)=ARRAY(56,IJ)
                 ENDDO
c Call to get ascending order index

            CALL INDEXX(NUMCASES*NUMCASEB,ARR1,INDX)

C  Save the best possible solution with minimum error in array
c  called  AVE_ARRAY
                DO ARRAY_ELEMENTS= 1,NUM_ARRAY_ELEMENTS
       AVE_ARRAY(ARRAY_ELEMENTS,1)=ARRAY(ARRAY_ELEMENTS,INDX(1))
                ENDDO

c If the minimum error is less tahn 3% average all the observations.
      DO IJ = 1,NUMCASEB*NUMCASES
         IF(ARRAY(56,INDX(IJ)) .LE.3.0) THEN
            INUM=INUM+1
            DO ARRAY_ELEMENTS= 1,NUM_ARRAY_ELEMENTS
        NEW_ARRAY(ARRAY_ELEMENTS,INUM)=ARRAY(ARRAY_ELEMENTS,INDX(IJ))
           ENDDO
         ENDIF
        ENDDO
          DO ARRAY_ELEMENTS= 1,NUM_ARRAY_ELEMENTS
              IF( INUM .GT.1) THEN
                    QCONTROL=0
                  DO NUMPOINTS=1,INUM
                    DATA(NUMPOINTS)=NEW_ARRAY(ARRAY_ELEMENTS,NUMPOINTS)
                   ENDDO
c save average solution in array called  AVE_ARRAY
                     CALL  ave_std(DATA,INUM,AVE,SDEV)
                    AVE_ARRAY(ARRAY_ELEMENTS,2)=AVE
c Solution number is not averaged,solution number is taken for the best solution
             IF(ARRAY_ELEMENTS.EQ.1)
     *  AVE_ARRAY(ARRAY_ELEMENTS,2)=(ARRAY(ARRAY_ELEMENTS,INDX(1)))
             IF(ARRAY_ELEMENTS.EQ.77)
     *  AVE_ARRAY(ARRAY_ELEMENTS,2)=(ARRAY(ARRAY_ELEMENTS,INDX(1)))

              ELSE
c   It should atleast have 3 observations
          IF(NUMCASEB*NUMCASES .ge.3 ) then
c If there is only one observation average atleast 3 observations

                 QCONTROL=8
                 Tot_epsilon=0.0
                  Tot_data=0.0
               n_number=0
              DO NUMPOINTS=1,3
               if(ARRAY(56,INDX(NUMPOINTS)) .gt.0.0) then
                   n_number=n_number+1
             epsilon(n_number)=(1./((ARRAY(56,INDX(NUMPOINTS))**2)))
             Tot_epsilon=tot_epsilon+epsilon(n_number)
             Tot_data=Tot_data+(ARRAY(ARRAY_ELEMENTS,INDX(NUMPOINTS))*
     *                     epsilon(n_number))
              endif
              enddo

c save average solution in array called  AVE_ARRAY
                    if(Tot_epsilon .gt.0) then
                    AVE= Tot_data/Tot_epsilon
                    else
                    AVE=0.0
                    endif
            AVE_ARRAY(ARRAY_ELEMENTS,2)=AVE
c Solution number is not averaged,solution number is taken for the best solution
             IF(ARRAY_ELEMENTS.EQ.1)
     *  AVE_ARRAY(ARRAY_ELEMENTS,2)=(ARRAY(ARRAY_ELEMENTS,INDX(1)))
             IF(ARRAY_ELEMENTS.EQ.77)
     *  AVE_ARRAY(ARRAY_ELEMENTS,2)=(ARRAY(ARRAY_ELEMENTS,INDX(1)))
c else  and endif   for atleast 3 observation else put the best solution
         ELSE
          AVE_ARRAY(ARRAY_ELEMENTS,2)= AVE_ARRAY(ARRAY_ELEMENTS,1)
        ENDIF
c endif for inum
        ENDIF
c ENDDO for Array_elements
          ENDDO
         RETURN
         END



C********************************************************************

        SUBROUTINE set_output(IDATA,AVE_ARRAY,NUM_ARRAY_ELEMENTS,
     *SDSTAU_best,SDSTAU_average,SDSTAUB_best,
     &SDSTAUB_average,SDSTAUS_best,SDSTAUS_average,
     &SDS_small_weighting,SDS_Least_error,
     &SDSASSY_best,SDSASSY_average,SDSBACK_best,SDSBACK_average,
     &SDS_effrad,SDS_RefF_best,SDS_RefF_average,SDS_TranF_best,
     &SDS_TranF_average,SDS_sol_INDX_small,SDS_sol_INDX_large,
     &SDS_ccn,SDS_mass_conc,SDS_angs_coeff1,SDS_angs_coeff2,
     &SDS_AOT_model)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine sorts according to minimum
C             error and writes  all the output varaibles
C             in ASCII format for science data only.
C
C!INPUT PARAMETERS:
C AVE_ARRAY           Two element array  for best and average solution
C NUM_ARRAY_ELEMENTS  Number of arrays for output
C IDATA               Index of box number
C!OUTPUT PARAMETERS:
C SDSTAU_best     Optical thickness for best solution
C SDSTAUS_best    Optical thickness contribution small particles for best solution
C SDSTAUB_best    Optical thickness contribution large particles for best solution
C SDSTAU_average  Optical thickness for best solution
C SDSTAUS_average Optical thickness contribution small particles for best solution
C SDSTAUB_average Optical thickness contribution large particles for best solution
C SDS_Least_error    Minimm Error function betwwen derived and computed radiances
C SDS_small_weighting Weight factor for large and small mode
C SDS_sol_INDX_small       Index for solution number small particles
C SDS_sol_INDX_large       Index for solution number large particles
C SDSASSY_best       Assymetry Factor for best solution
C SDSASSY_average    Assymetry Factor for Average solution
C SDSBACK_best       Backscattering Ratio for best solution
C SDSBACK_average    Backscattering Ratio for Average solution
C SDS_effrad         Effective Radiance
C SDS_AOT_model Ration of optical Thickess small
C SDS_RefF_best      Reflected Flux Best solution
C SDS_RefF_average   Reflected Flux Average solution
C SDS_TranF_best     Transmitted Flux Best solution
C SDS_TranF_average  Transmitted Flux Average solution
C SDS_ccn            Cloud condensation Neculii
C SDS_mass_conc      Mass concentration
C SDS_angs_coeff1    Angstrom coeff1
C SDS_angs_coeff2    Angstrom coeff2
C
C
C!REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c fixed prolog;
C
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS:
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------

        IMPLICIT NONE
        SAVE

        INCLUDE 'mod04.inc'

            integer NUM_ARRAY_ELEMENTS,IDATA,ICASE
            REAL AVE_ARRAY(NUM_ARRAY_ELEMENTS,NUM_solutions)
            REAL  SDS_ccn(NUMCELLS,NUM_solutions),
     &            SDS_mass_conc(NUMCELLS,NUM_solutions)
           INTEGER*2 SDSTAU_best(NUMCELLS,NWAV),
     &            SDSTAU_average(NUMCELLS,NWAV),
     &            SDSTAUB_best(NUMCELLS,NWAV),
     &            SDSTAUB_average(NUMCELLS,NWAV),
     &            SDSTAUS_best(NUMCELLS,NWAV),
     &            SDSTAUS_average(NUMCELLS,NWAV),
     &            SDSASSY_best(NUMCELLS,NWAV),
     &            SDSASSY_average(NUMCELLS,NWAV),
     &            SDSBACK_best(NUMCELLS,NWAV),
     &            SDSBACK_average(NUMCELLS,NWAV),
     &            SDS_RefF_best(NUMCELLS,NWAV),
     &            SDS_RefF_average(NUMCELLS,NWAV),
     &            SDS_TranF_best(NUMCELLS,NWAV),
     &            SDS_TranF_average(NUMCELLS,NWAV)
       INTEGER * 2
     &            SDS_small_weighting(NUMCELLS,NUM_solutions),
     &            SDS_Least_error(NUMCELLS,NUM_solutions),
     &            SDS_effrad(NUMCELLS,NUM_solutions),
     &            SDS_sol_INDX_small(NUMCELLS,NUM_solutions),
     &            SDS_sol_INDX_large(NUMCELLS,NUM_solutions),
     &            SDS_angs_coeff1(NUMCELLS,NUM_solutions),
     &            SDS_angs_coeff2(NUMCELLS,NUM_solutions),
     &            SDS_AOT_model(NUMCELLS,num_model_index)

c write ascii output for best and average solutions

c            IJ=1
c        WRITE(36,100)idata,(AVE_ARRAY(IK,IJ),IK=1,NUM_ARRAY_ELEMENTS)
c             IJ = 2
c         WRITE(37,100)idata,(AVE_ARRAY(IK,IJ),IK=1,NUM_ARRAY_ELEMENTS)
c
C  Store into array's for HDF file format for best case(Taua )
c
      SDSTAU_best(IDATA,1)  = nint(AVE_ARRAY(3,1)*SCALE3+OFFSET3)
      SDSTAU_best(IDATA,2)  = nint(AVE_ARRAY(4,1)*SCALE3+OFFSET3)
      SDSTAU_best(IDATA,3)  = nint(AVE_ARRAY(5,1)*SCALE3+OFFSET3)
      SDSTAU_best(IDATA,4)  = nint(AVE_ARRAY(6,1)*SCALE3+OFFSET3)
      SDSTAU_best(IDATA,5)  = nint(AVE_ARRAY(7,1)*SCALE3+OFFSET3)
      SDSTAU_best(IDATA,6)  = nint(AVE_ARRAY(8,1)*SCALE3+OFFSET3)
      SDSTAU_best(IDATA,7)  = nint(AVE_ARRAY(9,1)*SCALE3+OFFSET3)
C
CStore into array's for HDF file format for average case(Taua )
c
      SDSTAU_average(IDATA,1)  = nint(AVE_ARRAY(3,2)*SCALE3+OFFSET3)
      SDSTAU_average(IDATA,2)  = nint(AVE_ARRAY(4,2)*SCALE3+OFFSET3)
      SDSTAU_average(IDATA,3)  = nint(AVE_ARRAY(5,2)*SCALE3+OFFSET3)
      SDSTAU_average(IDATA,4)  = nint(AVE_ARRAY(6,2)*SCALE3+OFFSET3)
      SDSTAU_average(IDATA,5)  = nint(AVE_ARRAY(7,2)*SCALE3+OFFSET3)
      SDSTAU_average(IDATA,6)  = nint(AVE_ARRAY(8,2)*SCALE3+OFFSET3)
      SDSTAU_average(IDATA,7)  = nint(AVE_ARRAY(9,2)*SCALE3+OFFSET3)
C
CStore into array's for HDF file format for best case(Taua_small)
c
      SDSTAUS_best(IDATA,1) = nint(AVE_ARRAY(10,1)*SCALE3+OFFSET3)
      SDSTAUS_best(IDATA,2) = nint(AVE_ARRAY(11,1)*SCALE3+OFFSET3)
      SDSTAUS_best(IDATA,3) = nint(AVE_ARRAY(12,1)*SCALE3+OFFSET3)
      SDSTAUS_best(IDATA,4) = nint(AVE_ARRAY(13,1)*SCALE3+OFFSET3)
      SDSTAUS_best(IDATA,5) = nint(AVE_ARRAY(14,1)*SCALE3+OFFSET3)
      SDSTAUS_best(IDATA,6) = nint(AVE_ARRAY(15,1)*SCALE3+OFFSET3)
      SDSTAUS_best(IDATA,7) = nint(AVE_ARRAY(16,1)*SCALE3+OFFSET3)
c
CStore into array's for HDF file format for average case(Taua_small )
c
      SDSTAUS_average(IDATA,1) = nint(AVE_ARRAY(10,2)*SCALE3+OFFSET3)
      SDSTAUS_average(IDATA,2) = nint(AVE_ARRAY(11,2)*SCALE3+OFFSET3)
      SDSTAUS_average(IDATA,3) = nint(AVE_ARRAY(12,2)*SCALE3+OFFSET3)
      SDSTAUS_average(IDATA,4) = nint(AVE_ARRAY(13,2)*SCALE3+OFFSET3)
      SDSTAUS_average(IDATA,5) = nint(AVE_ARRAY(14,2)*SCALE3+OFFSET3)
      SDSTAUS_average(IDATA,6) = nint(AVE_ARRAY(15,2)*SCALE3+OFFSET3)
      SDSTAUS_average(IDATA,7) = nint(AVE_ARRAY(16,2)*SCALE3+OFFSET3)
C
CStore into array's for HDF file format for best case(Taua_large )
c
      SDSTAUB_best(IDATA,1) = nint(AVE_ARRAY(17,1)*SCALE3+OFFSET3)
      SDSTAUB_best(IDATA,2) = nint(AVE_ARRAY(18,1)*SCALE3+OFFSET3)
      SDSTAUB_best(IDATA,3) = nint(AVE_ARRAY(19,1)*SCALE3+OFFSET3)
      SDSTAUB_best(IDATA,4) = nint(AVE_ARRAY(20,1)*SCALE3+OFFSET3)
      SDSTAUB_best(IDATA,5) = nint(AVE_ARRAY(21,1)*SCALE3+OFFSET3)
      SDSTAUB_best(IDATA,6) = nint(AVE_ARRAY(22,1)*SCALE3+OFFSET3)
      SDSTAUB_best(IDATA,7) = nint(AVE_ARRAY(23,1)*SCALE3+OFFSET3)
C
CStore into array's for HDF file format for average case(Taua_large)
c
      SDSTAUB_average(IDATA,1) = nint(AVE_ARRAY(17,2)*SCALE3+OFFSET3)
      SDSTAUB_average(IDATA,2) = nint(AVE_ARRAY(18,2)*SCALE3+OFFSET3)
      SDSTAUB_average(IDATA,3) = nint(AVE_ARRAY(19,2)*SCALE3+OFFSET3)
      SDSTAUB_average(IDATA,4) = nint(AVE_ARRAY(20,2)*SCALE3+OFFSET3)
      SDSTAUB_average(IDATA,5) = nint(AVE_ARRAY(21,2)*SCALE3+OFFSET3)
      SDSTAUB_average(IDATA,6) = nint(AVE_ARRAY(22,2)*SCALE3+OFFSET3)
      SDSTAUB_average(IDATA,7) = nint(AVE_ARRAY(23,2)*SCALE3+OFFSET3)
C
CStore into array's for HDF file format for best case(Assymetry)
c
      SDSASSY_best(IDATA,1) = nint(AVE_ARRAY(26,1)*SCALE3+OFFSET3)
      SDSASSY_best(IDATA,2) = nint(AVE_ARRAY(27,1)*SCALE3+OFFSET3)
      SDSASSY_best(IDATA,3) = nint(AVE_ARRAY(28,1)*SCALE3+OFFSET3)
      SDSASSY_best(IDATA,4) = nint(AVE_ARRAY(29,1)*SCALE3+OFFSET3)
      SDSASSY_best(IDATA,5) = nint(AVE_ARRAY(30,1)*SCALE3+OFFSET3)
      SDSASSY_best(IDATA,6) = nint(AVE_ARRAY(31,1)*SCALE3+OFFSET3)
      SDSASSY_best(IDATA,7) = nint(AVE_ARRAY(32,1)*SCALE3+OFFSET3)
C
CStore into array's for HDF file format for average case(Assymetry)
c
      SDSASSY_average(IDATA,1) = nint(AVE_ARRAY(26,2)*SCALE3+OFFSET3)
      SDSASSY_average(IDATA,2) = nint(AVE_ARRAY(27,2)*SCALE3+OFFSET3)
      SDSASSY_average(IDATA,3) = nint(AVE_ARRAY(28,2)*SCALE3+OFFSET3)
      SDSASSY_average(IDATA,4) = nint(AVE_ARRAY(29,2)*SCALE3+OFFSET3)
      SDSASSY_average(IDATA,5) = nint(AVE_ARRAY(30,2)*SCALE3+OFFSET3)
      SDSASSY_average(IDATA,6) = nint(AVE_ARRAY(31,2)*SCALE3+OFFSET3)
      SDSASSY_average(IDATA,7) = nint(AVE_ARRAY(32,2)*SCALE3+OFFSET3)

C
C
CStore into array's for HDF file format for best case(Backscattering)
c
      SDSBACK_best(IDATA,1) = nint(AVE_ARRAY(33,1)*SCALE3+OFFSET3)
      SDSBACK_best(IDATA,2) = nint(AVE_ARRAY(34,1)*SCALE3+OFFSET3)
      SDSBACK_best(IDATA,3) = nint(AVE_ARRAY(35,1)*SCALE3+OFFSET3)
      SDSBACK_best(IDATA,4) = nint(AVE_ARRAY(36,1)*SCALE3+OFFSET3)
      SDSBACK_best(IDATA,5) = nint(AVE_ARRAY(37,1)*SCALE3+OFFSET3)
       SDSBACK_best(IDATA,6) = nint(AVE_ARRAY(38,1)*SCALE3+OFFSET3)
       SDSBACK_best(IDATA,7) = nint(AVE_ARRAY(39,1)*SCALE3+OFFSET3)
C
CStore into array's for HDF file format for average case(Backscattering)
c
      SDSBACK_average(IDATA,1) = nint(AVE_ARRAY(33,2)*SCALE3+OFFSET3)
      SDSBACK_average(IDATA,2) = nint(AVE_ARRAY(34,2)*SCALE3+OFFSET3)
      SDSBACK_average(IDATA,3) = nint(AVE_ARRAY(35,2)*SCALE3+OFFSET3)
      SDSBACK_average(IDATA,4) = nint(AVE_ARRAY(36,2)*SCALE3+OFFSET3)
      SDSBACK_average(IDATA,5) = nint(AVE_ARRAY(37,2)*SCALE3+OFFSET3)
      SDSBACK_average(IDATA,6) = nint(AVE_ARRAY(38,2)*SCALE3+OFFSET3)
      SDSBACK_average(IDATA,7) = nint(AVE_ARRAY(39,2)*SCALE3+OFFSET3)
C
CStore into array's for HDF file format for best case(Reflected_flux)
c
      SDS_RefF_best(IDATA,1) = nint(AVE_ARRAY(42,1)*SCALE3+OFFSET3)
      SDS_RefF_best(IDATA,2) = nint(AVE_ARRAY(43,1)*SCALE3+OFFSET3)
      SDS_RefF_best(IDATA,3) = nint(AVE_ARRAY(44,1)*SCALE3+OFFSET3)
      SDS_RefF_best(IDATA,4) = nint(AVE_ARRAY(45,1)*SCALE3+OFFSET3)
      SDS_RefF_best(IDATA,5) = nint(AVE_ARRAY(46,1)*SCALE3+OFFSET3)
      SDS_RefF_best(IDATA,6) = nint(AVE_ARRAY(47,1)*SCALE3+OFFSET3)
      SDS_RefF_best(IDATA,7) = nint(AVE_ARRAY(48,1)*SCALE3+OFFSET3)
C
CStore into array's for HDF file format for average case(Reflected_flux)
c
      SDS_RefF_average(IDATA,1) = nint(AVE_ARRAY(42,2)*SCALE3+OFFSET3)
      SDS_RefF_average(IDATA,2) = nint(AVE_ARRAY(43,2)*SCALE3+OFFSET3)
      SDS_RefF_average(IDATA,3) = nint(AVE_ARRAY(44,2)*SCALE3+OFFSET3)
      SDS_RefF_average(IDATA,4) = nint(AVE_ARRAY(45,2)*SCALE3+OFFSET3)
      SDS_RefF_average(IDATA,5) = nint(AVE_ARRAY(46,2)*SCALE3+OFFSET3)
      SDS_RefF_average(IDATA,6) = nint(AVE_ARRAY(47,2)*SCALE3+OFFSET3)
      SDS_RefF_average(IDATA,7) = nint(AVE_ARRAY(48,2)*SCALE3+OFFSET3)
C
CStore into array's for HDF file format for best case(Transmitted_flux)
c
      SDS_TranF_best(IDATA,1) = nint(AVE_ARRAY(49,1)*SCALE3+OFFSET3)
      SDS_TranF_best(IDATA,2) = nint(AVE_ARRAY(50,1)*SCALE3+OFFSET3)
      SDS_TranF_best(IDATA,3) = nint(AVE_ARRAY(51,1)*SCALE3+OFFSET3)
      SDS_TranF_best(IDATA,4) = nint(AVE_ARRAY(52,1)*SCALE3+OFFSET3)
      SDS_TranF_best(IDATA,5) = nint(AVE_ARRAY(53,1)*SCALE3+OFFSET3)
      SDS_TranF_best(IDATA,6) = nint(AVE_ARRAY(54,1)*SCALE3+OFFSET3)
      SDS_TranF_best(IDATA,7) = nint(AVE_ARRAY(55,1)*SCALE3+OFFSET3)
C
CStore into array's for HDF file format for average case(Transmitted_flux)
c
      SDS_TranF_average(IDATA,1) = nint(AVE_ARRAY(49,2)*SCALE3+OFFSET3)
      SDS_TranF_average(IDATA,2) = nint(AVE_ARRAY(50,2)*SCALE3+OFFSET3)
      SDS_TranF_average(IDATA,3) = nint(AVE_ARRAY(51,2)*SCALE3+OFFSET3)
      SDS_TranF_average(IDATA,4) = nint(AVE_ARRAY(52,2)*SCALE3+OFFSET3)
      SDS_TranF_average(IDATA,5) = nint(AVE_ARRAY(53,2)*SCALE3+OFFSET3)
      SDS_TranF_average(IDATA,6) = nint(AVE_ARRAY(54,2)*SCALE3+OFFSET3)
      SDS_TranF_average(IDATA,7) = nint(AVE_ARRAY(55,2)*SCALE3+OFFSET3)
C
      DO ICASE = 1, NUM_solutions

      SDS_mass_conc(IDATA,ICASE)=AVE_ARRAY(24,ICASE)*SCALE1+OFFSET1
      SDS_effrad(IDATA,ICASE)=nint(AVE_ARRAY(76,ICASE)*SCALE3+OFFSET3)
      SDS_ccn(IDATA,ICASE)=AVE_ARRAY(25,ICASE)*SCALE1+OFFSET1
      SDS_angs_coeff1(IDATA,ICASE)=nint(AVE_ARRAY(40,ICASE)*
     *                               SCALE3+OFFSET3)
      SDS_angs_coeff2(IDATA,ICASE)=nint(AVE_ARRAY(41,ICASE)*
     *                               SCALE3+OFFSET3)
      SDS_Least_error(IDATA,ICASE) = nint((AVE_ARRAY(56,ICASE)/100)*
     *                          SCALE3+OFFSET3)

      SDS_small_weighting(IDATA,ICASE) = nint(AVE_ARRAY(57,ICASE)*
     *                         SCALE3+OFFSET3)
      
      SDS_sol_INDX_small(IDATA,ICASE)=nint(AVE_ARRAY(1,ICASE))
      SDS_sol_INDX_large(IDATA,ICASE)=nint(AVE_ARRAY(77,ICASE))

       ENDDO
        DO ICASE = 1, num_model_index
c  fill every index of models with zero
         SDS_AOT_model(IDATA,ICASE)=0.0
c  fill only small and large index with optical depth of average values for small and large mode at 0.55 um
        SDS_AOT_model(IDATA,SDS_sol_INDX_small(IDATA,2))=
     *      nint(AVE_ARRAY(11,2)*SCALE3+OFFSET3)
        SDS_AOT_model(IDATA,SDS_sol_INDX_large(IDATA,2))=
     *      nint(AVE_ARRAY(18,2)*SCALE3+OFFSET3)
       enddo
c         SDS_correc_small_weighting(IDATA)=SDS_small_weighting(IDATA,2)
100      FORMAT(i4,10(10f10.4,/))

         RETURN
         END



C********************************************************************

       subroutine shlsrt(n,arr1,arr2)

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
C !REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c Revision 1.2  1997/01/27  19:56:07  jguu
c Size for a filename string buffer is changed to  PGS_PC_FILE_PATH_MAX.
c The number of scans of the processed granule is passed in
c WIS_ANC and READ_AVERAGE_MODIS_DEPEN.
c All "GO TO"s are eliminated.
c Strict equality comparisions are changed to inequality comparisions.
c
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



C********************************************************************

      SUBROUTINE INDEXX( N, ARR, INDX )

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine findes the  index of array ARR
C
C!INPUT PARAMETERS:
C         N     Number of points in array ARR
c        ARR     Data Array
C!OUTPUT PARAMETERS:
C       INDX    INDX returns index of array ARR arranged in ascending order
C
C!REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c fixed prolog;
C
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS:
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------

      IMPLICIT NONE

c       Numerical Recipes ver. 2.06
c     .. Parameters ..
      INTEGER   M, NSTACK
      PARAMETER ( M = 7, NSTACK = 50 )
c     ..
c     .. Scalar Arguments ..
      INTEGER   N
c     ..
c     .. Array Arguments ..
      INTEGER   INDX( N )
      REAL      ARR( N )
c     .. Local Scalars ..
      INTEGER   I, INDXT, IR, ITEMP, J, JSTACK, K, L
      REAL      A
c     ..
c     .. Local Arrays ..
      INTEGER   ISTACK( NSTACK )
      DO 10 J = 1, N
         INDX( J ) = J
   10 CONTINUE
      JSTACK = 0
      L      = 1
      IR     = N

   20 CONTINUE
      IF( IR - L .LT. M ) THEN
         DO 50 J = L + 1, IR
            INDXT  = INDX( J )
            A      = ARR( INDXT )
            DO 30  I = J - 1, L, - 1
               IF( ARR( INDX(I) ) .LE. A ) GO TO 40
               INDX( I + 1 ) = INDX( I )
   30       CONTINUE
            I = L - 1
   40       CONTINUE
            INDX( I + 1 ) = INDXT
   50    CONTINUE
         IF( JSTACK.EQ.0 ) RETURN
         IR     = ISTACK( JSTACK )
         L      = ISTACK( JSTACK - 1 )
         JSTACK = JSTACK - 2
      ELSE
         K      = ( L + IR ) / 2
         ITEMP  = INDX( K )
         INDX( K ) = INDX( L + 1 )
         INDX( L + 1 ) = ITEMP
         IF( ARR( INDX(L) ) .GT. ARR( INDX(IR) ) ) THEN
            ITEMP  = INDX( L )
            INDX( L ) = INDX( IR )
            INDX( IR ) = ITEMP
         END IF
         IF( ARR( INDX(L + 1) ) .GT. ARR( INDX(IR) ) ) THEN
            ITEMP  = INDX( L + 1 )
            INDX( L + 1 ) = INDX( IR )
            INDX( IR ) = ITEMP
         END IF
         IF( ARR( INDX(L) ) .GT. ARR( INDX(L + 1) ) ) THEN
            ITEMP  = INDX( L )
            INDX( L ) = INDX( L + 1 )
            INDX( L + 1 ) = ITEMP
         END IF
         I      = L + 1
         J      = IR
         INDXT  = INDX( L + 1 )
         A      = ARR( INDXT )
   60    CONTINUE
         I = I + 1
         IF( ARR( INDX(I) ) .LT. A ) GO TO 60
   70    CONTINUE
         J = J - 1
         IF( ARR( INDX(J) ) .GT. A ) GO TO 70
         IF( J.LT.I ) GO TO 80
         ITEMP  = INDX( I )
         INDX( I ) = INDX( J )
         INDX( J ) = ITEMP
         GO TO 60
   80    CONTINUE
         INDX( L + 1 ) = INDX( J )
         INDX( J ) = INDXT
         JSTACK = JSTACK + 2
         IF( JSTACK.GT.NSTACK ) PAUSE 'NSTACK too small in INDEXX'
         IF( IR - I + 1 .GE. J - L ) THEN
            ISTACK( JSTACK ) = IR
            ISTACK( JSTACK - 1 ) = I
            IR  = J - 1
         ELSE
            ISTACK( JSTACK ) = J - 1
            ISTACK( JSTACK - 1 ) = L
            L = I
         END IF
      END IF

      GO TO 20
      END



C*****************************************************************

      SUBROUTINE ave_std(DATA,N,AVE,SDEV)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:  This subroutine is borrowed from "numerical Recipes"
C               page 458.  This subroutine returns the values of mean
C               average and standard deviation
C
C!INPUT PARAMETERS:
C     INTEGER   N           Number of elements in input array, DATA
C     REAL      DATA(N)     Array of N elements
C
C!OUTPUT PARAMETERS:
C     REAL      AVE         Arithmetic average of the N values of the
C                           array DATA
C     REAL      SDEV        Standard Deviation of the N values of the
C                           array DATA.
C
C!REVISION HISTORY: Updated code to comply with most MODIS software
C                   standards.
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

      INTEGER J,N
      REAL DATA(N),S,P,AVE,VAR,SDEV

C This line was commented out by SDST vlin
C      IF(N.LE.1)PAUSE 'N must be at least 2'
      S=0.
      DO 11 J=1,N
        S=S+DATA(J)
11    CONTINUE
      AVE=S/N
      VAR=0.0
      DO 12 J=1,N
        S=DATA(J)-AVE
        P=S*S
        VAR=VAR+P
12    CONTINUE
        IF(VAR.GT.0)THEN
           VAR=VAR/(N-1)
           SDEV=SQRT(VAR)
        ELSE
          SDEV=0.0
      ENDIF

       RETURN
       END



C*****************************************************************

      SUBROUTINE  AVERAGE_to_500meter(W659_SYN,W865_SYN,W470_SYN,
     *  W550_SYN,W124_SYN,W164_SYN,W213_SYN,W443o_SYN,W551o_SYN,
     *  W667o_SYN,W869o_SYN,ref_interm,NCLDMSK_syn,
     * NUMDATA_659,NUMDATA_865,NUMDATA_470,NUMDATA_550,
     * NUMDATA_124,NUMDATA_164,NUMDATA_213,NUMDATA_443o,NUMDATA_551o,
     * NUMDATA_667o, NUMDATA_869o,START_1KM,
     * END_1KM,VFlag_w659,VFlag_w865,VFlag_w470,VFlag_w550,VFlag_w124,
     *VFlag_w164,VFlag_w213,VFlag_w443o,VFlag_w551o,VFlag_w667o,
     *VFlag_w869o,cloudy,High_Cloud_Flag_500,W138_SYN,Ref_ray,Wave,
     * CLD_FRAC)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine averages the reflectances for each
C              10*10 pixel  square and finds standdard deviation for
C               averaged Reflectances
C
C!INPUT PARAMETERS:
C    IDATA        Index of bin number in a swath
C
C   START_1KM     Begining index of the swath in 10*10 km BOX
C   END_1KM      Ending   Index of the swath in 10*10 km BOX
C    Reflectances of wavelengths used modis,ch1-ch7  identified by W*_SYN
C    Extra wavelengths from ocean color ch9,ch12,ch13 and ch16 will be
C    used for substituting if Ch1 - ch7 do not have valid data.
C    These channels are identified by W*o_SYN.
C       W659_SYN
C       W865_SYN
C       W470_SYN
C       W550_SYN
C       W124_SYN
C       W164_SYN
C       W213_SYN
C       W443o_SYN
C       W551o_SYN
C       W667o_SYN
C       W869o_SYN
C!OUTPUT PARAMETERS:
C     Reflectances averaged for 500 meter resolution
C    wavelengths used modis,ch1-ch7  identified by ref_interm_w*
C    Extra wavelengths from ocean color ch9,ch12,ch13 and ch16 will be
C    used for substituting if Ch1 - ch7 do not have valid data.
C    These channels are identified by ref_interm_w*o
C       ref_interm_w659
C       ref_interm_w865
C       ref_interm_w470
C       ref_interm_w550
C       ref_interm_w124
C       ref_interm_w164
C       ref_interm_w213
C       ref_interm_w443o
C       ref_interm_w551o
C       ref_interm_w667o
C       ref_interm_w869o
c Number of data points in each 500 meter box for all wavelengths used
C      NUMDATA_659
C      NUMDATA_865
C      NUMDATA_470
C      NUMDATA_550
C      NUMDATA_124
C      NUMDATA_164
C      NUMDATA_213
C      NUMDATA_443o
C      NUMDATA_551o
C      NUMDATA_667o
C      NUMDATA_869o
C     Total number of valid data  500 meter resolution
C     wavelengths used modis,ch1-ch7  identified by VFlag_w*
C    Extra wavelengths from ocean color ch9,ch12,ch13 and ch16 will be
C    used for substituting if Ch1 - ch7 do not have valid data.
C    These channels are identified by VFlag_w*o
C     VFlag_w659
C     VFlag_w865
C     VFlag_w470
C     VFlag_w550
C     VFlag_w124
C     VFlag_w164
C     VFlag_w213
C     VFlag_w443o
C     VFlag_w551o
C     VFlag_w667o
C     VFlag_w869o
C
C!REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c fixed prolog;
C
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS:
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------
       IMPLICIT NONE
       SAVE

       INCLUDE 'mod04.inc'

       REAL W659_SYN(4*ISWATH,4*ILINE),W865_SYN(4*ISWATH,4*ILINE),
     *     W470_SYN(2*ISWATH,2*ILINE),W550_SYN(2*ISWATH,2*ILINE),
     *     W124_SYN(2*ISWATH,2*ILINE),W164_SYN(2*ISWATH,2*ILINE),
     *     W213_SYN(2*ISWATH,2*ILINE),W138_SYN(ISWATH,ILINE),
     *     W443o_SYN(ISWATH,ILINE),W551o_SYN(ISWATH,ILINE),
     *     W667o_SYN(ISWATH,ILINE),W869o_SYN(ISWATH,ILINE)
      REAL arefw659,arefw865,Ref_ray(NWAV),wave(Nwav)
      REAL ref_interm(NWAV,2*IGRIDX*2*IGRIDY),CLD_FRAC
      INTEGER NCLDMSK_SYN(2*ISWATH,2*ILINE+1)
      INTEGER START_1KM,END_1KM,JJ,II
      INTEGER   numaver1,numaver2
      INTEGER vFlag_w659,vFlag_w865,vFlag_w470,
     * vFlag_w550,vFlag_w124,vFlag_w164,vFlag_w213,vFlag_w443o,
     * vFlag_w551o,vFlag_w667o, vFlag_w869o
      INTEGER NUMDATA_659,NUMDATA_865,NUMDATA_470,
     * NUMDATA_550,NUMDATA_124,NUMDATA_164,NUMDATA_213,NUMDATA_443o,
     * NUMDATA_551o,NUMDATA_667o, NUMDATA_869o
      INTEGER JMASK,IMASK,IBLUE,JBLUE,IXX,IYY,IX,IY,IIJ,IIK,cloudy
      INTEGER  High_Cloud_Flag_500(2*ISWATH,2*ILINE)
      INTEGER  sed_mask(2*ISWATH,2*ILINE),savecldmask(2*ISWATH,2*ILINE)
      Integer num_cloudy_pixels, Total_pixels
       CLD_FRAC=-99
       cloudy=0
      Qcontrol_cirrus=0
       aREFW659=0.0
       aREFW865=0.0
        NUMDATA_470=0
       NUMDATA_550=0
        NUMDATA_659=0
       NUMDATA_865=0
         NUMDATA_124=0
       NUMDATA_164=0
       NUMDATA_213=0
         numaver1=0
         numaver2=0
        vFlag_w470=0
        vFlag_w550=0
        vFlag_w659=0
        vFlag_w865=0
        vFlag_w124=0
        vFlag_w164=0
        vFlag_w213=0 

        DO   IYY = 1,IGRIDY*2
       DO  IXX=START_1KM*2,END_1KM*2
        savecldmask(IXX,IYY)=0
        enddo
        enddo
         num_cloudy_pixels=0
         Total_pixels=0
        DO   IYY = 1,IGRIDY
         IMASK=4*IYY-3
         IBLUE=2*IYY-1
        DO  IXX=START_1KM,END_1KM
          JMASK=4*IXX-3
          JBLUE=2*IXX-1
c
c Since cloud mask has 0 and 1 values finding product of
c rellectance of channels and cldmask helps to reduce CPU time than
C using  IF_ELSE_THEN statement in cloud mask to determine clear pixels.
c
c
       DO IY =IBLUE,2*IYY
           IIJ=2*IY-1
         DO IX= JBLUE,2*IXX
            IIK=2*IX-1
c  Sediment Mask 
            savecldmask(IX,IY)=NCLDMSK_SYN(IX,IY)
             call make_sediment(W470_syn,W550_SYN,
     *     W124_SYN,W164_SYN,W213_SYN,sed_mask,IX,IY,wave)
c   CIRRUS DETECTION , change the NCLDMSK_SYN to cloudy
            DO JJ= IIJ,2*IY
            DO II= IIK,2*IX 
      IF (W138_syn(IXX,IYY) .gt. 0.03)then
             NCLDMSK_SYN(IX,IY)=0
c save cloud mask before applying cirrus pixels
         savecldmask(IX,IY)=NCLDMSK_SYN(IX,IY)
      Else
c if reflactance 1.38 between 0.01 and  and 0.03 apply ratio
      If (W138_syn(IXX,IYY).gt. 0.01 .and.W138_syn(IXX,IYY).le.0.03) then
c  set ratio 
      IF(W124_syn(IX,IY).gt.0)then 
        IF((W138_syn(IXX,IYY)/W124_syn(IX,IY)).GE.0.30)
     *  NCLDMSK_SYN(IX,IY)=0  
      Endif
        Else
c lower the quality for retrivals 
        if( W124_syn(IX,IY) .gt.0) then
            IF (NCLDMSK_SYN(IX,IY) .eq.1 .and.
     *      W659_syn(II,JJ) .gt. (1.50*Ref_ray(3)).and.
     *      ((W138_syn(IXX,IYY)/W124_syn(IX,IY)) .gt.0.10 .and.
     *      (W138_syn(IXX,IYY)/W124_syn(IX,IY)) .lT.0.30).and.
     *      (W138_syn(IXX,IYY) .gt.0.01 .and.W138_syn(IXX,IYY)  .le.0.03))
     *           Qcontrol_cirrus=1
        
         ENDIF
c
    
      Endif
       Endif
          Enddo
          Enddo

      IF(savecldmask(IX,IY) .EQ.0)num_cloudy_pixels=num_cloudy_pixels+1
         Total_pixels=Total_pixels+1

         IF(NCLDMSK_SYN(IX,IY)* High_Cloud_Flag_500(IX,IY)*
     *       sed_mask (IX,IY) .GT. 0) THEN
c   For each clear pixel find product of
c   Reflectance of channel 1 and ch2 and cldmask.
             numaver1=0
             numaver2=0
             AREFW659=0.0
             AREFW865=0.0
              NUMDATA_659=NUMDATA_659+1
              NUMDATA_865=NUMDATA_865+1
            DO JJ= IIJ,2*IY
            DO II= IIK,2*IX
          numaver1=numaver1+NCLDMSK_SYN(IX,IY)* High_Cloud_Flag_500(IX,IY)
     *                                      *sed_mask (IX,IY)
      AREFW659=AREFW659+(W659_syn(II,JJ)*NCLDMSK_SYN(IX,IY)*
     *              High_Cloud_Flag_500(IX,IY)* sed_mask (IX,IY))
       numaver2=numaver2+NCLDMSK_SYN(IX,IY)* High_Cloud_Flag_500(IX,IY)
     *                                  * sed_mask (IX,IY)            
       AREFW865=AREFW865+(W865_syn(II,JJ)*NCLDMSK_SYN(IX,IY)*
     *           High_Cloud_Flag_500(IX,IY)* sed_mask (IX,IY)  )
C ENDDO FOR 250METER RESOLUTION
            ENDDO
            ENDDO
            IF( numaver1 .GT.0)THEN
           ref_interm(3,NUMDATA_659)=AREFW659/numaver1
           ENDIF
          IF(numaver2 .GT.0)THEN
              ref_interm(4,NUMDATA_865)=AREFW865/numaver2
         ENDIF
c    Foe clear pixel find product all Reflectances for
c    channel 3-channel 7 and cloud mask
C
C     WAVE 0.470 UM
C
          NUMDATA_470= NUMDATA_470+1
         ref_interm(1,NUMDATA_470)=(W470_syn(IX,IY)*
     *         NCLDMSK_SYN(IX,IY)* High_Cloud_Flag_500(IX,IY)
     *                                                  *sed_mask (IX,IY))

C     WAVE 0.550 UM
C
         
   
         vFlag_W550=vFlag_W550+1
          NUMDATA_550= NUMDATA_550+1
         ref_interm(2,NUMDATA_550)=(W550_syn(IX,IY)*
     * NCLDMSK_SYN(IX,IY)* High_Cloud_Flag_500(IX,IY)*sed_mask (IX,IY))

C     WAVE 1.24 UM
C
c       IF(W124_syn(IX,IY).GT. 0.0 .AND. W124_syn(IX,IY).LE. 1.0) THEN
         vFlag_W124=vFlag_W124+1
         NUMDATA_124=NUMDATA_124+1
         ref_interm(5,NUMDATA_124)=(W124_syn(IX,IY)*
     * NCLDMSK_SYN(IX,IY)* High_Cloud_Flag_500(IX,IY)*sed_mask (IX,IY))

C     WAVE 1.64 UM
C
C      IF(W164_syn(IX,IY).GT. 0.0 .AND.W164_syn(IX,IY).LE. 1.0) THEN
          vFlag_W164=vFlag_W164+1
          NUMDATA_164=NUMDATA_164+1
          ref_interm(6,NUMDATA_164)=(W164_syn(IX,IY)*
     *       NCLDMSK_SYN(IX,IY)* High_Cloud_Flag_500(IX,IY)
     *                                                *sed_mask (IX,IY))

C     WAVE 2.13 UM
C

c        IF(W213_syn(IX,IY).GT. 0.0 .AND. W213_syn(IX,IY).LE. 1.0) THEN
         vFlag_W213=vFlag_W213+1
         NUMDATA_213= NUMDATA_213+1
         ref_interm(7,NUMDATA_213)=(W213_syn(IX,IY)*
     * NCLDMSK_SYN(IX,IY)* High_Cloud_Flag_500(IX,IY)*sed_mask (IX,IY))
        ENDIF
c    ENDDO for 500 * 500 meter 20*20 box
          ENDDO
          ENDDO
        
C      ENDDO's for 1km pixels for 10*10 box
         ENDDO
         ENDDO
         CLD_FRAC=real(num_cloudy_pixels)/Real(Total_pixels) 
 
         RETURN
         END
      

C*****************************************************************

      SUBROUTINE INTsolarzenith_albedo_tran(THET0,ALBEDO_R_SMALL,
     *           ALBEDO_R_BIG,ALBEDO_T_SMALL,ALBEDO_T_BIG,MTHET0,
     *           ALBEDO_R_SMALL_tau,ALBEDO_R_BIG_tau,ALBEDO_T_SMALL_tau,
     *           ALBEDO_T_BIG_tau,KSZAM1,KSZAP1)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine interpolates albedo and transmission on
C             solar zenith anfgle
C
C!INPUT PARAMETERS:
C THET0          Solar Zenith angle
c  Albedo for monodirectionally incident radiation Small and large
C ALBEDO_R_SMALL
C ALBEDO_R_BIG
c Transmission for monodirectionally incident radiation Small and large
C ALBEDO_T_SMALL
C ALBEDO_T_BIG
c measured solar zenith angle
C MTHET0
C!OUTPUT PARAMETERS:
c Interpolated Albedo for monodirectionally incident radiation Small and large
C  ALBEDO_R_SMALL_tau
C  ALBEDO_R_BIG_tau
c InterpolatedTransmission for monodirectionally incident radiation Small and
C large
C ALBEDO_T_SMALL_tau
C ALBEDO_T_BIG_tau)
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

      INCLUDE 'mod04.inc'

      REAL ALBEDO_R_SMALL(NTH0,NTAU,NWAV,NUMCASES)
      REAL ALBEDO_R_BIG(NTH0,NTAU,NWAV,NUMCASEB)
      REAL ALBEDO_T_SMALL(NTH0,NTAU,NWAV,NUMCASES)
      REAL ALBEDO_T_BIG(NTH0,NTAU,NWAV,NUMCASEB)
      REAL ALBEDO_R_SMALL_tau(NTAU,NWAV,NUMCASES)
      REAL ALBEDO_R_BIG_tau(NTAU,NWAV,NUMCASEB)
      REAL ALBEDO_T_SMALL_tau(NTAU,NWAV,NUMCASES)
      REAL ALBEDO_T_BIG_tau(NTAU,NWAV,NUMCASEB)
      INTEGER IJ,ITH0,IWAV,LOPT,ITAU,ICASE
      REAL  THET0(NTH0)
      REAL  MTHET0
      REAL  X(100),Y(100),YY1(100),Y1
      INTEGER KSZAM1,KSZAP1,LL
      SAVE
C LOOP IS AROUND THET0,NEW FILE FOR EACH THETA0
          Y1=0.0
          DO IJ = 1,100
          X(IJ)=0.0
          Y(IJ)=0.0
          YY1(IJ)=0.0
          ENDDO
C    INTEROOLATE FOR SMALL CASES
          DO 5  ICASE=1,NUMCASES
          DO 10 IWAV=1,NWAV
          DO 20 ITAU=1,NTAU
          LL=0
          DO 30 ITH0 = KSZAM1,KSZAP1
             LL=LL+1
            X(LL)=THET0(ITH0)
            Y(LL)=ALBEDO_R_SMALL(ITH0,ITAU,IWAV,ICASE)
            YY1(LL)=ALBEDO_T_SMALL(ITH0,ITAU,IWAV,ICASE)
 30        CONTINUE
             y1=0.0
            CALL INTERP(LL,MTHET0,X,Y,Y1)
           ALBEDO_R_SMALL_tau(ITAU,IWAV,ICASE)=Y1
            y1=0.0
             CALL INTERP(LL,MTHET0,X,YY1,Y1)
            ALBEDO_T_SMALL_tau(ITAU,IWAV,ICASE)=Y1
  20       CONTINUE
  10       CONTINUE
   5       CONTINUE
C    INTEROOLATE FOR LARGE CASES
          DO 15  ICASE=1,NUMCASEB
          DO 110 IWAV=1,NWAV
          DO 120 ITAU=1,NTAU
           LL=0
          DO 130 ITH0 =KSZAM1,KSZAP1
             LL=LL+1
            X(LL)=THET0(ITH0)
            Y(LL)=ALBEDO_R_BIG(ITH0,ITAU,IWAV,ICASE)
            YY1(LL)=ALBEDO_T_BIG(ITH0,ITAU,IWAV,ICASE)
 130        CONTINUE
             y1=0.0
             CALL INTERP(LL,MTHET0,X,Y,Y1)
             ALBEDO_R_BIG_tau(ITAU,IWAV,ICASE)=Y1
            y1=0.0
             CALL INTERP(LL,MTHET0,X,YY1,Y1)
          ALBEDO_T_BIG_tau(ITAU,IWAV,ICASE)=Y1
  120       CONTINUE
  110       CONTINUE
   15       CONTINUE
c
         
            RETURN
            END



C*****************************************************************

       SUBROUTINE INTtau_albedo_tran(ALBEDO_R_SMALL_tau,
     *       ALBEDO_R_BIG_tau,ALBEDO_T_SMALL_tau,ALBEDO_T_BIG_tau,
     *       ALBEDO_R_SMALL_final,ALBEDO_R_BIG_final,
     *       ALBEDO_T_SMALL_final,ALBEDO_T_BIG_final,TAUAS,TAUAB,
     *       ISMALL,IBIG)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine interpolates albedo and transmission on
C            optical computed
C
C!INPUT PARAMETERS:
c  Albedo for monodirectionally incident radiation Small and large
C  ALBEDO_R_SMALL_tau
C  ALBEDO_R_BIG_tau
c InterpolatedTransmission for monodirectionally incident radiation Small and
C large
C ALBEDO_T_SMALL_tau
C ALBEDO_T_BIG_tau)
C ISMALL            Index for small mode
C IBIG              Index for Large mode
c Optical thicknesses for small and large mode
CTAUAS
CTAUAB
C!OUTPUT PARAMETERS:
c Interpolated Albedo for monodirectionally incident radiation Small and large
C   ALBEDO_R_SMALL_final
C   ALBEDO_R_BIG_final
c InterpolatedTransmission for monodirectionally incident radiation Small and
C large
C  ALBEDO_T_SMALL_final
C  ALBEDO_T_BIG_final
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

      INCLUDE 'mod04.inc'

      REAL ALBEDO_R_SMALL_tau(NTAU,NWAV,NUMCASES)
      REAL ALBEDO_R_BIG_tau(NTAU,NWAV,NUMCASEB)
      REAL ALBEDO_T_SMALL_tau(NTAU,NWAV,NUMCASES)
      REAL ALBEDO_T_BIG_tau(NTAU,NWAV,NUMCASEB)
      REAL ALBEDO_R_SMALL_final(NWAV,NUMCASES)
      REAL ALBEDO_R_BIG_final(NWAV,NUMCASEB)
      REAL ALBEDO_T_SMALL_final(NWAV,NUMCASES)
      REAL ALBEDO_T_BIG_final(NWAV,NUMCASEB)
      REAL TAUAS(NUMCASES,NWAV,NTAU)
      REAL TAUAB(NUMCASEB,NWAV,NTAU)
      INTEGER IJ,IWAV,LOPT,ITAU,ISMALL,IBIG
      REAL  X(100),Y(100),yy1(100),Y1,XBAR
      SAVE
C LOOP IS AROUND THET0,NEW FILE FOR EACH THETA0
          Y1=0.0
          DO IJ = 1,100
          X(IJ)=0.0
          Y(IJ)=0.0
          YY1(IJ)=0.0
          ENDDO
C    INTEROOLATE FOR SMALL CASES for total optical depth at 0.55
        DO 10 IWAV=1,NWAV
         XBAR=TAU_COMPUTED(Index_wave_550)
          DO 20 ITAU=1,NTAU
            X(ITAU)=TAUAS(ISMALL,Index_wave_550,ITAU)
            Y(ITAU)=ALBEDO_R_SMALL_tau(ITAU,IWAV,ISMALL)
            YY1(ITAU)=ALBEDO_T_SMALL_tau(ITAU,IWAV,ISMALL) 
  20      CONTINUE
                y1=0.0
             CALL INTERP(NTAU,XBAR,X,Y,Y1)
c           CALL RATINT(X,Y,NTAU,XBAR,y1)
             ALBEDO_R_SMALL_final(IWAV,ISMALL)=Y1
            y1=0.0
              CALL INTERP(NTAU,XBAR,X,YY1,Y1)
c               CALL RATINT(X,YY1,NTAU,XBAR,y1)
           ALBEDO_T_SMALL_final(IWAV,ISMALL)=Y1
  10      CONTINUE
C    INTEROOLATE FOR LARGE CASES
          DO 110 IWAV=1,NWAV
        XBAR=TAU_COMPUTED(Index_wave_550)
          DO 120 ITAU=1,NTAU
            X(ITAU)=TAUAB(IBIG,Index_wave_550,ITAU)
            Y(ITAU)=ALBEDO_R_BIG_tau(ITAU,IWAV,IBIG)
            YY1(ITAU)=ALBEDO_T_BIG_tau(ITAU,IWAV,IBIG)
 120        CONTINUE
             y1=0.0
             CALL INTERP(NTAU,XBAR,X,Y,Y1)
c               CALL RATINT(X,Y,NTAU,XBAR,y1)
             ALBEDO_R_BIG_final(IWAV,IBIG)=Y1
            y1=0.0
              CALL INTERP(NTAU,XBAR,X,YY1,Y1)
c            CALL RATINT(X,YY1,NTAU,XBAR,y1)
           ALBEDO_T_BIG_final(IWAV,IBIG)=Y1
  110       CONTINUE

            RETURN
            END



C*****************************************************************

       SUBROUTINE SET_index_inter(MTHET0,MTHET,MPHI,THET0,
     * KSZAM1,KSZAP1,KTHEM1,KTHEP1,KPHIM1,KPHIP1)

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine sets the index for ending and starting
C             positions from the lookup table
C
C!INPUT PARAMETERS:
C              MTHET0      Measured solar Zenith angle
C               MTHET      Measured view  Zenith angle
C                MPHI      Measured Azimuthal Angle
C               THET0      array of  solar Zenith angle in look_up table
C
C!OUTPUT PARAMETERS:
C               KSZAM1     Starting Index for solar zenith angle
C               KSZAP1     Ending Index for solar zenith angle
C               KTHEM1     Starting Index for view angle
C               KTHEP1     Ending   Index for  view angle
c               KPHIM1     Starting Index for  azimuth angle
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

       INCLUDE 'mod04.inc'

       REAL MTHET0,MTHET,MPHI,THET0(NTH0),DEL
       INTEGER KSZA,KSZAM1,KSZAP1,KTHE,KTHEM1,KTHEP1,KPHI,KPHIM1,KPHIP1

c  set theta0
       KSZA=0
       KSZAM1=0
       KSZAP1=0
       KTHE=0
       KTHEM1=0
       KTHEP1=0
       KPHI=0
       KPHIM1=0
       KPHIP1=0
       IF(MTHET0 .LE.THET0(5))THEN
          DEL=12.0
          KSZA=MTHET0/DEL+1
       ELSEIF(MTHET0 .GT.THET0(5))THEN
           DEL=6.0
           KSZA=MTHET0/DEL-3
       ENDIF
        KSZAM1=KSZA
        KSZAP1=KSZA+1
       IF(KSZAM1.LE.0)THEN
          KSZAM1=1
           KSZAP1=2
       ENDIF
       IF(KSZAP1.GE.NTH0)THEN
          KSZAM1=KSZA-1
          KSZAP1=KSZAM1+1
       ENDIF
c  set theta
       KTHE=MTHET/6.0+1
        KTHEM1=KTHE
       KTHEP1=KTHE+1
       IF(KTHEM1.LE.0)THEN
         KTHEM1=1
         KTHEP1=2
       ENDIF
       IF(KTHEP1.GE.NTHET)THEN
         KTHEM1=KTHE-1
         KTHEP1= KTHEM1+1
       ENDIF
c  set phi
        KPHI=MPHI/12.0+1
        KPHIM1=KPHI
        KPHIP1=KPHI+1
       IF(KPHIM1.LE.0)THEN
         KPHIM1=1
         KPHIP1=2
       ENDIF
       IF(KPHIP1.GE.NPHI)THEN
         KPHIM1=KPHI-1
         KPHIP1=KPHIM1+1
       ENDIF
100    format(f5.1,3i6)

       RETURN
       END



C*****************************************************************

       Subroutine Fill_QAflag_ocean(QA_Flag_Ocean,SDS_QCONTROL_Ocean,
     *              Idata)
C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine sets the array for quality control flags
C
C!INPUT PARAMETERS:
C               QA_Flag_Ocean           Quality flag
C               Idata                   Index of 10*10 box
C
C!OUTPUT PARAMETERS:
C               SDS_QCONTROL_Ocean      HDF array for quality control
C
C!REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/18/1999 fhliang
c added SAVE statement;
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

       Integer QA_Flag_Ocean(12),idata,ii
       BYTE    SDS_QCONTROL_Ocean(QA_Ocean,NUMCELLS),QA_Temp


c Product Quality & Retrieval Processing QA flags over ocean
C Quality of 3 and 4 ( Average solution)are repeated tion have
c as best and average solution have same quality.
c

c        QA_Flag_Ocean(1)=0     NO retrieval ( Not useful)
c        QA_Flag_Ocean(1)=1     retrieval    (  useful)
c
c    For all non retrieval boxes QA_Flag_Ocean is set to zero to indicate
c    not useful and QA_Flag_Ocean(5) is set to values of QCONTROL to
c    indicate the cause of Retrieval. QA_Flag_Ocean(6) is set to 11 for
c    no retrieval
         If ( QCONTROL .lt. 0) then
           QA_Flag_Ocean(1)=0
           QA_Flag_Ocean(2)=0
           QA_Flag_Ocean(3)=0
           QA_Flag_Ocean(4)=0
         QA_Flag_Ocean(5)=abs(QCONTROL)
         QA_Flag_Ocean(6)=15
         Else
            QA_Flag_Ocean(1)=1
            QA_Flag_Ocean(3)=1
c  All retrieval boxes
c Estimated Quality of aerosol parameters
c        QA_Flag_Ocean(2)=3     very Good
c        QA_Flag_Ocean(2)=2     Good
c        QA_Flag_Ocean(2)=1     Marginal
c        QA_Flag_Ocean(2)=0     Bad
c        QA_Flag_Ocean(4)=3     very Good
c        QA_Flag_Ocean(4)=2     Good
c        QA_Flag_Ocean(4)=1     Marginal
c        QA_Flag_Ocean(4)=0     Bad


          if( Quality_dust_flag_glint .eq.1)QCONTROL=12
          if(Qcontrol_cirrus.eq.1)QCONTROL=13
          if( Quality_dust_flag_off_glint .eq.1)QCONTROL=14

c
c       IF QCONTROL is 0( see doc.)quality of retrieval  is very good
c
       If( QCONTROL .eq. 0 )QA_Flag_Ocean(2)=3
       If( QCONTROL .eq. 0 )QA_Flag_Ocean(4)=3
c
c       IF QCONTROL is 7 or 14( see doc.)quality of retrieval  is  good
c
       If( QCONTROL .eq. 7 .or. QCONTROL .eq. 14)QA_Flag_Ocean(2)=2
       If( QCONTROL .eq. 7 .or. QCONTROL .eq. 14)QA_Flag_Ocean(4)=2
c
c       IF QCONTROL is 1,3,4,6,8 or 10  quality of retrieval  is Average
c
       If( QCONTROL .eq. 1  .or. QCONTROL .eq. 3 .or. QCONTROL .eq. 4
     *   .or. QCONTROL .eq. 6 .or. QCONTROL .eq. 8
     *   .or. QCONTROL .eq. 10)QA_Flag_Ocean(2)=1
       If( QCONTROL .eq. 1  .or. QCONTROL .eq. 3 .or. QCONTROL .eq. 4
     *   .or. QCONTROL .eq. 6 .or. QCONTROL .eq. 8
     *   .or. QCONTROL .eq. 10)QA_Flag_Ocean(4)=1
c
c       IF QCONTROL is 2,5,9,12,13 quality of retrieval  is poor
        
c
       If( QCONTROL .eq. 2  .or. QCONTROL .eq. 5 .or. QCONTROL .eq. 9 
     *.or. QCONTROL .eq. 12 .or. QCONTROL.eq.13)
     * QA_Flag_Ocean(2)=0
     
       If( QCONTROL .eq. 2  .or. QCONTROL .eq. 5 .or. QCONTROL .eq. 9 
     *.or. QCONTROL .eq. 12 .or. QCONTROL.eq.13)
     * QA_Flag_Ocean(4)=0


         QA_Flag_Ocean(5)=0
         QA_Flag_Ocean(6)=QCONTROL
         endif
c        if(QCONTROL_thick_dust .eq. 12) write(36,*)QCONTROL,QA_Flag_Ocean(2)
C
C Store QA flags into Quality_Assurance_Ocean array according to the order
C of bits in MODIS atmosphere QA plan
C
      QA_Temp=0
      CALL BYTE_SET(QA_Flag_Ocean(1),0,QA_Temp)
      CALL BYTE_SET(QA_Flag_Ocean(2),1,QA_Temp)
      CALL BYTE_SET(QA_Flag_Ocean(3),4,QA_Temp)
      CALL BYTE_SET(QA_Flag_Ocean(4),5,QA_Temp)
C  End of 1 byte

      SDS_QCONTROL_Ocean(1,IDATA)=QA_Temp
c     write(33,*)idata, (QA_Flag_Ocean(ii),ii=1,4),
c     *  QA_Temp,SDS_QCONTROL_Ocean(1,IDATA)

c
c Retrieval processing QA flags_Processing path flags
c For all nonRetrieval boxes change -ve numbers to +ve to repreasent
c the bit array.
c For all Retrieval boxes fill another array
      QA_Temp=0
      CALL BYTE_SET(QA_Flag_Ocean(5),0,QA_Temp)
      CALL BYTE_SET(QA_Flag_Ocean(6),4,QA_Temp)
C  End of 2 byte

      SDS_QCONTROL_Ocean(2,IDATA)=QA_Temp
c       write(33,*)'sec',idata, (QA_Flag_Ocean(ii),ii=5,6),
c     *  QA_Temp,SDS_QCONTROL_Ocean(2,IDATA)

c      write(33,10)idata,(QA_Flag_Ocean(II),II=1,12)
c10     format(12I7)

       Return
       end



C*****************************************************************

      Subroutine Total_retrieval_ocean(Success_Ret_Ocean,
     *                                  Fail_Ret_Ocean)

C----------------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine counts the total number of boxes based on the
C retreival or non-retreival.
C
C!INPUT PARAMETERS:
C
C      QCONTROL           Through the mod04.inc
C
C!OUTPUT PARAMETERS:
C
C      Success_Ret_Ocean  Total number of boxes where retreival was made
C      Fail_Ret_Ocean     Total number of boxes where no retreival was made
C
C!REVISION HISTORY:
C $Log: Process_ocean_V2.f,v $
c 10/25/1999 fhliang
c added SAVE statement;
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

      integer Fail_Ret_Ocean,Success_Ret_Ocean

      if( QCONTROL .lt.0.0)then
         Fail_Ret_Ocean=Fail_Ret_Ocean+1
      else
        Success_Ret_Ocean=Success_Ret_Ocean+1
      endif
      return
      end
         Subroutine make_sediment(W470_syn,W550_SYN,
     *     W124_SYN,W164_SYN,W213_SYN,sed_mask,IX,IY,wave)
C----------------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine identifies the sediments 
C and sets a mask.
C
C!INPUT PARAMETERS:
C      IX        index of 10*10 box
C      IY        index of 10*10 box
C    Reflectances of wavelengths used modis,ch1-ch7  identified by W*_SYN
C       W470_SYN
C       W550_SYN
C       W124_SYN
C       W164_SYN
C       W213_SYN
C        WAVE    wavelength index
C!OUTPUT PARAMETERS:
C
C      sed_mask   Sediment mask 0 = no sediment 1= sediment
C
C!REVISION HISTORY:
C  
C
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS  Shana Mattoo
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------
           IMPLICIT NONE
            SAVE
          INCLUDE 'mod04.inc'
         REAL W470_SYN(2*ISWATH,2*ILINE),W550_SYN(2*ISWATH,2*ILINE),
     *     W124_SYN(2*ISWATH,2*ILINE),W164_SYN(2*ISWATH,2*ILINE),
     *     W213_SYN(2*ISWATH,2*ILINE),a,b,wave(nwav),xa
           Real ref_55_inter
           Integer sed_mask(2*ISWATH,2*ILINE),ij,ix,iy
           real x(nwav),y(nwav),sig(nwav),Del_wav55
            sed_mask(ix,iy)=1
c IF FILL VALUES.........
          IF( W470_syn(IX,IY) .gt. 0 .and. W124_SYN(IX,IY) .gt.0
     *  .and.W164_SYN(IX,IY) .gt.0 .and. W213_SYN(IX,IY) .gt.0) THEN
cc  Numerical version of  fit
             do ij=1,4
             if(ij.eq.1)x(ij)=alog(wave(1))
            if(ij.eq.2)x(ij)=alog(wave(5))
            if(ij.eq.3)x(ij)=alog(wave(6))
            if(ij.eq.4)x(ij)=alog(wave(7))
             if( ij .eq.1)y(ij)=alog(W470_syn(IX,IY))
            if( ij .eq.2)y(ij)=alog(W124_SYN(IX,IY))
            if( ij .eq.3)y(ij)=alog(W164_SYN(IX,IY))
             if( ij .eq.4)y(ij)=alog(W213_SYN(IX,IY))
               enddo 
c ...  Added by Kathy Strabala 15 December 2005
               do ij = 1, NWAV
                  SIG(ij) = 1.0
               enddo
              call  FIT_line(X,Y,SIG,4,A,B)
           XA=ALOG(wave(2))
             ref_55_inter=EXP(A+(B*XA)) 
            Del_wav55=W550_SYN(IX,IY)-ref_55_inter
     
c mask1 true it is sedement+smoke+dust set mask to sedements
             if(W213_SYN(IX,IY) .lt.0.10 .and. Del_wav55 .ge. 0.015)then  
               sed_mask(ix,iy)=0
c mask2 true it is  smoke+dust and no sedements set previuos sedement mask
c to no sedement because it smoke or dust.
               if(W470_syn(IX,IY) .ge. 0.25 .and. Del_wav55 .ge. 0.015)then  
               sed_mask(ix,iy)=1
                endif
              endif
c  ELSE FOR ALL FILL VALUES.........
          ELSE
         sed_mask(ix,iy)=0
        ENDIF
         return 
         end

C rhucek 08/09/02:  Added array argument SIG to conform to FORTRAN standard
C when passing adjustable size arrays
C     SUBROUTINE FIT_line(X,Y,NDATA,A,B)
      SUBROUTINE FIT_line(X,Y,SIG,NDATA,A,B)
C----------------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C THIS SUBROUTINE IS TAKEN FROM NUMERICAL RECIPES PAGE 508
C GIVEN A SET OF NDATA POINTS X(I),Y(I) WITH STANDARD DEVIATIONS
C SIG(I),FIT THEM TO A STRIGHT LINE Y=A+BX BY MINIMIZING X**2
C RETURNED ARE A,B AND THEIR RESPECTIVE PROBABLE UNCERTAINTIES.
C SIGA AND SIGB ,THE CHI-SQUARE AND THE GOODNESS OF FIT PROBABILITY Q
C (THAT THE FIT WOULD HAVE X**2 THIS LARGE OR LARGER). IF MWT=O ON INPUT
C THEN STANDARD DEVIATIONS ARE ASSUMED TO BE UNAVAILABLE:Q IS RETURNED
C AS 1.0 AND THE NORMALIZATION OF CHI-SQARE IS TO UNIT STANDARD DEVATION
C ON ALL POINTS
C
C!INPUT PARAMETERS:
C     X        data set x
C     Y        data set y
C    NDATA  number of data points
C
C!OUTPUT PARAMETERS:
C
C     A      intercept
C     B       slope
C
C!REVISION HISTORY:
C  
C
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS  Shana Mattoo
C
C!DESIGN NOTES:
C
C!END
C----------------------------------------------------------------------

c rhucek 8/9/02: replaced dimension statment with a declaration
      Integer ndata
      REAL      X(NDATA),Y(NDATA),SIG(NDATA)
      REAL      A,B
c ... Local variables
      integer I,MWT
      real    SX,SY,ST2,WT,SS,SXOSS,T,SIGA,SIGB
      
      SX=0.
      SY=0.
      ST2=0.
      B=0.
       MWT=0
      IF(MWT.NE.0) THEN
        SS=0.
        DO 11 I=1,NDATA
          WT=1./(SIG(I)**2)
          SS=SS+WT
          SX=SX+X(I)*WT
          SY=SY+Y(I)*WT
11      CONTINUE
      ELSE
        DO 12 I=1,NDATA
          SX=SX+X(I)
          SY=SY+Y(I)
12      CONTINUE
        SS=FLOAT(NDATA)
      ENDIF
      SXOSS=SX/SS
      IF(MWT.NE.0) THEN
        DO 13 I=1,NDATA
          T=(X(I)-SXOSS)/SIG(I)
          ST2=ST2+T*T
          B=B+T*Y(I)/SIG(I)
13      CONTINUE
      ELSE
        DO 14 I=1,NDATA
          T=X(I)-SXOSS
          ST2=ST2+T*T
          B=B+T*Y(I)
14      CONTINUE
      ENDIF
      B=B/ST2
      A=(SY-SX*B)/SS
      SIGA=SQRT((1.+SX*SX/(SS*ST2))/SS)
      SIGB=SQRT(1./ST2)
      RETURN
      END
