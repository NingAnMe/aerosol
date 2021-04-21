       SUBROUTINE FILLVALUE_Ocean(IDATA,SDS_ref,SDS_ref_STD,
     *SDSTAU_best,SDSTAUS_best,SDSTAUB_best,SDSTAU_average,
     *SDSTAUS_average,SDSTAUB_average,SDS_Least_error,
     *SDS_small_weighting,SDS_sol_INDX_small,SDS_sol_INDX_large,
     *SDSASSY_best,SDSASSY_average,SDS_ccn,sds_mass_conc,
     *SDSBACK_best,SDSBACK_average,SDS_effrad,SDS_AOT_model,
     *SDS_RefF_best,SDS_RefF_average,SDS_TranF_best,SDS_TranF_average,
     *SDS_angs_coeff1,SDS_angs_coeff2,SDS_SCAT_ANGLE_OCEAN,
     *SDS_QCONTROL_ocean,SDS_NUMPIXELS,SDS_CLDFRC_ocean,
     *SDS_Tau_Land_Ocean_img,Qcontrol_special,SDS_correc_small_weighting)

C-----------------------------------------------------------------------
C !F77
C
C !DESCRIPTION: This subroutine Fills the HDF array's with fill values.
C
C !INPUT PARAMETERS:
C IDATA        Index of box number
C !OUTPUT PARAMETERS:
C SDS_ref         Averaged Reflectance array
C SDS_ref_STD     Standard Deviation of Reflectance
C SDSTAU_best     Optical thickness for best solution
C SDSTAUS_best    Optical thickness contribution small particles for best solution
C SDSTAUB_best    Optical thickness contribution large particles for best solution
C SDSTAU_average  Optical thickness for best solution
C SDSTAUS_average Optical thickness contribution small particles for best solution
C SDSTAUB_average Optical thickness contribution large particles for best solution
C SDS_Least_error    Minimm Error function betwwen derived and computed radiances
C SDS_small_weighting Weight factor for large and small mode
C SDS_sol_INDX       Index for solution number
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
C QCONTROL           Value for Quality Control
C SDS_SCAT_ANGLE_OCEAN Scattering angle ocean
C SDS_QCONTROL         Quality control SDS array
C SDS_NUMPIXELS        Number of Pixels used

C !REVISION HISTORY:
C 10/15/1999 fhliang
C fixed prolog.
C
C !TEAM-UNIQUE HEADER: - NOT YET DEFINED
C
C !REFERENCES AND CREDITS
C      WRITTEN BY: Shana Mattoo
C
C !DESIGN NOTES:
C
C !END
C----------------------------------------------------------------------

       IMPLICIT NONE
       SAVE

       INCLUDE 'mod04.inc'

       BYTE    SDS_QCONTROL_ocean(QA_LAND,NUMCELLS)
       INTEGER  IDATA,ICASE,IWAV
       INTEGER*2  SDS_ref(NUMCELLS,NWAV),SDS_ref_STD(NUMCELLS,NWAV)
       INTEGER*2  SDSTAU_best(NUMCELLS,NWAV),
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

       INTEGER*2  SDS_small_weighting(NUMCELLS,NUM_solutions),
     &            SDS_correc_small_weighting(NUMCELLS),
     &            SDS_Least_error(NUMCELLS,NUM_solutions),
     &            SDS_effrad(NUMCELLS,NUM_solutions),
     &            SDS_sol_INDX_small(NUMCELLS,NUM_solutions),
     &            SDS_sol_INDX_large(NUMCELLS,NUM_solutions),
     &            SDS_angs_coeff1(NUMCELLS,NUM_solutions),
     &            SDS_angs_coeff2(NUMCELLS,NUM_solutions),
     &            SDS_AOT_model(NUMCELLS,num_model_index),
     &            SDS_CLDFRC_ocean(NUMCELLS),
     &            SDS_Tau_Land_Ocean_img(NUMCELLS)
       REAL SDS_mass_conc(NUMCELLS,NUM_solutions),
     &      SDS_ccn(NUMCELLS,NUM_solutions)
        INTEGER  INT_Fill_value,Qcontrol_special
        REAL     FLOAT_Fill_value
       INTEGER*2 SDS_NUMPIXELS(NUMCELLS),
     &   SDS_SCAT_ANGLE_OCEAN(NUMCELLS)
c         Qcontrol_special .le.0 condition to fill everything with fill values 
          INT_Fill_value=-9999
          FLOAT_Fill_value=-999.0
          IF(Qcontrol_special .le.0)then
          SDS_NUMPIXELS(idata)=INT_Fill_value
          SDS_SCAT_ANGLE_OCEAN(IDATA)=INT_Fill_value
            if(SDS_CLDFRC_ocean(IDATA) .lt.0)
     *     SDS_CLDFRC_ocean(IDATA)=INT_Fill_value
          DO IWAV= 1,NWAV
            SDS_ref(IDATA,IWAV) =INT_Fill_value
            SDS_ref_STD(IDATA,IWAV) =INT_Fill_value
            SDSASSY_best(IDATA,IWAV) =INT_Fill_value
            SDSASSY_average(IDATA,IWAV) =INT_Fill_value
            SDSBACK_BEST(IDATA,IWAV) =INT_Fill_value
            SDSBACK_AVERAGE(IDATA,IWAV) =INT_Fill_value 
            SDSTAU_BEST(IDATA,IWAV) = INT_Fill_value
            SDSTAUS_BEST(IDATA,IWAV) = INT_Fill_value
            SDSTAUB_BEST(IDATA,IWAV) = INT_Fill_value
            SDSTAU_AVERAGE(IDATA,IWAV) = INT_Fill_value
            SDSTAUS_AVERAGE(IDATA,IWAV) = INT_Fill_value
            SDSTAUB_AVERAGE(IDATA,IWAV) = INT_Fill_value
            ENDDO
            ENDIF
C        Qcontrol_special .eq.1 condition to fill optical thicknesses with zero for
C        ref at 0.865 is very small
             IF(Qcontrol_special .eq.1)then
            SDS_NUMPIXELS(idata)=INT_Fill_value
            SDS_SCAT_ANGLE_OCEAN(IDATA)=INT_Fill_value
             if(SDS_CLDFRC_ocean(IDATA) .lt.0)
     *         SDS_CLDFRC_ocean(IDATA)=INT_Fill_value
            DO IWAV= 1,NWAV
            SDS_ref(IDATA,IWAV) =INT_Fill_value
            SDS_ref_STD(IDATA,IWAV) =INT_Fill_value
            SDSASSY_best(IDATA,IWAV) =INT_Fill_value
            SDSASSY_average(IDATA,IWAV) =INT_Fill_value
            SDSBACK_BEST(IDATA,IWAV) =INT_Fill_value
            SDSBACK_AVERAGE(IDATA,IWAV) =INT_Fill_value
            SDSTAU_BEST(IDATA,IWAV) = 0
            SDSTAUS_BEST(IDATA,IWAV) =0
            SDSTAUB_BEST(IDATA,IWAV) = 0
            SDSTAU_AVERAGE(IDATA,IWAV) = 0
            SDSTAUS_AVERAGE(IDATA,IWAV) = 0
            SDSTAUB_AVERAGE(IDATA,IWAV) = 0
          ENDDO
          ENDIF
C         Qcontrol_special .eq.3 condition to fill Reflectances,sd and number of pixels
C          with real values over glint area 
C
            IF(Qcontrol_special .eq.3)then
            SDS_SCAT_ANGLE_OCEAN(IDATA)=INT_Fill_value
            SDS_CLDFRC_ocean(IDATA)=INT_Fill_value
           DO IWAV= 1,NWAV
            SDSASSY_best(IDATA,IWAV) =INT_Fill_value
            SDSASSY_average(IDATA,IWAV) =INT_Fill_value
            SDSBACK_BEST(IDATA,IWAV) =INT_Fill_value
            SDSBACK_AVERAGE(IDATA,IWAV) =INT_Fill_value
            SDSTAU_BEST(IDATA,IWAV) = INT_Fill_value
            SDSTAUS_BEST(IDATA,IWAV) = INT_Fill_value
            SDSTAUB_BEST(IDATA,IWAV) = INT_Fill_value
            SDSTAU_AVERAGE(IDATA,IWAV) = INT_Fill_value
            SDSTAUS_AVERAGE(IDATA,IWAV) = INT_Fill_value
            SDSTAUB_AVERAGE(IDATA,IWAV) = INT_Fill_value
           ENDDO
           ENDIF
c  Fill with Fill values in all cases
           DO icase=1,NUM_solutions
            SDS_angs_coeff1(IDATA,ICASE) =INT_Fill_value
            SDS_angs_coeff2(IDATA,ICASE) =INT_Fill_value
            SDS_effrad(IDATA,ICASE) =INT_Fill_value
            SDS_Least_error(IDATA,ICASE) = INT_Fill_value
            SDS_small_weighting(IDATA,ICASE) = INT_Fill_value
            SDS_sol_INDX_small(IDATA,ICASE) = INT_Fill_value
            SDS_sol_INDX_large(IDATA,ICASE) = INT_Fill_value
            SDS_ccn(IDATA,ICASE) =FLOAT_Fill_value
            sds_mass_conc(IDATA,ICASE)= FLOAT_Fill_value
         ENDDO
c           SDS_correc_small_weighting(IDATA) = INT_Fill_value
           DO icase=1,num_model_index
            SDS_AOT_model(IDATA,ICASE) = INT_Fill_value
            enddo
            Qcontrol_special=0
           END


C***********************************************************************
      SUBROUTINE FILLVALUE_LAND(IDATA,SDS_Tau_Land_Ocean_img,
     *SDS_Aerosol_Type,SDSTAU_corrected_213,
     *SDS_SCAT_ANGLE_land,SDS_mass_conc_land,
     *SDS_angs_coeff_land,SDS_CLDFRC_land,SDS_dust_weighting,
     *SDS_est_uncer,SDS_RefF_land,SDS_TranF_land,
     *SDS_NUMPIXELS_land,SDSTAU_corrected,SDS_ref_land,
     *SDS_ref_STD_land,SDS_QCONTROL_land,SDSTAU_small_land,
     *SDS_Surface_Reflectance_Land,
     * SDS_Fitting_Error_Land,Qcontrol_special_land)
C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:     This subroutine stores all the arrays to be written
C                  as output(HDF) file
C
C!INPUT PARAMETERS: all varaiables to be wrriten as output
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
C!OUTPUT PARAMETERS:ARRAY of 13 variables for output to HDF FILE
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
C  SDS10           HDF array of the number of blue channel clear pixels
C  SDS11          HDF array of the number of red channel clear pixels
C  SDS12          HDF array of retrieval procedure ID (0-4)
C  SDS13          HDF array of aerosol type (0-3)
C  SDS14          HDF array of Error flag (0-4)
C
C!REVISION HISTORY: Updated code to comply with most MODIS software
C                   standards.
C
C!TEAM-UNIQUE HEADER: - NOT YET DEFINED
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
      SAVE

      INCLUDE 'mod04.inc'

      INTEGER IDATA
      BYTE         SDS_QCONTROL_land(QA_LAND,NUMCELLS)
      REAL         SDS_mass_conc_land(NUMCELLS)
      INTEGER *2   SDS_Tau_Land_Ocean_img(NUMCELLS),
     &             SDS_Aerosol_Type(NUMCELLS),
     &             SDS_SCAT_ANGLE_land(NUMCELLS),
     &             SDS_angs_coeff_land(NUMCELLS),
     &             SDS_CLDFRC_land(NUMCELLS),
     &             SDS_dust_weighting(NUMCELLS), 
     &             SDS_NUMPIXELS_land(NUMCELLS,Land_Sol1),
     &             SDSTAU_corrected(NUMCELLS,Land_Sol3),
     &             SDS_ref_land(NUMCELLS,Band_land),
     &             SDS_ref_STD_land(NUMCELLS,Band_land),
C   9/2005 ( two new SDS's)......     
     &     SDS_Surface_Reflectance_Land(NUMCELLS,Land_Sol3),
     &     SDS_Fitting_Error_Land(NUMCELLS),
     &     SDSTAU_corrected_213(NUMCELLS),
     &     SDSTAU_small_land(NUMCELLS,Land_Sol4) 

C
C Obsolete (02/2006) Land SDS Arrays
C
      INTEGER *2   
     &            SDS_est_uncer(NUMCELLS,Land_Sol1),
     &            SDS_RefF_land(NUMCELLS,Land_Sol2),
     &            SDS_TranF_land(NUMCELLS,Land_Sol1)


      INTEGER  INT_Fill_value,Qcontrol_special_land
      REAL     FLOAT_Fill_value
C
C Set to Fill_values for integer and real variables
C

      INT_Fill_value=-9999
      FLOAT_Fill_value=-999.0


C Set Fill_Values to SDS arrays
C
      
       If ( Qcontrol_special_land .le.0) then
      if(SDS_CLDFRC_land(IDATA) .lt.0)
     *SDS_CLDFRC_land(IDATA)=INT_Fill_value
         SDS_mass_conc_land(IDATA)= FLOAT_Fill_value
        SDS_QCONTROL_land(1,IDATA)=0
        SDS_QCONTROL_land(2,IDATA)=0
        SDS_QCONTROL_land(3,IDATA)=0
        SDS_QCONTROL_land(4,IDATA)=0
        SDS_QCONTROL_land(5,IDATA)=0
         SDS_Aerosol_Type(IDATA)=INT_Fill_value
         SDS_SCAT_ANGLE_land(IDATA)=INT_Fill_value
         SDSTAU_corrected(IDATA,1)=INT_Fill_value
         SDSTAU_corrected(IDATA,2)=INT_Fill_value
         SDSTAU_corrected(IDATA,3)=INT_Fill_value
         SDSTAU_corrected_213(IDATA)=INT_Fill_value
         SDSTAU_small_land(IDATA,1)=INT_Fill_value
         SDSTAU_small_land(IDATA,2)=INT_Fill_value
         SDSTAU_small_land(IDATA,3)=INT_Fill_value
         SDSTAU_small_land(IDATA,4)=INT_Fill_value
         
         SDS_angs_coeff_land(IDATA)=INT_Fill_value
c         SDS_CLDFRC_land(IDATA)=INT_Fill_value
         SDS_dust_weighting(IDATA)=INT_Fill_value
         SDS_NUMPIXELS_land(IDATA,1)=INT_Fill_value
         SDS_NUMPIXELS_land(IDATA,2)=INT_Fill_value
         SDS_ref_land(IDATA,1) =INT_Fill_value
         SDS_ref_land(IDATA,2) =INT_Fill_value
         SDS_ref_land(IDATA,3) =INT_Fill_value
         SDS_ref_land(IDATA,4) =INT_Fill_value
         SDS_ref_land(IDATA,5) =INT_Fill_value
         SDS_ref_land(IDATA,6) =INT_Fill_value
         SDS_ref_land(IDATA,7) =INT_Fill_value
         SDS_ref_STD_land(IDATA,1)=INT_Fill_value
         SDS_ref_STD_land(IDATA,2)=INT_Fill_value
         SDS_ref_STD_land(IDATA,3)=INT_Fill_value
         SDS_ref_STD_land(IDATA,4)=INT_Fill_value
         SDS_ref_STD_land(IDATA,5)=INT_Fill_value
         SDS_ref_STD_land(IDATA,6)=INT_Fill_value
         SDS_ref_STD_land(IDATA,7)=INT_Fill_value
         SDS_Surface_Reflectance_Land(IDATA,1) =INT_Fill_value
         SDS_Surface_Reflectance_Land(IDATA,2) =INT_Fill_value
         SDS_Surface_Reflectance_Land(IDATA,3) =INT_Fill_value 
         SDS_Fitting_Error_Land(IDATA)=INT_Fill_value 
          ENDIF
C if Path is B ie Iprocedure=2 then report only optical depth for 0.46 and 0.55 um
C
      IF(Qcontrol_special_land .eq.1)then
      if(SDS_CLDFRC_land(IDATA) .lt.0)
     *SDS_CLDFRC_land(IDATA)=INT_Fill_value
c        SDS_mass_conc_land(IDATA)= FLOAT_Fill_value
        SDS_QCONTROL_land(1,IDATA)=0
        SDS_QCONTROL_land(2,IDATA)=0
        SDS_QCONTROL_land(3,IDATA)=0
        SDS_QCONTROL_land(4,IDATA)=0
        SDS_QCONTROL_land(5,IDATA)=0
c         SDS_Aerosol_Type(IDATA)=INT_Fill_value
         SDS_SCAT_ANGLE_land(IDATA)=INT_Fill_value
c         SDSTAU_corrected(IDATA,1)=INT_Fill_value
c         SDSTAU_corrected(IDATA,2)=INT_Fill_value
          SDSTAU_corrected(IDATA,3)=INT_Fill_value
          SDSTAU_corrected_213(IDATA)=INT_Fill_value
          SDSTAU_small_land(IDATA,1)=INT_Fill_value
         SDSTAU_small_land(IDATA,2)=INT_Fill_value
         SDSTAU_small_land(IDATA,3)=INT_Fill_value
         SDSTAU_small_land(IDATA,4)=INT_Fill_value
          SDS_angs_coeff_land(IDATA)=INT_Fill_value
c         SDS_CLDFRC_land(IDATA)=INT_Fill_value
         SDS_dust_weighting(IDATA)=INT_Fill_value
         SDS_NUMPIXELS_land(IDATA,1)=INT_Fill_value
         SDS_NUMPIXELS_land(IDATA,2)=INT_Fill_value
         SDS_ref_land(IDATA,1) =INT_Fill_value
         SDS_ref_land(IDATA,2) =INT_Fill_value
         SDS_ref_land(IDATA,3) =INT_Fill_value
         SDS_ref_land(IDATA,4) =INT_Fill_value
         SDS_ref_land(IDATA,5) =INT_Fill_value
         SDS_ref_land(IDATA,6) =INT_Fill_value
         SDS_ref_land(IDATA,7) =INT_Fill_value
         SDS_ref_STD_land(IDATA,1)=INT_Fill_value
         SDS_ref_STD_land(IDATA,2)=INT_Fill_value
         SDS_ref_STD_land(IDATA,3)=INT_Fill_value
         SDS_ref_STD_land(IDATA,4)=INT_Fill_value
         SDS_ref_STD_land(IDATA,5)=INT_Fill_value
         SDS_ref_STD_land(IDATA,6)=INT_Fill_value
         SDS_ref_STD_land(IDATA,7)=INT_Fill_value
         SDS_Surface_Reflectance_Land(IDATA,1) =INT_Fill_value
         SDS_Surface_Reflectance_Land(IDATA,2) =INT_Fill_value
         SDS_Surface_Reflectance_Land(IDATA,3) =INT_Fill_value 
         SDS_Fitting_Error_Land(IDATA)=INT_Fill_value
          ENDIF
         RETURN
         END

C-------------------------------------------------------------


        SUBROUTINE FILLVALUE_LAND_extra(IDATA,index_wave,
     *SDS_Mean_Reflectance_Land_All,
     *SDS_SDev_Reflectance_Land_All,     
     *      SDS_Path_Radiance_Land,   
     *      SDS_Critical_Reflectance_Land,  
     *      SDS_Error_Crit_Reflectance_Land,     
     *      SDS_Error_Path_Radiance_Land,  
     *     SDS_QFlag_Critical_Ref_Land,
     *     SDS_QFlag_Path_Radiance_Land)
       IMPLICIT NONE
       SAVE

      INCLUDE 'mod04.inc'
C
C EXTRA LAND SDS_ARRAYS..........FOR LAND Statistics ONLY
C
       INTEGER iwav,IDATA,index_wave,startwav,endwav
       INTEGER  INT_Fill_value
       INTEGER *2  SDS_Mean_Reflectance_Land_All(NUMCELLS,Land_Sol3), 
     &SDS_SDev_Reflectance_Land_All(NUMCELLS,Land_Sol3),     
     &SDS_Path_Radiance_Land(NUMCELLS,Land_Sol1),   
     & SDS_Critical_Reflectance_Land(NUMCELLS,Land_Sol1),  
     & SDS_Error_Crit_Reflectance_Land(NUMCELLS,Land_Sol1),      
     & SDS_Error_Path_Radiance_Land(NUMCELLS,Land_Sol1),  
     & SDS_QFlag_Critical_Ref_Land(NUMCELLS,Land_Sol1) ,
     & SDS_QFlag_Path_Radiance_Land(NUMCELLS,Land_Sol1)
             INT_Fill_value=-9999
        do iwav=1,3
        SDS_Mean_Reflectance_Land_All(IDATA,iwav)=INT_Fill_value
        SDS_SDev_Reflectance_Land_All(IDATA,iwav)=
     *  INT_Fill_value
       enddo
         if(index_wave .eq.1) then
          startwav=1
          endwav=1
          endif
          if(index_wave .eq.2) then
          startwav=2
          endwav=2
           endif
         if(index_wave .eq.3) then
           startwav=1
             endwav=2
          endif

          do iwav=startwav,endwav
        SDS_Path_Radiance_Land(IDATA,iwav)=INT_Fill_value
        SDS_Critical_Reflectance_Land(IDATA,iwav)=INT_Fill_value
        SDS_Error_Crit_Reflectance_Land(IDATA,iwav)=INT_Fill_value
        SDS_Error_Path_Radiance_Land(IDATA,iwav)=INT_Fill_value
        SDS_QFlag_Critical_Ref_Land(IDATA,iwav)=INT_Fill_value
        SDS_QFlag_Path_Radiance_Land(IDATA,iwav)=INT_Fill_value
        enddo
      RETURN
         END


