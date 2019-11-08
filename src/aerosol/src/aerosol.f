C--------------------------------------------------------------------
C  Copyright (C) 2004,  Space Science and Engineering Center,
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
       PROGRAM AEROSOL

C----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C
C       DIRECT BROADCAST VERSION OF THE MODIS AEROSOL PRODUCT
C  The MODIS ocean aerosol product consists of aerosol optical thickness
C  and size parameters estimates derived on a 10x10 (1-km) pixel spatial
C  array.  The measured radiances in a wide spectral range (0.47-2.13
C  microns) are inverted assuming a bi-lognormal size distribution.
C  The volume and the mean particle size for each log-normal mode are
C  determined. 
C
C    This is the main program for the Direct Broadcast
C    version of the MODIS Aerosol product (MODIS product MOD04).
C
C!Input Parameters:
C    To run:  aerosol.exe aerosol.cfg Satellite data_month
C
C The program reads a list of input files from the aserosol.cfg
C  file.
C
C  Required
C   Inputs Are:  MODIS 1 km flat file radiances and reflectances
C                MODIS quarter km flat file radiances and reflectances
C                MODIS half km flat file radiances and reflectances
C                MODIS flat file geolocation parameters
C                MODIS flat file Metadata information (Day/Night
C                       flag, etc.)
C                MODIS Cloud Mask binary flat file (.img)
C                Global Data Assimilation System (GDAS) model
C                       profiles of temperature and moisture 
C                       in a binary format (not required)
C                Global Ozone binary flat file (not required)
C                       in binary format
C
C                month - Data set numeric month (1-12).  Used to
C                        determine correct coefficients
C!Output Parameters:
C
C  Ouput:  Writes binary flat file of float arrays of 14 output
C          parameters.  For more specific
C          information on the arrays included in the output file,
C          see the Aerosol_Description.txt file.
C
C!REFERENCES AND CREDITS:
C
C  ORIGINAL DAAC VERSION WRITTEN BY:
C  SHANA MATTOO                E-MAIL:mattoo@climate.gsfc.nasa.gov
C  APPLIED RESEARCH CORPORATION          PHONE:  (301) 286-1025
C  NASA/GODDARD SPACE FLIGHT CENTER      FAX:    (301) 286-1759
C  CODE 913                             OFFICE: BLDG.22  RM.C109E
C  GREENBELT, MD 20771
C
C  PORTED TO DIRECT BROADCAST VERSION BY:
C  Kathy Strabala   Kathy.Strabala@ssec.wisc.edu
C  University of Wisconsin-Madison/SSEC/CIMSS
C  Phone:  608-263-8752

C    Refl_1(4x,4y)   Storage buffer for band 1 reflectances.
C    Refl_2(4x,4y)   Storage buffer for band 2 reflectances.
C    Refl_3(2x,2y)   Storage buffer for band 3 reflectances.
C    Refl_4(2x,2y)   Storage buffer for band 4 reflectances.
C    Refl_5(2x,2y)   Storage buffer for band 5 reflectances.
C    Refl_6(2x,2y)   Storage buffer for band 6 reflectances.
C    Refl_7(2x,2y)   Storage buffer for band 7 reflectances.
C    Refl_9(x,y)     Storage buffer for band 9 reflectances.
C    Refl_12(x,y)    Storage buffer for band 12 reflectances.
C    Refl_13(x,y)    Storage buffer for band 13 reflectances.
C    Refl_16(x,y)    Storage buffer for band 16 reflectances.
C    Refl_26(x,y)    Storage buffer for band 26 reflectances.
C    Rad_20(x,y)     Storage buffer for band 20 radiances.
C    Rad_31(x,y)     Storage buffer for band 31 radiances.
C    Rad_32(x,y)     Storage buffer for band 32 radiances.

C!END
C-----------------------------------------------------------------------
C
      IMPLICIT  NONE
      SAVE

      include 'db_mod04uw_debug.inc'
      include 'platform_name.inc'
      include 'COMMONS.inc'
      include 'mod04.inc'

c ... Function name
      CHARACTER*(*)   FUNCNAME
      PARAMETER      (FUNCNAME = 'MOD_PR04_V2')

c ... local arrays 
      character*4 choice_flag, cmonth, interleave_1km, 
     +            interl_mask
      character*5 sat_name
      character*132 cfgname
      character*255 l1b_1km_hdr, l1b_hkm_hdr, l1b_qkm_hdr, l1b_geo_hdr, 
     +              mask_qa_hdr, mask_hdr
      integer imonth(MAXNUM_SCANS), Data_Size(2)
      character*80 bandnames_1km(INBAND),bnames_mask(NMASK),
     +             bandunits_1km(INBAND)

      real Lat(ISWATH,ILINE),Lon(ISWATH,ILINE),
     +     SatZen(ISWATH,ILINE),SolZen(ISWATH,ILINE),
     +     SatAz(ISWATH,ILINE),SolAz(ISWATH,ILINE),
     +     RelAz(ISWATH,ILINE),Height(ISWATH,ILINE),
     +     Refl_1(4*ISWATH,4*ILINE),Refl_2(4*ISWATH,4*ILINE),
     +     Refl_3(2*ISWATH,2*ILINE),Refl_4(2*ISWATH,2*ILINE),
     +     Refl_5(2*ISWATH,2*ILINE),Refl_6(2*ISWATH,2*ILINE),
     +     Refl_7(2*ISWATH,2*ILINE),
     +     Refl_9(ISWATH,ILINE),Refl_12(ISWATH,ILINE),
     +     Refl_13(ISWATH,ILINE),Refl_16(ISWATH,ILINE),
     +     Refl_26(ISWATH,ILINE),
     +     Rad_20(ISWATH,ILINE),Rad_31(ISWATH,ILINE),
     +     Rad_32(ISWATH,ILINE)

      real W659_SYN(4*ISWATH,4*ILINE),W865_SYN(4*ISWATH,4*ILINE),
     *     W470_SYN(2*ISWATH,2*ILINE),W550_SYN(2*ISWATH,2*ILINE),
     *     W124_SYN(2*ISWATH,2*ILINE),W164_SYN(2*ISWATH,2*ILINE),
     *     W213_SYN(2*ISWATH,2*ILINE),W1100_SYN_500m(2*ISWATH,2*ILINE),
     *     W443o_SYN(ISWATH,ILINE),W551o_SYN(ISWATH,ILINE),
     *     W667o_SYN(ISWATH,ILINE),W869o_SYN(ISWATH,ILINE),
     *     W395_SYN(ISWATH,ILINE),W138_SYN(ISWATH,ILINE),
     *     W1100_SYN(ISWATH,ILINE),W1200_SYN(ISWATH,ILINE)
      real SDSLAT(NUMCELLS),SDSLON(NUMCELLS), 
     *     SDS_mass_conc_land(NUMCELLS), 
     *     SDS_ccn(NUMCELLS,NUM_solutions),
     *     SDS_mass_conc(NUMCELLS,NUM_solutions)
c ... New cldmask
      real RMED(ISWATH*2 ),RMEDSQ(ISWATH*2 )
      real RMED_1km(ISWATH),RMEDSQ_1km(ISWATH)

      byte Cloud(Buf_cldmsk,ISWATH,ILINE),
     +     QA_Cloud(Buf_cldmsk_QA,ISWATH,ILINE),
     +     SDS_CldMskQA(NUMCELLS),
     +     SDS_QCONTROL(QA_OCEAN,NUMCELLS),
     +     SDS_QCONTROL_ocean(QA_Ocean,NUMCELLS)
      integer*2 SDS_Tau_Land_Ocean(NUMCELLS), SDS_MPHI(NUMCELLS),
     &          SDS_Tau_Land_Ocean_img(NUMCELLS),
     +          SDS_MPHI0(NUMCELLS), SDS_MTHET(NUMCELLS),
     +          SDS_MTHET0(NUMCELLS), SDS_Scattering_Angle(NUMCELLS),
     +          SDS_ratio_small_Land_Ocean(NUMCELLS),
     +          SDS_Reflected_flux_Land_Ocean(NUMCELLS),
     +          SDSASSY_best(NUMCELLS,NWAV_S), 
     +          SDSASSY_average(NUMCELLS,NWAV_S),
     +          SDSBACK_best(NUMCELLS,NWAV_S), 
     +          SDSBACK_average(NUMCELLS,NWAV_S),
     +          SDSTAU_best(NUMCELLS,NWAV_S), 
     +          SDSTAU_average(NUMCELLS,NWAV_S),
     +          SDSTAUB_best(NUMCELLS,NWAV_S), 
     +          SDSTAUB_average(NUMCELLS,NWAV_S),
     +          SDSTAUS_best(NUMCELLS,NWAV_S), 
     +          SDSTAUS_average(NUMCELLS,NWAV_S),
     +          SDS_angs_coeff1(NUMCELLS,NUM_solutions),
     +          SDS_angs_coeff2(NUMCELLS,NUM_solutions),
     +          SDS_AOT_model(NUMCELLS,num_model_index),
     +          SDS_CLDFRC_ocean(NUMCELLS), 
     +          SDS_correc_small_weighting(NUMCELLS),
     +          SDS_effrad(NUMCELLS,NUM_solutions),
     +          SDS_Least_error(NUMCELLS,NUM_solutions),
     +          SDS_NUMPIXELS(NUMCELLS), SDS_ref(NUMCELLS,NWAV_S),
     +          SDS_RefF_average(NUMCELLS,NWAV_S), 
     +          SDS_RefF_best(NUMCELLS,NWAV_S), 
     +          SDS_ref_STD(NUMCELLS,NWAV_S),
     +          SDS_SCAT_ANGLE_OCEAN(NUMCELLS),
     +          SDS_small_weighting(NUMCELLS,NUM_solutions),
     +          SDS_sol_INDX_large(NUMCELLS,NUM_solutions),
     +          SDS_sol_INDX_small(NUMCELLS,NUM_solutions),
     +          SDS_TranF_best(NUMCELLS,NWAV_S),
     +          SDS_TranF_average(NUMCELLS,NWAV_S)

C LAND SDS_ARRAYS..........FOR LAND ONLY
C
      byte      SDS_QCONTROL_land(QA_LAND,NUMCELLS)
      byte      SDS_QCONTROL_CritRef_land(QA_LAND,NUMCELLS)
      integer*2 SDS_Aerosol_Type(NUMCELLS),
     +          SDS_SCAT_ANGLE_land(NUMCELLS),
     +          SDS_angs_coeff_land(NUMCELLS),
     +          SDS_CLDFRC_land(NUMCELLS),
     +          SDS_dust_weighting(NUMCELLS),
     +          SDSTAU_corrected(NUMCELLS,Land_Sol3),
     +          SDS_NUMPIXELS_land(NUMCELLS,Land_Sol1),
     +          SDS_ref_land(NUMCELLS,Band_land),
     +          SDS_ref_STD_land(NUMCELLS,Band_land),
C 9/2005 ( two new SDS's)......     
     +          SDS_Surface_Reflectance_Land(NUMCELLS,Land_Sol3),
     +          SDS_Fitting_Error_Land(NUMCELLS),
     +          SDSTAU_corrected_213(NUMCELLS),
     +          SDSTAU_small_land(NUMCELLS,Land_Sol4)


C EXTRA LAND SDS_ARRAYS..........FOR LAND Statistics ONLY
C
      integer*2 SDS_Mean_Reflectance_Land_All(NUMCELLS,Land_Sol3),
     +          SDS_SDev_Reflectance_Land_All(NUMCELLS,Land_Sol3),
     +          SDS_Path_Radiance_Land(NUMCELLS,Land_Sol1),
     +          SDS_Critical_Reflectance_Land(NUMCELLS,Land_Sol1),
     +          SDS_Error_Crit_Reflectance_Land(NUMCELLS,Land_Sol1),
     +          SDS_Error_Path_Radiance_Land(NUMCELLS,Land_Sol1),
     +          SDS_QFlag_Critical_Ref_Land(NUMCELLS,Land_Sol1),
     +          SDS_QFlag_Path_Radiance_Land(NUMCELLS,Land_Sol1)

C
C Obsolete (02/2006) Land SDS Arrays
C
      integer*2 SDS_est_uncer(NUMCELLS,Land_Sol1),
     +          SDS_RefF_land(NUMCELLS,Land_Sol2),
     +          SDS_TranF_land(NUMCELLS,Land_Sol1)

      integer CldMsk_250(4*ISWATH,4*ILINE),
     +        CldMsk_500(2*ISWATH,2*ILINE),
     +        CldMsk_1km(ISWATH,ILINE),
     +        DET_Flag(ISWATH,ILINE),
     +        UFQ_Flag(ISWATH,ILINE),
     +        DayNight_Flag(ISWATH,ILINE),
     +        SunGlint_Flag(ISWATH,ILINE),
     +        SnowIce_Flag(ISWATH,ILINE),
     +        SnowMsk_Ratio(ISWATH,ILINE),
     +        SnowMsk_500m(2*ISWATH,2*ILINE),
     +        LandSea_Flag(ISWATH,ILINE),
     +        Non_CloudOb_Flag(ISWATH,ILINE),
     +        Thin_CirNIR_Flag(ISWATH,ILINE),
     +        Shadow_Flag(ISWATH,ILINE),
     +        Thin_CirIR_Flag(ISWATH,ILINE),
     +        Cloud_SimpIR_Flag(ISWATH,ILINE),
     +        High_Cloud_Flag(ISWATH,ILINE),
     +        Cloud_IRTemp_Flag(ISWATH,ILINE),
     +        Cloud_3p75_11_Flag(ISWATH,ILINE),
     +        Cloud_VisRat_Flag(ISWATH,ILINE),
     +        Cloud_SpatVar_Flag(ISWATH,ILINE),
     +        High_Cloud_Flag_500(2*ISWATH,2*ILINE), 
     +        QA_Flag_Land(19),QA_Flag_Ocean(12)



c ... Local scalars
      logical  error_flag
      real error_1km, sfctmp, ugrd, vgrd, pwat, ozone,
     +     Lat_center, Lon_center, MTHET0, MTHET, MPHI0, MPHI, MDPHI,
     +     MSCATT, MHGHT, G_factor, Glint_angle, AA
      integer iostat, iargc, num_arg, month, beg_lin, nlins, beg_ele,
     +        neles, l1b_1km_lun, l1b_hkm_lun, l1b_qkm_lun, geo_1km_lun,
     +        mask_lun, mask_qa_lun, scan_1km_lun, anc_met_lun, 
     +        ozone_lun, mod04_lun, hdr_lun, status, datatype_1km, 
     +        scan_flag, mirror_side, resol_mask,resolution_1km, 
     +        offset_1km, offset_mask,samples_1km, sampl_mask,bands_1km,
     +        bands_mask,lines_1km, lines_mask,datat_mask, nscans, 
     +        npixels, out_lines_10km, out_elements_10km, beg_scan, 
     +        ibes, Iscan, NUMSCAN, Buf_Size1, Buf_Size2, i, 
     +        Success_Ret_Land, Fail_Ret_Land, NO_Ret_Land, 
     +        Success_Ret_Ocean, Fail_Ret_Ocean, NO_Ret_Ocean, 
     +        RTN_MOD05, RTN_MOD07, RTN_NCEP, RTN_DAO, RTN_TOMS, 
     +        RTN_TOVS, Set_Counter_Land, Set_Counter_Ocean, 
     +        Set_Counter_Ocean_cloud, RTN, NUMSQ, IDATA, START_500, 
     +        END_500, START_250, END_250, START_1KM, END_1KM, Water, 
     +        Land, cloud_num, cloud_num_land, Glint, Snowice, 
     +        Qcontrol_special, index_wave, QCONTROL_land_wav1,
     +        QCONTROL_land_wav2, Land_CLDMSK_forfraction(ISWATH,ILINE),
     +        NROWS(ISWATH*2), quality_cirrus(ISWATH,ILINE),
     +        Ret_Quality_cirrus, quality_land, Qcontrol_special_land,
     +        Quality_flag_forJoint
      byte QA_Temp

c ... External functions
      external get_mod04_data, db_get_swath_metadata
      integer  get_mod04_data, db_get_swath_metadata

      write( *, '(2x,''Initiated Direct Broadcast Aerosol '',
     +                 ''Processing  '')' )

      choice_flag = 'MA'
      error_flag = .false.

c ... Initialize return variables
      status = -1
      RTN_TOMS=1
      RTN_TOVS=1
      RTN_DAO=1
      RTN_MOD05=1
      RTN_MOD07=1
      Success_Ret_Land=0
      Fail_Ret_Land=0
      NO_Ret_Land=0
      Success_Ret_Ocean=0
      Fail_Ret_Ocean=0
      NO_Ret_Ocean=0
      NUMSQ = -1
      RTN_NCEP = -1

c --- Get input argument of configuration file containing source
c ---  input and output file information.

c --- Check number of arguments
      num_arg = iargc()
      if (num_arg .ne. 3) then
        print *, 'Usage: aerosol.exe cfgname sat_name month'
        print *, 'where'
        print *, 'cfgname: name of the aserosol configuration file'
        print *, 'sat_name: MODIS satellite platform name (Aqua or Terra)'
        print *, 'month: numeric month (1-12) of dataset'
        call exit(-1)
      endif

c ... Extract arguments
      call getarg(1, cfgname)
      call getarg(2, sat_name)
      call getarg(3, cmonth)
      read(cmonth,'(I2)',iostat=iostat) month

      if (month .lt. 1 .or. month .gt. 12) then
        print *, 'Incorrect month entered: Must be (1-12) ',  month
        call exit(-1)
      endif

c ... Check input platform name to make sure it is either Aqua or Terra
      platform_name = ' '
      if (sat_name(1:5) .eq. 'Terra'   .or.
     +    sat_name(1:5) .eq. 'terra'   .or.
     +    sat_name(1:5) .eq. 'TERRA') then
          platform_name = 'Terra'
      elseif (sat_name(1:4) .eq. 'Aqua'   .or.
     +    sat_name(1:4) .eq. 'aqua'   .or.
     +    sat_name(1:4) .eq. 'AQUA') then
          platform_name = 'Aqua'
      else
          print '(''Error: Incorrect Satellite Platform name entered '', a)',
     +              sat_name(1: len(sat_name))
          call exit(-1)
      endif
c ... Write out platform name
      print '('' For Satellite Platform '', a)',
     +              platform_name(1: len(platform_name))

c ... Get debug value and processing interval information
      call db_mod04_get_rp(cfgname,debug,l1b_1km_hdr,l1b_hkm_hdr,
     +                     l1b_qkm_hdr,l1b_geo_hdr,mask_hdr,mask_qa_hdr,
     +                     beg_lin,nlins,beg_ele,neles)

c ... Open needed files
      call db_mod04_file_open(cfgname,choice_flag,neles,l1b_1km_lun,
     +                        l1b_hkm_lun,l1b_qkm_lun,geo_1km_lun,
     +                        mask_lun,mask_qa_lun,scan_1km_lun,
     +                        anc_met_lun,ozone_lun,handle_LUT466,
     +                        handle_LUT553,handle_LUT644,handle_LUT213,
     +                        handle_LUTMAP,handle_INSCI,handle_S,
     +                        handle_L,mod04_lun,hdr_lun,h_output,
     +                        RTN_NCEP)

c ... Get file metadata information out of the header files
      call db_mod04_Get_Metadata(l1b_1km_hdr,mask_hdr,
     +     nscans,npixels,datatype_1km,datat_mask,interleave_1km,
     +     interl_mask,resolution_1km,resol_mask,offset_1km,
     +     offset_mask,samples_1km,sampl_mask,lines_1km,lines_mask,
     +     error_1km,bands_1km,bands_mask,bandnames_1km,
     +     bnames_mask,bandunits_1km)

      call db_mod04_chk_input(nscans,npixels,beg_lin,nlins,beg_ele,
     +                        neles,beg_scan,ibes,out_lines_10km,
     +                        out_elements_10km)


c ... NUMSCAN gives number of scans of a granule
      numscan = nscans

c ... Fill in month array
      do i = 1, numscan
         imonth(i) = month
      enddo

      Buf_Size1 = ISWATH
      Buf_Size2 = ILINE

c ... Set_Counter_Ocean is set to read the table once as first entry
c ...  into ocean algorithm

      Set_Counter_Ocean=0
      Set_Counter_Land=0
    
      DO 9999 Iscan = 1,NUMSCAN
      
c ...   Write debug info
        write( *, '(''Processing scan # '',i4)' ) Iscan

c ...   Get the swath metadata
        scan_flag = -1
        mirror_side = -1
        status = db_get_swath_metadata(scan_1km_lun,Iscan,scan_flag,
     &                                 mirror_side)
        if (status .ne. 0) then
            call message('aerosol main',
     &                  'FAILED - Unable to extract scan metadata. ',
     &                   0, 2)
        endif

c ....  Only implement the code if day scan
        IF (scan_flag .eq. 1) THEN

c        Set_Counter_Ocean_cloud is set to make cloud mask once for
c         each scan inside ocean algorithm

         Set_Counter_Ocean_cloud=0

c ....   Temporary 
         RTN = get_mod04_data(NUMSQ,l1b_1km_lun,l1b_hkm_lun,
     +     l1b_qkm_lun,geo_1km_lun,mask_lun,mask_qa_lun,
     +     datatype_1km,datat_mask,interleave_1km,interl_mask,
     +     resolution_1km,resol_mask,offset_1km,offset_mask,
     +     samples_1km,sampl_mask,lines_1km,lines_mask,error_1km,
     +     bands_1km,bands_mask,bandnames_1km,
     +     bnames_mask,bandunits_1km,Iscan,Buf_Size1,Buf_Size2,
     +     Data_Size,Lat,Lon,SatZen,SatAz,SolZen,SolAz,RelAz,Height,
     +     Refl_1,Refl_2,Refl_3,Refl_4,Refl_5,Refl_6,Refl_7,
     +     Refl_9,Refl_12,Refl_13,Refl_16,Refl_26,Rad_20,Rad_31,Rad_32,
     +     CldMsk_250,CldMsk_500,CldMsk_1km,DET_Flag,UFQ_Flag,
     +     DayNight_Flag,SunGlint_Flag,SnowIce_Flag,
     +     LandSea_Flag,Non_CloudOb_Flag,Thin_CirNIR_Flag,
     +     Shadow_Flag,Thin_CirIR_Flag,Cloud_SimpIR_Flag,
     +     High_Cloud_Flag,Cloud_IRTemp_Flag,Cloud_3p75_11_Flag,
     +     Cloud_VisRat_Flag,Cloud_SpatVar_Flag,Cloud,QA_CLOUD)

C ...    Vanderlie  Cloud mask...........
         CALL CldMsk_Land(Data_Size,ISWATH,ILINE,Refl_3,Refl_5,
     +      Refl_26,CldMsk_250,CldMsk_500,CldMsk_1km, RMED,RMEDSQ,
     +      RMED_1km,RMEDSQ_1km,iscan,nrows,quality_cirrus,
     +      Land_CLDMSK_forfraction)


         DO 9000 IDATA = 1,NUMSQ

           SDS_Tau_Land_Ocean_img(IDATA)=-9999
           SDS_Tau_Land_Ocean(IDATA)=-9999

c ...      Call to SET_INDEX sets the indexing for Modis Channels
           call SET_INDEX(START_500,END_500,START_250,END_250,
     +        START_1KM,END_1KM,IDATA)

c ...      Call to geoLoc_angle computes geolocation and geometry in
c ...       center of 10* 10 bin.
           call GEOLOC_ANGLE(Lat,Lon,SatZen,SatAz,SolZen,SolAz,
     +       Height,MTHET0,MTHET,MPHI0,MPHI,MDPHI,MSCATT,MHGHT,
     +       START_1KM,Lat_center,Lon_center,iscan)

c ...      Call to  READ_ANC_DATA reads NCEP data modified by UW group.
c ...      At this point it is only reading the data

           if (RTN_NCEP .eq. 0) then
             call DB_READ_ANC_DATA(Lat_center,Lon_center,anc_met_lun,
     +                            ozone_lun,sfctmp,ugrd,vgrd,pwat,ozone)
           endif

           call TRANS_2WAY(QA_Flag_Land,QA_Flag_Ocean,START_250,END_250,
     *        START_500,END_500,START_1KM,END_1KM,RTN_MOD05,
     *        RTN_MOD07,RTN_NCEP,RTN_DAO,RTN_TOMS,RTN_TOVS,
     *        ISWATH,ILINE,MTHET,MTHET0,Refl_1,Refl_2,Refl_3,Refl_4,
     *        Refl_5,Refl_6,Refl_7,Refl_9,Refl_12,Refl_13,Refl_16,
     *        Refl_26,Rad_20,Rad_31,Rad_32,LandSea_Flag,CldMsk_1km,
     *        SnowMsk_500m,SnowMsk_Ratio,pwat,ozone,W659_SYN,
     *        W865_SYN,W470_SYN,W550_SYN,W124_SYN,W164_SYN,W213_SYN,
     *        W443o_SYN,W551o_SYN,W667o_SYN,W869o_SYN,W395_SYN,W138_SYN,
     *        W1100_SYN,W1100_SYN_500m,W1200_SYN,G_factor)

c ...      Call to determine counts of land, water, cloud, sun-glint and
c ...        snow/ice pixels
           call LAND_WATER_CLOUD(DET_Flag,CldMsk_1km,CldMsk_500,
     +        CldMsk_250,High_Cloud_Flag,High_Cloud_Flag_500,
     +        LandSea_Flag,SunGlint_Flag,SnowIce_Flag,SnowMsk_Ratio,
     +        Shadow_Flag,cloud_num,cloud_num_land,Water,Land,Glint,
     +        Snowice,START_1KM,END_1KM,QA_Flag_Land,QA_Flag_Ocean,
     +        QA_Temp,Quality_cirrus, Ret_Quality_cirrus,
     +        Land_CLDMSK_forfraction)

c ...      Call to POPULATE_common_arrays sets the SDS for HDF write for
c ...        Geolocation and angles for 10*10 bin size.
           call POPULATE_COMMON_ARRAYS(IDATA,Lat_center,Lon_center,
     +         MTHET0,MTHET,MPHI0,MPHI,MSCATT,QA_Temp,SDSLAT,
     +         SDSLON,SDS_MTHET0,SDS_MTHET,SDS_MPHI0,SDS_MPHI,
     +         SDS_Scattering_Angle,SDS_CldMskQA)


c ...      Restore cloud fraction in QA_Flag_Land to pass to Process_Land

           call COMPUTE_GLINTANGLE(MTHET0,MTHET,MDPHI,
     +                             GLINT_ANGLE,QA_Flag_Ocean)


c ...      If all pixels of 10*10 bin are water, then processing ocean
c ...        algorithm, else  processing land algorithm; if the number
c ...        of cloudy pixels >90 then reject any processing

           Qcontrol_special_land=0
c ...

           cloud_num=0
           Qcontrol=-1
           IF(water.ge.100 .or. land.gt. 0) THEN
             IF(WATER .GE. 100) THEN

              Set_Counter_Ocean=Set_Counter_Ocean+1
              Set_Counter_Ocean_cloud=Set_Counter_Ocean_cloud+1
              call PROCESS_ocean(HANDLE_S,HANDLE_L,
     +          ISCAN,IDATA,NUMSQ,MTHET0,MTHET,MDPHI,START_500,
     +          END_500,START_250,END_250,START_1KM,END_1KM,
     +          W659_SYN,W865_SYN,W470_SYN,W550_SYN,W124_SYN,
     +          W164_SYN,W213_SYN,W551o_SYN,W667o_SYN,W869o_SYN,
     +          Sunglint_Flag,CldMsk_500,Set_Counter_Ocean,
     +          SDSTAU_best,SDSTAUS_best,SDSTAUB_best,
     +          SDSTAU_average,SDSTAUS_average,SDSTAUB_average,
     +          SDS_Least_error,SDS_small_weighting,SDS_sol_INDX_small,
     +          SDS_sol_INDX_large,SDS_ref,SDS_ref_STD,SDSASSY_best,
     +          SDSASSY_average,SDSBACK_best,SDSBACK_average,SDS_effrad,
     +          SDS_RefF_best,SDS_ccn,SDS_mass_conc,
     +          SDS_RefF_average,SDS_TranF_best,SDS_TranF_average,
     +          SDS_QCONTROL_ocean,SDS_NUMPIXELS,SDS_SCAT_ANGLE_OCEAN,
     +          SDS_AOT_model,SDS_CLDFRC_ocean,
     +          Set_Counter_Ocean_cloud,QA_Flag_ocean,GLINT_ANGLE,
     +          SDS_angs_coeff1,SDS_angs_coeff2,SDS_Tau_Land_Ocean,
     +          Refl_4,Qcontrol_special,SDS_correc_small_weighting,
     +          SDS_Tau_Land_Ocean_img,High_Cloud_Flag_500,W138_SYN,
     +          Refl_3,Refl_1,data_size)


c ...         Call Subroutine to get total number of successful 
c ...          and failed retreivals
              call Total_retrieval_ocean(Success_Ret_Ocean,
     +                                   Fail_Ret_Ocean)

c ...          2002   Fill small weighting for ocean from average value
               if (SDSTAU_average(IDATA,2).gt.0) then
                  aa= ((SDSTAUS_average(IDATA,2)/(SCALE3+OFFSET3))
     *                /(SDSTAU_average(IDATA,2)/SCALE3+OFFSET3))
                  SDS_ratio_small_Land_Ocean(IDATA)= (aa*SCALE3+OFFSET3)
               else
                  SDS_ratio_small_Land_Ocean(IDATA) =
     +                                       SDSTAU_average(IDATA,2)
               endif
               SDS_Reflected_flux_Land_Ocean(IDATA) = 
     +                                       SDS_RefF_average(IDATA,2)
               call POPULATE_TAU_LAND_OCEAN(IDATA,
     +                                  SDS_Tau_Land_Ocean_img,
     +                                  SDSTAU_average,NUMCELLS,NWAV_S)
               call POPULATE_TAU_LAND_OCEAN(IDATA,SDS_Tau_Land_Ocean,
     +                                SDSTAU_average,NUMCELLS,NWAV_S)

c ...          Filled with Fill_Value for land
               SDS_CLDFRC_land(IDATA)=-99

               call FILLVALUE_LAND(IDATA,SDS_Tau_Land_Ocean_img,
     +              SDS_Aerosol_Type,SDSTAU_corrected_213,
     +              SDS_SCAT_ANGLE_land,SDS_mass_conc_land,
     +              SDS_angs_coeff_land,SDS_CLDFRC_land,
     +              SDS_dust_weighting,
     +              SDS_est_uncer,SDS_RefF_land,SDS_TranF_land,
     +              SDS_NUMPIXELS_land,SDSTAU_corrected,SDS_ref_land,
     +              SDS_ref_STD_land,SDS_QCONTROL_land,
     +              SDSTAU_small_land,SDS_Surface_Reflectance_Land,
     +              SDS_Fitting_Error_Land,Qcontrol_special_land)
               NO_Ret_Land=NO_Ret_Land+1

               index_wave=3
               QCONTROL_land_wav1=0
               QCONTROL_land_wav2=0

               call  Fill_QAflag_CritRef_land(QCONTROL_land_wav1,
     +               QCONTROL_land_wav2,SDS_QCONTROL_CritRef_land,Idata)

               call FILLVALUE_LAND_extra(IDATA,index_wave,
     +              SDS_Mean_Reflectance_Land_All,
     +              SDS_SDev_Reflectance_Land_All,
     +              SDS_Path_Radiance_Land,
     +              SDS_Critical_Reflectance_Land,
     +              SDS_Error_Crit_Reflectance_Land,
     +              SDS_Error_Path_Radiance_Land,
     +              SDS_QFlag_Critical_Ref_Land,
     +              SDS_QFlag_Path_Radiance_Land)

c ...        else for Land
             ELSE
               if( Land .GT.0) then

c ...          If not all pixels detected as water pixels, 
c ...            proceed land algorithm

               quality_land=1

               Set_Counter_Land=Set_Counter_Land+1
               if( water .gt.0) quality_land=0

               call PROCESS_Land(HANDLE_LUT466,HANDLE_LUT553,
     +              HANDLE_LUT644,HANDLE_LUT213,HANDLE_LUTMAP,
     +              IMONTH(ISCAN),ISCAN,IDATA,NUMSQ,MTHET0,MTHET,MDPHI,
     +              MHGHT,Lat_center,Lon_center,START_500,END_500,
     +              START_250,END_250,START_1KM,END_1KM,W470_syn,
     +              W550_SYN,W659_syn,W865_syn,W124_SYN,W164_SYN,
     +              W213_syn,CldMsk_250,Set_Counter_Land,QA_Flag_Land,
     +              Success_Ret_Land,Fail_Ret_Land,SDSLAT,SDSLON,
     +              SDS_MTHET0,SDS_MTHET,SDS_MPHI,SDS_Tau_Land_Ocean,
     +              CLDMSK_500,SDS_Tau_Land_Ocean_img,SDS_Aerosol_Type,
     +              SDS_SCAT_ANGLE_land,SDS_mass_conc_land,
     +              SDS_angs_coeff_land,SDS_CLDFRC_land,
     +              SDS_dust_weighting,SDS_est_uncer,SDS_RefF_land,
     +              SDS_TranF_land,SDS_NUMPIXELS_land,SDSTAU_corrected,
     +              SDS_ref_land,SDS_ref_STD_land,SDS_QCONTROL_land,
     +              SDS_Mean_Reflectance_Land_All,
     +              SDS_SDev_Reflectance_Land_All,
     +              SDS_Path_Radiance_Land,
     +              SDS_Critical_Reflectance_Land,
     +              SDS_Error_Crit_Reflectance_Land,
     +              SDS_Error_Path_Radiance_Land,
     +              SDS_QFlag_Critical_Ref_Land,
     +              SDS_QFlag_Path_Radiance_Land,
     +              SDS_QCONTROL_CritRef_land,
     +              G_factor,quality_land,Ret_Quality_cirrus,
     +              cloud_num_land,SDS_Surface_Reflectance_Land,
     +              SDS_Fitting_Error_Land,Qcontrol_special_land,
     +              SDSTAU_corrected_213,Quality_flag_forJoint,
     +              SDSTAU_small_land)

c ...          2002   Fill small weighting for land
               SDS_ratio_small_Land_Ocean(IDATA) = 
     +                                    SDS_dust_weighting(idata)
               SDS_Reflected_flux_Land_Ocean(IDATA) = 
     +                                    SDS_RefF_land(IDATA,2)

               call POPULATE_TAU_LAND_OCEAN(IDATA,
     +              SDS_Tau_Land_Ocean_img,SDSTAU_corrected,NUMCELLS,
     +               Land_Sol3)

               if(Quality_flag_forJoint.eq.1 .or.
     +            Quality_flag_forJoint.eq.2 .or. 
     +            Quality_flag_forJoint.eq.3)
     +            call POPULATE_TAU_LAND_OCEAN(IDATA,
     +                 SDS_Tau_Land_Ocean,SDSTAU_corrected,NUMCELLS,
     +                 Land_Sol3)

c ...          Filled with Fill_Value for land

               Qcontrol=-7
               call Fill_QAflag_ocean(QA_Flag_Ocean,SDS_QCONTROL_Ocean,
     +              Idata)

               SDS_CLDFRC_OCEAN(IDATA)=-99

               call FILLVALUE_Ocean(IDATA,SDS_ref,SDS_ref_STD,
     +              SDSTAU_best,SDSTAUS_best,SDSTAUB_best,
     +              SDSTAU_average,SDSTAUS_average,SDSTAUB_average,
     +              SDS_Least_error,SDS_small_weighting,
     +              SDS_sol_INDX_small,SDS_sol_INDX_large,
     +              SDSASSY_best,SDSASSY_average,SDS_ccn,sds_mass_conc,
     +              SDSBACK_best,SDSBACK_average,SDS_effrad,
     +              SDS_AOT_model,SDS_RefF_best,SDS_RefF_average,
     +              SDS_TranF_best,SDS_TranF_average,SDS_angs_coeff1,
     +              SDS_angs_coeff2,SDS_SCAT_ANGLE_OCEAN,
     +              SDS_QCONTROL_ocean,SDS_NUMPIXELS,SDS_CLDFRC_OCEAN,
     +              SDS_Tau_Land_Ocean_img,Qcontrol_special,
     +              SDS_correc_small_weighting)

c ...        endif for Land pixels
             ENDIF
c ...        endif for water at ocean
             ENDIF

c ...      endif for clouds
           ELSE

             NO_Ret_Land=NO_Ret_Land+1
             SDS_CLDFRC_land(IDATA)=-99

             call FILLVALUE_LAND(IDATA,SDS_Tau_Land_Ocean_img,
     +            SDS_Aerosol_Type,SDSTAU_corrected_213,
     +            SDS_SCAT_ANGLE_land,SDS_mass_conc_land,
     +            SDS_angs_coeff_land,SDS_CLDFRC_land,
     +            SDS_dust_weighting,SDS_est_uncer,SDS_RefF_land,
     +            SDS_TranF_land,SDS_NUMPIXELS_land,
     +            SDSTAU_corrected,SDS_ref_land,SDS_ref_STD_land,
     +            SDS_QCONTROL_land,SDSTAU_small_land,
     +            SDS_Surface_Reflectance_Land,
     +            SDS_Fitting_Error_Land,Qcontrol_special_land)

             index_wave=3

             QCONTROL_land_wav1=0
             QCONTROL_land_wav2=0

            call  Fill_QAflag_CritRef_land(QCONTROL_land_wav1,
     +            QCONTROL_land_wav2,SDS_QCONTROL_CritRef_land,Idata)

            call FILLVALUE_LAND_extra(IDATA,index_wave,
     +           SDS_Mean_Reflectance_Land_All,
     +           SDS_SDev_Reflectance_Land_All,
     +           SDS_Path_Radiance_Land,
     +           SDS_Critical_Reflectance_Land,
     +           SDS_Error_Crit_Reflectance_Land,
     +           SDS_Error_Path_Radiance_Land,
     +           SDS_QFlag_Critical_Ref_Land,
     +           SDS_QFlag_Path_Radiance_Land)

c ...       2002   Fill small weighting for land

            SDS_ratio_small_Land_Ocean(IDATA) =SDS_dust_weighting(idata)
            SDS_Reflected_flux_Land_Ocean(IDATA)= SDS_RefF_land(IDATA,2)

            call POPULATE_TAU_LAND_OCEAN(IDATA,SDS_Tau_Land_Ocean_img,
     +           SDSTAU_corrected,NUMCELLS,Land_Sol3)

            if(Quality_flag_forJoint.eq.1 .or.
     +         Quality_flag_forJoint.eq.2 .or. 
     +         Quality_flag_forJoint.eq.3)
     +         CALL POPULATE_TAU_LAND_OCEAN(IDATA,
     +         SDS_Tau_Land_Ocean,SDSTAU_corrected,NUMCELLS,Land_Sol3)

            QCONTROL=-2

            call Fill_QAflag_ocean(QA_Flag_Ocean,SDS_QCONTROL_Ocean,
     +           Idata)

            SDS_CLDFRC_OCEAN(IDATA)=-99

            call FILLVALUE_Ocean(IDATA,SDS_ref,SDS_ref_STD,
     +           SDSTAU_best,SDSTAUS_best,SDSTAUB_best,SDSTAU_average,
     +           SDSTAUS_average,SDSTAUB_average,SDS_Least_error,
     +           SDS_small_weighting,SDS_sol_INDX_small,
     +           SDS_sol_INDX_large,SDSASSY_best,SDSASSY_average,
     +           SDS_ccn,sds_mass_conc,SDSBACK_best,SDSBACK_average,
     +           SDS_effrad,SDS_AOT_model,SDS_RefF_best,
     +           SDS_RefF_average,SDS_TranF_best,SDS_TranF_average,
     +           SDS_angs_coeff1,SDS_angs_coeff2,SDS_SCAT_ANGLE_OCEAN,
     +           SDS_QCONTROL_ocean,SDS_NUMPIXELS,SDS_CLDFRC_OCEAN,
     +           SDS_Tau_Land_Ocean_img,Qcontrol_special,
     +           SDS_correc_small_weighting)

c ...       2002   Fill small weighting for ocean from average value
            if (SDSTAU_average(IDATA,2).gt.0) then
                aa = ((SDSTAUS_average(IDATA,2)/(SCALE3+OFFSET3))
     *               /(SDSTAU_average(IDATA,2)/SCALE3+OFFSET3))
                SDS_ratio_small_Land_Ocean(IDATA) = (aa*SCALE3+OFFSET3)
            else
               SDS_ratio_small_Land_Ocean(IDATA)=SDSTAU_average(IDATA,2)
            endif

            SDS_Reflected_flux_Land_Ocean(IDATA) = 
     +          SDS_RefF_average(IDATA,2)

            call POPULATE_TAU_LAND_OCEAN(IDATA,SDS_Tau_Land_Ocean_img,
     +            SDSTAU_average,NUMCELLS,NWAV_S)

            call POPULATE_TAU_LAND_OCEAN(IDATA,
     +           SDS_Tau_Land_Ocean,SDSTAU_average,NUMCELLS,NWAV_S)

          ENDIF

9000     CONTINUE
 
c ...  Call to write out MOD04 output file

       call db_mod04_write_products(mod04_lun,hdr_lun,Iscan,NUMSCAN,
     +                              NUMSQ,SDSLAT,SDSLON,
     +                              SDS_Tau_Land_Ocean,
     +                              SDS_ratio_small_Land_Ocean,
     +                              SDSTAU_corrected,SDSTAU_average)

c ...  Continue to next scan line
       ENDIF

9999  CONTINUE

c ... Close files
      call db_mod04_file_close(l1b_1km_lun,l1b_hkm_lun,l1b_qkm_lun,
     +                        geo_1km_lun,mask_lun,mask_qa_lun,
     +                        scan_1km_lun,anc_met_lun,ozone_lun,
     +                        handle_LUT466,handle_LUT553,handle_LUT644,
     +                        handle_LUT213,handle_LUTMAP,handle_INSCI,
     +                        handle_S,handle_L,mod04_lun,hdr_lun)
c
c ... End of program

      write( *, '(2x,''Direct Broadcast Aerosol Processing'',
     +                 '' Completed. '')' )

      end


C*********************************************************************
      SUBROUTINE SET_INDEX(START_500,END_500,START_250,END_250, 
     &                   START_1KM,END_1KM,IDATA) 
C---------------------------------------------------------------------- 
C!F77 
C 
C!DESCRIPTION: 
C 
C        This subroutine sets up the indexing for 10* 10 pixel boxes 
C
C!INPUT PARAMETERS:
C
C        IDATA         10*10 array index on the swath
C
C!OUTPUT PARAMETERS:
C
C        START_500     Starting  Index for 500 meter resolution data
C        END_500       Ending    Index for 500 meter resolution data
C        START_250     Starting  Index for 250 meter resolution data
C        END_250       Ending    Index for 250 meter resolution data
C        START_1KM     Starting  Index for 1 km  resolution data
C        END_1KM       Ending    Index for 1 km  resolution data
C
C
C!REVISION HISTORY:
C
C!TEAM-UNIQUE HEADER:
C
c!END
C----------------------------------------------------------------------

      IMPLICIT  NONE

      INTEGER START_500,END_500,START_250,END_250,IDATA,START_1KM
      INTEGER END_1KM
      IF( IDATA .EQ.1) THEN
        START_1KM=1
        END_1KM=10
        START_500=1
        END_500=20
        START_250=1
        END_250=40
      ELSE
        START_500=START_500+20
        END_500=END_500+20
        START_250=START_250+40
        END_250=END_250+40
        START_1KM=START_1KM+10
        END_1KM=END_1KM+10
      ENDIF

      RETURN
      END
C*********************************************************************
      SUBROUTINE GEOLOC_ANGLE(LAT,LON,SatZen,SatAz,SolZen,
     &SolAz,Height,MTHET0,MTHET,MPHI0,MPHI,MDPHI,MSCATT,MHGHT,
     &START_1KM,Lat_center,Lon_center,iscan)

C---------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
C
C          This subroutine findes the averaged latitude, longitude, height
C          and geometrical angles at the center of 10x10 km^2 area
C
C!INPUT PARAMETERS:
C
C        LAT         Lat array for 10*10 box
C        LON         Lon array for 10*10 box
C        SatZen      Satellite zenith angle
C        SatAz       Satellite Azimuth
C        SolZen      Solar Zenith Angle
C        SolAz       Solar Azimuth
C        RelAz       Relative Azimuth
c        START_500   Starting  Index for 500 m resolution data
C        END_500     Ending    Index for 500 m resolution data
C        START_250   Starting  Index for 250 m resolution data
C        END_250     Ending    Index for 250 m resolution data
C        START_1KM   Starting  Index for 1 km  resolution data
C        END_1KM     Ending    Index for 1 km  resolution data
C
C!OUTPUT PARAMETERS:
C
C        Lat_center  Averaged latitude (in degree)
C        Lon_center  Averaged londitude (in degree)
C        MTHET0      Averaged solar zenith angle (in degree)
C        MTHET       Averaged satellite zenith angle (in degree)
C        MPHI0       Averaged solar azimuth angle (in degree)
C        MPHI        Averaged satellite azimuth angle (in degree)
C        MDPHI       Averaged relative azimuth angle (in degree)
C        MHGHT       Averaged topographic altitude (in km)
c
C!REVISION HISTORY:
c 10/15/1999 fhliang
c fixed prolog.
C
C!TEAM-UNIQUE HEADER:
C
c!END
C----------------------------------------------------------------------

      IMPLICIT NONE
      SAVE

      INCLUDE 'mod04.inc'
        REAL diff, sumdif
      INTEGER START_1KM
      INTEGER IX1,IX2,IY1,IY2,I,iscan
      REAL Lat_center,Lon_center,Lon_Min,Lon_Max,Lon_4(4)
      REAL MTHET0,MTHET,MPHI0,MPHI,MDPHI,MSCATT,MHGHT,AVE1,AVE2
      REAL Lat(ISWATH,ILINE),Lon(ISWATH,ILINE),
     2     SatZen(ISWATH,ILINE),SolZen(ISWATH,ILINE),
     3     SatAz(ISWATH,ILINE),SolAz(ISWATH,ILINE),
     4     Height(ISWATH,ILINE)
      IX1=START_1KM+4
      IX2=START_1KM+5
      IY1=IGRIDY/2
      IY2=IGRIDY/2+1

C
C Finding minimum and maximum of longitudes
C

      Lon_4(1)=LON(IX1,IY1)
      Lon_4(2)=LON(IX2,IY1)
      Lon_4(3)=LON(IX1,IY2)
      Lon_4(4)=LON(IX2,IY2)
      Lon_Min=Lon_4(1)
      Lon_Max=Lon_4(1)

      DO I=1,4
        IF(Lon_4(I).LE.Lon_Min) Lon_Min=Lon_4(I)
        IF(Lon_4(I).GE.Lon_Max) Lon_Max=Lon_4(I)
      ENDDO

C
C If Lon_Max <-180 means fill values are found and if Lon_Min <-180 means at least one'
C fill value is found, under these two condition fill value is set to longitude,
C latitude, and all the angles
C
      IF(Lon_Max.LT.-180.0.OR.Lon_Min.LT.-180.0) THEN

        Lon_center=-999.0
        Lat_center=-999.0
        MTHET0=-99.99
        MTHET=-99.99
        MPHI0=-99.99
        MPHI=-99.99
        MSCATT=-99.99

      ELSE

C
C Otherwise, check for other condisitons that may contain fill value
C
      sumdif = 0.

c.....take longitude difference of pixels 2 through 4 relative to pixel 1
      do i = 2,4
          diff = Lon_4(i) - Lon_4(1)

c..........we are working on a sphere, take shortest arc between points
          if ( diff  .gt.  180. ) diff = diff   - 360.
          if ( diff  .lt.  -180. ) diff = 360. + diff

           sumdif = sumdif + diff
      enddo

c.....mathematically equivalent to {Lon_4(1)+Lon_4(2)+Lon_4(3)+Lon_4(4)} / 4
c    using shortest arcs
      lon_center = Lon_4(1) + sumdif/4.

      if ( lon_center  .gt. +180. ) lon_center = lon_center  - 360.
      if ( lon_center  .lt.   -180. ) lon_center = lon_center + 360.

      Lat_center=(LAT(IX1,IY1)+LAT(IX2,IY1)+
     *            LAT(IX1,IY2)+LAT(IX2,IY2))/4.
         MTHET0=(SolZen(IX1,IY1)+SolZen(IX2,IY1)+
     *        SolZen(IX1,IY2)+SolZen(IX2,IY2))/4.

      MTHET =(SatZen(IX1,IY1)+SatZen(IX2,IY1)+
     *        SatZen(IX1,IY2)+SatZen(IX2,IY2))/4.

      MPHI0 =(SolAz(IX1,IY1)+SolAz(IX2,IY1)+
     *        SolAz(IX1,IY2)+SolAz(IX2,IY2))/4.

      MHGHT =(Height(IX1,IY1)+Height(IX2,IY1)+
     *        Height(IX1,IY2)+Height(IX2,IY2))/4000.

      MPHI  =(satAz(IX1,IY1)+SatAz(IX2,IY1)+
     *        SatAz(IX1,IY2)+SatAz(IX2,IY2))/4.
c
C      MPHI0 =MPHI0 - 180.0
       MDPHI = abs(MPHI0 - MPHI -180.0)
       IF(MDPHI.GT.360.0) MDPHI=amod(MDPHI,360.0)
       IF(MDPHI.GT.180.0) MDPHI=360.0-MDPHI

      IF(MTHET0.GT.0.0.AND.MTHET.GT.0.0.AND.MDPHI.GT.0.0) THEN
        MSCATT = -COS(MTHET0*DTR)*COS(MTHET*DTR)
     *          +SIN(MTHET0*DTR)*SIN(MTHET*DTR)
     *          *COS(MDPHI*DTR)
        MSCATT = ACOS(MSCATT)*RTD
      ELSE
        MSCATT=-99.99
      ENDIF
C      MDPHI  =(RelAz(IX1,IY1)+RelAz(IX2,IY1)+
C     *        RelAz(IX1,IY2)+RelAz(IX2,IY2))/4.

      ENDIF

      RETURN
      END

C************************************************************************
      SUBROUTINE TRANS_2WAY(QA_Flag_Land,QA_Flag_Ocean,START_250,END_250,
     *   START_500,END_500,START_1KM,END_1KM,RTN_MOD05,
     *   RTN_MOD07,RTN_NCEP,RTN_DAO,RTN_TOMS,RTN_TOVS,
     *   ISWATH,ILINE,SatZen,SolZen,Refl_1,Refl_2,Refl_3,Refl_4,Refl_5,
     *   Refl_6,Refl_7,Refl_9,Refl_12,Refl_13,Refl_16,Refl_26,Rad_20,
     *   Rad_31,Rad_32,LandSea_Flag,CldMsk_1km,SnowMsk_500m,
     *   SnowMsk_Ratio,Total_H2O,Total_O3,W659_SYN,W865_SYN,W470_SYN,
     *   W550_SYN,W124_SYN,W164_SYN,W213_SYN,W443o_SYN,W551o_SYN,
     *   W667o_SYN,W869o_SYN,W395_SYN,W138_SYN,W1100_SYN,W1100_SYN_500m,
     *   W1200_SYN,G_factor)
C-----------------------------------------------------------------------
C!F77
C
C!Description:
C
C         To correct reflectance by gases (H2O,O3,CO2) absorption and
C         cirrus clouds.
C
C!Input Parameters:
C
C         QA_Flag_Land     : QA flags over land
C         QA_Flag_Ocean    : QA flags over ocean
C         RTN_MOD05         : Return number of MOD05 product
C         RTN_MOD07         : Return number of MOD07 product
C         RTN_NCEP          : Return number of NCEP data
C         RTN_DAO           : Return number of DAO data
C         RTN_TOMS          : Return number of TOMS data
C         RTN_TOVS          : Return number of TOVS data
C         ISWATH            : Number of pixels across scan
C         ILINE             : NUmber of pixels along scan
C         SatZen            : Satellite zenith angle
C         SolZen            : Solar zenith angle
C         Refl_1            : Reflectance from MODIS channel 1
C         Refl_2            : Reflectance from MODIS channel 2
C         Refl_3            : Reflectance from MODIS channel 3
C         Refl_4            : Reflectance from MODIS channel 4
C         Refl_5            : Reflectance from MODIS channel 5
C         Refl_6            : Reflectance from MODIS channel 6
C         Refl_7            : Reflectance from MODIS channel 7
C         Refl_9            : Reflectance from MODIS channel 9
C         Refl_12           : Reflectance from MODIS channel 12
C         Refl_13           : Reflectance from MODIS channel 13
C         Refl_16           : Reflectance from MODIS channel 16
C         Refl_26           : Reflectance from MODIS channel 26
C         Rad_20            : Radiance from MODIS channel 20
C         Rad_31            : Radiance from MODIS channel 31
C         Refl_32           : Radiance from MODIS channel 32
C         Total_H2O         : Total precipitable water
C         Total_O3          : Total ozone
C
C!Output Parameters:
C
C         W659_SYN          : Corrected reflectance for MODIS channel 1
C         W865_SYN          : Corrected reflectance for MODIS channel 2
C         W470_SYN          : Corrected reflectance for MODIS channel 3
C         W550_SYN          : Corrected reflectance for MODIS channel 4
C         W124_SYN          : Corrected reflectance for MODIS channel 5
C         W164_SYN          : Corrected reflectance for MODIS channel 6
C         W213_SYN          : Corrected reflectance for MODIS channel 7
C         W443o_SYN         : Corrected reflectance for MODIS channel 9
C         W551o_SYN         : Corrected reflectance for MODIS channel 12
C
C         W869o_SYN         : Corrected reflectance for MODIS channel 16
C         W138_SYN          : Corrected reflectance for MODIS channel 26
C         W395_SYN          : Corrected radiance for MODIS channel 20
C         W1100_SYN         : Corrected radiance for MODIS channel 31
C         W1200_SYN         : Corrected radiance for MODIS channel 32
C
C!Revision History:
C
C         WRITTEN BY:
C         Dr. Allen Chu
C         Code 913/SSAI
C         NASA Goddard Space Flight Center
C         Greenbelt, MD 20771
C
C!Team-unique Header:
C
C
C!End
C-----------------------------------------------------------------------

      IMPLICIT NONE

C *\ Global variables

      INTEGER START_500,END_500,START_250,END_250,START_1KM,END_1KM
      INTEGER ISWATH,ILINE,RTN_MOD05,RTN_MOD07,RTN_NCEP,RTN_DAO,
     *        RTN_TOMS,RTN_TOVS,
     *        QA_Flag_Land(19),QA_Flag_Ocean(12),ISNOW
      INTEGER LandSea_Flag(ISWATH,ILINE),CldMsk_1km(ISWATH,ILINE),
     *        SnowMsk_500m(2*ISWATH,2*ILINE),SnowMsk_Ratio(ISWATH,ILINE)

       REAL SatZen,SolZen,Total_H2O,Total_O3

      REAL Refl_1 (4*ISWATH,4*ILINE),Refl_2 (4*ISWATH,4*ILINE),
     *     Refl_3 (2*ISWATH,2*ILINE),Refl_4 (2*ISWATH,2*ILINE),
     *     Refl_5 (2*ISWATH,2*ILINE),Refl_6 (2*ISWATH,2*ILINE),
     *     Refl_7 (2*ISWATH,2*ILINE),
     *     Refl_9 (ISWATH,ILINE),Refl_12(ISWATH,ILINE),
     *     Refl_13(ISWATH,ILINE),Refl_16(ISWATH,ILINE),
     *     Refl_26(ISWATH,ILINE),
     *     Rad_20(ISWATH,ILINE),Rad_31(ISWATH,ILINE),
     *     Rad_32(ISWATH,ILINE)

      REAL W659_SYN(4*ISWATH,4*ILINE),W865_SYN(4*ISWATH,4*ILINE),
     *     W470_SYN(2*ISWATH,2*ILINE),W550_SYN(2*ISWATH,2*ILINE),
     *     W124_SYN(2*ISWATH,2*ILINE),W164_SYN(2*ISWATH,2*ILINE),
     *     W213_SYN(2*ISWATH,2*ILINE),
     *     W443o_SYN(ISWATH,ILINE),W551o_SYN(ISWATH,ILINE),
     *     W667o_SYN(ISWATH,ILINE),W869o_SYN(ISWATH,ILINE),
     *     W395_SYN(ISWATH,ILINE),W138_SYN(ISWATH,ILINE),
     *     W1100_SYN(ISWATH,ILINE),W1200_SYN(ISWATH,ILINE),
     *     W1100_SYN_500m(2*ISWATH,2*ILINE)

C *\ Local variables

      INTEGER IL,IS,IL2,IS2,IL4,IS4,IMASK2,JMASK2,IMASK4,JMASK4,
     *        N_Red,N_0p86,I
      REAL G_factor,LOGCON,LOGCON2,EXPONENT,DEGRAD,Total_Red,
     *     Corr_Cirrus(3),Corr_Cirrus_S,Total_0p86,
     *     Multi_factor_Ch1,Multi_factor_Ch2,
     *     Multi_factor_Ch3,Multi_factor_Ch4,
     *     Multi_factor_Ch5,Multi_factor_Ch6,
     *     Multi_factor_Ch7,
     *     RTrans_H2O_Ch1,RTrans_H2O_Ch2,RTrans_H2O_Ch5,
     *     RTrans_H2O_Ch6,RTrans_H2O_Ch7,
     *     RTrans_O3_Ch1,RTrans_O3_Ch3,RTrans_O3_Ch4,
     *     RTrans_CO2_Ch5,RTrans_CO2_Ch6,RTrans_CO2_Ch7

      REAL Opt_H2O_Clim_Ch1,Opt_H2O_Clim_Ch2,Opt_H2O_Clim_Ch5,
     *     Opt_H2O_Clim_Ch6,Opt_H2O_Clim_Ch7,
     *     Opt_O3_Clim_Ch1,Opt_O3_Clim_Ch3,Opt_O3_Clim_Ch4,
     *     Opt_CO2_Clim_Ch5,Opt_CO2_Clim_Ch6,Opt_CO2_Clim_Ch7
      REAL B1_H2O_Coef(3),B2_H2O_Coef(3),B5_H2O_Coef(3),
     *     B6_H2O_Coef(3),B7_H2O_Coef(3),
     *     B1_O3_Coef,B3_O3_Coef,B4_O3_Coef

      REAL Ratio

c temperature conversion variables
       real  Planck_constant,Speed_light,Boltz_cons
       real   w_meter,wave
       real  wav1,wav2,wav3,c1,c2
       integer ij, Y2_offset, X2_offset,l,p,y2,x2
       parameter(Planck_constant=6.6260755e-34,
     &             Speed_light=2.9979246e+8,
     &Boltz_cons=1.380658e-23,wav1=3.75,wav2=11.0,wav3=12.0)

c gfireman 7/02/04 moved DATA statements below variable declarations
      DATA Opt_H2O_Clim_Ch1/1.543E-02/,Opt_H2O_Clim_Ch2/1.947E-02/,
     *     Opt_H2O_Clim_Ch5/1.184E-02/,Opt_H2O_Clim_Ch6/9.367E-03/,
     *     Opt_H2O_Clim_Ch7/5.705E-02/,
     *     Opt_O3_Clim_Ch1/2.478E-02/,Opt_O3_Clim_Ch3/2.432E-03/,
     *     Opt_O3_Clim_Ch4/2.957E-02/,
     *     Opt_CO2_Clim_Ch5/4.196E-04/,Opt_CO2_Clim_Ch6/8.260E-03/,
     *     Opt_CO2_Clim_Ch7/2.164E-02/

      DATA B1_H2O_Coef/-5.73888,0.925534,-0.0188365/,
     *     B2_H2O_Coef/-5.32960,0.824260,-0.0277443/,
     *     B5_H2O_Coef/-6.39296,0.942186,-0.0131901/,
     *     B6_H2O_Coef/-7.76288,0.979707,0.007784/,
     *     B7_H2O_Coef/-4.05388,0.872951,-0.0268464/,
     *     B1_O3_Coef/5.09E-5/,B3_O3_Coef/4.26E-6/,
     *     B4_O3_Coef/1.05E-4/


      DEGRAD=ACOS(-1.)/180.

C
C change total precipitable water unit from mm (kg/m^2) to cm
C

      Total_H2o=Total_H2o/10.
      Corr_Cirrus_S=0.0

C
C Calculate gemoetric factor for 2-way transmission
C
          G_factor=-1.0
          IF(SatZen.GT.0.0.AND.SolZen.GT.0.0) THEN
            G_factor=1./COS(DEGRAD*SatZen)
     *              +1./COS(DEGRAD*SolZen)
          ENDIF

C
C Calculate 2-way H2O transmission
C

          IF(RTN_NCEP.EQ.0.AND.Total_H2O.GT.0.0.AND.G_factor.GT.0.0) THEN

            LOGCON=ALOG(Total_H2O*G_factor)
            LOGCON2=LOGCON*LOGCON
            EXPONENT=B1_h2o_Coef(1)+B1_h2o_Coef(2)*LOGCON
     *              +B1_h2o_Coef(3)*LOGCON2
            RTrans_H2O_Ch1=EXP(EXP(EXPONENT))

            EXPONENT=B2_h2o_Coef(1)+B2_h2o_Coef(2)*LOGCON
     *              +B2_h2o_Coef(3)*LOGCON2
            RTrans_H2O_Ch2=EXP(EXP(EXPONENT))

            EXPONENT=B5_h2o_Coef(1)+B5_h2o_Coef(2)*LOGCON
     *              +B5_h2o_Coef(3)*LOGCON2
            RTrans_H2O_Ch5=EXP(EXP(EXPONENT))

            EXPONENT=B6_h2o_Coef(1)+B6_h2o_Coef(2)*LOGCON
     *              +B6_h2o_Coef(3)*LOGCON2
            RTrans_H2O_Ch6=EXP(EXP(EXPONENT))

            EXPONENT=B7_h2o_Coef(1)+B7_h2o_Coef(2)*LOGCON
     *              +B7_h2o_Coef(3)*LOGCON2
            RTrans_H2O_Ch7=EXP(EXP(EXPONENT))

          ELSE

            RTrans_H2O_Ch1=EXP(Opt_H2O_Clim_Ch1*G_factor)
            RTrans_H2O_Ch2=EXP(Opt_H2O_Clim_Ch2*G_factor)
            RTrans_H2O_Ch5=EXP(Opt_H2O_Clim_Ch5*G_factor)
            RTrans_H2O_Ch6=EXP(Opt_H2O_Clim_Ch6*G_factor)
            RTrans_H2O_Ch7=EXP(Opt_H2O_Clim_Ch7*G_factor)

          ENDIF

C
C Calculate 2-way O3 transmission
C

          IF(RTN_NCEP.EQ.0.AND.Total_O3.GT.0.0.AND.G_factor.GT.0.0) THEN

            EXPONENT=Total_O3*G_factor
            RTrans_O3_Ch1=EXP(B1_O3_Coef*EXPONENT)
            RTrans_O3_Ch3=EXP(B3_O3_Coef*EXPONENT)
            RTrans_O3_Ch4=EXP(B4_O3_Coef*EXPONENT)

          ELSE

            RTrans_O3_Ch1=EXP(Opt_O3_Clim_Ch1*G_factor)
            RTrans_O3_Ch3=EXP(Opt_O3_Clim_Ch3*G_factor)
            RTrans_O3_Ch4=EXP(Opt_O3_Clim_Ch4*G_factor)

          ENDIF

C
C Calculate 2-way CO2 transmission (assuning climatology)
C

            RTrans_CO2_Ch5=EXP(Opt_CO2_Clim_Ch5*G_factor)
            RTrans_CO2_Ch6=EXP(Opt_CO2_Clim_Ch6*G_factor)
            RTrans_CO2_Ch7=EXP(Opt_CO2_Clim_Ch7*G_factor)

C
C Correction reflectance due to gas absorption and cirrus (or Polar
C Stratospheric Aerosol) reflectance
C

          Multi_factor_Ch1=RTrans_H2O_Ch1*RTrans_O3_Ch1
          Multi_factor_Ch2=RTrans_H2O_Ch2
          Multi_factor_Ch3=RTrans_O3_Ch3
          Multi_factor_Ch4=RTrans_O3_Ch4
          Multi_factor_Ch5=RTrans_H2O_Ch5*RTrans_CO2_Ch5
          Multi_factor_Ch6=RTrans_H2O_Ch6*RTrans_CO2_Ch6
          Multi_factor_Ch7=RTrans_H2O_Ch7*RTrans_CO2_Ch7

C
C Loop (1x1 km reoslution) over along and across scan line within a
C granule; IMASK2 and JMASK2 are used for 500 m resolution
C ; IMASK4 and JMASK4 are for 250 m resolution
C (ILINE=10, ISWATH=1500)
C

      DO 100 IL=1,ILINE
        DO 110 IS=START_1KM,END_1KM

        IMASK2=2*IS -1
        JMASK2=2*IL -1
        IMASK4=4*IS -3
        JMASK4=4*IL -3

C
C Perform cirrus correction (if Reflectance of 1.38 micron < 1 %
C and Reflectance of 0.66 micron > 4%)
C

        N_Red=0
        Total_Red=0.0
          DO 400 IL4=JMASK4,4*IL
            DO 410 IS4=IMASK4,4*IS
              IF(Refl_1(IS4,IL4).GT.0.0) THEN
                Total_Red=Total_Red+Refl_1(IS4,IL4)
                N_Red=N_Red+1
              ENDIF
410         CONTINUE
400       CONTINUE

        IF(N_Red.GT.0) THEN
          Total_Red=Total_Red/N_Red
        ELSE
          Total_Red=0.
        ENDIF

C
C LandSea_Flag = 0 : OCEAN  ; LandSea_Flag = 1 COASTAL
C LandSea_Flag = 2 : DESERT ; LandSea_Flag = 3 LAND
C
C QA_Flag_Ocean(10) is used instead of QA_Flag_Ocean(9) (9/99)
C

        IF(LandSea_Flag(IS,IL).EQ.0) THEN

          IF(Refl_26(IS,IL).GT.0.0)THEN

            IF(Total_Red.GE.0.04.AND.Refl_26(IS,IL).LT.0.005) THEN
C              Corr_Cirrus(1)=Refl_26(IS,IL)*SLOPE_MEAN_OCEAN(1)
C              Corr_Cirrus(2)=Refl_26(IS,IL)*SLOPE_MEAN_OCEAN(2)
C              Corr_Cirrus(3)=Refl_26(IS,IL)*SLOPE_MEAN_OCEAN(3)
              Corr_Cirrus_S=2.*Refl_26(IS,IL)
              QA_Flag_Land(16)=0
              QA_Flag_Ocean(10)=0
            ENDIF

            IF(Total_Red.LT.0.04) THEN
              DO I=1,3
                Corr_Cirrus(I)=9999.0
              ENDDO
C              CldMsk_1km(IS,IL)=0.0
              Corr_Cirrus_S=9999.0
              QA_Flag_Land(16)=2
              QA_Flag_Ocean(10)=2
            ENDIF

            IF(Refl_26(IS,IL).GT.0.005) THEN
              DO I=1,3
                Corr_Cirrus(I)=9999.0
              ENDDO
C              CldMsk_1km(IS,IL)=0.0
              Corr_Cirrus_S=9999.0
              QA_Flag_Land(16)=3
              QA_Flag_Ocean(10)=3
            ENDIF

          ELSE

            DO I=1,3
              Corr_Cirrus(I)=9999.0
            ENDDO
C            CldMsk_1km(IS,IL)=0.0
            Corr_Cirrus_S=9999.0
            QA_Flag_Land(16)=1
            QA_Flag_Ocean(10)=1

          ENDIF
C
C For
C LandSea_Flag = 1 - COASTAL, LandSea_Flag = 2 - DESERT and LandSea_Flag = 3 - LAND
C
        ELSE

          IF(Refl_26(IS,IL).GT.0.0)THEN

            IF(Total_Red.GE.0.04.AND.Refl_26(IS,IL).LT.0.005) THEN
C              Corr_Cirrus(1)=Refl_26(IS,IL)*SLOPE_MEAN_LAND(1)
              Corr_Cirrus_S=2.*Refl_26(IS,IL)
              QA_Flag_Land(16)=0
              QA_Flag_Ocean(10)=0
            ENDIF

            IF(Total_Red.LT.0.04) THEN
              Corr_Cirrus(1)=9999.0
              Corr_Cirrus_S=9999.0
C              CldMsk_1km(IS,IL)=0.0
              QA_Flag_Land(16)=2
              QA_Flag_Ocean(10)=2
            ENDIF

            IF(Refl_26(IS,IL).GT.0.005) THEN
              Corr_Cirrus(1)=9999.0
              Corr_Cirrus_S=9999.0
C              CldMsk_1km(IS,IL)=0.0
              QA_Flag_Land(16)=3
              QA_Flag_Ocean(10)=3
            ENDIF

          ELSE

            Corr_Cirrus(1)=9999.0
            Corr_Cirrus_S=9999.0
C            CldMsk_1km(IS,IL)=0.0
            QA_Flag_Land(16)=1
            QA_Flag_Ocean(10)=1

          ENDIF

        ENDIF

C
C To be deleted for delivery
C
C        Multi_factor_Ch1=1
C        Multi_factor_Ch2=1
C        Multi_factor_Ch3=1
C        Multi_factor_Ch4=1
C        Multi_factor_Ch5=1
C        Multi_factor_Ch6=1
C        Multi_factor_Ch7=1
C
C Correct radiance due to gaseous absorption and cirrus clouds contamination
C

        IF(LandSea_Flag(IS,IL).EQ.0) THEN

C
C For Ocean
C
          DO 200 IL4=JMASK4,4*IL
            DO 210 IS4=IMASK4,4*IS
              IF(Refl_1(IS4,IL4).GT.0.0) THEN
                W659_SYN(IS4,IL4)=Refl_1(IS4,IL4)*Multi_factor_Ch1
C     1                         -Corr_Cirrus(1)
              ELSE
                W659_SYN(IS4,IL4)=Refl_1(IS4,IL4)
              ENDIF
              IF(Refl_2(IS4,IL4).GT.0.0) THEN
                W865_SYN(IS4,IL4)=Refl_2(IS4,IL4)*Multi_factor_Ch2
C     1                         -Corr_Cirrus(1)
              ELSE
                W865_SYN(IS4,IL4)=Refl_2(IS4,IL4)
              ENDIF
210         CONTINUE
200       CONTINUE

          DO 300 IL2=JMASK2,2*IL
            DO 310 IS2=IMASK2,2*IS
              IF(Refl_3(IS2,IL2).GT.0.0) THEN
                W470_SYN(IS2,IL2)=Refl_3(IS2,IL2)*Multi_factor_Ch3
C     1                         -Corr_Cirrus(1)
              ELSE
                W470_SYN(IS2,IL2)=Refl_3(IS2,IL2)
              ENDIF
              IF(Refl_4(IS2,IL2).GT.0.0) THEN
                W550_SYN(IS2,IL2)=Refl_4(IS2,IL2)*Multi_factor_Ch4
C     1                         -Corr_Cirrus(1)
              ELSE
                W550_SYN(IS2,IL2)=Refl_4(IS2,IL2)
              ENDIF
              IF(Refl_5(IS2,IL2).GT.0.0) THEN
                W124_SYN(IS2,IL2)=Refl_5(IS2,IL2)*Multi_factor_Ch5
C     1                         -Corr_Cirrus(1)
              ELSE
                W124_SYN(IS2,IL2)=Refl_5(IS2,IL2)
              ENDIF
              IF(Refl_6(IS2,IL2).GT.0.0) THEN
                W164_SYN(IS2,IL2)=Refl_6(IS2,IL2)*Multi_factor_Ch6
C     1                         -Corr_Cirrus(2)
              ELSE
                W164_SYN(IS2,IL2)=Refl_6(IS2,IL2)
              ENDIF
              IF(Refl_7(IS2,IL2).GT.0.0) THEN
                W213_SYN(IS2,IL2)=Refl_7(IS2,IL2)*Multi_factor_Ch7
C     1                         -Corr_Cirrus(3)
              ELSE
                W213_SYN(IS2,IL2)=Refl_7(IS2,IL2)
              ENDIF
310         CONTINUE
300       CONTINUE

          IF(Refl_9(IS,IL).GT.0.0) THEN
            W443o_SYN(IS,IL)=Refl_9(IS,IL)*Multi_factor_Ch3
C     1                         -Corr_Cirrus(1)
          ENDIF
          IF(Refl_12(IS,IL).GT.0.0) THEN
            W551o_SYN(IS,IL)=Refl_12(IS,IL)*Multi_factor_Ch4
C     1                         -Corr_Cirrus(1)
          ENDIF
          IF(Refl_13(IS,IL).GT.0.0) THEN
            W667o_SYN(IS,IL)=Refl_13(IS,IL)*Multi_factor_Ch1
C     1                         -Corr_Cirrus(1)
          ENDIF
          IF(Refl_16(IS,IL).GT.0.0) THEN
            W869o_SYN(IS,IL)=Refl_16(IS,IL)*Multi_factor_Ch2
C     1                         -Corr_Cirrus(1)
          ENDIF

        ELSE
C
C For Land
C
          DO 600 IL4=JMASK4,4*IL
            DO 610 IS4=IMASK4,4*IS
              IF(Refl_1(IS4,IL4).GT.0.0) THEN
                W659_SYN(IS4,IL4)=Refl_1(IS4,IL4)*Multi_factor_Ch1
C     1                         -Corr_Cirrus(1)
              ELSE
                W659_SYN(IS4,IL4)=Refl_1(IS4,IL4)
              ENDIF
              IF(Refl_2(IS4,IL4).GT.0.0) THEN
                W865_SYN(IS4,IL4)=Refl_2(IS4,IL4)*Multi_factor_Ch2
C     1                         -Corr_Cirrus(1)
              ELSE
                W865_SYN(IS4,IL4)=Refl_2(IS4,IL4)
              ENDIF
610         CONTINUE
600       CONTINUE

          DO 500 IL2=JMASK2,2*IL
            DO 510 IS2=IMASK2,2*IS
              IF(Refl_3(IS2,IL2).GT.0.0) THEN
                W470_SYN(IS2,IL2)=Refl_3(IS2,IL2)*Multi_factor_Ch3
C     1                         -Corr_Cirrus(1)
              ELSE
                W470_SYN(IS2,IL2)=Refl_3(IS2,IL2)
              ENDIF
              IF(Refl_4(IS2,IL2).GT.0.0) THEN
                W550_SYN(IS2,IL2)=Refl_4(IS2,IL2)*Multi_factor_Ch4
C     1                         -Corr_Cirrus(1)
              ELSE
                W550_SYN(IS2,IL2)=Refl_4(IS2,IL2)
              ENDIF
              IF(Refl_5(IS2,IL2).GT.0.0) THEN
                W124_SYN(IS2,IL2)=Refl_5(IS2,IL2)*Multi_factor_Ch5
C     1                         -Corr_Cirrus(1)
              ELSE
                W124_SYN(IS2,IL2)=Refl_5(IS2,IL2)
              ENDIF
              IF(Refl_6(IS2,IL2).GT.0.0) THEN
                W164_SYN(IS2,IL2)=Refl_6(IS2,IL2)*Multi_factor_Ch6
C     1                         -Corr_Cirrus(1)
              ELSE
                W164_SYN(IS2,IL2)=Refl_6(IS2,IL2)
              ENDIF
              IF(Refl_7(IS2,IL2).GT.0.0) THEN
                W213_SYN(IS2,IL2)=Refl_7(IS2,IL2)*Multi_factor_Ch7
C     1                         -Corr_Cirrus(1)*0.3
              ELSE
                W213_SYN(IS2,IL2)=Refl_7(IS2,IL2)
              ENDIF
510         CONTINUE
500       CONTINUE

        ENDIF
C
C No change for the rest of 4 channels
C
            W138_SYN(IS,IL)=Refl_26(IS,IL)

c          W395_SYN(IS,IL)=  Rad_20(IS,IL)
c          W1100_SYN(IS,IL)=Rad_31(IS,IL)
c          W1200_SYN(IS,IL)=Rad_32(IS,IL)


C compute Temprature channels ( convert from radiance to temperature)
c Derive constants 
         c1=2.0*Planck_constant*(Speed_light*Speed_light)
         c2=(Planck_constant*Speed_light)/Boltz_cons
c convert wavelength to meters
           do ij=1,3
           if( ij .eq.1)wave=wav1
           if( ij .eq.2)wave=wav2
           if( ij .eq.3)wave=wav3
           w_meter=(1.0e-6*wave)
          if( ij .eq.1 .and.Rad_20(IS,IL).gt.0 )W395_SYN(IS,IL)=
     &  c2/(w_meter*alog(c1/(1.0e+6*Rad_20(IS,IL)*w_meter**5)+1.0))
          if( ij .eq.2 .and. Rad_31(IS,IL).gt.0)W1100_SYN(IS,IL)=
     &  c2/(w_meter*alog(c1/(1.0e+6*Rad_31(IS,IL)*w_meter**5)+1.0))
          if( ij .eq.3 .and.Rad_32(IS,IL).gt.0)W1200_SYN(IS,IL)=
     &  c2/(w_meter*alog(c1/(1.0e+6*Rad_32(IS,IL)*w_meter**5)+1.0))
          enddo

110     CONTINUE
100   CONTINUE
c convert to 500 meter resolution
          DO IL = 1,ILINE
          DO IS=START_1KM,END_1KM
             Y2_offset =( IL-1)*2
             X2_offset = (IS-1)*2
        IF(W1100_SYN(IS,IL) .gt.0) then
                  DO l = 1,2
                  DO p = 1,2
                    Y2 = Y2_offset + l
                   X2 = X2_offset + p
                   W1100_SYN_500m(X2,Y2) =W1100_SYN(IS,IL)
               ENDDO
               ENDDO
          ENDIF
       ENDDO
       ENDDO
c           DO IL = 1,ILINE*2
c       write(39,*)  IL,(W1100_SYN_500m(X2,il),x2=1,20)
c       enddo
c         DO IL = 1,ILINE 
c        write(40,*)IL,(W1100_SYN(x2,IL),x2=1,10)
c        enddo
C
C Snow-masking scheme based upon Ref(1.24 micron)/Ref(0.86 micron) < 1 at 500 m resolution
C 2/26/2000

      DO 120 IL=1,ILINE*2
        DO 130 IS=START_500,END_500

          IMASK2=2*IS -1
          JMASK2=2*IL -1

          N_0p86=0
          Total_0p86=0.0
          DO 420 IL2=JMASK2,2*IL
            DO 430 IS2=IMASK2,2*IS
              IF(Refl_2(IS2,IL2).GT.0.0) THEN
                Total_0p86=Total_0p86+Refl_2(IS2,IL2)
                N_0p86=N_0p86+1
              ENDIF
430         CONTINUE
420       CONTINUE
             Ratio=0
          IF(Total_0p86.GT.0.0.AND.Refl_5(IS,IL).GT.0.0) THEN
            IF(N_0p86.GT.0) THEN
c rong -rong def (ref86-ref1.24)/(ref86+ref1.24)

         Ratio=((Total_0p86/N_0p86)-Refl_5(IS,IL))/
     &         ((Total_0p86/N_0p86)+Refl_5(IS,IL))
            ELSE
              Total_0p86=0.0
                Ratio=0.0
            ENDIF
          ELSE
            Total_0p86=0.0
             Ratio=0.0
          ENDIF

           SnowMsk_500m(IS,IL)=1
Cccc    IF(ratio.gt.0.20 .and.(Total_0p86/N_0p86).gt.0.08) THEN

        IF(ratio.gt.0.01 .and. W1100_SYN_500m(IS,IL) .lt. 285) THEN
            SnowMsk_500m(IS,IL)=0
          ENDIF

130     CONTINUE
120   CONTINUE

      DO 140 IL=1,ILINE
        DO 150 IS=START_1KM,END_1KM

          IMASK2=2*IS -1
          JMASK2=2*IL -1
          ISNOW=0

          DO 440 IL2=JMASK2,2*IL
            DO 450 IS2=IMASK2,2*IS

              IF(SnowMsk_500m(IS2,IL2).EQ.0) THEN
                ISNOW=ISNOW+1
              ENDIF

450         CONTINUE
440       CONTINUE

          SnowMsk_Ratio(IS,IL)=1
          IF(ISNOW.GE.1) THEN
            SnowMsk_Ratio(IS,IL)=0
          ENDIF
c           write(38,*)'sc',IL,IS,SnowMsk_Ratio(IS,IL)
150     CONTINUE
140   CONTINUE
C 2/26/2001
      RETURN
      END
*********************************************************************
      SUBROUTINE LAND_WATER_CLOUD(DET_Flag,CldMsk_1km,CldMsk_500,
     *         CldMsk_250,High_Cloud_Flag,High_Cloud_Flag_500,
     *         LandSea_Flag,SunGlint_Flag,SnowIce_Flag,SnowMsk_Ratio,
     *         Shadow_Flag,
     *         cloud_num,cloud_num_land,Water,Land,Glint,Snowice,
     *         START_1KM,END_1KM,QA_Flag_Land,QA_Flag_Ocean,QA_Temp,
     *         Quality_cirrus, Ret_Quality_cirrus,
     *         Land_CLDMSK_forfraction)

C---------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine counts water, land, cloudy, glint and
C             snow/ice pixels in a 10x10 km area
C
C             (1) If 100% water pixels go to water process,
C                 otherwise goes to land.
C             (2) If 100% cloudy pixels, no aerosol retrieval is done.
C             (3) Glint and snow/ice pixels will be set to as cloudy pixel
C                 to prohibit any retrievals.
C
C!INPUT PARAMETERS:
C
C       CldMsk_1km     1km cloud mask
C       LandSea_Flag   Land_sea Flag from cloud mask
C       START_1KM      Starting Index for 1 km  resolution data
C       END_1KM        Ending Index for 1 km  resolution data
C
C!OUTPUT PARAMETERS:
C
C        Cloud_num     Number of cloudy pixels
C        Water         Number of pixels which identify water
C        Land          Number of pixels which identify land
C
c
C!REVISION HISTORY:
c 10/20/1999 fhliang
c fixed prolog.
C!TEAM-UNIQUE HEADER:
C
C This software was developed by the MODIS Atmosphere Science Team
C for the National Aeronautics and Space Administration at
C Goddard Space Flight Center.
C
C!END
C----------------------------------------------------------------------

      IMPLICIT NONE
      SAVE

      INCLUDE 'mod04.inc'

      INTEGER Water,Land,Desert,Glint,Snowice,cloud_num,cloud_num_land,
     &        IYY,IXX,I,J,START_1KM,END_1KM,Det_cldmsk,Y4_offset,X4_offset,
     &        Y2_offset,X2_offset,Y4,X4,Y2,X2,l,p,l11,p11,N,coast
      INTEGER LandSea_Flag(ISWATH,ILINE),CldMsk_1km(ISWATH,ILINE),
     &        CldMsk_250(4*ISWATH,4*ILINE),CldMsk_500(2*ISWATH,2*ILINE),
     &        SunGlint_Flag(ISWATH,ILINE),SnowIce_Flag(ISWATH,ILINE),
     &        SnowMsk_Ratio(ISWATH,ILINE),
     &        DET_Flag(ISWATH,ILINE),Shadow_Flag(ISWATH,ILINE),
     &        High_Cloud_Flag(ISWATH,ILINE),
     &        High_Cloud_Flag_500(2*ISWATH,2*ILINE)
      Integer Land_CLDMSK_forfraction(ISWATH,ILINE)
      INTEGER QA_Flag_Land(19),QA_Flag_Ocean(12)
      integer Ret_Quality_cirrus,Quality_cirrus(ISWATH,ILINE)
      BYTE QA_Temp
      Ret_Quality_cirrus=1
      QA_Temp=0
      Det_cldmsk=0
      cloud_num=0
      cloud_num_land=0
      Water=0
      Land=0
      Desert=0
      Glint=0
      Snowice=0
C cloud_num and cloud_num _land after cloud mask at 1km  before ice,snow flag 
         DO IYY = 1,IGRIDY
          DO IXX=START_1KM,END_1KM
            IF(CldMsk_1km(IXX,IYY).EQ.0) THEN
              cloud_num=cloud_num+1
            ENDIF

            IF(Land_CLDMSK_forfraction(IXX,IYY).EQ.0)then
            cloud_num_land =cloud_num_land +1
            ENDIF
       ENDDO
          ENDDO



C
C Calculating number of land, water, cloudy, sun-glint and snow/ice
C pixels
C

        DO IYY = 1,IGRIDY
          DO IXX=START_1KM,END_1KM

            IF(DET_Flag(IXX,IYY).EQ.1) THEN
              Det_cldmsk=Det_cldmsk+1
            ENDIF

            IF(SunGlint_Flag(IXX,IYY).EQ.0) THEN
              Glint=Glint+1
            ENDIF

            IF(SnowIce_Flag(IXX,IYY).EQ.0) THEN
              Snowice=Snowice+1
            ENDIF

       IF(DET_Flag(IXX,IYY) .eq.1 .and.LandSea_Flag(IXX,IYY).EQ.0 )THEN
              Water=Water+1
           ELSE IF((DET_Flag(IXX,IYY) .eq.1.and. LandSea_Flag(IXX,IYY).EQ.3)
     *  .or.(DET_Flag(IXX,IYY) .eq.1 .And.LandSea_Flag(IXX,IYY).EQ. 2 ))THEN
               Land=Land+1
             ENDIF
           ENDDO
         ENDDO
C
C Glint, snow/ice and shadow pixels are treated as cloudy pixels to
C exclude them in the process
C

        DO IYY = 1,IGRIDY
          DO IXX = START_1KM,END_1KM

C
C High cloud flag for ocean in 500 m resolution
C

            Y2_offset = (IYY-1)*2
            X2_offset = (IXX-1)*2
            DO l = 1,2
            DO p = 1,2
              Y2 = Y2_offset + l
              X2 = X2_offset + p
              High_Cloud_Flag_500(X2,Y2) = 1
            ENDDO
            ENDDO
            IF(High_Cloud_Flag(IXX,IYY).EQ.0) THEN
              DO l = 1,2
              DO p = 1,2
                Y2 = Y2_offset + l
                X2 = X2_offset + p
                High_Cloud_Flag_500(X2,Y2) = 0
              ENDDO
              ENDDO
            ENDIF

Change 8/6/1999     IF(SunGlint_Flag(IXX,IYY).EQ.0.OR.
Change 2/21/2001 5x5 window neighboring pixel screening

           IF(SnowIce_Flag(IXX,IYY).EQ.0.OR.
     &         SnowMsk_Ratio(IXX,IYY).EQ.0.OR.
     &        Shadow_Flag(IXX,IYY).EQ.0) THEN

              IF(IYY.GT.2.AND.IYY.LT.IGRIDY-1) THEN
                IF(IXX.GT.START_1KM+1.AND.IXX.LT.END_1KM-1) THEN
                  DO l11=IYY-2,IYY+2
                  DO p11=IXX-2,IXX+2
                    CldMsk_1km(p11,l11) = 0
                  ENDDO
                  ENDDO
                ENDIF
              ENDIF
           ENDIF

c  Enddo for 10 * 10   box.
          ENDDO
        ENDDO

        DO IYY = 1,IGRIDY
          DO IXX=START_1KM,END_1KM

          IF(CldMsk_1km(IXX,IYY).EQ.0) THEN

             Y4_offset = (IYY-1)*4
             X4_offset = (IXX-1)*4

             DO l = 1,4
             DO p = 1,4
               Y4 = Y4_offset + l
               X4 = X4_offset + p
               CldMsk_250(X4,Y4) = 0
             ENDDO
             ENDDO

             Y2_offset = (IYY-1)*2
             X2_offset = (IXX-1)*2

             DO l = 1,2
             DO p = 1,2
               Y2 = Y2_offset + l
               X2 = X2_offset + p
               CldMsk_500(X2,Y2) = 0
             ENDDO
             ENDDO

          ENDIF

          ENDDO
        ENDDO

c   Setting quality Flag to retrive with lower quality for cirrus or smoke?
       DO IYY = 1,IGRIDY
          DO IXX=START_1KM,END_1KM

             Y4_offset = (IYY-1)*4
             X4_offset = (IXX-1)*4
             N=0
             DO l = 1,4
             DO p = 1,4
               Y4 = Y4_offset + l
               X4 = X4_offset + p
               IF(quality_cirrus(IXX,IYY).EQ.1 .and.
     *      CldMsk_250(X4,Y4) .eq.1) N=N+1
             ENDDO
             ENDDO
            IF(N .EQ. 16) Ret_Quality_cirrus=0
       ENDDO
             ENDDO

Change 2/21/2001
C
C Set QA array values; QA_Land(3)=1 set for daytime only, which is
C checked by the solar zenith angle in MOD_PR04_PR05_V2.f
C

        DO I=1,19
          QA_Flag_Land(I)=0
        ENDDO

        IF(cloud_num.EQ.100) THEN
          QA_Flag_Land(1)=1
        ELSE
          QA_Flag_Land(1)=0
        ENDIF
c Setting 2nd bit of ist byte all to Zero. Since Wisconsin group cloud mask is not used
C this bit is not valid

         if( cloud_num .gt.0 .and. cloud_num .le.30)QA_Flag_Land(2)=0
         if( cloud_num .gt.31 .and. cloud_num .le.60)QA_Flag_Land(2)=1
         if( cloud_num .gt.61 .and. cloud_num .le.90)QA_Flag_Land(2)=2
         if( cloud_num .ge.91)QA_Flag_Land(2)=3

         QA_Flag_Ocean(7)=0

        IF(Snowice.GE.90) THEN
          QA_Flag_Land(3)=0
        ELSE
          QA_Flag_Land(3)=1
        ENDIF

        IF(Water.GE.90) THEN
          QA_Flag_Land(4)=0
        ELSE IF(Desert.EQ.100) THEN
          QA_Flag_Land(4)=2
        ELSE IF(Desert.NE.100.AND.Land.EQ.100) THEN
          QA_Flag_Land(4)=3
        ELSE
          QA_Flag_Land(4)=1
        ENDIF

        IF(Water.GE.100) THEN
          QA_Flag_Ocean(8)=1
        ELSE
          QA_Flag_Ocean(8)=0
        ENDIF

C
C Set Cloud Mask QA bit to QA_Temp array
C

      CALL BYTE_SET(QA_Flag_Land(1),0,QA_Temp)
      CALL BYTE_SET(QA_Flag_Land(2),1,QA_Temp)
      CALL BYTE_SET(QA_Flag_Land(3),3,QA_Temp)
      CALL BYTE_SET(QA_Flag_Land(4),4,QA_Temp)

      RETURN
      END

 
C****************************************************************
      SUBROUTINE BYTE_SET(QA_V,Bit_SP,QA_Byte)
      IMPLICIT NONE
      SAVE
C-------------------------------------------------------------------
C
C !F77
C
C !DESCRIPTION:
C                 This subroutine is to set QA bit given nth byte
C                 Note: the QA byte setting starts from the rightmost
C                       (not the leftmost) bit
C
C        1st word       7th  6th  5th  4th  3rd  2nd  1st  0th  (bit)
C        2nd word       7th  6th  5th  4th  3rd  2nd  1st  0th  (bit)
C        3rd word       7th  6th  5th  4th  3rd  2nd  1st  0th  (bit)
C        4th word       7th  6th  5th  4th  3rd  2nd  1st  0th  (bit)
C        5th word       7th  6th  5th  4th  3rd  2nd  1st  0th  (bit)
C
C !INPUT PARAMETERS:
C
C        QA_V       QA parameter value
C        Bit_SP     Bit starting position of Ith QA parameter
C                    (see MODIS atmosphere QA plan)
C
C !OUTPUT PARAMETERS:
C
C        QA_Byte    Byte set for quality control
C
C !REVISION HISTORY:
C
C        WRITTEN BY
C        Dr. Allen Chu          11/25/97
C        Code 913
C        NASA Goddard Space Flight Center
C        Greenbelt, MD 20771
C
C !TEAM-UNIQUE HEADER:
C
C   This software was developed by the MODIS Atmosphere Science Team
C   for the National Aeronautics and Space Administration at
C   Goddard Space Flight Center.
C
C !END
C--------------------------------------------------------------------
C
      Intrinsic ibset,ibclr
      INTEGER QA_V,Bit_SP,Byte_Temp
      BYTE QA_Byte
      Byte_Temp=QA_Byte

      IF(QA_V.EQ.0) THEN

        Byte_Temp = ibclr(Byte_Temp,Bit_SP)

      ELSE IF(QA_V.EQ.1) THEN

        Byte_Temp = ibset(Byte_Temp,Bit_SP)

      ELSE IF(QA_V.EQ.2) THEN

        Byte_Temp = ibclr(Byte_Temp,Bit_SP)
        Byte_Temp = ibset(Byte_Temp,Bit_SP+1)

      ELSE IF(QA_V.EQ.3) THEN

        Byte_temp = ibset(Byte_Temp,Bit_SP)
        Byte_temp = ibset(Byte_Temp,Bit_SP+1)

      ELSE IF(QA_V.EQ.4) THEN

        Byte_Temp = ibclr(Byte_Temp,Bit_SP)
        Byte_Temp = ibclr(Byte_Temp,Bit_SP+1)
        Byte_temp = ibset(Byte_Temp,Bit_SP+2)

      ELSE IF(QA_V.EQ.5) THEN

        Byte_Temp = ibset(Byte_Temp,Bit_SP)
        Byte_Temp = ibclr(Byte_Temp,Bit_SP+1)
        Byte_temp = ibset(Byte_Temp,Bit_SP+2)

      ELSE IF(QA_V.EQ.6) THEN

        Byte_Temp = ibclr(Byte_Temp,Bit_SP)
        Byte_Temp = ibset(Byte_Temp,Bit_SP+1)
        Byte_temp = ibset(Byte_Temp,Bit_SP+2)

      ELSE IF(QA_V.EQ.7) THEN

        Byte_Temp = ibset(Byte_Temp,Bit_SP)
        Byte_Temp = ibset(Byte_Temp,Bit_SP+1)
        Byte_temp = ibset(Byte_Temp,Bit_SP+2)

      ELSE IF(QA_V.EQ.8) THEN

        Byte_Temp = ibclr(Byte_Temp,Bit_SP)
        Byte_Temp = ibclr(Byte_Temp,Bit_SP+1)
        Byte_temp = ibclr(Byte_Temp,Bit_SP+2)
        Byte_temp = ibset(Byte_Temp,Bit_SP+3)

      ELSE IF(QA_V.EQ.9) THEN

        Byte_Temp = ibset(Byte_Temp,Bit_SP)
        Byte_Temp = ibclr(Byte_Temp,Bit_SP+1)
        Byte_temp = ibclr(Byte_Temp,Bit_SP+2)
        Byte_temp = ibset(Byte_Temp,Bit_SP+3)

      ELSE IF(QA_V.EQ.10) THEN

        Byte_Temp = ibclr(Byte_Temp,Bit_SP)
        Byte_Temp = ibset(Byte_Temp,Bit_SP+1)
        Byte_temp = ibclr(Byte_Temp,Bit_SP+2)
        Byte_temp = ibset(Byte_Temp,Bit_SP+3)

      ELSE IF(QA_V.EQ.11) THEN

        Byte_Temp = ibset(Byte_Temp,Bit_SP)
        Byte_Temp = ibset(Byte_Temp,Bit_SP+1)
        Byte_temp = ibclr(Byte_Temp,Bit_SP+2)
        Byte_temp = ibset(Byte_Temp,Bit_SP+3)

      ELSE IF(QA_V.EQ.12) THEN

        Byte_Temp = ibclr(Byte_Temp,Bit_SP)
        Byte_Temp = ibclr(Byte_Temp,Bit_SP+1)
        Byte_temp = ibset(Byte_Temp,Bit_SP+2)
        Byte_temp = ibset(Byte_Temp,Bit_SP+3)

      ELSE IF(QA_V.EQ.13) THEN

        Byte_Temp = ibset(Byte_Temp,Bit_SP)
        Byte_Temp = ibclr(Byte_Temp,Bit_SP+1)
        Byte_temp = ibset(Byte_Temp,Bit_SP+2)
        Byte_temp = ibset(Byte_Temp,Bit_SP+3)

      ELSE IF(QA_V.EQ.14) THEN

        Byte_Temp = ibclr(Byte_Temp,Bit_SP)
        Byte_Temp = ibset(Byte_Temp,Bit_SP+1)
        Byte_temp = ibset(Byte_Temp,Bit_SP+2)
        Byte_temp = ibset(Byte_Temp,Bit_SP+3)

      ELSE IF(QA_V.EQ.15) THEN

        Byte_Temp = ibset(Byte_Temp,Bit_SP)
        Byte_Temp = ibset(Byte_Temp,Bit_SP+1)
        Byte_temp = ibset(Byte_Temp,Bit_SP+2)
        Byte_temp = ibset(Byte_Temp,Bit_SP+3)

      ENDIF

      QA_Byte=Byte_Temp

      RETURN
      END

C*********************************************************************
      SUBROUTINE POPULATE_COMMON_ARRAYS(IDATA,Lat_center,Lon_center,
     &         MTHET0,MTHET,MPHI0,MPHI,MSCATT,QA_Temp,SDSLAT,
     &         SDSLON,SDS_MTHET0,SDS_MTHET,SDS_MPHI0,SDS_MPHI,
     &         SDS_Scattering_Angle,SDS_CldMskQA)
C
C-----------------------------------------------------------------------
c!F77
C
c!Description:
C This subroutine populates common arrays such as latitude,
C longitude, and the angles of sun, satellite and the relative
C azimuth
c!INPUT PARAMETERS:
C
c        Lat_center    Latitude  at center of 10*10 box
C        Lon_center    Londitude at center of 10*10 box
c           MTHET0     Solar Zenith Angle at center of 10*10 box
C            MTHET     View  Angle at center of 10*10 box
C            MPHI0     Satellite Azimuth  at center of 10*10 box
C             MPHI     Solar Azimuth  at center of 10*10 box
C            MDPHI     Diff Satellite Azimuth  and Solar Azimuth
C
c!OUTPUT PARAMETERS:
C        all SDS* for all above variables for HDF write.
C
c!Revision History:
C
C
c!Team-unique Header:
C
C
c!End
C----------------------------------------------------------------------

      IMPLICIT  NONE
      SAVE

      INCLUDE 'mod04.inc'

      INTEGER IDATA
      REAL Lat_center,Lon_center,MTHET0,MTHET,MPHI0,MPHI,MSCATT
      REAL SDSLAT(NUMCELLS),SDSLON(NUMCELLS)
      INTEGER*2 SDS_MTHET0(NUMCELLS),SDS_MTHET(NUMCELLS),
     &          SDS_MPHI0(NUMCELLS),SDS_MPHI(NUMCELLS),
     &          SDS_Scattering_Angle(NUMCELLS)
      BYTE      SDS_CldMskQA(NUMCELLS),QA_Temp



      SDSLAT(IDATA) = LAT_center*SCALE1+OFFSET1
      SDSLON(IDATA) = Lon_center*SCALE1+OFFSET1
      SDS_MTHET0(IDATA) = MTHET0*SCALE2+OFFSET1
      SDS_MTHET(IDATA) =  MTHET*SCALE2+OFFSET1
      SDS_MPHI0(IDATA) = MPHI0*SCALE2+OFFSET1
      SDS_MPHI(IDATA) = MPHI*SCALE2+OFFSET1
      SDS_Scattering_Angle(IDATA)=MSCATT*SCALE2+OFFSET1
      SDS_CldMskQA(IDATA) = QA_Temp*SCALE1+OFFSET1

      RETURN
      END

***********************************************************************
       SUBROUTINE  COMPUTE_GLINTANGLE(MTHET0,MTHET,MDPHI,
     *             GLINT_ANGLE,QA_Flag_Ocean)
C
C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine computes Glint_angle
C
C
C!INPUT PARAMETERS:
C
C           MTHET0     Solar Zenith Angle at center of a 10*10 km^2 box
C            MTHET     View  Angle at center of a 10*10 km^2 box
C            MDPHI     Difference of Satellite Azimuth and Solar Azimuth
C                      Angle of a 10*10 km^2 box
C
C!OUTPUT PARAMETERS:
C
C      GLINT_ANGLE     Glint_angle
C    QA_Flag_Ocean     Runtime QA Flags for Ocean
C
C !REVISION HISTORY:
C
C        WRITTEN BY
C        Shana Mattoo
C        Code 913
C        NASA Goddard Space Flight Center
C        Greenbelt, MD 20771
C
C !TEAM-UNIQUE HEADER:
C
C   This software was developed by the MODIS Atmosphere Science Team
C   for the National Aeronautics and Space Administration at
C   Goddard Space Flight Center.
C
C !END
C
C----------------------------------------------------------------------

       IMPLICIT NONE
       SAVE

       INCLUDE 'mod04.inc'

       REAL GLINT_ANGLE, MTHET0,MTHET,MDPHI
       INTEGER QA_Flag_Ocean(12)

      GLINT_ANGLE = 0.0
        IF(MTHET0.GT.0.0.AND.MTHET.GT.0.0.AND.MDPHI.GT.0.0) THEN
         GLINT_ANGLE=(COS(MTHET0*DTR))*(COS(MTHET*DTR))
     *            +((SIN(MTHET0*DTR))*(SIN(MTHET*DTR))
     *            *( COS(MDPHI*DTR)))
         GLINT_ANGLE = (ACOS(GLINT_ANGLE))*RTD
        ENDIF

       IF(GLINT_ANGLE .GE.GLINT_THRESHOLD) then
         QA_Flag_Ocean(9)=1
       ELSE
         QA_Flag_Ocean(9)=0
       ENDIF

       RETURN
       END

C**************************************************************************
       SUBROUTINE POPULATE_TAU_LAND_OCEAN(IDATA,SDS_Tau_Land_Ocean_img,
     *     SDSTAU_array,NX,NY)
C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION: This subroutine populates optical depth at 0.5 micron
C              for both land and ocean
C
C
C!INPUT PARAMETERS:
C
C              IDATA     index of grid box
C       SDSTAU_ARRAY     Array of optical depth at 0.5 micron for input
C
C!OUTPUT PARAMETERS:
C
C SDS_Tau_Land_Ocean    Common Array of optical depth at 0.5 micron for
C                       both land and ocean
C
C !REVISION HISTORY:
C
C        WRITTEN BY
C        Shana Mattoo
C        Code 913
C        NASA Goddard Space Flight Center
C        Greenbelt, MD 20771
C
C !TEAM-UNIQUE HEADER:
C
C   This software was developed by the MODIS Atmosphere Science Team
C   for the National Aeronautics and Space Administration at
C   Goddard Space Flight Center.
C
C !END
C
C----------------------------------------------------------------------

       IMPLICIT NONE
       SAVE

C       INCLUDE 'mod04.inc'

       INTEGER IDATA,NX,NY
       INTEGER*2 SDSTAU_array(NX,NY),SDS_Tau_Land_Ocean_img(NX)

       SDS_Tau_Land_Ocean_img(IDATA)= SDSTAU_array(IDATA,2)

       RETURN
       END

       SUBROUTINE CldMsk_Land(Data_Size,ISWATH,ILINE,Refl_3,refl_5
     * ,Refl_26,CldMsk_250,CldMsk_500,CldMsk_1km, RMED,RMEDSQ, 
     &RMED_1km,RMEDSQ_1km,iscan,nrows,quality_cirrus,
     & Land_CLDMSK_forfraction)
C----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:   
C               Generate cloud mask over land using spatial variability
C               of 0.47 (>0.01) and 1.38 um (> 0.007) reflectance as well 
C               as absolute  value of 1.38 um > 0.1
C
C!INPUT PARAMETERS:
C
C             ISWATH      Number of pixels at 1 km resolution along scan
C              ILINE      Number of pixels at 1 km resolution against scan
C             Refl_3      Reflectance at 0.47 um
C            Refl_26      Reflectance at 1.38 um
C
C!OUTPUT PARAMETERS:
C
C         CLDMSK_1KM      Cloud mask at 1 km resolution
C         CldMsk_500      Cloud mask at 500m resolution
C         CldMsk_250      Cloud mask at 250m resolution
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
C
      IMPLICIT  NONE
      SAVE
      INTEGER ISTART,IEND,ISWATH,ILINE,Data_Size(2)
      INTEGER IX,IY,l,p,l2,p2,ll,pp,ll2,pp2,N,iscan,NROWS(ISWATH*2)
      INTEGER X2_offset,Y2_offset,X4_offset,Y4_offset
      Integer Land_CLDMSK_forfraction(ISWATH,ILINE)
      REAL Refl_3(ISWATH*2,ILINE*2),Refl_26(ISWATH,ILINE)
      real Refl_5(ISWATH*2,ILINE*2) 
      REAL THRHLD1380_1,THRHLD1380_2
      REAL cloud_threhold_land47_1,cloud_threhold_land47_2
      REAL RMED(ISWATH*2),RMEDSQ(ISWATH*2)
      REAL RMED_1km(ISWATH),RMEDSQ_1km(ISWATH)
      INTEGER CldMsk_250(ISWATH*4,ILINE*4),CldMsk_500(ISWATH*2,ILINE*2),
     *        CldMsk_1km(ISWATH,ILINE),quality_cirrus(ISWATH,ILINE)
c         real  Real_CldMsk_500(ISWATH*2,ILINE*2)
       DATA cloud_threhold_land47_1/0.0025/,cloud_threhold_land47_2/0.4/
c        DATA THRHLD1380_1/0.007/,THRHLD1380_2/0.01/
c       DATA cloud_threhold_land47_1/0.01/,cloud_threhold_land47_2/0.4/
        DATA THRHLD1380_1/0.003/,THRHLD1380_2/0.01/


C
C Initialize cloud mask arrays (cloudy)
C
      
      DO IY=1,Data_Size(2)*4
        DO  IX=1,Data_Size(1)*4
          CldMsk_250(IX,IY)=0
        ENDDO
      ENDDO

      DO IY=1,Data_Size(2)*2
        DO  IX=1,Data_Size(1)*2
          CldMsk_500(IX,IY)=0
        ENDDO
      ENDDO

      DO IY=1,Data_Size(2)
        DO  IX=1,Data_Size(1)
          CldMsk_1km(IX,IY)=0
           Quality_cirrus(IX,IY)=0
         Land_CLDMSK_forfraction(IX,IY)=0
        ENDDO
      ENDDO

C
C  All pixels clear(option 1 of cloud mask) ?????
C
C
C      RMED(1)=0
C      RMEDSQ(1)=0

C
C Cloud mask based upon spatial variability of 0.47 micron 500 m
C resolution reflectance data
C

 
         CALL CldMsk_3by3_HKM(ISWATH,ILINE,Refl_3,
     &cloud_threhold_land47_1,NROWS,RMED,RMEDSQ,CldMsk_500,data_size)
 
c reflactance test for 0.47 um
        DO IY=1,Data_Size(2)*2
         DO IX=1,Data_Size(1)*2
      if(Refl_3(ix,iy) .gt.cloud_threhold_land47_2)CldMsk_500(IX,IY)=0
         Enddo
       Enddo

      CALL CldMsk_3by3_1KM(Data_Size,ISWATH,ILINE,Refl_26,THRHLD1380_1,
     *THRHLD1380_2,CldMsk_1km,RMED_1km,RMEDSQ_1km,refl_5,quality_cirrus,
     * Land_CLDMSK_forfraction)
      DO IY=1,Data_Size(2)
       DO IX=1,Data_Size(1)
          Y2_offset = (IY-1)*2
          X2_offset = (IX-1)*2 
          DO l = 1,2
          DO p = 1,2 
            l2 = Y2_offset + l
            p2 = X2_offset + p
           IF(CldMsk_1km(IX,IY).EQ.1 .and.CldMsk_500(p2,l2).EQ.1)THEN
            CldMsk_500(p2,l2)=1
           ELSE
            CldMsk_500(p2,l2)=0
            ENDIF
          ENDDO
          ENDDO
          Y4_offset = (IY-1)*4
          X4_offset = (IX-1)*4
          N=0
          DO l = 1,2
          DO p = 1,2
            l2 = Y2_offset + l
            p2 = X2_offset + p
            IF(CldMsk_500(p2,l2).EQ.1) THEN
            N=N+1
            ENDIF
          ENDDO
          ENDDO

C
C  N=4 to overwrite 1.38 micro channel cloud screening results
C

          IF(N.EQ.4) THEN 

            DO ll = 1,4
            DO pp = 1,4

              ll2 = Y4_offset + ll
              pp2 = X4_offset + pp

              CldMsk_250(pp2,ll2) = 1

            ENDDO
            ENDDO
  
          ENDIF 

      ENDDO
      ENDDO
      
      RETURN
      END
C***************************************************************************
     
C***************************************************************************
        SUBROUTINE CldMsk_3by3_HKM( ISWATH,ILINE,REF1KM,THRHLD,
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

c            IF(REF1KM(JX,JY).GT.0.0.AND.REF1KM(JX+1,JY).GT.0.0.AND.REF1KM(JX-1,JY).GT.0.0) THEN
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
c New definitation.....
                    STD=STD* (RMED(JX)/3.0)
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
C***************************************************************************
      SUBROUTINE CldMsk_3by3_1KM(Data_Size,ISWATH,ILINE,REF1KM,THRHLD1,
     *THRHLD2,CLDMSK,RMED,RMEDSQ,refl_5, Quality_cirrus, 
     *Land_CLDMSK_forfraction)
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
      INTEGER IY,JY,NY,JX,ISWATH,ILINE,Data_Size(2),ISTART,IEND
      INTEGER CLDMSK(ISWATH,ILINE), Quality_cirrus(ISWATH,ILINE)
      integer Land_CLDMSK_forfraction(ISWATH,ILINE)
      REAL REF1KM(ISWATH,ILINE),THRHLD1,THRHLD2
      REAL RMED(ISWATH),RMEDSQ(ISWATH),STD,VAR
      INTEGER Y2_offset,X2_offset,l2,p2,l,p
      Real Refl_5(ISWATH*2,ILINE*2)
      SAVE

C
C Checking for 1.38 micron spatial variability
C

      NY=0

      DO IY=2,Data_Size(2)-1
        ISTART=IY-1
        IEND=ISTART+2

        DO JY=ISTART,IEND
          NY=NY+1 

          DO JX=2,Data_Size(1)-1

c            IF(REF1KM(JX,JY).GT.0.0.AND.REF1KM(JX+1,JY).GT.0.0.AND.REF1KM(JX-1,JY).GT.0.0) THEN

              RMED(JX)=RMED(JX)+(REF1KM(JX,JY)+REF1KM(JX+1,JY)+REF1KM(JX-1,JY))
              RMEDSQ(JX)=RMEDSQ(JX)+(REF1KM(JX,JY)*REF1KM(JX,JY)
     1                             + REF1KM(JX+1,JY)*REF1KM(JX+1,JY)
     2                             + REF1KM(JX-1,JY)*REF1KM(JX-1,JY))

              IF(NY.EQ.3) THEN

                VAR=9.0/8.0*(RMEDSQ(JX)/9.0-RMED(JX)*RMED(JX)/81.0)
                IF(VAR.GT.0.0) THEN
                  STD=SQRT(VAR) 
                  
                     IF(STD.LT.THRHLD1) THEN
                    CLDMSK(JX,IY)=1
                  ENDIF
                ENDIF

                RMED(JX)=0.0
                RMEDSQ(JX)=0.0

               ENDIF

c            ENDIF

          ENDDO
        ENDDO

        NY=0

      ENDDO 

      DO JX=1,Data_Size(1)
        CLDMSK(JX,1)=CLDMSK(JX,2)
        CLDMSK(JX,Data_size(2))=CLDMSK(JX,Data_Size(2)-1)
      ENDDO
   
      DO JY=1,Data_Size(2)
        CLDMSK(1,JY)=CLDMSK(2,JY)
        CLDMSK(Data_Size(1),JY)=CLDMSK(Data_Size(1)-1,JY)
      ENDDO

C
C Checking for 1.38 micron reflectance
C
  
      DO JY=1,Data_Size(2)
        DO JX=1,Data_Size(1) 
       Land_CLDMSK_forfraction(JX,JY)=CLDMSK(JX,JY)
      IF(REF1KM(JX,JY).gt.0.025) then
             CLDMSK(JX,JY)=0
c save for cloud fraction
       Land_CLDMSK_forfraction(JX,JY)=CLDMSK(JX,JY)
      Else
c if Variabily cloud mask says cloud free and smoke or cirrus reduce quality
         IF(CLDMSK(JX,JY) .eq.1 .and.REF1KM(JX,JY).gt. 0.01 
     *    .and.  REF1KM(JX,JY) .le.0.025 ) then
c  cloud free  may be cirrus  or smoke , will be retrived but put a quality value  
              Quality_cirrus(JX,JY)=1  
      
       ENDIF
cENDIF   cloud free  may be cirrus  or smoke 
       ENDIF   
      
        ENDDO
      ENDDO

      RETURN
      END
