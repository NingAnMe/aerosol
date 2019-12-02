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
C
        integer function get_mod04_data(NUMSQ,l1b_1km_lun,l1b_hkm_lun,
     +     l1b_qkm_lun,geo_1km_lun,mask_lun,mask_qa_lun,
     +     datatype_1km,datat_mask,interleave_1km,interl_mask,
     +     resolution_1km,resol_mask,offset_1km,offset_mask,
     +     samples_1km,sampl_mask,lines_1km,lines_mask,error_1km,
     +     bands_1km,bands_mask,bandnames_1km,bnames_mask,
     +     bandunits_1km,cube,Buf_Size1,Buf_Size2,Data_Size,rlat,
     +     rlon,view_zen,sens_azim,solar_zen,
     +     solar_azim,rel_azim,elev,Refl_1,Refl_2,Refl_3,Refl_4,
     +     Refl_5,Refl_6,Refl_7,Refl_9,Refl_12,Refl_13,Refl_16,
     +     Refl_26,Rad_20,Rad_31,Rad_32,CldMsk_250,CldMsk_500,
     +     CldMsk_1km,DET_Flag,UFQ_Flag,DayNight_Flag,SunGlint_Flag,
     +     SnowIce_Flag,LandSea_Flag,Non_CloudOb_Flag,Thin_CirNIR_Flag,
     +     Shadow_Flag,Thin_CirIR_Flag,Cloud_SimpIR_Flag,
     +     High_Cloud_Flag,Cloud_IRTemp_Flag,Cloud_3p75_11_Flag,
     +     Cloud_VisRat_Flag,Cloud_SpatVar_Flag,Cloud,QA_CLOUD)

      implicit none
      save

C-----------------------------------------------------------------------
C !F77
C
C !PURPOSE:
C    Extract the information needed to begin processing the
C     data for the MODIS Aerosol (MOD04) Direct Broadcast algorithm.
C
C !DESCRIPTION:
C    Extracts information from the Geolocation, 
C    L1b files, Cloud Mask  and ancillary data files.
C
C !INPUT PARAMETERS:
C    l1b_1km_lun        LUN for open 1km L1B data file
C    l1b_hkm_lun        LUN for open half km L1B data file
C    l1b_qkm_lun        LUN for open quarter km L1B data file
C    geo_1km_lun        LUN for open geo file
C    mask_lun           LUN for open Cloud Mask file
C    mask_qa_lun        LUN for open Cloud Mask QA file
C    datatype_1km    Format of data in L1b file
C    datat_mask      Format of data in cloud mask file
C    interleave_1km  Order data is saved in L1b file
C    interl_mask     Order data is saved in cloud mask file
C    resolution_1km  L1b file data resolution
C    resol_mask      Cloud mask file data resolution
C    offset_1km      Offset, if any, of 1km L1B data
C    offset_mask     Offset, if any, of cloud mask file data
C    samples_1km     Number of elements in 1km L1b file
C    sampl_mask      Number of elements in cloud mask file
C    lines_1km       Number of lines of data in 1km L1b file
C    lines_mask      Number of lines of data in cloud mask file
C    error_1km       Bad data value of data in 1km L1b file
C    bands_1km       Number of bands in 1km L1b file
C    bands_mask      Number of bands in cloud mask file
C    bandnames_1km   Band numbers in 1km L1b file
C    bnames_mask     Band numbers in cloud mask file
C    bandunits_1km   Units for each band in the 1km L1b data file
C    cube            Scan cube number
C    Buf_Size(1)     Buffer size for first array index
C    Buf_Size(2)     Buffer size for second array index
C
C !OUTPUT PARAMETERS:
C    Data_Size       Size of returning data set arrays (1 km fields)
C    rlat            Array containing scan cube of 1km pixel latitude
C                    values
C    rlon            Array containing scan cube of 1km pixel longitude
C                    values
C    view_zen        Array containing scan cube of 1km pixel viewing
C                    zenith angles
C    solar_zen       Array containing scan cube of 1km pixel solar
C                    zenith angles
C    view_azim       Array containing scan cube of 1km pixel viewing
C                    azimuth angles
C    solar_azim      Array containing scan cube of 1km pixel solar
C                    azimuth angles
C    rel_azim        Array containing scan cube of 1km pixel relative
C                    azimuth angles
C    elev            Array containing scan cube of 1km pixel elevation
C                    values
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
C    DET_Flag             Cloud Determined Flag
C    UFQ_Flag             Unobstructed FOV Quality Flag
C    DayNight_Flag        Day/Night Flag
C    SunGlint_Flag        SunGlint Flag
C    SnowIce_Flag         Snow/Ice Flag
C    LandSea_Flag         Land/Sea Flag
C    Non_CloudOb_Flag     Non-Cloud obstruction (dust) Flag
C    Thin_CirNIR_Flag     Thin Cirrus detected Flag (Solar)
C    Shadow_Flag          Shadow Flag
C    Thin_CirIR_Flag      Thin Cirrus detected Flag (IR)
C    Cloud_SimpIR_Flag    Cloud Flag-Simple Threshold Test
C    High_Cloud_Flag      High Cloud Flag - 1.38 Micron Test
C    Cloud_IRTemp_Flag    Cloud Flag - IR Temperature Difference
C    Cloud_3p75_11_Flag   Cloud Flag - 3.75-11 Micron Test
C    Cloud_VisRat_Flag    Cloud Flag - Visible Ratio Test
C    Cloud_SpatVar_Flag   Cloud Flag - Spatial Variability
C    Cloud           Array containing scan of cloud mask data
C    QA_Count        Array containing scan of cloud mask QA data
C
C !REVISION HISTORY
C
C !TEAM-UNIQUE HEADER:
C    Developed by the MODIS Group, CIMSS/SSEC, UW-Madison.
C
C !END
C----------------------------------------------------------------------

      include           'mod04.inc'
      include           'db_mod04uw_debug.inc'

c ... array arguments
      character*(*) interleave_1km,interl_mask,
     +              bandnames_1km(*),bnames_mask(*),bandunits_1km(*)

      integer Buf_Size1, Buf_Size2
      real rlat(Buf_Size1,Buf_Size2), rlon(Buf_Size1,Buf_Size2), 
     +     view_zen(Buf_Size1,Buf_Size2),
     +     solar_zen(Buf_Size1,Buf_Size2), elev(Buf_Size1,Buf_Size2), 
     +     sens_azim(Buf_Size1,Buf_Size2), 
     +     solar_azim(Buf_Size1,Buf_Size2), 
     +     rel_azim(Buf_Size1,Buf_Size2),
     +     Refl_1_1km(Buf_Size1,Buf_Size2),
     +     Refl_2_1km(Buf_Size1,Buf_Size2),
     +     Refl_3_1km(Buf_Size1,Buf_Size2),
     +     Refl_4_1km(Buf_Size1,Buf_Size2),
     +     Refl_5_1km(Buf_Size1,Buf_Size2),
     +     Refl_6_1km(Buf_Size1,Buf_Size2),
     +     Refl_7_1km(Buf_Size1,Buf_Size2),
     +     Refl_1(4*Buf_Size1,4*Buf_Size2),
     +     Refl_2(4*Buf_Size1,4*Buf_Size2),
     +     Refl_3(2*Buf_Size1,2*Buf_Size2),
     +     Refl_4(2*Buf_Size1,2*Buf_Size2),
     +     Refl_5(2*Buf_Size1,2*Buf_Size2),
     +     Refl_6(2*Buf_Size1,2*Buf_Size2),
     +     Refl_7(2*Buf_Size1,2*Buf_Size2),
     +     Refl_9(Buf_Size1,Buf_Size2),
     +     Refl_12(Buf_Size1,Buf_Size2),
     +     Refl_13(Buf_Size1,Buf_Size2),
     +     Refl_16(Buf_Size1,Buf_Size2),
     +     Refl_26(Buf_Size1,Buf_Size2),
     +     Rad_20(Buf_Size1,Buf_Size2),
     +     Rad_31(Buf_Size1,Buf_Size2),Rad_32(Buf_Size1,Buf_Size2)
      integer CldMsk_250(4*Buf_Size1,4*Buf_Size2),
     +        CldMsk_500(2*Buf_Size1,2*Buf_Size2),
     +        CldMsk_1km(Buf_Size1,Buf_Size2),
     +        DET_Flag(Buf_Size1,Buf_Size2),
     +        UFQ_Flag(Buf_Size1,Buf_Size2),
     +        DayNight_Flag(Buf_Size1,Buf_Size2),
     +        SunGlint_Flag(Buf_Size1,Buf_Size2),
     +        SnowIce_Flag(Buf_Size1,Buf_Size2),
     +        LandSea_Flag(Buf_Size1,Buf_Size2),
     +        Non_CloudOb_Flag(Buf_Size1,Buf_Size2),
     +        Thin_CirNIR_Flag(Buf_Size1,Buf_Size2),
     +        Shadow_Flag(Buf_Size1,Buf_Size2),
     +        Thin_CirIR_Flag(Buf_Size1,Buf_Size2),
     +        Cloud_SimpIR_Flag(Buf_Size1,Buf_Size2),
     +        High_Cloud_Flag(Buf_Size1,Buf_Size2),
     +        Cloud_IRTemp_Flag(Buf_Size1,Buf_Size2),
     +        Cloud_3p75_11_Flag(Buf_Size1,Buf_Size2),
     +        Cloud_VisRat_Flag(Buf_Size1,Buf_Size2),
     +        Cloud_SpatVar_Flag(Buf_Size1,Buf_Size2),
     +        Data_Size(2)
      byte Cloud(Buf_cldmsk,Buf_Size1,Buf_Size2),
     +     QA_Cloud(Buf_cldmsk_QA,Buf_Size1,Buf_Size2)


c ... scalar arguments
      integer datatype_1km,datat_mask,
     +        resolution_1km,resol_mask,
     +        offset_1km,offset_mask,samples_1km,
     +        sampl_mask,lines_1km,lines_mask,
     +        bands_1km,bands_mask,cube,l1b_1km_lun,
     +        l1b_hkm_lun,l1b_qkm_lun,geo_1km_lun,mask_lun,mask_qa_lun,
     +        NUMSQ

      real error_1km

c ... Local scalars
      character*4       interleave_geo
      character*80      bandnames_geo(20)
      character*80      bandunits_geo(20)

      integer datatype_geo, resolution_geo, offset_geo, samples_geo,
     +        lines_geo, bands_geo, status, ip1, ip2, isp2, iep2,
     +        ip4, isp4, iep4, il1, il2, isl2, iel2, il4, isl4, iel4

      real error_geo, cossza, x_cossza

c ... Parameters
      REAL       SolZen_Threshold
      PARAMETER (SolZen_Threshold=80)
      REAL Rel_Equality_Eps, Zero_Eps
      PARAMETER (Rel_Equality_Eps=0.000001, Zero_Eps=0.000001)

c ... functions
      external mod04_get_angles, mod04_get_L1b, mod04_get_mask
      integer mod04_get_angles, mod04_get_L1b, mod04_get_mask

c ... Get geolocation values for this scan.
c ...  Set up extraction parameters
      datatype_geo = datatype_1km
      interleave_geo = interleave_1km
      resolution_geo = resolution_1km
      error_geo = -999.0
      offset_geo = offset_1km
      samples_geo = samples_1km
      lines_geo = lines_1km
      bands_geo = 8
      bandnames_geo(1) = 'Latitude'
      bandnames_geo(2) = 'Longitude'
      bandnames_geo(3) = 'SensorZenith'
      bandnames_geo(4) = 'SensorAzimuth'
      bandnames_geo(5) = 'SolarZenith'
      bandnames_geo(6) = 'SolarAzimuth'
      bandnames_geo(7) = 'Elevation'
      bandnames_geo(8) = 'LandSea'
      bandunits_geo(1) = 'deg'
      bandunits_geo(2) = 'deg'
      bandunits_geo(3) = 'deg'
      bandunits_geo(4) = 'deg'
      bandunits_geo(5) = 'deg'
      bandunits_geo(6) = 'deg'
      bandunits_geo(7) = 'm  '
      bandunits_geo(8) = '   '

      status = mod04_get_angles(
     +                          cube,
     +                          geo_1km_lun,
     +                          datatype_geo,
     +                          interleave_geo,
     +                          resolution_geo,
     +                          error_geo,
     +                          offset_geo,
     +                          samples_geo,
     +                          lines_geo,
     +                          bands_geo,
     +                          bandnames_geo,
     +                          bandunits_geo,
     +                          rlat,
     +                          rlon,
     +                          view_zen,
     +                          solar_zen,
     +                          sens_azim,
     +                          solar_azim,
     +                          rel_azim,
     +                          elev
     +                         )

       if( status.lt.0 ) then
           call message('get_mod04_data:',
     +                  'FAILED - could not extract geolocation data',
     +                   0, 3 )
           get_mod04_data = -1
       endif
c ...............................................................
c ...  Get radiance/reflectance data
       status = mod04_get_L1b(l1b_1km_lun, l1b_hkm_lun,
     +         l1b_qkm_lun, datatype_1km, interleave_1km,
     +         resolution_1km, offset_1km, samples_1km, lines_1km,
     +         error_1km, bands_1km, bandnames_1km, bandunits_1km,
     +         cube, Buf_Size1, Buf_Size2, Refl_1_1km, Refl_2_1km, Refl_3_1km,
     +         Refl_4_1km, Refl_5_1km, Refl_6_1km, Refl_7_1km, Refl_9, Refl_12,
     +         Refl_13, Refl_16, Refl_26, Rad_20, Rad_31, Rad_32,
     +         NUMSQ, Data_Size)
        write( *, '(''Processing mod04_get_L1b # '',i4)' ) cube
       if( status.lt.0 ) then
           call message('get_mod04_data:',
     +                  'FAILED - could not extract L1B data',
     +                   0, 2 )
           get_mod04_data = -1
       endif
c ..................................................................

C
C----------------------------------------------------------------------
C Normalize sensor radiance data to reflectance units
C----------------------------------------------------------------------
C
C Loop over 1-km lines and pixels in scan cube
C
      Do 30 il1 = 1, Data_Size(2)
      Do 30 ip1 = 1, Data_Size(1)

         isl4 = (il1-1)*4 + 1
         iel4 = il1*4
         isp4 = (ip1-1)*4 + 1
         iep4 = ip1*4

         isl2 = (il1-1)*2 + 1
         iel2 = il1*2
         isp2 = (ip1-1)*2 + 1
         iep2 = ip1*2

         cossza = cos(dtr*solar_zen(ip1,il1))

         If (abs(cossza) .LT. Zero_Eps) Then
            x_cossza = 0.0
         Else
            x_cossza = 1.0/cossza
         End If
C
C Normalize 1-km bands to reflectance units
C
         if ( abs((solar_zen(ip1,il1)-FV_GEO)/FV_GEO)
     &       .LT. Rel_Equality_Eps ) then
            Refl_9(ip1,il1) = FV_L1B
            Refl_12(ip1,il1) = FV_L1B
            Refl_13(ip1,il1) = FV_L1B
            Refl_16(ip1,il1) = FV_L1B
            Refl_26(ip1,il1) = FV_L1B

         else if (abs(cossza) .LT. Zero_Eps) then
            Refl_9(ip1,il1) = FV_L1B
            Refl_12(ip1,il1) = FV_L1B
            Refl_13(ip1,il1) = FV_L1B
            Refl_16(ip1,il1) = FV_L1B
            Refl_26(ip1,il1) = FV_L1B

         else if (solar_zen(ip1,il1) .LT. SolZen_Threshold) then

            if (abs((Refl_9(ip1,il1)-FV_L1B)/FV_L1B)
     &         .GT. Rel_Equality_Eps) then
               Refl_9(ip1,il1) = x_cossza*Refl_9(ip1,il1)
            end if
            if (abs((Refl_12(ip1,il1)-FV_L1B)/FV_L1B)
     &         .GT. Rel_Equality_Eps) then
               Refl_12(ip1,il1) = x_cossza*Refl_12(ip1,il1)
            end if
            if (abs((Refl_13(ip1,il1)-FV_L1B)/FV_L1B)
     &         .GT. Rel_Equality_Eps) then
               Refl_13(ip1,il1) = x_cossza*Refl_13(ip1,il1)
            end if
            if (abs((Refl_16(ip1,il1)-FV_L1B)/FV_L1B)
     &         .GT. Rel_Equality_Eps) then
               Refl_16(ip1,il1) = x_cossza*Refl_16(ip1,il1)
            end if
            if (abs((Refl_26(ip1,il1)-FV_L1B)/FV_L1B)
     &         .GT. Rel_Equality_Eps) then
               Refl_26(ip1,il1) = x_cossza*Refl_26(ip1,il1)
            end if
c
         else
            Refl_9(ip1,il1) = FV_L1B
            Refl_12(ip1,il1) = FV_L1B
            Refl_13(ip1,il1) = FV_L1B
            Refl_16(ip1,il1) = FV_L1B
            Refl_26(ip1,il1) = FV_L1B
         endif

C
C Loop over 500-m lines and pixels between 1-km footprints.
C
         Do 40 il2 = isl2, iel2
         Do 40 ip2 = isp2, iep2

            if ( abs((solar_zen(ip1,il1)-FV_GEO)/FV_GEO)
     &          .LT. Rel_Equality_Eps ) then
               Refl_3(ip2,il2) = FV_L1B
               Refl_4(ip2,il2) = FV_L1B
               Refl_5(ip2,il2) = FV_L1B
               Refl_6(ip2,il2) = FV_L1B
               Refl_7(ip2,il2) = FV_L1B


            else if (abs(cossza) .LT. Zero_Eps) then
               Refl_3(ip2,il2) = FV_L1B
               Refl_4(ip2,il2) = FV_L1B
               Refl_5(ip2,il2) = FV_L1B
               Refl_6(ip2,il2) = FV_L1B
               Refl_7(ip2,il2) = FV_L1B

            else if (solar_zen(ip1,il1) .LT. SolZen_Threshold) then
               if (abs((Refl_3_1km(ip1,il1)-FV_L1B)/FV_L1B)
     &            .GT. Rel_Equality_Eps) then
                  Refl_3(ip2,il2) = x_cossza*Refl_3_1km(ip1,il1)
               end if
               if (abs((Refl_4_1km(ip1,il1)-FV_L1B)/FV_L1B)
     &            .GT. Rel_Equality_Eps) then
                  Refl_4(ip2,il2) = x_cossza*Refl_4_1km(ip1,il1)
               end if
               if (abs((Refl_5_1km(ip1,il1)-FV_L1B)/FV_L1B)
     &            .GT. Rel_Equality_Eps) then
                  Refl_5(ip2,il2) = x_cossza*Refl_5_1km(ip1,il1)
               end if
               if (abs((Refl_6_1km(ip1,il1)-FV_L1B)/FV_L1B)
     &            .GT. Rel_Equality_Eps) then
                  Refl_6(ip2,il2) = x_cossza*Refl_6_1km(ip1,il1)
               end if
               if (abs((Refl_7_1km(ip1,il1)-FV_L1B)/FV_L1B)
     &            .GT. Rel_Equality_Eps) then
                  Refl_7(ip2,il2) = x_cossza*Refl_7_1km(ip1,il1)
               end if
            else
               Refl_3(ip2,il2) = FV_L1B
               Refl_4(ip2,il2) = FV_L1B
               Refl_5(ip2,il2) = FV_L1B
               Refl_6(ip2,il2) = FV_L1B
               Refl_7(ip2,il2) = FV_L1B
            endif
   40    continue
C
C Loop over 250-m lines and pixels between 1-km footprints.
C
         Do 50 il4 = isl4, iel4
         Do 50 ip4 = isp4, iep4
C
C            if ((ip1.eq.1 .and. il1.eq.1) .and. (idebug .eq. 1)) then
C               print *, 'GetModisData before normalization'
C               print *, 'refl_1,refl_2',ip4,il4,refl_1(ip4,il4),
C     2                   refl_2(ip4,il4)
C            end if
C
            if ( abs((solar_zen(ip1,il1)-FV_GEO)/FV_GEO)
     &          .LT. Rel_Equality_Eps ) then
               Refl_1(ip4,il4) = FV_L1B
               Refl_2(ip4,il4) = FV_L1B

            else if (abs(cossza) .LT. Zero_Eps) then
               Refl_1(ip4,il4) = FV_L1B
               Refl_2(ip4,il4) = FV_L1B

            else if (solar_zen(ip1,il1) .LT. SolZen_Threshold) then

               if (abs((Refl_1_1km(ip1,il1)-FV_L1B)/FV_L1B)
     &            .GT. Rel_Equality_Eps) then
                  Refl_1(ip4,il4) = x_cossza*Refl_1_1km(ip1,il1)
               end if

               if (abs((Refl_2_1km(ip1,il1)-FV_L1B)/FV_L1B)
     &            .GT. Rel_Equality_Eps) then
                  Refl_2(ip4,il4) = x_cossza*Refl_2_1km(ip1,il1)
               end if

            else
               Refl_1(ip4,il4) = FV_L1B
               Refl_2(ip4,il4) = FV_L1B
            end if

   50    continue

   30 continue

c ... Get cloud mask data

      status = mod04_get_mask(mask_lun, mask_qa_lun,
     +        datat_mask, interl_mask, resol_mask, offset_mask,
     +        sampl_mask, lines_mask, bands_mask, bnames_mask,
     +        cube, Buf_Size1, Buf_Size2, Buf_cldmsk, Buf_cldmsk_QA,
     +        Cloud, QA_Cloud)
      write( *, '(''Processing mod04_get_mask  # '',i4)' ) cube
       if( status.lt.0 ) then
           call message('get_mod04_data:',
     +                  'FAILED - could not extract cloud mask data',
     +                   0, 2 )
           get_mod04_data = -1
       endif

c ... Get Cloud mask specific flags, 250 m and 500 m mask
c ...  and account for mis-registration
      call db_CldMsk_Info_MOD04(Buf_cldmsk,
     +    Buf_Size1,Buf_Size2,
     +    Cloud,CldMsk_250,CldMsk_500,CldMsk_1km,DET_Flag,
     +    UFQ_Flag,DayNight_Flag,SunGlint_Flag,SnowIce_Flag,
     +    LandSea_Flag,Non_CloudOb_Flag,Thin_CirNIR_Flag,Shadow_Flag,
     +    Thin_CirIR_Flag,Cloud_SimpIR_Flag,High_Cloud_Flag,
     +    Cloud_IRTemp_Flag,Cloud_3p75_11_Flag,Cloud_VisRat_Flag,
     +    Cloud_SpatVar_Flag)
C
C      Return Status flag
      write( *, '(''Processing db_CldMsk_Info_MOD04  # '',i4)' ) cube
       get_mod04_data = 0

      end
