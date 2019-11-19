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
       SUBROUTINE db_CldMsk_Info_MOD04(No_Byte,Fmax,Lmax,
     1   Count,CldMsk_250,CldMsk_500,CldMsk_1km,DET_Flag,
     2   UFQ_Flag,DayNight_Flag,SunGlint_Flag,SnowIce_Flag,LandSea_Flag,
     3   Non_CloudOb_Flag,Thin_CirNIR_Flag,Shadow_Flag,Thin_CirIR_Flag,
     4   Cloud_SimpIR_Flag,High_Cloud_Flag,Cloud_IRTemp_Flag,
     5   Cloud_3p75_11_Flag,Cloud_VisRat_Flag,Cloud_SpatVar_Flag)

C-----------------------------------------------------------------------
C !F77
C
C !DESCRIPTION:
C
C    Extract cloud mask flags values from MODIS Cloud Mask
C    HDF of typical 200 scan cubes (a granule).
C    Cloud mask QA flags are not used at this moment
C
C !INPUT PARAMETERS:
C
C    No_Byte    Number of byte of cloud mask storage (=6)
C    Fmax       Maximum frame number per scan line.
C    Lmax       Maximum line number per scan cube.
C    Count      Cloud mask flags
C
C !OUTPUT PARAMETERS: (All integers)
C
C   C CldMsk(4*x,4*y)      Cloud Mask (250m resolution from 1km resolution)
C   C LandSea_FLag         Land/Sea Flag
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
C
c!REVISION HISTORY:
c 07/29/04 kathy strabala 
c ported from operational DAAC version
c
c!TEAM-UNIQUE HEADER:
c
c Developed by MODIS Aerosol/Solar Water Vapor Retrieval Team
c GSFC, Greenbelt, MD
c
C !DESIGN NOTES:
C
C   Internals:
C
C      Variables:
C        Dim_Size(3)     Array specifying the size of hdf SDS data array.
C        l,l1,l4,l2      Line and pixel counters p,p1,p4,p2
C        l4_offset       250-m line offset to 1-km cell position
C        p4_offset       250-m pixel offset to 1-km cell position
C        l2_offset       500-m line offset to 1-km cell position
C        p2_offset       500-m pixel offset to 1-km cell position
C        count(15000)    A temporary buffer for data of the target array.
C        QA_count(15000) A temporary buffer for data of the target array.
C
C ! WRITTEN By
C
C      Dr. Allen Chu
C      Code 913/SSAI
C      NASA Goddard Space Flight Center
C      Greenbelt, MD 20771

C !END
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
      save

      include 'db_mod04uw_debug.inc'

      INTEGER  I,l,l1,l4,l2,l2_offset,l4_offset,p,p1,p2,p2_offset,p4,
     1         p4_offset,p11,l11,j
      INTEGER  No_Byte,Fmax,Lmax,Dim_Size(3),
     2         IR_cloud
      BYTE    Count(No_byte*Fmax*Lmax)

      INTEGER CldMsk_250(4*Fmax,4*Lmax),CldMsk_500(2*Fmax,2*Lmax),
     1    CldMsk_1km(Fmax,Lmax),DET_Flag(Fmax,Lmax),UFQ_Flag(Fmax,Lmax),
     2    DayNight_Flag(Fmax,Lmax),SunGlint_Flag(Fmax,Lmax),
     3    SnowIce_Flag(Fmax,Lmax),LandSea_Flag(Fmax,Lmax),
     4    Non_CloudOb_Flag(Fmax,Lmax),Thin_CirNIR_Flag(Fmax,Lmax),
     5    Shadow_Flag(Fmax,Lmax),Thin_CirIR_Flag(Fmax,Lmax),
     6    Cloud_SimpIR_Flag(Fmax,Lmax),High_Cloud_Flag(Fmax,Lmax),
     7    Cloud_IRTemp_Flag(Fmax,Lmax),Cloud_3p75_11_Flag(Fmax,Lmax),
     8    Cloud_VisRat_Flag(Fmax,Lmax),Cloud_SpatVar_Flag(Fmax,Lmax)
C     9    Cloud_CO2_Flag(Fmax,Lmax),Cloud_6p7_Flag(Fmax,Lmax)
C
C Initialization
C ... Set dimension sizes
      Dim_Size(2)=Fmax
      Dim_Size(3)=Lmax
C
C Get 1-km Cloud MASK data
C
C-----------------------------------------------------------------------
C Derive cloud mask for 250-m pixels based on the results of the
C visible cloud test.  Also extract Land/Sea flag at 1-km resolution.
C Begin looping over 1-km pixels.
C-----------------------------------------------------------------------

       I = -5

       Do 80 l1 = 1, Dim_Size(3)
       Do 80 p1 = 1, Dim_Size(2)
C       Do 80 l1 = 1, 10
C       Do 80 p1 = 1, 1354
C
C The Cloud Mask consists of 6 separate 1-byte words.  Increment
C memory buffer index by 6 to cycle through 1-km frames in a line
C

          I = I + 6
C
C         Set 250-m line and pixel offsets to corner of current 1-km
C         cell.  Then, loop over the 16 250-m pixels within the 1-km.
C         0-based bit positions 16-31 of the 1-km INTEGER cloud mask
C         value are associated with 250-m sublines (1-4) and samples
C         (1-4) with line varying most rapidly.
C
          l4_offset = (l1-1)*4
          p4_offset = (p1-1)*4
          l2_offset = (l1-1)*2
          p2_offset = (p1-1)*2

          DET_Flag(p1,l1) = ibits(count(I),0,1)
          UFQ_Flag(p1,l1) = ibits(count(I),1,2)
          DayNight_Flag(p1,l1) = ibits(count(I),3,1)
          SunGlint_Flag(p1,l1) = ibits(count(I),4,1)
          SnowIce_Flag(p1,l1) = ibits(count(I),5,1)
          LandSea_Flag(p1,l1) = ibits(count(I),6,2)
          Non_CloudOb_Flag(p1,l1) = 1
          Thin_CirNIR_Flag(p1,l1) = ibits(count(I+1),1,1)
          Shadow_Flag(p1,l1) = 1
c    following four tests are used for ocean to compute High_cloud flag
c     Thin_CirIR_Flag                   Thin cirrus test
c     Cloud_SimpIR_Flag )            co2 threshold test
c     Cloud_3p75_11_Flag           6.7 um test
c     Cloud_IRTemp_Flag             IR temperature difference test
c
          Thin_CirIR_Flag(p1,l1) = ibits(count(I+1),3,1)
          Cloud_SimpIR_Flag(p1,l1) = ibits(count(I+1),6,1)
          Cloud_3p75_11_Flag(p1,l1) = ibits(count(I+1),7,1)
          Cloud_IRTemp_Flag(p1,l1) = ibits(count(I+2),2,1)
          Cloud_VisRat_Flag(p1,l1) = ibits(count(I+2),5,1)
          Cloud_SpatVar_Flag(p1,l1) = ibits(count(I+3),4,1)

C          WRITE(*,*) l1,p1,UFQ_Flag(p1,l1),LandSea_Flag(p1,l1),count(I),I
C          WRITE(*,*) 'Thin_CirIR,Cloud_IRTemp,Cloud_6p7,Cloud_CO2',
C     1  Thin_CirIR_Flag(p1,l1),Cloud_IRTemp_Flag(p1,l1),
C     2  Cloud_SimpIR_Flag(p1,l1),Cloud_3p75_11_Flag(p1,l1)
C
C-----------------------------------------------------------------------
C Examine first byte of cloud mask to determine whether cloud mask
C was even determined.
C (Zero-based bit 0 is 1 for determined, and 0 for not determined)
C
C If cloud mask not determined, set 250-m CldMsk(p4,l4) to 0
C Also set 1-km LandSea_Flag to 0, not processed.
C
C For Version 2, LandSea_Flag may take 4 values:
C [0 (water); 1 (coastal), 2 (desert), 3 (land)]
C-----------------------------------------------------------------------
C
C Default set to clear (in favor of clear pixels)
C 

             CldMsk_1km(p1,l1) = 1
             Cloud_VisRat_Flag(p1,l1)= 1

             Do 90 l = 1,4
             Do 90 p = 1,4
               l4 = l4_offset + l
               p4 = p4_offset + p
               CldMsk_250(p4,l4) = 1
   90        continue

             Do 91 l = 1,2
             Do 91 p = 1,2
               l2 = l2_offset + l
               p2 = p2_offset + p
               CldMsk_500(p2,l2) = 1
   91        continue

C
C High cloud flag for ocean; default set to cloudy if cloud mask is not determined 
C (0: cloudy; 1: clear)
C
c1. identifying the IR cirrus (mainly sensitive to thin cirrus clouds), 
c2.homogeneous thin cirrus and high clouds are the CO2 test
c(using 13.9 m band, sensitive mainly to high clouds in cold regions of the atmosphere), 
c3.the 6.7m test (sensitive to high clouds in cold regions of the atmosphere), 
c 4.and the Delta-IR test, all described in details by Ackerman et al. (1998).

          High_Cloud_Flag(p1,l1)=0
          IF(ibits(count(I),0,1).EQ.1) THEN
            IR_cloud=0
            High_Cloud_Flag(p1,l1)=1
            IF(Thin_CirIR_Flag(p1,l1).EQ.0)IR_cloud=IR_cloud+1
c            IF(Cloud_SimpIR_Flag(p1,l1).EQ.0)IR_cloud=IR_cloud+1
             IF(Cloud_3p75_11_Flag(p1,l1).EQ.0)IR_cloud=IR_cloud+1
            IF(Cloud_IRTemp_Flag(p1,l1).EQ.0)IR_cloud=IR_cloud+1
            IF(IR_cloud.GE.1) High_Cloud_Flag(p1,l1)=0
          ENDIF

C
C Using Wisonsin University Cloud Mask over land 
C

          If (ibits(count(I),0,1) .EQ. 0) Then

C If not determined, set cloud mask to be cloudy (=0) --> NOT processed
C Need to set to cloudy

             LandSea_Flag(p1,l1) = -1

             CldMsk_1km(p1,l1) = 0
             Cloud_VisRat_Flag(p1,l1)=0

             Do 88 l = 1,4
             Do 88 p = 1,4
               l4 = l4_offset + l
               p4 = p4_offset + p
               CldMsk_250(p4,l4) = 0
   88        continue

             Do 89 l = 1,2
             Do 89 p = 1,2
               l2 = l2_offset + l
               p2 = p2_offset + p
               CldMsk_500(p2,l2) = 0
   89        continue

          Else
C-----------------------------------------------------------------------
C Cloud Mask determined.  Extract value of 1-km LandSea_Flag.
C Also get results (a simple yes/no switch) of 250-m visible cloud test.
C-----------------------------------------------------------------------
C
C Extract bits from 0 to 25 for use in land and ocean aerosol algorithm
C
C
C IF UFQ_Flag(p1,l1) NE 0 and 1 ---> indicating clear pixel
C Also for both 250m and 500m resolution pixels (see cloud mask ATBD)
C

            IR_cloud=0

            IF(UFQ_Flag(p1,l1).EQ.0.OR.UFQ_Flag(p1,l1).EQ.1) THEN
C     &         .OR.UFQ_Flag(p1,l1).EQ.2) THEN

                CldMsk_1km(p1,l1) = 0
                Cloud_VisRat_Flag(p1,l1)=0

               Do 95 l = 1,2
               Do 95 p = 1,2
                 l2 = l2_offset + l
                 p2 = p2_offset + p
                 CldMsk_500(p2,l2) = 0
   95          continue

               Do 96 l = 1,4
               Do 96 p = 1,4
                 l4 = l4_offset + l
                 p4 = p4_offset + p
                 CldMsk_250(p4,l4) = 0
   96          continue
              
            ELSE
C
C IF UFQ_Flag(p1,l1) EQ 0 and 1 ---> indicating cloudy pixel
C IR test will not overwirte the decision if more than 2
C tests indicate high-cloud free. 
C

              IF(Thin_CirIR_Flag(p1,l1).EQ.0)IR_cloud=IR_cloud+1
              IF(Cloud_SimpIR_Flag(p1,l1).EQ.0)IR_cloud=IR_cloud+1
              IF(Cloud_3p75_11_Flag(p1,l1).EQ.0)IR_cloud=IR_cloud+1
              IF(Cloud_IRTemp_Flag(p1,l1).EQ.0)IR_cloud=IR_cloud+1

              IF(IR_cloud.GE.2) THEN

                CldMsk_1km(p1,l1) = 0

                Do 97 l = 1,2
                Do 97 p = 1,2
                  l2 = l2_offset + l
                  p2 = p2_offset + p
                  CldMsk_500(p2,l2) = 0
   97           continue

                Do 98 l = 1,4
                Do 98 p = 1,4
                  l4 = l4_offset + l
                  p4 = p4_offset + p
                  CldMsk_250(p4,l4) = 0
   98           continue

              ENDIF

            ENDIF

C
C Non_cloud obstruction bit will overwrite the cloudy pixel (disabled for now)
C Non_CloudOb_Flag(p1,l1) is set to 1 (see above)
C

               IF(High_Cloud_Flag(p1,l1).EQ.1) THEN

               IF(LandSea_Flag(p1,l1).NE.0.AND.Non_CloudOb_Flag(p1,l1).EQ.0) THEN

                 CldMsk_1km(p1,l1) = 1

                 Do 92 l = 1,2
                 Do 92 p = 1,2
                   l2 = l2_offset + l
                   p2 = p2_offset + p
                   CldMsk_500(p2,l2) = 1
   92            continue


                 Do 93 p = 1,4
                   l4 = l4_offset + l
                   p4 = p4_offset + p
                   CldMsk_250(p4,l4) = 1
   93            continue

              ENDIF
     
              ENDIF

          End If

   80  continue

C
C 3 x 3 windhowing: Cloud_SpatVar_Flag(p1,l1).EQ.0 is now replaced with dust bit (bit 28)
C

       Do 888 l1 = 1, Dim_Size(3)
       Do 888 p1 = 1, Dim_Size(2)

               IF(Cloud_VisRat_Flag(p1,l1).EQ.0) THEN
                 IF(l1.GT.1.AND.l1.LT.Dim_Size(3)) THEN
                 IF(p1.GT.1.AND.P1.LT.Dim_Size(2)) THEN
                   DO l11=l1-1,l1+1
                   DO p11=p1-1,p1+1
                     CldMsk_1km(p11,l11) = 0
                     CldMsk_1km(p1,l1) = 0
                   ENDDO
                   ENDDO
                 ENDIF 
                 ENDIF
               ENDIF 

  888 continue

       Do 85 l1 = 1, Dim_Size(3)
       Do 85 p1 = 1, Dim_Size(2)

          l4_offset = (l1-1)*4
          p4_offset = (p1-1)*4
          l2_offset = (l1-1)*2
          p2_offset = (p1-1)*2

          IF(CldMsk_1km(p1,l1).EQ.0) THEN

          Do 86 l = 1,2
          Do 86 p = 1,2
            l2 = l2_offset + l
            p2 = p2_offset + p
            CldMsk_500(p2,l2) = 0
   86     continue

          Do 87 l = 1,4
          Do 87 p = 1,4
            l4 = l4_offset + l
            p4 = p4_offset + p
            CldMsk_250(p4,l4) = 0
   87     continue

          ENDIF  

   85 continue

       Do 81 l1 = 1, Dim_Size(3)
       Do 81 p1 = 1, Dim_Size(2)

               IF(Cloud_SpatVar_Flag(p1,l1).EQ.0) THEN
                 IF(l1.GT.1.AND.l1.LT.Dim_Size(3)) THEN
                 IF(p1.GT.1.AND.P1.LT.Dim_Size(2)) THEN
                   DO l11=l1-1,l1+1
                   DO p11=p1-1,p1+1
                     CldMsk_1km(p11,l11) = 1
                   ENDDO
                   ENDDO
                 ENDIF 
                 ENDIF
               ENDIF 

   81 continue

       Do 82 l1 = 1, Dim_Size(3)
       Do 82 p1 = 1, Dim_Size(2)

          l4_offset = (l1-1)*4
          p4_offset = (p1-1)*4
          l2_offset = (l1-1)*2
          p2_offset = (p1-1)*2

          IF(CldMsk_1km(p1,l1).EQ.1) THEN

          Do 83 l = 1,2
          Do 83 p = 1,2
            l2 = l2_offset + l
            p2 = p2_offset + p
            CldMsk_500(p2,l2) = 1
   83     continue

          Do 84 l = 1,4
          Do 84 p = 1,4
            l4 = l4_offset + l
            p4 = p4_offset + p
            CldMsk_250(p4,l4) = 1
   84     continue

          ENDIF  

   82 continue


c ... debug statement ............................................
      if( debug.gt.1) then
         write(h_output,*)
         write(h_output,*)
         write(h_output,'(A)') 'first 11 UFQ Flags for ' //
     +                   '10 lines'
         write(h_output,'(11I4)') ((UFQ_Flag(i,j),i=1,11),
     +                                  j=1,10)
         write(h_output,'(30I4)') ((CldMsk_1km(i,j),i=1,30),
     +                                  j=1,10)
      endif
c ................................................................

       Return
       END
