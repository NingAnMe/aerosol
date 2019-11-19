
c----------------------------------------------------------------------
C!F77-INC
C
C!DESCRIPTION:
C                This is an include file for the declaration of
C                variables used to write the Vdata and SDS arrays for
C                mod04 land algorithm.
C
c!Input Parameters:  N/A
c!Output Parameters: N/A
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
C       Robert Levy              4/15/2004
C       Code 913/SSAI
C       NASA Goddard Space Flight Center
C       Greenbelt, MD 20771
C       levy@climate.gsfc.nasa.gov
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

      INTEGER iwave_466, iwave_644, iwave_553, iwave_212
      PARAMETER(iwave_466=1, iwave_644=3, iwave_553=2, iwave_212=4)

	 
C Define parameters for new land (_NL) lookup tables
C

      INTEGER  NLWAV,NLTHET0,NLTHE,NLPHI,NLTAU,NLTABLE
      PARAMETER (NLWAV=4,NLTHET0=9,NLTHE=16,NLPHI=16,NLTAU=7,NLTABLE=5)
      INTEGER NLSIZE, NLETA
      PARAMETER (NLETA = 13, NLSIZE=2)
      INTEGER DTABLE_NL, MTABLE_NL(2)
      PARAMETER (DTABLE_NL = 5)

      REAL MINMTHET0_NL,MAXMTHET0_NL,MINMTHET_NL,
     *   MAXMTHET_NL,MINMPHI_NL,MAXMPHI_NL
      PARAMETER(MINMTHET0_NL=0.0,MAXMTHET0_NL=60.0,MINMTHET_NL=0.0,
     *   MAXMTHET_NL=60.0,MINMPHI_NL=0.0,MAXMPHI_NL=180.0)

      REAL REFW466L,REFW553L,REFW644L,REFW866L,REFW212L
      REAL REFW164L,REFW124L

c     Define parameters based on lookup tables
      REAL WAV_NL,PHI_NL,THE_NL,THET0_NL

c 
	  
      COMMON/THREE/
     *              PHI_NL(NLPHI),
     *              THE_NL(NLTHE),
     *              THET0_NL(NLTHET0),
     *              WAV_NL(NLWAV),
     *              REFW466L,REFW644L,REFW212L,
     *              REFW553L,REFW866L,REFW164L,REFW124L




