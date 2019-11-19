/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 1999, Raytheon Systems Company, its vendors, */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*******************************************************************************
BEGIN_FILE_PROLOG:

NAME:
   PGS_TD_Prototypes.h

DESCRIPTION:
   This file contains the function prototypes for the SDP (aka PGS) 
   Toolkit Time and Date Tools.

END_FILE_PROLOG:
*******************************************************************************/

#ifndef _PGS_TD_Prototypes_H_
#define _PGS_TD_Prototypes_H_

/*     Function prototypes    */

#ifdef __cplusplus
extern "C" {
#endif

PGSt_integer PGS_TD_julday(PGSt_integer, PGSt_integer, PGSt_integer);

void PGS_TD_calday(PGSt_integer, PGSt_integer*, PGSt_integer*, PGSt_integer*);

PGSt_SMF_status PGS_TD_SCtime_to_UTC(PGSt_tag, PGSt_scTime [][8], 
                PGSt_integer, char*, PGSt_double []); 

PGSt_SMF_status PGS_TD_UTCtoTAIjd(char*, PGSt_double [2]);    

PGSt_SMF_status PGS_TD_UTCtoTDTjed(char*, PGSt_double [2]);    

PGSt_SMF_status PGS_TD_UTCtoTDBjed(char*, PGSt_double [2]);    

PGSt_double PGS_TD_gmst(PGSt_double [2]);

PGSt_double PGS_TD_gast(PGSt_double, PGSt_double, PGSt_double [2]);

PGSt_SMF_status PGS_TD_timeCheck(char*);

PGSt_SMF_status PGS_TD_UTCtoUTCjd(char*, PGSt_double [2]); 
    
PGSt_SMF_status PGS_TD_LeapSec(PGSt_double [2], PGSt_double*, 
              PGSt_double*, PGSt_double*, char [10]);
 
PGSt_SMF_status PGS_TD_UTCtoTAI(char*, PGSt_double*);

PGSt_SMF_status PGS_TD_TAItoUT1jd(PGSt_double, PGSt_double [2]);

PGSt_SMF_status PGS_TD_TAItoUT1pole(PGSt_double, PGSt_double [2], 
                 PGSt_double*, PGSt_double*, PGSt_double*, PGSt_double*);

PGSt_SMF_status PGS_TD_TAItoUTC(PGSt_double, char*);

PGSt_SMF_status PGS_TD_TAItoUTCjd(PGSt_double, PGSt_double [2]);

PGSt_SMF_status PGS_TD_TAItoGAST(PGSt_double, PGSt_double*);

PGSt_SMF_status PGS_TD_EOSAMtoUTC(unsigned char*, char*);

PGSt_SMF_status PGS_TD_EOSPMtoUTC(unsigned char*, char*);

PGSt_SMF_status PGS_TD_EOSPMGIIStoUTC(unsigned char*, char*);

PGSt_SMF_status PGS_TD_EOSPMGIRDtoUTC(unsigned char*, char*);

PGSt_SMF_status PGS_TD_EOSAURAtoUTC(unsigned char*, char*);

PGSt_SMF_status PGS_TD_TRMMtoUTC(unsigned char*, char*);

PGSt_SMF_status PGS_TD_EOSAMtoTAI(unsigned char*, PGSt_double*);

PGSt_SMF_status PGS_TD_EOSPMtoTAI(unsigned char*, PGSt_double*);

PGSt_SMF_status PGS_TD_EOSPMGIIStoTAI(unsigned char*, PGSt_double*);

PGSt_SMF_status PGS_TD_EOSPMGIRDtoTAI(unsigned char*, PGSt_double*);

PGSt_SMF_status PGS_TD_EOSAURAGIIStoTAI(unsigned char*, PGSt_double*);

PGSt_SMF_status PGS_TD_EOSAURAGIRDtoTAI(unsigned char*, PGSt_double*);

PGSt_SMF_status PGS_TD_TRMMtoTAI(unsigned char*, PGSt_double*);

PGSt_SMF_status PGS_TD_UTC_to_SCtime(PGSt_tag, char*, unsigned char*);

PGSt_SMF_status PGS_TD_UTCtoEOSAM(char*, unsigned char*);

PGSt_SMF_status PGS_TD_UTCtoEOSPM(char*, unsigned char*);

PGSt_SMF_status PGS_TD_UTCtoEOSPMGIIS(char*, unsigned char*);

PGSt_SMF_status PGS_TD_UTCtoEOSPMGIRD(char*, unsigned char*);

PGSt_SMF_status PGS_TD_UTCtoEOSAURAGIIS(char*, unsigned char*);
 
PGSt_SMF_status PGS_TD_UTCtoEOSAURAGIRD(char*, unsigned char*);

PGSt_SMF_status PGS_TD_UTCtoTRMM(char*, unsigned char*);

PGSt_SMF_status PGS_TD_ASCIItime_AtoB(char*, char*);

PGSt_SMF_status PGS_TD_ASCIItime_BtoA(char*, char*);

PGSt_SMF_status PGS_TD_GPStoUTC(PGSt_double, char*);

PGSt_SMF_status PGS_TD_UTCtoGPS(char*, PGSt_double*);

PGSt_SMF_status PGS_TD_TimeInterval(PGSt_double, PGSt_double, PGSt_double*);

PGSt_SMF_status PGS_TD_UTCtoUT1(char*, PGSt_double*);

PGSt_SMF_status PGS_TD_UTCtoUT1jd(char*, PGSt_double [2]);

PGSt_SMF_status PGS_TD_sortArrayIndices(PGSt_double [], PGSt_integer, 
                PGSt_integer []);

PGSt_double * PGS_TD_JulianDateSplit(PGSt_double [2], PGSt_double [2]);

PGSt_double PGS_TD_TAIjdtoTAI(PGSt_double [2]);

PGSt_double * PGS_TD_TAIjdtoTDTjed(PGSt_double [2], PGSt_double [2]);

PGSt_double * PGS_TD_TDTjedtoTAIjd(PGSt_double [2], PGSt_double [2]);

PGSt_SMF_status PGS_TD_TAIjdtoUTCjd(PGSt_double [2], PGSt_double [2]);

PGSt_double * PGS_TD_TAItoTAIjd(PGSt_double, PGSt_double [2]);

PGSt_double * PGS_TD_TDTjedtoTDBjed(PGSt_double [2], PGSt_double [2]);

PGSt_double * PGS_TD_TDBjedtoTDTjed(PGSt_double [2], PGSt_double [2]);

PGSt_SMF_status PGS_TD_UTCjdtoTAIjd(PGSt_double [2], PGSt_boolean, 
                      PGSt_double [2]);

PGSt_SMF_status PGS_TD_UT1jdtoUTCjd(PGSt_double [2], PGSt_double [2]);

PGSt_SMF_status PGS_TD_UTCjdtoUT1jd(PGSt_double [2], PGSt_boolean, 
                      PGSt_double [2]);

PGSt_SMF_status PGS_TD_UTCjdtoUTC(PGSt_double [2], PGSt_boolean, char*);

PGSt_double * PGS_TD_JDtoMJD(PGSt_double [2], PGSt_double [2]);

PGSt_double * PGS_TD_JDtoTJD(PGSt_double [2], PGSt_double [2]);

PGSt_double * PGS_TD_TJDtoJD(PGSt_double [2], PGSt_double [2]);

PGSt_double * PGS_TD_MJDtoJD(PGSt_double [2], PGSt_double [2]);

PGSt_SMF_status PGS_TD_PB5toUTCjd(PGSt_scTime [9], PGSt_double [2]);

PGSt_SMF_status PGS_TD_UTCjdtoPB5(PGSt_double [2], PGSt_boolean, 
                      PGSt_scTime [9]);

PGSt_SMF_status PGS_TD_PB5toTAI(PGSt_scTime [9], PGSt_double*);

PGSt_SMF_status PGS_TD_PB5CtoUTCjd(PGSt_scTime [7], PGSt_double [2]);

PGSt_SMF_status PGS_TD_UTCjdtoPB5C(PGSt_double [2], PGSt_boolean, 
                      PGSt_scTime [7]);

PGSt_SMF_status PGS_TD_FGDCtoUTC(char [9], char [18], char [28]);

PGSt_SMF_status PGS_TD_UTCtoFGDC(char[28], char[6], char [9], char [18]);

PGSt_SMF_status PGS_TD_ISOinttoTAI(PGSt_integer, PGSt_double*);

PGSt_SMF_status PGS_TD_ISOinttoUTCjd(PGSt_integer, PGSt_double[2]);

PGSt_SMF_status PGS_TD_TAItoISOint(PGSt_double, PGSt_integer*);

PGSt_SMF_status PGS_TD_UTCjdtoISOint(PGSt_double[2], PGSt_integer*);

PGSt_SMF_status PGS_TD_UDTFtoTAI(PGSt_integer[2], PGSt_double*);

PGSt_SMF_status PGS_TD_UDTFtoUTCjd(PGSt_integer[2], PGSt_double[2]);

PGSt_SMF_status PGS_TD_TAItoUDTF(PGSt_double, PGSt_integer[2]);

PGSt_SMF_status PGS_TD_UTCjdtoUDTF(PGSt_double[2], PGSt_boolean, 
                      PGSt_integer[2]);

PGSt_SMF_status PGS_TD_ManageUTCF(PGSt_integer, PGSt_double*);

PGSt_SMF_status PGS_TD_ManageTMDF(PGSt_integer, PGSt_double*, 
                      PGSt_double*, PGSt_double*);

PGSt_SMF_status PGS_TD_ADEOSIItoUTC(PGSt_scTime[5], char[28]);

PGSt_SMF_status PGS_TD_ADEOSIItoTAI(PGSt_scTime[5], PGSt_double*);

PGSt_SMF_status PGS_TD_UTCtoADEOSII(char[28], PGSt_scTime[5]);

#ifdef __cplusplus
}
#endif

#endif
