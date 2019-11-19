#include <stdlib.h>
#include <PGS_SMF.h>

/* if ECS version is not defned then just define it as "None" */

#ifndef PGSd_ECS_VERSION
#    define PGSd_ECS_VERSION "None"
#endif

/* define strings that will be displayed by the using the UNIX "what" command
   on a file containing these strings */

#define  PGSd_BANNER  "@(#)## =================  SDP Toolkit  ================="
#if ( defined(LINUX) || defined(MACINTOSH) )
#define  PGSd_ECS_VER "@(#)## ECS Version: "PGSd_ECS_VERSION
#define  PGSd_TK_VER  "@(#)## Toolkit Version: "PGSd_TOOLKIT_VERSION_STRING
#define  PGSd_DATE    "@(#)## Build date: "__DATE__" @ "__TIME__

#else
#define  PGSd_ECS_VER "@(#)## ECS Version: "##PGSd_ECS_VERSION
#define  PGSd_TK_VER  "@(#)## Toolkit Version: "##PGSd_TOOLKIT_VERSION_STRING
#define  PGSd_DATE    "@(#)## Build date: "##__DATE__##" @ "##__TIME__
#endif

const char *PGSg_LibraryVersionString01 = PGSd_BANNER;
const char *PGSg_LibraryVersionString02 = PGSd_ECS_VER;
const char *PGSg_LibraryVersionString03 = PGSd_TK_VER;
const char *PGSg_LibraryVersionString04 = PGSd_DATE;
const char *PGSg_LibraryVersionString05 = PGSd_BANNER;

