static char SccsId[] = "@(#)dateplus.c 2.2 07/14/04";
/*--------------------------------------------------------------------*/
/*                            dateplus.c                              */
/* ------------------------------------------------------------------ */
/*                                                                    */
/*  Copyright (c) 1995-2004 by Bob Orlando.  All rights reserved.     */
/*                                                                    */
/*  Permission to use, copy, modify and distribute this software      */
/*  and its documentation for any purpose and without fee is hereby   */
/*  granted, provided that the above copyright notice appear in all   */
/*  copies, and that both the copyright notice and this permission    */
/*  notice appear in supporting documentation, and that the name of   */
/*  Bob Orlando not be used in advertising or publicity pertaining    */
/*  to distribution of the software without specific, written prior   */
/*  permission.  Bob Orlando makes no representations about the       */
/*  suitability of this software for any purpose.  It is provided     */
/*  "as is" without express or implied warranty.                      */
/*                                                                    */
/*  BOB ORLANDO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS          */
/*  SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY     */
/*  AND FITNESS.  IN NO EVENT SHALL BOB ORLANDO BE LIABLE FOR ANY     */
/*  SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES         */
/*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER   */
/*  IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,    */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF    */
/*  THIS SOFTWARE.                                                    */
/*                                                                    */
/* ------------------------------------------------------------------ */
/*          Program documentation and notes located in the            */
/*                   show_documentation() function.                   */
/*--------------------------------------------------------------------*/

/*====================================================================*/
/*                       D E C L A R A T I O N S                      */
/*====================================================================*/
#include <stdio.h>
#include <stdlib.h>       /* abs, atoi, exit */
#include <string.h>       /* strlen, strncpy */
#include <ctype.h>        /* isdigit         */
#include <time.h>         /* time routines   */

#define H24SSS 86400      /* Number of seconds in a day */
#define FALSE  0
#define TRUE   1
#define STRNCPY(to, from, n) {strncpy(to, from, n);to[n] = 0x0;}

/*====================================================================*/
/*                         P R O T O T Y P E S                        */
/*====================================================================*/
long calc_basedate(int yyyy, int mm, int dd);
int  leap_year(int yyyy);
void show_documentation(char *progname);
int  this_years_days(int yyyy);
void usage(char *progname);
void validate_mm_dd(int mm, int dd);

/*====================================================================*/
/*                  G L O B A L    V A R I A B L E S                  */
/*====================================================================*/
int  yyyy;
int  ddddd;
const char *months[] = /* Month names */
      {
        "January",  "February",  "March"    ,
        "April"  ,  "May"     ,  "June"     ,
        "July"   ,  "August"  ,  "September",
        "October",  "November",  "December"
      };

/*====================================================================*/
/*                              M A I N                               */
/*====================================================================*/
main(int argc, char *argv[])
{
  int julian_days[] = {0,31,59,90,120,151,181,212,243,273,304,334,365};
  int julian_leap[] = {0,31,60,91,121,152,182,213,244,274,305,335,366};
  int *accum_days = &julian_days[0]; /* julian_days address to ptr */

/*--------------------------------------------------------------------*/
/* Variables for weekday calculations                                 */
/*                                                                    */
  const char *weekdays_abbr[]
    = {"Sun","Mon","Tue","Wed","Thu","Fri","Sat"};
  const char *weekdays_long[]
    = {"Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"};
  int weekday_wanted      = FALSE;              /* Initially false.   */
  int weekday_abbr_wanted = FALSE;              /*    ''      ''      */
  int weekday_long_wanted = FALSE;              /*    ''      ''      */
/*                                                                    */
/*--------------------------------------------------------------------*/

  int  i, n; /* Counters */
  int  mm;
  int  dd;
  int  since_1970          = 0; /* Option flag */
  int  Since_yyyymmdd      = 0; /*   ''    ''  */
  int  since_yyyymmdd      = 0; /*   ''    ''  */
  int  basedate            = 0;

  long ddddd               = 0;
  long ddddd_yyyy          = 0;
  int  iterations          = 1;
  int  yyyymmdd_to_jjj     = 0;
  int  yyyyjjj_to_yyyymmdd = 0;

  int  adj_days            = 0;
  int  abs_adj             = 0;
  int  valid_opt           = FALSE; /* Pessimistic by nature I am. */

  char yyyymmdd[9];
  char four_char[5];
  char two_char[3];
  char opt = 0x0;                /* Command line option            */
  char *p_argv      = argv[0];   /* Instantiate pointer to argv[0] */
  char progname[64] = {0x0};

  /*---------------------------------------------------------------*/
  /* Begin by placing today's date into yyyymmdd string variable.  */
  /*---------------------------------------------------------------*/
  struct tm *time_now;
  time_t secs_now;
  time(&secs_now); /* Convert calendar time to local */
  time_now = localtime(&secs_now);
  strftime(yyyymmdd,9,"%Y""%m""%d",time_now);

  /*-------------------------------------------------*/
  /* Parse today's date and calc its ddddd value.    */
  /*-------------------------------------------------*/
  STRNCPY(four_char,  yyyymmdd,       4); yyyy = atoi(four_char);
  STRNCPY(two_char, (&yyyymmdd[0]+4), 2); mm   = atoi(two_char );
  STRNCPY(two_char, (&yyyymmdd[0]+6), 2); dd   = atoi(two_char );
  ddddd = calc_basedate(yyyy, mm, dd);

  /*---------------------------------------------------------*/
  /* Lowercase argv[0] and assign the basename to progname.  */
  /*---------------------------------------------------------*/
  for (n=i=0; *(p_argv+n); *(p_argv+n)=tolower(*(p_argv+n)), n++)
    if ((*(p_argv+n) == '/') || (*(p_argv+n) == '\\'))
      i = n + 1;

  strcpy(progname,argv[0]+i); /* Portion that follows last / (or \). */

  /*-----------------------------------------------------------------*/
  /* If user supplies no args (argc == 1) then show usage and exit.  */
  /* Else, see if they gave us options.  If so, process accordingly. */
  /*-----------------------------------------------------------------*/
  if (argc == 1)        /* This zero-based numbering drives me nuts! */
  {
    usage(progname);
    exit(7);
  }

  /*-------------------------------------*/
  /* If we have iterations, validate'em. */
  /*-------------------------------------*/
  if (argc > 3)
  {
    p_argv = argv[3]; /* Instantiate pointer to argv[3] */
    for (n=0; *(p_argv+n); n++)
    {
      if (isdigit(*(p_argv+n)))
        continue; /* A-OK */
      fprintf(stderr,"\nIterations (%s) invalid: must be integer.\n",
        argv[3]);
      fprintf(stderr,"%s terminated.\a\n", progname);
      exit(8);
    }
    iterations = atoi(argv[3]);
  }

  /*------------------------------------------------------------*/
  /* If the first argument's leading character is a hyphen      */
  /* (indicating potential option), then see is it is, indeed   */
  /* an option, or simply a negative adjustment value.          */
  /*------------------------------------------------------------*/
  if ((opt = *argv[1]) == '-')
  {
    switch (opt = *(argv[1]+1))
    {
      case 'h': /* User wants short help       */
            usage(progname);
            exit(9);
            break;

      case 'H': /* User wants detailed help    */
            show_documentation(progname);
            exit(10);
            break;

      case 'y': /* User wants yesterday's date */
            if (argc > 2)
            {
              fprintf(stderr,"No other arguments allowed with -y "
                "(yesterday) option.\n",argv[1]);
              fprintf(stderr,"%s terminated.\a\n", progname);
              exit(11);
            }

            secs_now -= H24SSS;
            time_now = localtime(&secs_now);
            strftime(yyyymmdd, 9, "%Y""%m""%d", time_now);
            printf("%s\n", yyyymmdd);
            exit(0);
            break;

      case 't': /* User wants tomorrow's date  */
            if (argc > 2)
            {
              fprintf(stderr,"No other arguments allowed with -t "
                "(tomorrow) option.\n");
              fprintf(stderr,"%s terminated.\a\n", progname);
              exit(12);
            }
            secs_now += H24SSS;
            time_now = localtime(&secs_now);
            strftime(yyyymmdd, 9, "%Y""%m""%d", time_now);
            printf("%s\n", yyyymmdd);
            exit(0);
            break;

      case 'S': /* Days since date (unsigned) */
            since_1970     = 0;
            Since_yyyymmdd = 1; /* Capitalized variable name */
            basedate       = 0;
            valid_opt      = TRUE;
            break;

      case 's': /* Days since date */
            since_1970     = 0;
            since_yyyymmdd = 1;
            basedate       = 0;
            valid_opt      = TRUE;
            break;

      case 'j': /* Gregorian to Julian */
            if (argc > 2)
            {
              if (strlen(argv[2]) != 8)
              {
                fprintf(stderr,
                  "Date (%s) must be in the form: yyyymmdd\n",
                    argv[2]);
                fprintf(stderr,"%s terminated.\a\n", progname);
                exit(13);
              }
              strcpy(yyyymmdd,argv[2]);
            }
            yyyymmdd_to_jjj = TRUE;
            iterations      = 1; /* Not used with Julian */
            adj_days        = 0;
            valid_opt       = TRUE;
            break;

      case 'J': /* Julian to Gregorian with yyyy */
            if (argc < 3 || strlen(argv[2]) != 7)
            {
              fprintf(stderr,
                "Julian-to-Gregorian conversion requires "
                "yyyyJJJ julian date.\n");
              fprintf(stderr,"%s terminated.\a\n", progname);
              exit(14);
            }

            strcpy(yyyymmdd,argv[2]);
            STRNCPY(     four_char, (&yyyymmdd[0]+4), 3);
            ddddd = atoi(four_char);
            STRNCPY(     four_char,   yyyymmdd, 4);
            yyyy  = atoi(four_char);

            if (leap_year(yyyy))
              accum_days = &julian_leap[0];
            /*-----------------------------------------------------*/
            /* Rip through accum_days until we find one that is    */
            /* too big.  Then use the preceding one as the correct */
            /* month number and decrement ddddd by the month's     */
            /* julian days.                                        */
            /*-----------------------------------------------------*/
            for (mm=1; mm<=12; mm++)
            {
              if ((ddddd - accum_days[mm]) <= 0)
              {
                 ddddd -= accum_days[mm-1];
                 break;
              }
            }
            printf("%04d%02d%02d\n", yyyy,mm,ddddd);
            exit(0);
            break;

      case 'w': /* Weekday option (returns weekday abbreviation) */
            weekday_wanted      = TRUE; /* Was false, now true   */
            weekday_abbr_wanted = TRUE; /* Was false, now true   */
            iterations          = 1;    /* Not used with Weekday */
            adj_days            = 0;
            valid_opt           = TRUE;
            break;

      case 'W': /* Weekday option (returns weekday)              */
            weekday_wanted      = TRUE; /* Was false, now true   */
            weekday_long_wanted = TRUE; /* Was false, now true   */
            iterations          = 1;    /* Not used with Weekday */
            adj_days            = 0;
            valid_opt           = TRUE;
            break;

      case 'b': /* Basedate from Jan. 1, 0001  */
            since_1970          = 0;
            since_yyyymmdd      = 0;
            basedate            = 1;
            valid_opt           = TRUE;
            break;

      case 'u': /* Basedate from Jan. 1, 1970  */
            since_1970          = 1;
            since_yyyymmdd      = 0;
            basedate            = 0;
            valid_opt           = TRUE;
            break;

      default:
            if (isdigit(opt))
            {
              valid_opt = TRUE;
              /*--------------------------------------------------*/
              /* Ensure that adjustment days are, indeed, numeric */
              /* numeric (before the atoi).                       */
              /*--------------------------------------------------*/
              p_argv = argv[1];             /* Pointer to argv[1] */
              for (n=0; *(p_argv+n); n++)
              {
                if (isdigit(*(p_argv+n)))
                  continue; /* A-OK */
                if (n==0 && (*(p_argv+n)=='-' || *(p_argv+n)=='+'))
                  continue; /* A-OK */
                fprintf(stderr,
                  "\nAdj days (%s) NOT [+-]numeric.\n", argv[0]);
                fprintf(stderr,"%s terminated.\a\n", progname);
                exit(15);
              }

              /*--------------------------------------------------*/
              /* Now, convert 1st arg (now argv[0]) to adj days.  */
              /*--------------------------------------------------*/
              adj_days = atoi(argv[1]);
            }
            break;
    } /* switch (opt = *(argv[1]+1)) */
  } /* if ((opt = *argv[1]) == '-') */
  else if (isdigit(opt) || opt == '+')
  {
    p_argv = argv[1]; /* Instantiate pointer to argv[1] */
    for (n=0; *(p_argv+n); n++)
    {
      if (isdigit(*(p_argv+n)))
        continue; /* A-OK */
      if (n==0 && *(p_argv+n)=='+')
        continue; /* A-OK */
      fprintf(stderr,"\nAdjust-days (%s) NOT numeric!\n", argv[1]);
      fprintf(stderr,"%s terminated.\a\n", progname);
      exit(16);
    }
    adj_days = atoi(argv[1]);
    valid_opt = TRUE;
  } /* else if (isdigit(opt) || opt == '+')  */

  /*---------------------------------------*/
  /* Anything else is an invalid argument. */
  /*---------------------------------------*/
  if (valid_opt != TRUE)
  {
    printf("%s: Invalid argument '%s'!\a\n\n", argv[0], argv[1]);
    usage(progname);
    exit(17);
  }

  /*-----------------------------------------------------------------*/
  /* If we have a yyyymmdd date argument, then do a basic validation */
  /* (must be 8 digits).  If it looks OK, assign it to yyyymmdd.     */
  /*-----------------------------------------------------------------*/
  if (argc > 2)
  {
    if (strlen(argv[2]) != 8)
    {
      fprintf(stderr,
        "Date (%s) must be in the form: yyyymmdd!\n",
           argv[2]);
      fprintf(stderr,"%s terminated.\a\n", argv[0]);
      exit(18);
    }
    /*------------------------------------------------------------*/
    /* Using a pointer to 1st character in argv[2], scan the arg  */
    /* (character-by-character) confirming that each is a digit.  */
    /* If any nonnumeric value, then fuss at the user and exit.   */
    /*------------------------------------------------------------*/
    p_argv = argv[2];                       /* Pointer to argv[2] */
    for (n=0; *(p_argv+n); n++)
    {
      if (isdigit(*(p_argv+n)))
        continue; /* A-OK */
      fprintf(stderr,"\nDate (%s) NOT numeric!\n", argv[0]);
      fprintf(stderr,"%s terminated.\a\n", argv[0]);
      exit(19);
    }
    strcpy(yyyymmdd,argv[2]); /* OK, assign argv[2] to yyyymmdd */
  } /* if (argc > 2) */

  /*----------------------------------------------------------------*/
  /* Are we returning days since a date or base dates?              */
  /*----------------------------------------------------------------*/
  if (since_yyyymmdd || since_1970 || argc > 2)
  {
    STRNCPY(four_char,  yyyymmdd,       4); yyyy = atoi(four_char);
    STRNCPY(two_char, (&yyyymmdd[0]+4), 2); mm   = atoi(two_char );
    STRNCPY(two_char, (&yyyymmdd[0]+6), 2); dd   = atoi(two_char );
    validate_mm_dd(mm, dd);
  }

  if (since_yyyymmdd || Since_yyyymmdd)  /* -S|s options */
  {
    ddddd_yyyy = calc_basedate(yyyy, mm, dd);
    ddddd -= ddddd_yyyy;
    printf("%d\n", ((Since_yyyymmdd) ? labs(ddddd) : ddddd));
    exit(0);
  }
  else if (since_1970) /* -u option */
  {
    ddddd = calc_basedate(yyyy, mm, dd);
    if (yyyy >= 1970)
    {
      ddddd_yyyy = calc_basedate(1970, 01, 01);
      ddddd -= ddddd_yyyy;
      printf("%d\n", ddddd);
      exit(0);
    }
    else
    {
      fprintf(stderr, "This is a trick question, right?\n"
        "The -u option with a date older than Jan. 1, 1970,\n"
        "and you expect me to give you the basedate!  :-))\n"
        "%s terminated.\n", progname);
      exit(20);
    }
  }                  /*--------------------------------------------*/
  else if (basedate) /* Calculate basedate for the date specified. */
  {                  /*--------------------------------------------*/
    ddddd = calc_basedate(yyyy, mm, dd);
    printf("%d\n", ddddd);
    exit(0);
  }

  /*-------------------------------------------------------------*/
  /* No option passed, only adj-days and (optionally) yyyymmdd.  */
  /*-------------------------------------------------------------*/
  if (argc == 1 && adj_days != 0)
  {
    secs_now += (adj_days * H24SSS);
    time_now = localtime(&secs_now);
    strftime(yyyymmdd, 9, "%Y""%m""%d", time_now);
    printf("%s\n", yyyymmdd);
    exit(0);
  }

  /*---------------------------------------------------------------*/
  /* Arriving here means it was none of the above (single option   */
  /* or argument), so begin by parsing yyyymmdd into yyyy, mm, and */
  /* ddddd, then assigning them to integers (dd's value is the     */
  /* seed to ddddd, hence the assignment to ddddd, not dd).        */
  /*---------------------------------------------------------------*/
  STRNCPY(four_char,  yyyymmdd, 4)      ; yyyy  = atoi(four_char);
  STRNCPY(two_char, (&yyyymmdd[0]+4), 2); mm    = atoi(two_char );
  STRNCPY(two_char, (&yyyymmdd[0]+6), 2); ddddd = atoi(two_char );

  validate_mm_dd(mm, ddddd);

  /*---------------------------------------------------------------*/
  /* Validation OK.  Add the previous month YTD days to our ddddd. */
  /*---------------------------------------------------------------*/
  for (n=1; n <= iterations; n++)
  {
    /*------------------------------------------------------------*/
    /* If leap year, assign address of julian_leap to accum_days. */
    /*------------------------------------------------------------*/
    if (leap_year(yyyy))
      accum_days = &julian_leap[0];

    /*------------------------------------------------------------*/
    ddddd += accum_days[mm-1]; /* January-preceding month's days  */
    /*------------------------------------------------------------*/

    /*------------------------------------------------------------*/
    /* If adjustment day(s) is negative, back up one year at a    */
    /* time -- adding that year's days to ddddd -- until we have  */
    /* enough accumulated days to handle a date that far back.    */
    /*------------------------------------------------------------*/
    if (adj_days < 0)
    {
      abs_adj = labs(adj_days);
      while (ddddd <= abs_adj)
         ddddd += (leap_year(--yyyy)) ? 366 : 365;
    }

    /*-----------------------------------*/
    /* Now, add in the adjustment day(s) */
    /*-----------------------------------*/
    ddddd += adj_days;

    /*----------------------------------------------------------*/
    /* If user wants weekday, print it, and exit (a zero-based  */
    /* number: 0=Sunday, 6=Saturday).                           */
    /*----------------------------------------------------------*/
    if (weekday_wanted == 1)
    {
      /*----------------------------------------------------------*/
      /* In the Day of the week formula below, the divisions are  */
      /* integer divisions, in which remainders are discarded     */
      /*                                                          */
      /*       14 - month                                         */
      /*  a =  ----------                                         */
      /*           12                                             */
      /*                                                          */
      /*  y = year - a                                            */
      /*                                                          */
      /*  m = month + 12a - 2                                     */
      /*                                                          */
      /*                     y    31m                             */
      /* Jd = (5+ day + y + --- + ---) mod 7          # Julian    */
      /*                     4     12                             */
      /*                                                          */
      /*                 y     y     y    31m                     */
      /* Gd = (day + y + --- - --- + --- + ---) mod 7 # Gregorian */
      /*                 4    100   400    12                     */
      /*----------------------------------------------------------*/
      while (--yyyy > 0)
        ddddd += (leap_year(yyyy)) ? 366 : 365;

      n = (ddddd % 7);
      if (weekday_abbr_wanted == 1)
        printf("%s\n", weekdays_abbr[n]);
      else
        printf("%s\n", weekdays_long[n]);
      exit(n); /* n = day number [0-6] */
    }

    /*------------------------------------------------------------*/
    /* By now we have the number of days (ddddd) from a Jan. 01   */
    /* date far enough back to handle any negative adjustment(s). */
    /* Beginning there (and providing ddddd has a year's worth),  */
    /* we subtract a year's worth of days from ddddd.  (We're     */
    /* careful to any leap day.)  We do this as long as ddddd is  */
    /* greater than the current year's days. Through this process */
    /* we calculate the new year and the number of days we have   */
    /* for that year.  From there it is a simple matter to reduce */
    /* the days to month and day values.                          */
    /*------------------------------------------------------------*/
    while (ddddd > this_years_days(yyyy))
    {
      ddddd -= this_years_days(yyyy);
      yyyy++;
    }

    /*------------------------------------------------------------*/
    /* If the days left in our accumulator (ddddd) are > 59, and  */
    /* the year proves to be a leap year, then use a YTD monthly  */
    /* accumulator that includes a leap day.                      */
    /*------------------------------------------------------------*/
    if ((ddddd > 59) && leap_year(yyyy))
      accum_days = &julian_leap[0];
    else
      accum_days = &julian_days[0];

    if (yyyymmdd_to_jjj)
    {
      printf("%03d\n", ddddd);
    }
    else
    {
      /*-----------------------------------------------------------*/
      /* Rip through accum_days until we find one that is too big. */
      /* Then use the preceding one as the correct month number    */
      /* and decrement ddddd by the month's julian days.           */
      /*-----------------------------------------------------------*/
      for (mm=1; mm<=12; mm++)
      {
        if ((ddddd - accum_days[mm]) <= 0)
        {
          ddddd -= accum_days[mm-1];
          break;
        }
      }
      printf("%04d%02d%02d\n", yyyy,mm,ddddd);
    }
  }

  exit(0);

  /*---------------------------------------------------------------*/
  /* Program never gets here, but this */ return 0; /* eliminates  */
  /* pesky compile message, about main not returning a value  (it  */
  /* seems that 'exit(0)' is just not good enough).                */
  /*---------------------------------------------------------------*/
}


/*====================================================================*/
/*                    U S E R    F U N C T I O N S                    */
/*                       (in alphabetical order)                      */
/*--------------------------------------------------------------------*/
long calc_basedate(int yyyy, int mm, int dd)
/*--------------------------------------------------------------------*/
{
  long ddddd;
  int  i;
  int  leap_days;
  int  cc_leapdays;
  int  cc;
  int  julian_days[] = {0,31,59,90,120,151,181,212,243,273,304,334,365};
  int  julian_leap[] = {0,31,60,91,121,152,182,213,244,274,305,335,366};
  int  *accum_days   = &julian_days[0];
  /*-------------------------------------------------------------*/
  /* We can't have a yyyy since there is no year 0000 in the CE. */
  /*-------------------------------------------------------------*/
  if ((yyyy - 1) > 0)
  {
    yyyy--;
    cc          = yyyy / 100;
    ddddd       = yyyy * 365;
    leap_days   = yyyy / 4;
    cc_leapdays = (cc > 0) ? (cc / 4) : 0;
    ddddd       = ddddd + leap_days - cc + cc_leapdays;
    yyyy++;
  }
  ddddd += dd;

  /*-------------------------------------------------------*/
  /* Add the previous month YTD days to our ddddd.  If it  */
  /* is a leap year, assign julian_leap[] to accum_days[]. */
  /* Then add in YTD days for previous months of this year */
  /*-------------------------------------------------------*/
  if (leap_year(yyyy))
    for (i = 0; i < 13; i++) accum_days[i] = julian_leap[i];
  else
    for (i = 0; i < 13; i++) accum_days[i] = julian_days[i];
  ddddd += accum_days[--mm];
  return (ddddd);
}

/*--------------------------------------------------------------------*/
int leap_year(int yyyy)
/*--------------------------------------------------------------------*/
{
  /*-------------------------------------------------------------*/
  /* if year is evenly divisible by 4, but yyyy is not evenly    */
  /* divisible by 100, OR yyyy IS evenly divisible by 400, then  */
  /* it's a leap year.                                           */
  /*-------------------------------------------------------------*/
  return (((yyyy%4 == 0 && yyyy%100 != 0) || (yyyy%400 == 0)) ? 1 : 0);
}

/*--------------------------------------------------------------------*/
void show_documentation(char *progname)
/*--------------------------------------------------------------------*/
{
#define P printf
  P("#===========================================================#\n");
  P("#                 D O C U M E N T A T I O N                 #\n");
  P("#===========================================================#\n");
  P(                                                             "\n");
  P("     Author: Bob Orlando <Bob@OrlandoKuntao.com>"           "\n");
  P(                                                             "\n");
  P("       Date: July 2, 1995"                                  "\n");
  P(                                                             "\n");
  usage(progname);
  P("    Purpose: This program calculates a date (or dates) that's\n");
  P("             nnnnn days (+ or -) distant from either today's \n");
  P("             date or another user-supplied date.  We can also\n");
  P("             calculate the number of days since or until the \n");
  P("             the date specified, as well as base dates since \n");
  P("             either Jan. 1, 0001 or the Unix epoch date,"   "\n");
  P("             Jan. 1, 1970"                                  "\n");
  P(                                                             "\n");
  P("Description: For all practical purposes, any date the user" "\n");
  P("             wishes to use as a yyyymmdd (from 01/01/0001"  "\n");
  P("             through 12/31/9999) is accepted.  The resultant \n");
  P("             date is returned via stdout in yyyymmdd form." "\n");
  P("             The program is especially useful for calculating\n");
  P("             yesterday's, tomorrow's, or any other desired" "\n");
  P("             date.  Additionally, by specifying an iteration \n");
  P("             value (the last argument) the program generates \n");
  P("             that number of dates, each adjusted on the"    "\n");
  P("             previous date (handy for calculating paydays and\n");
  P("             the like).  Because the program writes to stdout\n");
  P("             its output is easily redirected to a file where \n");
  P("             the results can be subsequently edited or munged\n");
  P("             as needed.  The program also comes with help"  "\n");
  P("             options (-h and -H for summary and detailed"   "\n");
  P("             outputs, respectively)."                       "\n");
  P(                                                             "\n");
  P(" Exit Codes: For all options except weekday (-w or -W)"     "\n");
  P("                Zero         = Normal   | Success"          "\n");
  P("                Nonzero (7+) = Abnormal | Failure"          "\n");
  P("             For weekday (-w or -W) option"                 "\n");
  P("                0-6 = Success (zero-based weekday: Sun = 0)""\n");
  P("                7+  = Failure"                              "\n");
  P(                                                             "\n");
  P("    History: The Gregorian correction to the Julian calendar \n");
  P("             made in October, 1582 dropped 10 days.  That"  "\n");
  P("             is, October 4, 1582 was followed immediately by \n");
  P("             October 15.  This Papal correction, although"  "\n");
  P("             scientifically correct, was not adopted by"    "\n");
  P("             non-Catholic countries until almost two"       "\n");
  P("             centuries later in 1752.  In September, 1752"  "\n");
  P("             the English calendar was adjusted to Pope"     "\n");
  P("             Gregory's method of correction and 11 days were \n");
  P("             dropped (September 14, followed September 2)." "\n");
  P(                                                             "\n");
  P("             While this routine easily calculates the date" "\n");
  P("             that far back, it does not drop October 5-14," "\n");
  P("             1582 or September 3-13, 1752.  If this routine""\n");
  P("             is used to calculate dates that far back, the" "\n");
  P("             previously adjusted and dropped dates will"    "\n");
  P("             appear as if no calendar corrections were ever""\n");
  P("             made.  All of this is only for technical"      "\n");
  P("             purists.  Most real-world applications will not \n");
  P("             go back that far.  This program's accuracy,"   "\n");
  P("             then, is sufficient for most purposes."        "\n");
  P(                                                             "\n");
  P("       Note: As with any program there are doubtless, many" "\n");
  P("             places where this code can be improved or"     "\n");
  P("             simplified.  Feel free, therefore, to forward" "\n");
  P("             comments/suggestions to Bob@OrlandoKuntao.com.""\n");
  P("             That said, my preference is for readable, easy""\n");
  P("             to follow code over highly efficient, but"     "\n");
  P("             difficult to follow (read obfuscated) code since\n");
  P("             readable code (even if less efficient) makes"  "\n");
  P("             maintenance much less a chore."                "\n");
  P(                                                             "\n");
  P(" +----------------------------------------------------------+\n");
  P(" |     For modifications, see 'Modified' section in the     |\n");
  P(" |     source code immediately following this statement.    |\n");
  P(" +----------------------------------------------------------+\n");
  P(                                                             "\n");
  /*-----------------------------------------------------------------*/
  /*                                                                 */
  /*    Modified: 2004-07-14 Bob Orlando                             */
  /*                 v2.2  * Add -W option (same as -w, but returns  */
  /*                         long weekday name).  Thanks to Hans     */
  /*                         Looyschelder of the Netherlands for     */
  /*                         both the idea and the clean code for    */
  /*                         accomplishing it.                       */
  /*                                                                 */
  /*              2004-05-05 Bob Orlando                             */
  /*                 v2.1  * Add -S option (same as -s, but returns  */
  /*                         unsigned integer regardless of whether  */
  /*                         the target date is future or past).     */
  /*                                                                 */
  /*     Revised: 2003-05-30 Bob Orlando                             */
  /*                 v2.0  * Incorporate most of basedate.c          */
  /*                         functionality (options -b, -u, and -s). */
  /*                                                                 */
  /*-----------------------------------------------------------------*/
#undef P
  return;
}

/*--------------------------------------------------------------------*/
int this_years_days(int yyyy)
/*--------------------------------------------------------------------*/
{
  return ((leap_year(yyyy)) ? 366 : 365);
}

/*--------------------------------------------------------------------*/
void usage(char *progname)
/*--------------------------------------------------------------------*/
{
#define P printf
  P(" Program ID: %s (07-14-2004 / v2.2)\n",                 progname);
  P(                                                             "\n");
  P("      Usage: %s -bHhJjSstuWwy\n"
    "               [-|+]adj_days [yyyymmdd [iterations]]\n",progname);
  P(                                                             "\n");
  P("                -b Basedate of today (or date specified)"   "\n");
  P("                   since Jan. 1, 0001"                      "\n");
  P("                -H Detailed help (Documentation)"           "\n");
  P("                -h Summary help (Usage)"                    "\n");
  P("                -J Julian (yyyyJJJ) to Gregorian (yyyymmdd)""\n");
  P("                -j Julian day (ddd) for today or yyyymmdd"  "\n");
  P("                -S Days since (or until) yyyymmdd (unsigned) \n");
  P("                -s Days since (or until) yyyymmdd **"       "\n");
  P("                -t Tomorrow's date"                         "\n");
  P("                -u Basedate of today (or date specified)"   "\n");
  P("                   since Jan. 1, 1970--the Unix epoch date" "\n");
  P("                   (essentially 'dateplus -s 19700101')"    "\n");
  P("                -W Date's long weekday"                     "\n");
  P("                -w Date's weekday"                          "\n");
  P("                   (-w or -W: see 'Exit Codes' below)"      "\n");
  P("                -y Yesterday's date"                        "\n");
  P(                                                             "\n");
  P("      Notes: adj_days may be positive, negative, or unsigned.\n");
  P(                                                             "\n");
  P("             adj_days by itself (without a yyyymmdd option" "\n");
  P("             argument), yields an adjusted date that is"    "\n");
  P("             calculated from today's date.  For example,"   "\n");
  P("             '%s 1' yields tomorrow's date).\n",        progname);
  P(                                                             "\n");
  P("             yyyymmdd is an optional date from which an"    "\n");
  P("             adjusted date is calculated."                  "\n");
  P(                                                             "\n");
  P("             iterations generate multiple dates that are"   "\n");
  P("             adj_days apart (i.e. paydays).  This argument" "\n");
  P("             is not allowed with the julian (-j) option."   "\n");
  P(                                                             "\n");
  P("             yyyymmdd also may be a future date (useful for""\n");
  P("             determining the number of days until a certain""\n");
  P("             date).  Days future are reported negatively."  "\n");
  P("             For example, run on October 27, 1997), the"    "\n");
  P("          ** 'dateplus -s 20000101' yielded -731, meaning"  "\n");
  P("             731 days until Y2K.  (For unsigned results use""\n");
  P("             -S option instead.)"                         "\n\n");
#undef P
  return;
}

/*--------------------------------------------------------------------*/
void validate_mm_dd(int mm, int dd)
/*--------------------------------------------------------------------*/
{
  int  mm_days;
  /*-----------------------------------------------------*/
  /* Validate the month and day.  Start by determining   */
  /* the maximum possible days for the month.  Then, see */
  /* if the day for the month given exceeds its maximum. */
  /*-----------------------------------------------------*/
  switch (mm)
  {
    case  1:
    case  3:
    case  5:
    case  7:
    case  8:
    case 10:
    case 12:
      mm_days=31;
      break;
    case  4:
    case  6:
    case  9:
    case 11:
      mm_days=30;
      break;
    case  2:
      mm_days = (leap_year(yyyy)) ? 29 : 28;
      break;
    default:
      fprintf(stderr,"Month %02d is invalid.\n", mm);
      exit(21);
  }

  if (dd > mm_days)
  {
    fprintf(stderr,"%s, %d does not have %d days.\n",
      months[mm-1], yyyy, dd);
    exit(22);
  }
  return; /* Passed with flying colors. */
}
