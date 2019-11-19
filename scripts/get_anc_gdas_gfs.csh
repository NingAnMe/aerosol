#!/bin/csh

# Get the NCEP Numerical Weather Prediction GRIB File that matches the 
#  input date and time.  Real time data sets will use the global model
#  forecast fields (gfs).  Other data sets will be processed using the global
#  analysis files (gdas1).  The analysis fields should be used whenever
#  possible, but these files can be as much as 7-9 hours behind real-time.
#
# This script searches for two envirnmental variables: 
#  $LOCAL_ANC_DIR - The ancillary directory on your local machine.  This
#                     This is where the script will look for a matching
#                     gdas or gfs file, and also where it will download
#                     it to if fetching from a remote ftp site. The 
#                     default directory is the current directory.The 
#  $REMOTE_ANC_DIR - The remote ancillary data location to search for a
#                     matching file.  The default site is the 
#                     UW SSEC ancillary data archive at:
#                     ftp://ftp.ssec.wisc.edu/pub/eosdb/ancillary
#
# Note: the script will search for files in a directory structure like this:
#        Main_directory/Date_directory (YYYY_MM_DD_DDD)/ for gdas 
#        Main_directory/Date_directory (YYYY_MM_DD_DDD)/forecast for gfs 
#        files.  It will also create directories on your local machine
#        if the files are downloaded.
#  Updated March 2011 to use "wget" instead of "ncftpget".  
#                     Removed the use of the "dateplus.exe" utility
#  Kathleen Strabala  UW-Madison, SSEC, kathy.strabala@ssec.wisc.edu
#
# --------------------------------------------------------------------------
# Check number of arguments
if ($#argv != 2) then
  echo "Usage: get_anc_gdas_gfs.csh date time"
  echo "where"
  echo "  date is the required date (yyyyddd)"
  echo "  date is the required time (hhmm)"
  exit 1
endif

# Before we begin, make sure that wget is available
which wget > /dev/null
if ($status != 0) then
    echo " "  > /dev/stderr
    echo "**** ERROR:  Could not find the wget utility. The ancillary" > /dev/stderr
    echo "**** data fetchers will not work without it. Please install" > /dev/stderr
    echo "**** and try again." > /dev/stderr
    echo " " > /dev/stderr
    exit 1
endif

# If the local ancillary directory variable is not set, then default to 
#  current directory

if ( ! $?LOCAL_ANC_DIR ) then
   setenv LOCAL_ANC_DIR ${PWD}
endif

# If the remote ancillary directory variable is not set, then default to 
#  the 
if ( ! $?REMOTE_ANC_DIR ) then
  setenv REMOTE_ANC_DIR ftp://ftp.ssec.wisc.edu/pub/eosdb/ancillary
endif

# Get arguments (note: leading zero is removed from time argument)
set date = $argv[1]
set time = `expr $argv[2] + 0`

#Fix problem with time = 0000 
if ($time > -1 && $time < 1 ) then
    set time = 1
endif

if ($date < 2000001 || $date > 2100365) then
  echo "Invalid date string:" $date
  exit 1
endif

# Check local directory
if (! -d $LOCAL_ANC_DIR) then
  echo "Local ancillary data directory does not exist:" $LOCAL_ANC_DIR
  exit 1
endif

#Now see if any of the files in the listing matches our data times
# Set the day and time of the closest GDAS1 file
if       ( $time >=   0 && $time <=  259 ) then
  set gdas_time="00"
  set gdas_delta=0
else if ( $time >=  300 && $time <=  859 ) then
  set gdas_time="06"
  set gdas_delta=0
else if ( $time >=  900 && $time <= 1459 ) then
  set gdas_time="12"
  set gdas_delta=0
else if ( $time >= 1500 && $time <= 2059 ) then
  set gdas_time="18"
  set gdas_delta=0
else if ( $time >= 2100 && $time <= 2359 ) then
  set gdas_time="00"
  set gdas_delta=1
endif

set jday1=`echo $date | cut -c5-7`
set jday=`expr $jday1 - 1`
set data_year=`echo $date | cut -c1-4`
set greg_date=`date --date="$data_year-01-01 + $jday days" "+%y%m%d"`
set greg_month=`date --date="$data_year-01-01 + $jday days" "+%m"`
set greg_day=`date --date="$data_year-01-01 + $jday days" "+%d"`

# Compute Gregorian date adjusted by delta (yymmdd)
set file_date=`date --date="$data_year-$greg_month-$greg_day $gdas_delta days" "+%y%m%d"`
set year = `echo "20"$file_date | cut -c1-4`
set month = `echo $file_date | cut -c3-4`
set dd = `echo $file_date | cut -c5-6`
set day=`date --date="$year-$greg_month-$greg_day $gdas_delta days" "+%j"`

# Find the GDAS1 file
set file_name="gdas1.PGrbF00."${file_date}.${gdas_time}"z"
echo "Searching for gdas file "$file_name > /dev/stderr

set DAY_DIR = ${year}_${month}_${dd}_${day}
set local_file = $LOCAL_ANC_DIR/$DAY_DIR/$file_name

# Check for file in local directory
set success=`find $LOCAL_ANC_DIR/$DAY_DIR -name $file_name`
if (${success} != "") then

  # File was found locally
  echo "File was found on local disk" > /dev/stderr
  echo $local_file
  exit 0

else

  # File was not found locally, so try to download it
  echo "Trying to download file from "$REMOTE_ANC_DIR/$DAY_DIR/$file_name > /dev/stderr
  wget -q -N -t 30 -O ${file_name} $REMOTE_ANC_DIR/$DAY_DIR/${file_name}
  if ($status == 0) then
    if (! -d $LOCAL_ANC_DIR/$DAY_DIR) mkdir $LOCAL_ANC_DIR/$DAY_DIR
    mv $file_name $local_file
    if ($status == 0) then
      echo "File was downloaded successfully" > /dev/stderr
      echo $local_file
      exit 0
    endif
    # Delete the file if it has zero length
    else if (-z $file_name) then
      rm -f $file_name
  endif

endif

# If you made it this far, then there were no matching gdas files
# for our data set.

# If there were no gdas files matching the input date/time then go
#  and look for a forecast file that matches.  You should attempt
#  to use a forecast time step that is closest to the analysis time
#  for example, if your data time is 15 UTC, you should try to use 
#  the 3 hour forecast field from the 12 UTC model run instead of the
#  9 hour forecast field from the 06 UTC run.

# Set the day and time of the closest GFS file
if      ( $time >=    0 && $time <=  130 ) then
     set gfs_time="0"
     set gfs_delta=0
 else if ( $time >  130 && $time <=  430 ) then
     set gfs_time="3"
     set gfs_delta=0
 else if ( $time >  430 && $time <=  730 ) then
     set gfs_time="6"
     set gfs_delta=0
 else if ( $time >  730 && $time <= 1030 ) then
     set gfs_time="9"
     set gfs_delta=0
 else if ( $time > 1030 && $time <= 1330 ) then
     set gfs_time="12"
     set gfs_delta=0
 else if ( $time > 1330 && $time <= 1630 ) then
     set gfs_time="15"
     set gfs_delta=0
 else if ( $time > 1630 && $time <= 1930 ) then
     set gfs_time="18"
     set gfs_delta=0
 else if ( $time > 1930 && $time <= 2230 ) then
     set gfs_time="21"
     set gfs_delta=0
 else if ( $time > 2230 && $time <= 2359 ) then
     set gfs_time="0"
     set gfs_delta=0
 endif 

foreach time_step ( 03 06 09 12 )

   foreach analysis ( 00 06 12 18 )

        # Compute the Gregorian date (yyyymmdd) of the closest GFS file
        set modis_date=$date
        set jday1=`echo $date | cut -c5-7`
        set jday=`expr $jday1  -  1`
        set data_year=`echo $date | cut -c1-4`
        set greg_month=`date --date="$data_year-01-01 + $jday days" "+%m"`
        set greg_day=`date --date="$data_year-01-01 + $jday days" "+%d"`
        set gfs_date=`date --date="$data_year-$greg_month-$greg_day $gfs_delta days" "+%y%m%d"`

        set valid_time = `expr ${time_step} + ${analysis}`

        if ( $valid_time >= 24 ) then 
           set valid_time = `expr ${valid_time} - 24`
           set gfs_date=`date --date="$data_year-$greg_month-$greg_day -1 days" "+%y%m%d"` 
           set gfs_date=`dateplus.exe -1 $modis_date | cut -c3-8`
        endif
           
        if ($valid_time == $gfs_time) then
      
          # Find the GFS file
          set gfs_name="gfs.t"${analysis}"."${gfs_date}".pgrbf"${time_step}
          echo "Searching for gfs file "$gfs_name > /dev/stderr
          set year = `echo "20"$gfs_date | cut -c1-4`
          set month = `echo $gfs_date | cut -c3-4`
          set dd = `echo $gfs_date | cut -c5-6`
          set day=`date --date="$year-$month-$dd" "+%j"`

          set DAY_DIR = ${year}_${month}_${dd}_${day}
          set local_file = $LOCAL_ANC_DIR/$DAY_DIR/forecast/$gfs_name

          # Check for file in local directory
          set success=`find $LOCAL_ANC_DIR/$DAY_DIR/forecast -name $gfs_name`
          if (${success} != "") then

              # File was found locally
              echo "File was found on local disk" > /dev/stderr
              echo $local_file
              exit 0

          else
             # File was not found locally, so try to download it
             echo "Trying to download file from "$REMOTE_ANC_DIR/$DAY_DIR/forecast/$gfs_name > /dev/stderr
             wget -q -N -t 30 -O ${gfs_name} $REMOTE_ANC_DIR/$DAY_DIR/forecast/${gfs_name}
             if ($status == 0) then
               if (! -d $LOCAL_ANC_DIR/$DAY_DIR) mkdir $LOCAL_ANC_DIR/$DAY_DIR
               if (! -d $LOCAL_ANC_DIR/$DAY_DIR/forecast) mkdir $LOCAL_ANC_DIR/$DAY_DIR/forecast
               mv $gfs_name $local_file
               if ($status == 0) then
                  echo "File was downloaded successfully" > /dev/stderr
                  echo $local_file
                  exit 0
               endif
             # Delete the file if it has zero length
             else if (-z $gfs_name) then
                rm -f $gfs_name
             endif
          endif
        endif

   end

end
  
# If we get here, no matching model grib files were found
echo "ERROR: Could not find any gfs or gdas model grib files matching data time"  > /dev/stderr

exit 1
