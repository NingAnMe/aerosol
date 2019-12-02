#!/bin/csh

# Get the NCEP Numerical Weather Prediction Global Data Assimilation
#  system (GDAS1) GRIB File that matches the input date and time most
#  closely.  This version fetches the files for the closest day that 
#  matches the time of day of the gdas file.  For example, if you
#  have a 19 UTC pass this script will search the data day, the day 
#  before, the day after, etc for an 18 UTC file. 
#
# Two environmental variables must be set in order to run this script.
#  $LOCAL_ANC_DIR - The ancillary directory on your local machine.  This
#                     This is where the script will look for a matching
#                     gdas or gfs file, and also where it will download
#                     it to if fetching from a remote ftp site.
#  $REMOTE_ANC_DIR - The remote ancillary data location to search for a
#                     matching file.  
#
# Note: the script will search for files in a directory structure like this:
#        Main_directory/Date_directory (YYYY_MM_DD_DDD)/ for gdas 
#        Main_directory/Date_directory (YYYY_MM_DD_DDD)/forecast for gfs 
#        files.  It will also create directories on your local machine
#        if the files are downloaded.
#
#  Updated March 2011 to use "wget" instead of "ncftpget".  
#  Kathleen Strabala  UW-Madison, SSEC, kathy.strabala@ssec.wisc.edu
#
# Uses dateplus.exe command found in /imapp_modisl2/bin directory. The
# source code can be found at: http://www.orlandokuntao.com/dateplus_c.html
# --------------------------------------------------------------------------
# Check number of arguments
if ($#argv != 2) then
  echo "Usage: get_nearest_gdas.csh date time"
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
if ( $time >= 0 && $time <= 259 ) then
  set gdas_time="00"
  set gdas_delta=0
else if ( $time >= 300 && $time <= 859 ) then
  set gdas_time="06"
  set gdas_delta=0
else if ( $time >= 900 && $time <= 1459 ) then
  set gdas_time="12"
  set gdas_delta=0
else if ( $time >= 1500 && $time <= 2059 ) then
  set gdas_time="18"
  set gdas_delta=0
else if ( $time >= 2100 && $time <= 2359 ) then
  set gdas_time="00"
  set gdas_delta=1
endif

# Compute Gregorian date (yyyymmdd)
set greg_date=`dateplus.exe -J $date`

# Compute Gregorian date adjusted by delta (yymmdd)
set file_date=`dateplus.exe $gdas_delta $greg_date | cut -c3-8`
set month = `echo $file_date | cut -c3-4`
set dd = `echo $file_date | cut -c5-6`

# Get year and day of year (yyyy, ddd) 
set year = `echo "20"$file_date | cut -c1-4`
set day = `dateplus.exe -j "20"$file_date`

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
  #Replaced ncftpget with wget  --  more flexible use for proxies
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
      rm $file_name
  endif

endif

# If you made it this far, then there were no exact matching gdas files
# for our data set. Search for the closest gdas1 file that matches
# the input date/time.

set date_range = ( 0 -1 1 -2 2 -3 3 -4 4 -5 5 -6 6 -7 7 )

foreach delta ($date_range)

     # Compute Gregorian date adjusted by delta (yymmdd)
     set file_date=`dateplus.exe $delta $greg_date | cut -c 3-8`
     set month = `echo $file_date | cut -c3-4`
     set dd = `echo $file_date | cut -c5-6`

     # Get year and day of year (yyyy, ddd) 
     set year = `echo "20"$file_date | cut -c1-4`
     set day = `dateplus.exe -j "20"$file_date`

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
            rm $file_name
          endif
     endif
end

# If we get here, no matching model grib files were found
echo "ERROR: Could not find any gdas model grib files matching data time" > /dev/stderr

exit 1
