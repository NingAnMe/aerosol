#!/bin/csh

#------------------------------------------------------------------------------
# Get the NCEP Ice Concentration ancillary data grib file closest to a given
#  date (yyyyddd)
#
#  Updated March 2011 to use "wget" instead of "ncftpget".  
#  Kathleen Strabala  UW-Madison, SSEC, kathy.strabala@ssec.wisc.edu
#
# Note that the following environment variables must be defined:
# LOCAL_ANC_DIR is the local ancillary data directory
#   e.g. $HOME/ancillary
# REMOTE_ANC_DIR is the remote FTP ancillary data directory (URL format)
#   e.g. ftp://ftp.ssec.wisc.edu/pub/eosdb/ancillary
#
# Uses dateplus.exe command found in /imapp_modisl2/bin directory. The
# source code can be found at: http://www.orlandokuntao.com/dateplus_c.html
#------------------------------------------------------------------------------

# Check number of arguments
if ($#argv != 1) then
  echo "Usage: get_anc_icec.csh date"
  echo "where"
  echo "  date is the required date (yyyyddd)"
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


#--- Dataset specific information ---
# Set ICEC filename head and tail
set file_head = "eng."
set file_tail = ""

# Set ICEC date range (7 day window centered on following day)
set date_range = (0 -1 1 -2 2 -3 3 -4 4 -5 5 -6 6 -7 7 \
     -8 8 -9 9 -10 -10 -11 11 -12 12 -13 13 -14 14)

#------------------------------------
# The rest of the script does not care if the file is NISE, ICE, or whatever.
# It simply searches directories named yyyy/ddd in the local and remote
# ancillary directories for files named ${file_head}yymmdd${tail} within
# the search window defined by date_range

# Check local directory
if (! -d $LOCAL_ANC_DIR) then
  echo "Local ancillary data directory does not exist:" $LOCAL_ANC_DIR
  exit 1
endif

# Get arguments
set date = $argv[1]
if ($date < 2000001 || $date > 2100365) then
  echo "Invalid date string:" $date
  exit 1
endif

# Compute Gregorian date (yyyymmdd)
set greg_date=`dateplus.exe -J $date`

# Search date range for an acceptable file
foreach delta ($date_range)

  # Compute Gregorian date adjusted by delta (yymmdd)
  set file_date=`dateplus.exe $delta $greg_date | cut -c 3-8`
  set month = `echo $file_date | cut -c3-4`
  set dd = `echo $file_date | cut -c5-6`
  
  # Set file name
  set file_name=$file_head$file_date$file_tail
  echo "Searching for file "$file_name > /dev/stderr
    
  # Get year and day of year (yyyy, ddd)
  set year = `echo "20"$file_date | cut -c1-4`
  set day = `dateplus.exe -j "20"$file_date`

  # Set local file path and name
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
  
end

# Ancillary file was not found
echo "ERROR: Ancillary Ice Concentration file could not be found" > /dev/stderr
exit 1
