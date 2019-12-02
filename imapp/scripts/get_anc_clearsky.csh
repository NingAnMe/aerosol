#!/bin/csh

#------------------------------------------------------------------------------
# Get the MODIS 8 Day Clear Sky ancillary file for Aqua or Terra 
# closest to an imput MODIS date (yyyyddd)
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
if ($#argv != 2) then
  echo "Usage: get_anc_clearsky.csh satellite date"
  echo "where"
  echo "  satellite is Aqua or Terra"
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


# Extract file names
set SAT = $argv[1]
set date = $argv[2]

if ("$SAT" == "Aqua" || "$SAT" == "aqua") then
  set PREFIX = "MYD"
else if ("$SAT" == "Terra" || "$SAT" == "terra") then
  set PREFIX = "MOD"
else
  echo "Satellite name incorrect" $SAT
  exit(1)
endif

#--- Dataset specific information ---
# Set NISE filename head and tail
set file_head = "${PREFIX}CSR_B.A"
set file_tail = "hdf"

# Set Clear Sky File date range 
# Because the dates on the files represent the first day of the 
#  compositing, you really want files that have date stamps 
#  8 days previous to the given date
set date_range = (-8 -9 -7 -10 -6 -11 -5 -12 -4 -13 -3 -14 \
  -2 -15 -1 -16 0 -17)

# Check local directory
if (! -d $LOCAL_ANC_DIR) then
  echo "Local ancillary data directory does not exist:" $LOCAL_ANC_DIR
  exit 1
endif

if ($date < 2000001 || $date > 2100365) then
  echo "Invalid date string:" $date
  exit 1
endif

# Compute Gregorian date (yyyymmdd)
set greg_date=`dateplus.exe -J $date`

# Search date range for an acceptable file
foreach delta ($date_range)
  
  # Compute Gregorian date adjusted by delta (yymmdd)
  set tmp_file_date=`dateplus.exe $delta $greg_date | cut -c 1-8`
  set year = `echo $tmp_file_date | cut -c1-4`
  set month = `echo $tmp_file_date | cut -c5-6`
  set dd = `echo $tmp_file_date | cut -c7-8`

  # Now convert back to Julian Date
  set file_date=`date +"%Y%j" --date="$year/$month/$dd"`
  
  # Set file name
  set file_name=${file_head}${file_date}
  echo "Searching for file with prefix "$file_name > /dev/stderr
    
  # Get year and day of year (yyyy, ddd)
  set day = `echo $file_date | cut -c5-8`
  
  # Set local file path and name
  set DAY_DIR = ${year}_${month}_${dd}_${day}
  set local_file = $LOCAL_ANC_DIR/$DAY_DIR

  # Check for file in local directory
  set success = `find $LOCAL_ANC_DIR/$DAY_DIR -name ${file_name}\*`
  if ($success != "") then

    # File was found locally
    echo "File was found on local disk" > /dev/stderr
    echo ${local_file}/${file_name}*
    exit 0

  else

    # File was not found locally, so try to download it
    echo "Trying to download file from "$REMOTE_ANC_DIR/$DAY_DIR/${file_name}\* > /dev/stderr
    #Replaced ncftpget with wget  --  more flexible use for proxies
#    echo wget -q -N -t 30 -nd --no-parent -r -l1 "$REMOTE_ANC_DIR/$DAY_DIR/${file_name}*.hdf"
    wget -q -N -t 30 -nd --no-parent -r -l1 "$REMOTE_ANC_DIR/$DAY_DIR/${file_name}*.hdf"
    set success1 = `find . -name ${file_name}\*`
    if ($success1 != "") then
      if (! -d $LOCAL_ANC_DIR/$DAY_DIR) mkdir $LOCAL_ANC_DIR/$DAY_DIR
      mv ${file_name}* ${local_file}/.
      if ($status == 0) then
        echo "File was downloaded successfully " > /dev/stderr
        echo ${local_file}/${file_name}*
        exit 0
      endif 
    endif

  endif

end

# Ancillary file was not found
echo "ERROR: Ancillary Clear Sky Bias file could not be found" > /dev/stderr
exit 1

