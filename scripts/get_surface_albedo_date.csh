#!/bin/csh

#------------------------------------------------------------------------------
# Get the correct surface albedo date based upon the input data time
#
#------------------------------------------------------------------------------

# Check number of arguments
if ($#argv != 1) then
  echo "Usage: get_surface_albedo_date.csh date"
  echo "where"
  echo "  file_date is the required date (yyyyddd)"
  exit 1
endif


# Set date ranges (file every 16 days)

set date_range = (353 337 321 305 289 273 257 241 225 209 \
                  193 177 161 145 129 113 97 81 65 49 33 17 1)

# Get arguments
set file_date = $argv[1]
if ($file_date < 2000001 || $file_date > 2100365) then
  echo "Invalid date string:" $file_date
  exit 1
endif

set tmp_doy=`echo $file_date | cut -c5-7`

#Remove leading zeros from julian date if they exist
set doy = `expr $tmp_doy + 0`

foreach delta ($date_range)

  if ($doy >=  $delta) then
     if ($delta < 10) then
       set JULDAY=00${delta}
       echo $JULDAY
       exit 0 
     endif
     if ($delta < 100) then
       set JULDAY=0${delta}
       echo $JULDAY
       exit 0 
     else 
       set JULDAY=${delta}
       echo $JULDAY
       exit 0 
     endif
  endif
end

exit 1 
