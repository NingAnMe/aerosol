#!/bin/csh -f

#-----------------------------------------------------------------------
#Copyright (C) 2000,  Space Science and Engineering Center, University
#of Wisconsin-Madison, Madison WI.
#      
#This program is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 2 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program; if not, write to the Free Software
#Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#--------------------------------------------------------------------------
#
# -------------------------------------------------------------------------------
# Extract binary arrays from NISE HDFEOS ice extent files on
# northern and southern hemisphere azimuthal equal area grids
# at 25 kilometer resolution. NISE files are available from
# http://nsidc.org/PROJECTS/NISE/ (e.g., NISE_SSMIF13_20010602.HDFEOS)
#
# Each output file contains 721x721 bytes of data.
#
#    Data grid value     Meaning
#             0          snow-free land
#         1-100          sea ice concentration percentage
#           101          permanent ice (Greenland, Antarctica)
#           102          not used
#           103          dry snow
#           104          wet snow
#       105-251          not used
#           252          mixed pixels at coastlines
#           253          suspect ice value
#           254          corners (undefined)
#           255          ocean
#
# Requires the HDF4 'hdp' utility available from
# ftp://ftp.ncsa.uiuc.edu/HDF/HDF/HDF_Current/bin/
# -------------------------------------------------------------------------------

# You must set the environmental variable MODIS_L2_HOME prior to execution

# Check arguments
if ($#argv != 3) then
  echo "Usage: extract_nsidc_nise.csh NISEFILE OUTFILE_NORTH OUTFILE_SOUTH"
  exit(1)
endif
if (! -e $argv[1]) then
  echo "Input file does not exist: " $argv[1]
  exit(1)
endif

# Extract Northern Hemisphere Ice Extent (first SDS in file)
${MODIS_L2_HOME}/bin/hdp dumpsds -i 0 -b -o $argv[2] $argv[1]
if ($status != 0) then
  echo "Error extracting northern hemisphere ice extent from "$argv[1]
  exit(1)
endif

# Extract Southern Hemisphere Ice Extent (third SDS in file)
${MODIS_L2_HOME}/bin/hdp dumpsds -i 2 -b -o $argv[3] $argv[1]
if ($status != 0) then
  echo "Error extracting southern hemisphere ice extent from "$argv[1]
  exit(1)
endif

# Exit cleanly
exit(0)
