#!/bin/bash


bashName=`basename $0`
echo $bashName
logName=`echo $bashName |awk -F '.' '{print $1}'`.log
echo `date +"%Y-%m-%d %H:%M:%S"` 'star gsics global crond...' >> $logName

pnums=`ps x |grep -w $bashName |grep -v grep | wc -l`
if [[ $pnums -ge 3 ]];then
   echo `date +"%Y-%m-%d %H:%M:%S"` 'process is two...' >> $logName
   exit
fi

stime=$1
etime=$2
out_path=/home/kts_project_v1/shanghai/aod/fnl
hour_list="00 06 12 18"
while :
do
       yyyy=${stime:0:4}
       mm=${stime:4:2}
       ymd=${stime:4:2}
       dd=${stime:6:2}
       echo $ymd   $dd
       full_opath=$out_path/$yyyy
       if [ ! -d $full_opath ];then
           mkdir -p $full_opath
       fi
       
       echo wget -N  -L --no-check-certificate  -np  --load-cookies auth.rda.ucar.edu.fnl  https://rda.ucar.edu/data/OS/ds083.2/grib2/${yyyy}/${yyyy}.${mm}/fnl_${stime}_00_00.grib2 -P $full_opath
       wget -N  -L --no-check-certificate  -np  --load-cookies auth.rda.ucar.edu.fnl  https://rda.ucar.edu/data/OS/ds083.2/grib2/${yyyy}/${yyyy}.${mm}/fnl_${stime}_00_00.grib2 -P $full_opath

       stime=`date -d "${stime} 1days" +"%Y%m%d"`
       if [[ $stime -gt $etime ]]; then
             break
       fi
done
