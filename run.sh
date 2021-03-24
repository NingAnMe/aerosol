# 重启以后需要使用root用户挂载CIMISS数据：curlftpfs -o ro,uid=1004,gid=1004,allow_other,umask=007 ftp://cloud:cloud@10.228.13.135:21 /home/aodo3/CIMISS_DATA
# 添加crontab应该source 一下环境变量：*/20 * * * * source /home/aodo3/.bashrc && bash /home/aodo3/FY3D_AEROSOL/aod/run.sh >& /home/aodo3/FY3D_AEROSOL/aod/run.log
cd /home/aodo3/FY3D_AEROSOL/aod
~/miniconda3/bin/python3 aerosol_a01_orbit_fy3d_runtime.py
if [ $? -eq 0 ]
    then  
        rm -r /home/aodo3/FY3D_AEROSOL_DATA/TMP/*
fi
