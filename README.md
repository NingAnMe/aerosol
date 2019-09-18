### 修改`~/.bashrc`，添加
```
export MODIS_L2_HOME=/mnt/hgfs/Projects/imapp_modisl2
source $MODIS_L2_HOME/env/imapp_modisl2.bash_env
```

### 测试MODIS的L2
先将ancillary文件夹中的数据cp到$MODIS_L2_HOME/ancillary文件夹中
```shell script
modis_level2.csh aqua $MODIS_L2_HOME/test_data/modis_l2_test_input/a1.17299.1910.1000m.hdf  $MODIS_L2_HOME/test_data/modis_l2_test_output
```
 
 ### 测试couldmask.exe
 ```shell script
# 必须进入文件目录，不能使用绝对路径
cd $MODIS_L2_HOME/test_data/cloudmask_test_input/
cloudmask.exe a1.17299.1910.mod35.cfg aqua $MODIS_L2_HOME/test_data/cloudmask_test_input/
```

### MOD35的相关资料
http://yangbaikal.blogspot.com/2017/10/modis35.html
https://github.com/spectralpython/spectral/blob/master/spectral/io/envi.py
ftp://ftp.ssec.wisc.edu/pub/eosdb/ancillary/

### 测试aerosol.exe
```shell script
# 必须进入文件目录，不能使用绝对路径
cd $MODIS_L2_HOME/test_data/aerosol_test_input/
aerosol.exe a1.17299.1910.mod04.cfg aqua 10
```

### dbvm安装
修改`~/.bashrz`, 添加`export DBVM_HOME=/home/modis/dbvm`
```shell script
cd $DBVM_HOME/scripts
install_dbvm.bash
```


### 测试dbvm
```shell script
dbvm_test.bash
```