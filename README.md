### 安装
#### 1. 修改imapp/env/imapp_modisl2.bash_env的MODIS_L2_HOME路径为imapp路径
```shell script
source $MODIS_L2_HOME/env/imapp_modisl2.bash_env
```
#### 2. 将ftp://ftp.ssec.wisc.edu/pub/eosdb/ancillary下载的数据放到$LOCAL_ANC_DIR

### 开发测试
#### 测试MODIS的L2
先将ancillary文件夹中的数据cp到$MODIS_L2_HOME/ancillary文件夹中
```shell script
modis_level2.csh aqua $MODIS_L2_HOME/test_data/modis_l2_test_input/a1.17299.1910.1000m.hdf  $MODIS_L2_HOME/test_data/modis_l2_test_output
```
#### 测试couldmask.exe
 ```shell script
# 必须进入文件目录，不能使用绝对路径
cd $MODIS_L2_HOME/test_data/cloudmask_test_input/
cloudmask.exe a1.17299.1910.mod35.cfg aqua $MODIS_L2_HOME/test_data/cloudmask_test_input/
```
#### 测试aerosol.exe
```shell script
# 必须进入文件目录，不能使用绝对路径
cd $MODIS_L2_HOME/test_data/aerosol_test_input/
aerosol.exe a1.17299.1910.mod04.cfg aqua 10
```

#### 开发编译
要求:4.7 <= gfortran版本 < 8.0
yum install gcc-gfortran
apt-get install gfortran

```shell script
source imapp/env/imapp_modisl2.bash_env
cd imapp/src/aerosol/src
make clean
make
cp -f aerosol.exe ../../../bin/aerosol.exe
```

### 开发DEBUG
#### 修改include/mod04.inc
将`NUMCELLS`修改为`210`，大于sample / 10
将`ISWATH`修改为`2100`，大于sample

#### dbvm安装
修改`~/.bashrz`, 添加`export DBVM_HOME=/home/modis/dbvm`
```shell script
cd $DBVM_HOME/scripts
install_dbvm.bash
```

#### 测试dbvm
```shell script
dbvm_test.bash
```
