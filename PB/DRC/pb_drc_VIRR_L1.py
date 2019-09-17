# coding: utf-8
"""
Created on 2017年9月7日
@author: wangpeng

@modify: anning 2018-07-30
1 增加了 Height BB 数据集
@modify: anning 2018-08-07
1 Time数据集的填充值改为 np.nan，修改了Time数据的获取方法
原来的数据获取不准确
2 增加了 self.extract_data 属性，用于提取
"""
from datetime import datetime
import os

import h5py

from PB import pb_sat
from PB.pb_time import get_ymd, get_hm
from pb_drc_hdf import ReadHDF5
import numpy as np


MainPath, MainFile = os.path.split(os.path.realpath(__file__))


class CLASS_VIRR_L1(ReadHDF5):

    def __init__(self):
        super(CLASS_VIRR_L1, self).__init__()

        self.error = False
        # 定标使用
        self.sat = 'FY3C'
        self.sensor = 'VIRR'
        self.res = 1000
        self.Band = 10
        self.obrit_direction = []
        self.obrit_num = []
        self.file_attr = {}

        self.Dn = {}
        self.Ref = {}
        self.Rad = {}
        self.Tbb = {}
        self.SV = {}
        self.BB = {}
        self.K0 = {}
        self.K1 = {}

        self.Height = []
        self.satAzimuth = []
        self.satZenith = []
        self.sunAzimuth = []
        self.sunZenith = []
        self.relaAzimuth = []
        self.relaZenith = []
        self.LandSeaMask = []
        self.LandCover = []
        self.Lons = []
        self.Lats = []
        self.Time = []

        # 其他程序使用
        self.LutFile = []
        self.IR_Coeff = []
        self.VIS_Coeff = []
        self.Rad_pre = {}  # 非线性系数修正前的Rad
        self.Tbb_coeff = {}  # 定标系数修正后的TBB

        # 红外通道的中心波数，固定值，MERSI_Equiv Mid_wn (cm-1)
        self.WN = {'CH_03': 2673.796, 'CH_04': 925.925, 'CH_05': 833.333}
        # 红外转tbb的修正系数，固定值
        self.TeA = {'CH_03': 1, 'CH_04': 1, 'CH_05': 1}
        self.TeB = {'CH_03': 0, 'CH_04': 0, 'CH_05': 0}

        # 所有通道的中心波数和对应的响应值 ，SRF
        self.waveNum = {}
        self.waveRad = {}

        # 提取Extract用
        self.extract_data = {}

    def Load(self, L1File):
        ipath = os.path.dirname(L1File)
        iname = os.path.basename(L1File)

        self.file_attr = self.read_file_attr(L1File)

        if 'FY3C' in iname[:4]:
            # 根据输入的L1文件自动拼接GEO文件

            geoFile = os.path.join(ipath, iname[0:-12] + 'GEOXX_MS.HDF')
            obcFile = os.path.join(ipath, iname[0:-12] + 'OBCXX_MS.HDF')
            print u'%s' % L1File
            print u'%s' % geoFile
            print u'%s' % obcFile

            #################### 读取L1文件 ######################
            try:
                with h5py.File(L1File, 'r') as h5File_R:
                    ary_ch3 = h5File_R.get('/Data/EV_Emissive')[:]
                    ary_ch7 = h5File_R.get('/Data/EV_RefSB')[:]
                    ary_offsets = h5File_R.get(
                        '/Data/Emissive_Radiance_Offsets')[:]
                    ary_scales = h5File_R.get(
                        '/Data/Emissive_Radiance_Scales')[:]
                    ary_ref_cal = h5File_R.attrs['RefSB_Cal_Coefficients']
                    ary_Nonlinear = h5File_R.attrs[
                        'Prelaunch_Nonlinear_Coefficients']
                    try:
                        ary_tb_coeff = h5File_R.attrs[
                            'Emmisive_BT_Coefficients']
                    except Exception:
                        ary_tb_coeff = h5File_R.attrs[
                            'Emissive_BT_Coefficients']
            except Exception as e:
                self.error = True
                print str(e)
                return
            #################### 读取GEO文件 ######################
            try:
                with h5py.File(geoFile, 'r') as h5File_R:
                    ary_satz = h5File_R.get('/Geolocation/SensorZenith')[:]
                    ary_sata = h5File_R.get('/Geolocation/SensorAzimuth')[:]
                    ary_sunz = h5File_R.get('/Geolocation/SolarZenith')[:]
                    ary_suna = h5File_R.get('/Geolocation/SolarAzimuth')[:]
                    ary_lon = h5File_R.get('/Geolocation/Longitude')[:]
                    ary_lat = h5File_R.get('/Geolocation/Latitude')[:]
                    ary_height = h5File_R.get('/Geolocation/DEM')[:]
                    ary_LandCover = h5File_R.get('/Geolocation/LandCover')[:]
                    ary_LandSeaMask = h5File_R.get(
                        '/Geolocation/LandSeaMask')[:]
            except Exception as e:
                self.error = True
                print str(e)
                return
            #################### 读取OBC文件 ####################
            try:
                with h5py.File(obcFile, 'r') as h5File_R:
                    ary_sv = h5File_R.get('/Calibration/Space_View')[:]
                    ary_bb = h5File_R.get('/Calibration/Blackbody_View')[:]
            except Exception as e:
                self.error = True
                print str(e)
                return

        else:
            # FY3A/FY3B VIRR
            # 根据输入的L1文件自动拼接OBC文件
            obcFile = os.path.join(ipath, iname[0:-12] + 'OBCXX_MS.HDF')
            print u'%s' % L1File
            print u'%s' % obcFile
            #################### 读取L1文件 ####################
            try:
                with h5py.File(L1File, 'r') as h5File_R:
                    ary_ch3 = h5File_R.get('/EV_Emissive')[:, 0:1800, 0:2048]
                    ary_ch7 = h5File_R.get('/EV_RefSB')[:, 0:1800, 0:2048]
                    ary_offsets = h5File_R.get(
                        '/Emissive_Radiance_Offsets')[0:1800, :]
                    ary_scales = h5File_R.get(
                        '/Emissive_Radiance_Scales')[0:1800, :]
                    ary_ref_cal = h5File_R.attrs['RefSB_Cal_Coefficients']
                    ary_Nonlinear = h5File_R.attrs[
                        'Prelaunch_Nonlinear_Coefficients']
                    try:
                        ary_tb_coeff = h5File_R.attrs[
                            'Emmisive_BT_Coefficients']
                    except Exception:
                        ary_tb_coeff = h5File_R.attrs[
                            'Emissive_BT_Coefficients']
                    ary_satz = h5File_R.get('/SensorZenith')[0:1800, 0:2048]
                    ary_sata = h5File_R.get('/SensorAzimuth')[0:1800, 0:2048]
                    ary_sunz = h5File_R.get('/SolarZenith')[0:1800, 0:2048]
                    ary_suna = h5File_R.get('/SolarAzimuth')[0:1800, 0:2048]
                    ary_lon = h5File_R.get('/Longitude')[0:1800, 0:2048]
                    ary_lat = h5File_R.get('/Latitude')[0:1800, 0:2048]
                    ary_height = h5File_R.get('/Height')[0:1800, 0:2048]
                    ary_LandCover = h5File_R.get('/LandCover')[0:1800, 0:2048]
                    ary_LandSeaMask = h5File_R.get(
                        '/LandSeaMask')[0:1800, 0:2048]
            except Exception as e:
                self.error = True
                print str(e)
                return
            #################### 读取OBC文件 ####################
            try:
                with h5py.File(obcFile, 'r') as h5File_R:
                    ary_sv = h5File_R.get('Space_View')[:]
                    ary_bb = h5File_R.get('Blackbody_View')[:]
            except Exception as e:
                self.error = True
                print (str(e))
                return

        # 通道的中心波数和光谱响应
        for i in xrange(self.Band):
            BandName = 'CH_%02d' % (i + 1)
            srfFile = os.path.join(
                MainPath, 'SRF', '%s_%s_SRF_CH%02d_Pub.txt' % (self.sat, self.sensor, (i + 1)))
            dictWave = np.loadtxt(
                srfFile, dtype={'names': ('num', 'rad'), 'formats': ('f4', 'f4')})
            if BandName in ['CH_03', 'CH_04', 'CH_05']:
                waveNum = 10 ** 4 / dictWave['num'][::-1]
            else:
                waveNum = 10 ** 7 / dictWave['num'][::-1]
            waveRad = dictWave['rad'][::-1]
            if BandName not in self.waveNum:
                self.waveNum[BandName] = waveNum
                self.waveRad[BandName] = waveRad
            else:
                self.waveNum[BandName] = np.concatenate(
                    (self.waveNum[BandName], waveNum))
                self.waveRad[BandName] = np.concatenate(
                    (self.waveRad[BandName], waveRad))
        ############### 数据大小 使用经度维度 ###############
        dshape = ary_lon.shape

        ##################可见光数据进行定标 ########
#         LutAry = np.loadtxt(self.LutFile, dtype={'names': ('TBB', 'CH_03', 'CH_04', 'CH_05'),
#                             'formats': ('i4', 'f4', 'f4', 'f4')})
        # 定标系数b,a
        proj_Cal_Coeff = np.full((7, 2), np.nan)
        for i in range(7):
            for j in range(2):
                proj_Cal_Coeff[i, j] = ary_ref_cal[i * 2 + j - 1]

        # 1 - 6 通道的ref
        for i in xrange(self.Band):
            BandName = 'CH_%02d' % (i + 1)
            if i < 2 or i >= 5:
                # 下标k (1 2 6 7 8 9 10存放在一个三维数组中)
                if i < 2:
                    k = i
                elif i >= 5:
                    k = i - 3

                # DN值存放无效值用nan填充
                DN = np.full(dshape, np.nan)
                idx = np.logical_and(ary_ch7[k] < 32767, ary_ch7[k] > 0)
                DN[idx] = ary_ch7[k][idx]

                # 反射率值存放无效值用nan填充
                k0 = proj_Cal_Coeff[k][1]
                k1 = proj_Cal_Coeff[k][0]
                Ref = (DN * k0 + k1) / 100.
                K0 = np.full(dshape, k0, dtype=np.float32)
                K1 = np.full(dshape, k1, dtype=np.float32)
                if BandName not in self.Dn:
                    self.Dn[BandName] = DN
                    self.Ref[BandName] = Ref
                    self.K0[BandName] = K0
                    self.K1[BandName] = K1
                else:
                    self.Dn[BandName] = np.concatenate((self.Dn[BandName], DN))
                    self.Ref[BandName] = np.concatenate(
                        (self.Ref[BandName], Ref))
                    self.K0[BandName] = np.concatenate((self.K0[BandName], K0))
                    self.K1[BandName] = np.concatenate((self.K1[BandName], K1))

            if 2 <= i <= 4:
                # DN Rad Tbb 值存放,默认 nan填充
                k = i - 2

                DN = np.full(dshape, np.nan)
                Rad_pre = np.full(dshape, np.nan)

                condition = np.logical_and(ary_ch3[k] < 32767, ary_ch3[k] > 0)
                idx = np.where(condition)

                # 下标i-2 (3,4,5通道DN存放在一个三维数组中)
                DN[idx] = ary_ch3[k][idx]

                Rad_pre[idx] = DN[idx] * ary_scales[idx[0], k] + \
                    ary_offsets[idx[0], k]
                k0 = ary_Nonlinear[k]
                k1 = ary_Nonlinear[3 + k]
                k2 = ary_Nonlinear[6 + k]
                Rad_nonlinearity = Rad_pre ** 2 * k2 + Rad_pre * k1 + k0

                K0 = np.full(dshape, np.nan)
                K0[:] = ary_scales[:, k].reshape(-1, 1)
                K1 = np.full(dshape, np.nan)
                K1[:] = ary_offsets[:, k].reshape(-1, 1)

                self.Rad_pre[BandName] = Rad_pre
                Rad = Rad_pre + Rad_nonlinearity

                Tbb = pb_sat.planck_r2t(Rad, self.WN[BandName])
                Tbb = Tbb * self.TeA[BandName] + self.TeB[BandName]

                k0_coeff = ary_tb_coeff[k * 2 + 1]
                k1_coeff = ary_tb_coeff[k * 2]
                Tbb_coeff = Tbb * k0_coeff + k1_coeff

                if BandName not in self.Dn:
                    self.Dn[BandName] = DN
                    self.Rad[BandName] = Rad
                    self.K0[BandName] = K0
                    self.K1[BandName] = K1
                    self.Tbb[BandName] = Tbb
                    self.Tbb_coeff[BandName] = Tbb_coeff
                else:
                    self.Dn[BandName] = np.concatenate((self.Dn[BandName], DN))
                    self.Rad[BandName] = np.concatenate(
                        (self.Rad[BandName], Rad))
                    self.K0[BandName] = np.concatenate((self.K0[BandName], K0))
                    self.K1[BandName] = np.concatenate((self.K1[BandName], K1))
                    self.Tbb[BandName] = np.concatenate(
                        (self.Tbb[BandName], Tbb))
                    self.Tbb_coeff[BandName] = np.concatenate(
                        (self.Tbb_coeff[BandName], Tbb_coeff))
        # SV, BB
        for i in xrange(self.Band):
            BandName = 'CH_%02d' % (i + 1)
            SV = np.full(dshape, np.nan)
            BB = np.full(dshape, np.nan)
            SV[:] = ary_sv[i, :, 0].reshape(-1, 1)
            BB[:] = ary_bb[i, :, 0].reshape(-1, 1)

            if BandName not in self.SV:
                self.SV[BandName] = SV
                self.BB[BandName] = BB
            else:
                self.SV[BandName] = np.concatenate((self.SV[BandName], SV))
                self.BB[BandName] = np.concatenate((self.BB[BandName], BB))
        ##################### 全局信息赋值 ############################
        # 对时间进行赋值合并
        Time = np.full(dshape, np.nan)
        ymd = get_ymd(L1File)
        hm = get_hm(L1File)
        file_date = datetime.strptime(ymd + hm, '%Y%m%d%H%M')
        secs = (file_date - datetime(1970, 1, 1, 0, 0, 0)).total_seconds()
        Time[:] = secs
        if self.Time == []:
            self.Time = Time
        else:
            self.Time = np.concatenate((self.Time, Time))

        # 土地覆盖
        ary_LandCover_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_LandCover >= 0, ary_LandCover <= 254)
        ary_LandCover_idx[condition] = ary_LandCover[condition]

        if self.LandCover == []:
            self.LandCover = ary_LandCover_idx
        else:
            self.LandCover = np.concatenate(
                (self.LandCover, ary_LandCover_idx))

        # 海陆掩码
        ary_LandSeaMask_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_LandSeaMask >= 0, ary_LandSeaMask <= 7)
        ary_LandSeaMask_idx[condition] = ary_LandSeaMask[condition]

        if self.LandSeaMask == []:
            self.LandSeaMask = ary_LandSeaMask_idx
        else:
            self.LandSeaMask = np.concatenate(
                (self.LandSeaMask, ary_LandSeaMask_idx))

        # 经纬度
        ary_lon_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_lon > -180., ary_lon < 180.)
        ary_lon_idx[condition] = ary_lon[condition]
        if self.Lons == []:
            self.Lons = ary_lon_idx
        else:
            self.Lons = np.concatenate((self.Lons, ary_lon_idx))

        ary_lat_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_lat > -90., ary_lat < 90.)
        ary_lat_idx[condition] = ary_lat[condition]
        if self.Lats == []:
            self.Lats = ary_lat_idx
        else:
            self.Lats = np.concatenate((self.Lats, ary_lat_idx))

        # 高度
        ary_height_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_height > -1000., ary_height < 10000.)
        ary_height_idx[condition] = ary_height[condition]
        if self.Height == []:
            self.Height = ary_height_idx
        else:
            self.Height = np.concatenate((self.Height, ary_height_idx))

        # 卫星方位角 天顶角
        ary_sata_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_sata > -18000, ary_sata < 18000)
        ary_sata_idx[condition] = ary_sata[condition]

        if self.satAzimuth == []:
            self.satAzimuth = ary_sata_idx / 100.
        else:
            self.satAzimuth = np.concatenate(
                (self.satAzimuth, ary_sata_idx / 100.))

        ary_satz_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_satz > 0, ary_satz < 18000)
        ary_satz_idx[condition] = ary_satz[condition]
        if self.satZenith == []:
            self.satZenith = ary_satz_idx / 100.
        else:
            self.satZenith = np.concatenate(
                (self.satZenith, ary_satz_idx / 100.))

        # 太阳方位角 天顶角
        ary_suna_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_suna > -18000, ary_suna < 18000)
        ary_suna_idx[condition] = ary_suna[condition]

        if self.sunAzimuth == []:
            self.sunAzimuth = ary_suna_idx / 100.
        else:
            self.sunAzimuth = np.concatenate(
                (self.sunAzimuth, ary_suna_idx / 100.))

        ary_sunz_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_sunz > 0, ary_sunz < 18000)
        ary_sunz_idx[condition] = ary_sunz[condition]

        if self.sunZenith == []:
            self.sunZenith = ary_sunz_idx / 100.
        else:
            self.sunZenith = np.concatenate(
                (self.sunZenith, ary_sunz_idx / 100.))

        self.get_extract_data()

    def get_extract_data(self):
        """
        将需要提取的数据放入self.data
        :return:
        """
        for i in xrange(self.Band):
            channel_name = 'CH_{:02}'.format(i + 1)
            self.extract_data[channel_name] = dict()
            # 加载一般数据
            if channel_name in self.Dn:
                self.extract_data[channel_name]['DN'] = self.Dn[channel_name]
            if channel_name in self.Ref:
                self.extract_data[channel_name]['REF'] = self.Ref[channel_name]
            if channel_name in self.Rad:
                self.extract_data[channel_name][
                    'RAD_NON'] = self.Rad[channel_name]
            if channel_name in self.Tbb:
                self.extract_data[channel_name]['TBB'] = self.Tbb[channel_name]
            if channel_name in self.SV:
                self.extract_data[channel_name]['SV'] = self.SV[channel_name]
            if channel_name in self.BB:
                self.extract_data[channel_name]['BB'] = self.BB[channel_name]
            if channel_name in self.Rad_pre:
                self.extract_data[channel_name][
                    'RAD_PRE'] = self.Rad_pre[channel_name]
            if channel_name in self.Tbb_coeff:
                self.extract_data[channel_name][
                    'TBB_COEFF'] = self.Tbb_coeff[channel_name]
            if channel_name in self.K0:
                self.extract_data[channel_name]['K0'] = self.K0[channel_name]
            if channel_name in self.K1:
                self.extract_data[channel_name]['K1'] = self.K1[channel_name]

        self.extract_data['Longitude'] = self.Lons
        self.extract_data['Latitude'] = self.Lats
        self.extract_data['Height'] = self.Height
        self.extract_data['LandSeaMask'] = self.LandSeaMask
        self.extract_data['LandCover'] = self.LandCover

        self.extract_data['SensorAzimuth'] = self.satAzimuth
        self.extract_data['SensorZenith'] = self.satZenith
        self.extract_data['SolarAzimuth'] = self.sunAzimuth
        self.extract_data['SolarZenith'] = self.sunZenith

        self.extract_data['Time'] = self.Time


if __name__ == '__main__':
    T1 = datetime.now()

    L1File = r'D:\data\VIRR\FY3C_VIRRX_GBAL_L1_20180404_0850_1000M_MS.HDF'
    virr = CLASS_VIRR_L1()
    virr.Load(L1File)
    T2 = datetime.now()
    print (T2 - T1).total_seconds()

#     for k, v in virr.file_attr.items():
#         print k, v

    def print_stats(data):
        print 'min: ', np.nanmin(data), 'max: ', np.nanmax(data)

    for i in xrange(3):
        g_channel_name = 'CH_{:02d}'.format(i + 3)
        #         print 'New:{}\n'.format(g_channel_name)
        #
        #         print 'Rad_pre'
        #         print_stats(virr.Rad_pre[g_channel_name])
#         print 'Rad_non'
#         print_stats(virr.Rad[g_channel_name])
#         print 'Tbb'
#         print_stats(virr.Tbb[g_channel_name])
        print 'Tbb_coeff'
        print_stats(virr.Tbb_coeff[g_channel_name])
#         print 'Scales'
#         print_stats(virr.K0[g_channel_name])
#         print 'Offsets'
#         print_stats(virr.K1[g_channel_name])
