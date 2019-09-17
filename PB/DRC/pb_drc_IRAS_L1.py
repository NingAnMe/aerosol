# coding: utf-8
'''
Created on 2018年5月2日

@author: wangpeng
'''

from datetime import datetime
import os
import pdb
import sys
import time

import h5py
sys.path.append('E:\KY\git')

from DV.dv_map import dv_map
from PB import pb_name, pb_sat
from PB.pb_time import fy3_ymd2seconds
import numpy as np


MainPath, MainFile = os.path.split(os.path.realpath(__file__))


class CLASS_IRAS_L1():

    def __init__(self):

        # 定标使用
        self.sat = 'FY3C'
        self.sensor = 'IRAS'
        self.res = 17000  # m
        self.Band = 20  # 目前只需要处理前20通道
        self.obrit_direction = []
        self.obrit_num = []

        self.Dn = {}
        self.Rad = {}
        self.Tbb = {}

        self.satAzimuth = []
        self.satZenith = []
        self.sunAzimuth = []
        self.sunZenith = []
        self.Lons = []
        self.Lats = []
        self.Time = []
        self.SV = {}
        self.BB = {}
        self.LandSeaMask = []
        self.LandCover = []

        # 其他程序使用
        self.LutFile = []
        self.IR_Coeff = []
        self.VIS_Coeff = []

        # 红外通道的中心波数，固定值，MERSI_Equiv Mid_wn (cm-1)
        self.WN = {'CH_01': 669.976914, 'CH_02': 680.162001, 'CH_03': 691.391561, 'CH_04': 702.858560,
                   'CH_05': 715.270436, 'CH_06': 732.203858, 'CH_07': 749.383836, 'CH_08': 801.671379,
                   'CH_09': 899.414299, 'CH_10': 1032.591246, 'CH_11': 1343.617931, 'CH_12': 1364.298075,
                   'CH_13': 1529.295554, 'CH_14': 2191.007796, 'CH_15': 2209.606615, 'CH_16': 2237.159430,
                   'CH_17': 2242.434450, 'CH_18': 2387.507219, 'CH_19': 2517.407819, 'CH_20': 2667.944995,
                   'CH_21': 14431.029680, 'CH_22': 11265.161110, 'CH_23': 10601.633020, 'CH_24': 10607.324440,
                   'CH_25': 8098.870570, 'CH_26': 6061.054448}

        # 红外转tbb的修正系数，固定值
        self.TeA = {}
        self.TeB = {}
        for i in xrange(self.Band):
            BandName = 'CH_%02d' % (i + 1)
            self.TeA[BandName] = 1
            self.TeB[BandName] = 0

        # 所有通道的中心波数和对应的响应值 ，SRF
        self.waveNum = {}
        self.waveRad = {}

    def Load(self, L1File):

        #################### 读取L1文件 ######################
        print u'读取 L1所有数据信息...... %s ' % L1File
        try:

            h5File_R = h5py.File(L1File, 'r')
            ary_ch26_dn = h5File_R.get('/Data/IRAS_DN')[:]
            ary_ch26_tb = h5File_R.get('/Data/IRAS_TB')[:]
            ary_day = h5File_R.get('/Data/Scnlin_daycnt')[:]
            ary_time = h5File_R.get('/Data/Scnlin_mscnt')[:]

            ary_satz = h5File_R.get('/Geolocation/SensorZenith')[:]
            ary_sata = h5File_R.get('/Geolocation/SensorAzimuth')[:]
            ary_sunz = h5File_R.get('/Geolocation/SolarZenith')[:]
            ary_suna = h5File_R.get('/Geolocation/SolarAzimuth')[:]
            ary_lon = h5File_R.get('/Geolocation/Longitude')[:]
            ary_lat = h5File_R.get('/Geolocation/Latitude')[:]
            ary_LandCover = h5File_R.get('/Geolocation/LandCover')[:]
            ary_LandSeaMask = h5File_R.get('/Geolocation/LandSeaMask')[:]

        except Exception as e:
            print str(e)
            return

        finally:
            h5File_R.close()
        # 通道的中心波数和光谱响应
        for i in xrange(self.Band):
            BandName = 'CH_%02d' % (i + 1)
            srfFile = os.path.join(
                MainPath, 'SRF', '%s_%s_SRF_CH%02d_Pub.txt' % (self.sat, self.sensor, (i + 1)))
            dictWave = np.loadtxt(
                srfFile, dtype={'names': ('num', 'rad'), 'formats': ('f4', 'f4')})
            waveNum = dictWave['num']
            waveRad = dictWave['rad']
            self.waveNum[BandName] = waveNum
            self.waveRad[BandName] = waveRad
        ############### 数据大小 使用经度维度 ###############
        dshape = ary_lon.shape

        ##################可见光数据进行定标 ########

        for i in xrange(self.Band):
            BandName = 'CH_%02d' % (i + 1)
            # DN值存放无效值用nan填充
            DN = np.full(dshape, np.nan)
            idx = np.logical_and(ary_ch26_dn[i] < 4095, ary_ch26_dn[i] > -4095)
            DN[idx] = ary_ch26_dn[i][idx]
            self.Dn[BandName] = DN

            # Tbb值存放无效值用nan填充
            Tbb = np.full(dshape, np.nan)
            idx = np.logical_and(ary_ch26_tb[i] < 350, ary_ch26_tb[i] > 150)
            Tbb[idx] = ary_ch26_tb[i][idx]
            self.Tbb[BandName] = Tbb
            # Rad值存放无效值用nan填充
            Rad = np.full(dshape, np.nan)
            Rad = pb_sat.planck_t2r(Tbb, self.WN[BandName])
            self.Rad[BandName] = Rad

        # 全局信息赋值 ############################
        # 对时间进行赋值合并
        v_ymd2seconds = np.vectorize(fy3_ymd2seconds)
        T1 = v_ymd2seconds(ary_day, ary_time)

        Time = np.full(dshape, -999)
        for i in xrange(ary_lon.shape[0]):
            Time[i, :] = T1[i]
        if self.Time == []:
            self.Time = Time
        else:
            self.Time = np.concatenate((self.Time, Time))

        # SV, BB
        for i in xrange(self.Band):
            BandName = 'CH_%02d' % (i + 1)
            SV = np.full(dshape, np.nan)
            BB = np.full(dshape, np.nan)

            if BandName not in self.SV.keys():
                self.SV[BandName] = SV
                self.BB[BandName] = BB
            else:
                self.SV[BandName] = np.concatenate((self.SV[BandName], SV))
                self.BB[BandName] = np.concatenate((self.BB[BandName], BB))

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
        print self.sunZenith


if __name__ == '__main__':
    T1 = datetime.now()

    L1File = 'd:/data/IRAS/FY3C_IRASX_GBAL_L1_20180310_1514_017KM_MS.HDF'
    iras = CLASS_IRAS_L1()
    iras.Load(L1File)
#     iras2 = CLASS_IRAS_L1()
#     L1File = 'D:/data/FY3C_IRAS/FY3C_IRASX_GBAL_L1_20180502_0719_017KM_MS.HDF'
#     iras2.Load(L1File)
#     T2 = datetime.now()
# #     print iras.Time.shape
#     print iras.Time[0, 0]
#     print time.gmtime(iras.Time[0, 0])
#     print iras.Time[-1, -1]
#     print time.gmtime(iras.Time[-1, -1])
# #     print iras.waveRad['CH_01']
    print iras.Tbb.keys()
    print np.nanmin(iras.Rad['CH_01']), np.nanmax(iras.Rad['CH_01'])
#     for band in sorted(iras.Tbb.keys()):
#         print band, np.nanmin(iras.Tbb[band]), np.nanmax(iras.Tbb[band])
# #         print  band, np.nanmin(iras.Rad[band]), np.nanmax(iras.Rad[band])
#     print 'times:', (T2 - T1).total_seconds()
#     value = np.full(iras.Lons.shape, 999)
#     p = dv_map(figsize=(6, 5))
#     p.easyplot(
#         iras.Lats, iras.Lons, value, vmin=0, vmax=300,
#         markersize=1.5, marker='.')
#
#     p.easyplot(iras2.Lats, iras2.Lons, iras2.Tbb[
#                'CH_01'], vmin=0, vmax=300, markersize=1.5, marker='.')

#     p.savefig('test.png')
