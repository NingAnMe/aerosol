# coding: utf-8

'''
Created on 2017年9月7日

@author: wangpeng
'''

from datetime import datetime
import os
import time

import h5py

from PB import pb_space
from PB.pb_sat import sun_earth_dis_correction
from PB.pb_time import npp_ymd2seconds
import numpy as np


# 配置文件信息，设置为全局
MainPath, MainFile = os.path.split(os.path.realpath(__file__))


class CLASS_VIIRS_L1():

    def __init__(self):
        self.sat = 'NPP'
        self.sensor = 'VIIRS'
        self.res = 750
        self.Band = 1
        self.obrit_direction = ['A']
        self.obrit_num = [1]

        self.Ref = {}
        self.Rad = {}
        self.Tbb = {}
        self.satAzimuth = []
        self.satZenith = []
        self.sunAzimuth = []
        self.sunZenith = []
        self.Lons = []
        self.Lats = []
        self.Time = []

        self.WN = {'CH_08': 8064.516, 'CH_09': 7256.894, 'CH_10': 6211.180, 'CH_11': 4444.444,
                   'CH_12': 2702.702, 'CH_13': 2469.135, 'CH_14': 1169.590, 'CH_15': 929.108, 'CH_16': 832.431}
        self.TeA = {'CH_15': 1.0}
        self.TeB = {'CH_15': 0}
        # 所有通道的中心波数和对应的响应值 ，SRF
        self.waveNum = {}
        self.waveRad = {}

        # 增加G矢量计算
        self.G_pos = None

    def Load(self, L1File):
        print u'读取 L1所有数据信息...... %s' % L1File
        if not os.path.isfile(L1File):
            print 'Error: %s not found' % L1File
            return
        try:
            h5File_R = h5py.File(L1File, 'r')
        except Exception as e:
            print str(e)
            return

        try:
            ary_lon = h5File_R.get('/All_Data/VIIRS-MOD-GEO_All/Longitude')[:]
            ary_lat = h5File_R.get('/All_Data/VIIRS-MOD-GEO_All/Latitude')[:]
            self.satAzimuth = h5File_R.get(
                '/All_Data/VIIRS-MOD-GEO_All/SatelliteAzimuthAngle')[:]
            self.satZenith = h5File_R.get(
                '/All_Data/VIIRS-MOD-GEO_All/SatelliteZenithAngle')[:]
            self.sunAzimuth = h5File_R.get(
                '/All_Data/VIIRS-MOD-GEO_All/SolarAzimuthAngle')[:]
            self.sunZenith = h5File_R.get(
                '/All_Data/VIIRS-MOD-GEO_All/SolarZenithAngle')[:]
#             Times = h5File_R.get('/All_Data/VIIRS-MOD-GEO_All/MidTime')[:]
            Times = h5File_R.get('/All_Data/VIIRS-MOD-GEO_All/StartTime')[:]
            tmpTime = np.full_like(ary_lon, -999, dtype='i4')
            v_ymd2seconds = np.vectorize(npp_ymd2seconds)
            T1 = v_ymd2seconds(Times)
            for i in xrange(len(tmpTime)):
                tmpTime[i, :] = T1[i / 16]
            self.Time = tmpTime
            ymd = time.gmtime(self.Time[0, 0])
            ymd = time.strftime('%Y%m%d', ymd)
#             print ymd

            dsol = sun_earth_dis_correction(ymd)

            for i in xrange(1, 17, 1):
                # sv,bb 赋值 None, 有值则赋真实值
                bandName = 'CH_%02d' % i

                if i < 12:
                    ref = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/Reflectance' % i)[:]
    #                 ref_fac = h5File_R.get('/All_Data/VIIRS-M%d-SDR_All/ReflectanceFactors' % i)[:][0]
                    # 由于不确定性 比较了14和15年的数据值一致，定值。偶尔出现-999.33导致计算错
                    ref_fac = 2.4417415E-5
                    idx = np.where(ref < 65500)
                    newRef = np.full_like(ref, np.nan, 'f4')
                    newRef[idx] = ref[idx]
                    self.Ref[bandName] = newRef * ref_fac * \
                        np.cos(np.deg2rad(self.sunZenith)) * dsol
                elif i == 13:
                    rad = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/Radiance' % i)[:]
                    tbb = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/BrightnessTemperature' % i)[:]
                    idx = np.where(rad > 0)
                    newRad = np.full_like(rad, np.nan, 'f4')
                    newRad[idx] = rad[idx] * \
                        ((10000 / self.WN['CH_%02d' % i]) ** 2) / 10.

                    idx = np.where(tbb > 0)
                    newTbb = np.full_like(tbb, np.nan, 'f4')
                    newTbb[idx] = tbb[idx]

                    self.Rad[bandName] = newRad
                    self.Tbb[bandName] = newTbb

                else:
                    rad = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/Radiance' % i)[:]
                    rad_fac = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/RadianceFactors' % i)[:][0]
                    rad_int = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/RadianceFactors' % i)[:][1]
                    tbb = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/BrightnessTemperature' % i)[:]
                    tbb_fac = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/BrightnessTemperatureFactors' % i)[:][0]
                    tbb_int = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/BrightnessTemperatureFactors' % i)[:][1]
                    idx = np.where(rad < 65500)
                    newRad = np.full_like(rad, np.nan, 'f4')
                    newRad[idx] = rad[idx] * \
                        ((10000 / self.WN['CH_%02d' % i]) ** 2) / 10.

                    idx = np.where(tbb < 65500)
                    newTbb = np.full_like(tbb, np.nan, 'f4')
                    newTbb[idx] = tbb[idx]

                    self.Rad[bandName] = newRad * rad_fac + rad_int
                    self.Tbb[bandName] = newTbb * tbb_fac + tbb_int

        except Exception as e:
            print str(e)
            return
        finally:
            h5File_R.close()

        # 通道的中心波数和光谱响应
#         for i in xrange(self.Band):
        i = 14
        BandName = 'CH_%02d' % (i + 1)
        srfFile = os.path.join(
            MainPath, 'SRF', '%s_%s_SRF_CH%02d_Pub.txt' % (self.sat, self.sensor, (i + 1)))
        dictWave = np.loadtxt(
            srfFile, dtype={'names': ('num', 'rad'), 'formats': ('f4', 'f4')})
        waveNum = 10 ** 7 / dictWave['num'][::-1]
        waveRad = dictWave['rad'][::-1]
        self.waveNum[BandName] = waveNum
        self.waveRad[BandName] = waveRad

        # 经纬度
        ary_lon_idx = np.full(ary_lon.shape, np.nan)
        condition = np.logical_and(ary_lon > -180., ary_lon < 180.)
        ary_lon_idx[condition] = ary_lon[condition]
        if self.Lons == []:
            self.Lons = ary_lon_idx
        else:
            self.Lons = np.concatenate((self.Lons, ary_lon_idx))

        ary_lat_idx = np.full(ary_lon.shape, np.nan)
        condition = np.logical_and(ary_lat > -90., ary_lat < 90.)
        ary_lat_idx[condition] = ary_lat[condition]
        if self.Lats == []:
            self.Lats = ary_lat_idx
        else:
            self.Lats = np.concatenate((self.Lats, ary_lat_idx))

    def get_G_P_L(self):

        # 增加G矢量计算
        # 第一组经纬度（成像仪）的ECEF坐标系下的值
        G_pos = np.zeros(np.append(self.Lons.shape, 3))
        high = np.zeros_like(self.Lons)
        G_pos[:, :, 0], G_pos[:, :, 1], G_pos[
            :, :, 2] = pb_space.LLA2ECEF(self.Lons, self.Lats, high)
        self.G_pos = G_pos
#         print G_pos.shape

if __name__ == '__main__':
    T1 = datetime.now()

    L1File = 'D:/data/npp_viirs_cris/GMODO-SVM01-SVM02-SVM03-SVM04-SVM05-SVM06-SVM07-SVM08-SVM09-SVM10-SVM11-SVM12-SVM13-SVM14-SVM15-SVM16_npp_d20170201_t0746499_e0752303_b27282_c20180524090019614137_noaa_ops.h5'
    virr = CLASS_VIIRS_L1()
#     virr.LutFile = 'C:\E\py_src\pysrc\OM\FY-3\exe\linux\FY3C-VIRR-LUT-TB-RB.txt'
    virr.Load(L1File)
    print virr.Rad['CH_15']
    print np.nanmin(virr.Tbb['CH_15'])
    print np.nanmax(virr.Tbb['CH_15'])
    T2 = datetime.now()
    print 'times:', (T2 - T1).total_seconds()
#
    pass
