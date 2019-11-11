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
        self.orbit_direction = ['A']
        self.orbit_num = [1]

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
        try:
            h5File_R = h5py.File(L1File, 'r')
            ary_lon = h5File_R.get('/All_Data/VIIRS-MOD-GEO_All/Longitude')[:]
            ary_lat = h5File_R.get('/All_Data/VIIRS-MOD-GEO_All/Latitude')[:]
            ary_satz = h5File_R.get(
                '/All_Data/VIIRS-MOD-GEO_All/SatelliteZenithAngle')[:]
            ary_sata = h5File_R.get(
                '/All_Data/VIIRS-MOD-GEO_All/SatelliteAzimuthAngle')[:]
            ary_sunz = h5File_R.get(
                '/All_Data/VIIRS-MOD-GEO_All/SolarZenithAngle')[:]
            ary_suna = h5File_R.get(
                '/All_Data/VIIRS-MOD-GEO_All/SolarAzimuthAngle')[:]
            ary_time = h5File_R.get('/All_Data/VIIRS-MOD-GEO_All/StartTime')[:]

            Ref = {}
            # 由于不确定性 比较了14和15年的数据值一致，定值。偶尔出现-999.33导致计算错
            Ref_a = 2.4417415E-5

            Rad = {}
            Rad_a = {}
            Rad_b = {}
            Tbb = {}
            Tbb_a = {}
            Tbb_b = {}

            for i in xrange(1, 17, 1):
                if i < 12:
                    Ref[i] = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/Reflectance' % i)[:]

                elif i == 13:
                    Rad[i] = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/Radiance' % i)[:]
                    Tbb[i] = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/BrightnessTemperature' % i)[:]
                    Rad_a[i] = 1.
                    Rad_b[i] = 0.
                    Tbb_a[i] = 1.
                    Tbb_b[i] = 0.
                else:
                    Rad[i] = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/Radiance' % i)[:]
                    Rad_a[i] = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/RadianceFactors' % i)[:][0]
                    Rad_b[i] = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/RadianceFactors' % i)[:][1]
                    Tbb[i] = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/BrightnessTemperature' % i)[:]
                    Tbb_a[i] = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/BrightnessTemperatureFactors' % i)[:][0]
                    Tbb_b[i] = h5File_R.get(
                        '/All_Data/VIIRS-M%d-SDR_All/BrightnessTemperatureFactors' % i)[:][1]

        except Exception as e:
            print str(e)
        finally:
            h5File_R.close()

        # 11111111通道的中心波数和光谱响应
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

        # 2222222 数据大小 使用经度维度 ###############
        dshape = ary_lon.shape

        # 对时间进行赋值合并
        v_ymd2seconds = np.vectorize(npp_ymd2seconds)
        T1 = v_ymd2seconds(ary_time)

        Time = np.full(dshape, -999)
        for i in xrange(len(Time)):
            Time[i, :] = T1[i / 16]
        if self.Time == []:
            self.Time = Time
        else:
            self.Time = np.concatenate((self.Time, Time))

        ymd = time.gmtime(self.Time[0, 0])
        ymd1 = time.strftime('%Y%m%d', ymd)
        dsol = sun_earth_dis_correction(ymd1)

        # 3333333 1-12通道的可见光数据进行定标 ########
        for i in range(1, 12, 1):
            BandName = 'CH_%02d' % i
            condition = np.logical_and(Ref[i] > 0, Ref[i] < 65500)
            idx = np.where(condition)
            Ref_data = np.full(dshape, np.nan)
            Ref_data[idx] = Ref[i][idx]

            Ref_data = Ref_data * Ref_a * np.cos(np.deg2rad(ary_sunz)) * dsol
            if BandName not in self.Ref.keys():
                self.Ref[BandName] = Ref_data
            else:
                self.Ref[BandName] = np.concatenate(
                    (self.Ref[BandName], Ref_data))

        # 红外
        for i in xrange(12, 17, 1):

            BandName = 'CH_%02d' % i
            condition = np.logical_and(Rad[i] > 0, Rad[i] < 65500)
            idx = np.where(condition)
            Rad_data = np.full(dshape, np.nan)
            Tbb_data = np.full(dshape, np.nan)
            Rad_data[idx] = Rad[i][idx] * \
                ((10000 / self.WN[BandName]) ** 2) / 10.
            Rad_data[idx] = Rad_data[idx] * Rad_a[i] + Rad_b[i]
            Tbb_data[idx] = Tbb[i][idx] * Tbb_a[i] + Tbb_b[i]

            if BandName not in self.Rad.keys():
                self.Rad[BandName] = Rad_data
            else:
                self.Rad[BandName] = np.concatenate(
                    (self.Rad[BandName], Rad_data))

            if BandName not in self.Tbb.keys():
                self.Tbb[BandName] = Tbb_data
            else:
                self.Tbb[BandName] = np.concatenate(
                    (self.Tbb[BandName], Tbb_data))

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
        # 角度信息
        if self.satAzimuth == []:
            self.satAzimuth = ary_sata
        else:
            self.satAzimuth = np.concatenate((self.satAzimuth, ary_sata))

        if self.satZenith == []:
            self.satZenith = ary_satz
        else:
            self.satZenith = np.concatenate((self.satZenith, ary_satz))

        if self.sunAzimuth == []:
            self.sunAzimuth = ary_suna
        else:
            self.sunAzimuth = np.concatenate((self.sunAzimuth, ary_suna))

        if self.sunZenith == []:
            self.sunZenith = ary_sunz
        else:
            self.sunZenith = np.concatenate((self.sunZenith, ary_sunz))

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

    L1File = 'D:/data/npp_viirs_cris/GMODO-SVM01-SVM02-SVM03-SVM04-SVM05-SVM06-SVM07-SVM08-SVM09-SVM10-SVM11-SVM12-SVM13-SVM14-SVM15-SVM16_npp_d20170201_t0746499_e0752303_b27282_c20180524090019614137_noaa_ops.h5'
    virr = CLASS_VIIRS_L1()
    virr.Load(L1File)
    print np.nanmin(virr.satAzimuth)
    print np.nanmax(virr.satAzimuth)
    print np.nanmin(virr.satZenith)
    print np.nanmax(virr.satZenith)
    print np.nanmin(virr.sunAzimuth)
    print np.nanmax(virr.sunAzimuth)
    print np.nanmin(virr.sunZenith)
    print np.nanmax(virr.sunZenith)
#     print np.where(np.isclose(virr.sunAzimuth, -180.))
#
    pass
