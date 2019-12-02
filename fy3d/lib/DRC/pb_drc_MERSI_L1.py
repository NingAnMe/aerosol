# coding: utf-8

'''
Created on 2017年9月7日

@author: wangpeng
'''
# 获取类py文件所在的目录

import os
import sys
import time

import h5py

from PB import pb_sat
from PB.pb_time import fy3_ymd2seconds
import numpy as np

MainPath, MainFile = os.path.split(os.path.realpath(__file__))


class CLASS_MERSI_L1():

    '''
    1km的mersi2数据类
    '''

    def __init__(self):

        # 定标使用
        self.sat = 'FY3C'
        self.sensor = 'MERSI'
        self.res = 1000
        self.Band = 20
        self.obrit_direction = []
        self.obrit_num = []
        self.Dn = {}
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
        self.SV = {}
        self.BB = {}
        self.LandSeaMask = []
        self.LandCover = []

        # 其他程序使用
        self.LutFile = []
        self.IR_Coeff = []
        self.VIS_Coeff = []

        # 红外通道的中心波数，固定值，MERSI_Equiv Mid_wn (cm-1)
        self.WN = {'CH_05': 869.565}
        # 红外转tbb的修正系数，固定值
        self.TeA = {'CH_05': 1}
        self.TeB = {'CH_05': 0}
        # 所有通道的中心波数和对应的响应值 ，SRF
        self.waveNum = {}
        self.waveRad = {}

    def Load(self, L1File):
        ipath = os.path.dirname(L1File)
        iname = os.path.basename(L1File)
        geoFile = os.path.join(ipath, iname[0:-12] + 'GEO1K_MS.HDF')
        print L1File
        print geoFile

        if 'FY3C' in iname[:4]:
            try:
                h5File_R = h5py.File(L1File, 'r')
                ary_ch1 = h5File_R.get('/Data/EV_250_Aggr.1KM_RefSB')[:]
                ary_ch5 = h5File_R.get('/Data/EV_250_Aggr.1KM_Emissive')[:]
                ary_ch6 = h5File_R.get('/Data/EV_1KM_RefSB')[:]
                ary_Cal_Coeff = h5File_R.get('/Calibration/VIS_Cal_Coeff')[:]
                ary_svdn = h5File_R.get('/Calibration/SV_DN_average')[:]
                ary_bbdn = h5File_R.get('/Calibration/BB_DN_average')[:]
            except Exception as e:
                print str(e)
                return
            finally:
                h5File_R.close()

                print sys.getsizeof(ary_ch1) / 1024. / 1024.

            try:
                # 读取GEO文件
                h5File_R = h5py.File(geoFile, 'r')
                ary_satz = h5File_R.get('/Geolocation/SensorZenith')[:]
                ary_sata = h5File_R.get('/Geolocation/SensorAzimuth')[:]
                ary_sunz = h5File_R.get('/Geolocation/SolarZenith')[:]
                ary_suna = h5File_R.get('/Geolocation/SolarAzimuth')[:]
                ary_lon = h5File_R.get('/Geolocation/Longitude')[:]
                ary_lat = h5File_R.get('/Geolocation/Latitude')[:]
                ary_LandCover = h5File_R.get('/Geolocation/LandCover')[:]
                ary_LandSeaMask = h5File_R.get('/Geolocation/LandSeaMask')[:]
                ary_day = h5File_R.get('/Timedata/Day_Count')[:]
                ary_time = h5File_R.get('/Timedata/Millisecond_Count')[:]
            except Exception as e:
                print str(e)
                return
            finally:
                h5File_R.close()
        else:
            # FY3A/FY3B MERSI
            # 读取L1文件
            try:
                h5File_R = h5py.File(L1File, 'r')
                ary_lon = h5File_R.get('/Longitude')[:]
                ary_lat = h5File_R.get('/Latitude')[:]
                ary_svdn = np.full_like(ary_lon, -999.)
                ary_bbdn = np.full_like(ary_lon, -999.)
                ary_ch1 = h5File_R.get('/EV_250_Aggr.1KM_RefSB')[:]
                ary_ch5 = h5File_R.get('/EV_250_Aggr.1KM_Emissive')[:]
                ary_ch6 = h5File_R.get('/EV_1KM_RefSB')[:]
                ary_Cal_Coeff = h5File_R.attrs['VIR_Cal_Coeff']

                ary_satz = h5File_R.get('/SensorZenith')[:]
                ary_sata = h5File_R.get('/SensorAzimuth')[:]
                ary_sunz = h5File_R.get('/SolarZenith')[:]
                ary_suna = h5File_R.get('/SolarAzimuth')[:]
                ary_LandCover = h5File_R.get('/LandCover')[:]
                ary_LandSeaMask = h5File_R.get('/LandSeaMask')[:]
                ary_svdn = h5File_R.get('/SV_DN_average')[:]
                ary_bbdn = h5File_R.get('/BB_DN_average')[:]

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
                srfFile, dtype = {'names': ('num', 'rad'), 'formats': ('f4', 'f4')})
            waveNum = 10 ** 7 / dictWave['num'][::-1]
            waveRad = dictWave['rad'][::-1]
            self.waveNum[BandName] = waveNum
            self.waveRad[BandName] = waveRad

        # 数据大小 使用经度维度 ###############
        dshape = ary_lon.shape

        # 通道信息赋值  #######################
        # 读取FY3C查找表
#         LutAry = np.loadtxt(self.LutFile, dtype={'names': ('TBB', '05'),
#                             'formats': ('i4', 'f4')})

        # RefSB_Cal_Coefficients这个属性直接写到hdf中，FY3A/B 需要转成19*3
        proj_Cal_Coeff = np.full((19, 3), -999.)
        if 'FY3C' in iname[:4]:
            proj_Cal_Coeff = ary_Cal_Coeff
        else:
            for i in range(19):
                for j in range(3):
                    proj_Cal_Coeff[i, j] = ary_Cal_Coeff[i * 3 + j]
        # 定标系数 19*3 转  20*3
        values = np.array([0, 0, 0])
        K = np.insert(ary_Cal_Coeff, 4, values, 0)

        # 可见
        for i in xrange(self.Band):
            BandName = 'CH_%02d' % (i + 1)
            if i < 4:
                DN = np.full(dshape, np.nan)
                idx = np.logical_and(ary_ch1[i] < 10000, ary_ch1[i] >= 0)
                DN[idx] = ary_ch1[i][idx]
                Ref = (DN ** 2 * K[i, 2] + DN * K[i, 1] + K[i, 0]) / 100.
                self.Dn[BandName] = DN
                self.Ref[BandName] = Ref
            elif i > 4:
                k = i - 5
                DN = np.full(dshape, np.nan)
                idx = np.logical_and(ary_ch6[k] < 10000, ary_ch6[k] >= 0)
                DN[idx] = ary_ch6[k][idx]
                Ref = (DN ** 2 * K[i, 2] + DN * K[i, 1] + K[i, 0]) / 100.
                self.Dn[BandName] = DN
                self.Ref[BandName] = Ref
            # 红外
            elif i == 4:
                # 数据空间
                DN = np.full(dshape, np.nan)
                Rad = np.full(dshape, np.nan)
                Tbb = np.full(dshape, np.nan)
                # 过滤无效值
                idx = np.logical_and(ary_ch5 < 10000, ary_ch5 >= 0)
                # dn
                DN[idx] = ary_ch5[idx]
                self.Dn[BandName] = DN
                # rad
                Rad = DN / 100.
                self.Rad[BandName] = Rad

                # tbb
                Tbb = pb_sat.planck_r2t(
                    Rad, self.WN[BandName], self.TeA[BandName], self.TeB[BandName])
                self.Tbb[BandName] = Tbb

#                 CA = interpolate.InterpolatedUnivariateSpline(LutAry['%02d' % (i + 1)], LutAry['TBB'])(DN[idx])
#                 self.CA[BandName] = np.full(dshape, np.nan)
#                 self.CA[BandName][idx] = CA / 100.

        # 全局信息赋值 ############################
        # 对时间进行赋值合并
        v_ymd2seconds = np.vectorize(fy3_ymd2seconds)
        T1 = v_ymd2seconds(ary_day, ary_time)

        Time = np.full(dshape, -999)
        for i in xrange(ary_lon.shape[0]):
            Time[i, :] = T1[i / 10, 0]
        if self.Time == []:
            self.Time = Time
        else:
            self.Time = np.concatenate((self.Time, Time))

        # SV, BB
        for i in xrange(self.Band):
            SV = np.full(dshape, np.nan)
            BB = np.full(dshape, np.nan)
            for j in xrange(ary_lon.shape[0]):
                SV[j, :] = ary_svdn[i][j / 10]
                BB[j, :] = ary_bbdn[i][j / 10]

            SV = np.ma.masked_where(SV < 0, SV)
            BB = np.ma.masked_where(BB < 0, BB)
            np.ma.filled(SV, np.nan)
            np.ma.filled(BB, np.nan)

            # 数据合并
            BandName = 'CH_%02d' % (i + 1)
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
        condition = np.logical_and(ary_sata > 0, ary_sata < 36000)
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
        condition = np.logical_and(ary_suna > 0, ary_suna < 36000)
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


if __name__ == '__main__':
#     L1File = 'D:/data/FY3C_MERSI/FY3C_MERSI_GBAL_L1_20150223_2340_1000M_MS.HDF'
#     mersi = CLASS_MERSI_L1()
#     mersi.Load(L1File)
#     print mersi.Dn['CH_05']
#     print mersi.Rad['CH_05']
#     print mersi.Tbb['CH_05']
#     print mersi.sunZenith[1000, 1000]
#     print time.gmtime(mersi.Time[0, 0])

    in_file = r'C:\Users\wape2\Desktop\FY3D_MERSI_GBAL_L1_20050501_1105_1000M_MS.HDF'
    h5r = h5py.File(in_file, 'r')
    print h5r.keys()
    pass
