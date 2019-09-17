# coding: utf-8
'''
Created on 2017年9月7日

@author: wangpeng
'''
# 配置文件信息，设置为全局

from datetime import datetime
import os
import sys
import time

import h5py

from DV import dv_plt
from PB import pb_sat
from PB import pb_space
from PB.pb_time import npp_ymd2seconds
import numpy as np


MainPath, MainFile = os.path.split(os.path.realpath(__file__))


class CLASS_CRIS_L1():

    def __init__(self, BandLst):

        self.sat = 'NPP'
        self.sensor = 'CRIS'
        self.res = 15000

        self.orbit_direction = ['A']
        self.orbit_num = [1]
        self.pixel_num = []

        # 字典类型物理量
        self.Tbb = {}
        self.Rad = {}

        # 二维矩阵
        self.Lons = []
        self.Lats = []
        self.Time = []

        self.satAzimuth = []
        self.satZenith = []
        self.satRange = []
        self.sunAzimuth = []
        self.sunZenith = []

        self.wavenumber = []
        # gapfilling 之前的波数
        self.wavenumber_old = []
        self.radiance = []
        # gapfilling 之前的辐射
        self.radiance_old = []

        # 增加矢量计算 G,P,L
        self.G_pos = None
        self.L_pos = None
        self.P_pos = None

    def LoadFull(self, L1File):

        # 增加切趾计算
        w0 = 0.23
        w1 = 0.54
        w2 = 0.23

        print u'读取 L1所有数据信息...... %s ' % L1File
        if not os.path.isfile(L1File):
            print 'Error: %s not found' % L1File
            return
        try:
            h5File_R = h5py.File(L1File, 'r')
            self.Lons = h5File_R.get('/All_Data/CrIS-SDR-GEO_All/Longitude')[:]
            self.Lats = h5File_R.get('/All_Data/CrIS-SDR-GEO_All/Latitude')[:]
            self.satAzimuth = h5File_R.get(
                '/All_Data/CrIS-SDR-GEO_All/SatelliteAzimuthAngle')[:]
            self.satZenith = h5File_R.get(
                '/All_Data/CrIS-SDR-GEO_All/SatelliteZenithAngle')[:]
            self.satRange = h5File_R.get(
                '/All_Data/CrIS-SDR-GEO_All/SatelliteRange')[:]
            self.sunAzimuth = h5File_R.get(
                '/All_Data/CrIS-SDR-GEO_All/SolarAzimuthAngle')[:]
            self.sunZenith = h5File_R.get(
                '/All_Data/CrIS-SDR-GEO_All/SolarZenithAngle')[:]
            real_lw = h5File_R.get('/All_Data/CrIS-FS-SDR_All/ES_RealLW')[:]
            real_mw = h5File_R.get('/All_Data/CrIS-FS-SDR_All/ES_RealMW')[:]
            real_sw = h5File_R.get('/All_Data/CrIS-FS-SDR_All/ES_RealSW')[:]
            Times = h5File_R.get('/All_Data/CrIS-SDR-GEO_All/FORTime')[:]

        except Exception as e:
            print str(e)
            return
        finally:
            h5File_R.close()

        Times2 = np.full_like(self.Lons, -999.)
        for i in xrange(Times2.shape[-1]):
            Times2[:, :, i] = Times
        print '111', Times2[0, 0, 0], Times[0, 0]
        v_ymd2seconds = np.vectorize(npp_ymd2seconds)
        T1 = v_ymd2seconds(Times2)
        print '11', T1[0, 0, 0]
        print '11', T1[-1, -1, -1]

        self.Lons = self.Lons.reshape(self.Lons.size, 1)
        self.Lats = self.Lats.reshape(self.Lats.size, 1)
        self.satAzimuth = self.satAzimuth.reshape(self.satAzimuth.size, 1)
        self.satZenith = self.satZenith.reshape(self.satZenith.size, 1)
        self.satRange = self.satRange.reshape(self.satRange.size, 1)
        self.sunAzimuth = self.sunAzimuth.reshape(self.sunAzimuth.size, 1)
        self.sunZenith = self.sunZenith.reshape(self.sunZenith.size, 1)
        self.Time = T1.reshape(T1.size, 1)

#         print u'切趾计算 w0*n-1 + w1*n + w2*n+1 当作n位置的修正值'
        # 开头和结尾不参与计算
        real_lw[:, :, :, 1:-1] = w0 * real_lw[:, :, :, :-2] + \
            w1 * real_lw[:, :, :, 1:-1] + w2 * real_lw[:, :, :, 2:]
        real_mw[:, :, :, 1:-1] = w0 * real_mw[:, :, :, :-2] + \
            w1 * real_mw[:, :, :, 1:-1] + w2 * real_mw[:, :, :, 2:]
        real_sw[:, :, :, 1:-1] = w0 * real_sw[:, :, :, :-2] + \
            w1 * real_sw[:, :, :, 1:-1] + w2 * real_sw[:, :, :, 2:]

        real_lw = real_lw[:, :, :, 2:-2]
        real_mw = real_mw[:, :, :, 2:-2]
        real_sw = real_sw[:, :, :, 2:-2]
#         print u'删除开头和结尾的俩个光谱 ', real_lw.shape

        # 波数范围和步长  # 全分辨率的暂时不对 应该是2211
        wave_lw = np.arange(650., 1095.0 + 0.625, 0.625)
        wave_mw = np.arange(1210.0, 1750 + 0.625, 0.625)
        wave_sw = np.arange(2155.0, 2550.0 + 0.625, 0.625)

        self.wavenumber_old = np.concatenate((wave_lw, wave_mw, wave_sw))
#         self.wavenumber = np.concatenate((wave_lw, wave_mw, wave_sw))
#         self.radiance = np.concatenate((real_lw, real_mw, real_sw), axis=3)
        self.radiance_old = np.concatenate((real_lw, real_mw, real_sw), axis=3)
#         lens = self.radiance.shape[-1]
        lens = self.radiance_old.shape[-1]
        self.radiance_old = self.radiance_old.reshape(
            self.radiance_old.size / lens, lens)
        print '222', self.wavenumber_old.shape, self.radiance_old.shape

        self.real_lw = real_lw.reshape(
            real_lw.size / real_lw.shape[-1], real_lw.shape[-1])
        self.real_mw = real_mw.reshape(
            real_mw.size / real_mw.shape[-1], real_mw.shape[-1])
        self.real_sw = real_sw.reshape(
            real_sw.size / real_sw.shape[-1], real_sw.shape[-1])

    def Load(self, L1File):

        # 增加切趾计算
        w0 = 0.23
        w1 = 1 - 2 * w0
        w2 = w0

        print u'读取 L1所有数据信息...... %s ' % L1File
        if not os.path.isfile(L1File):
            print 'Error: %s not found' % L1File
            return
        try:
            h5File_R = h5py.File(L1File, 'r')
            self.Lons = h5File_R.get('/All_Data/CrIS-SDR-GEO_All/Longitude')[:]
            self.Lats = h5File_R.get('/All_Data/CrIS-SDR-GEO_All/Latitude')[:]
            self.satAzimuth = h5File_R.get(
                '/All_Data/CrIS-SDR-GEO_All/SatelliteAzimuthAngle')[:]
            self.satZenith = h5File_R.get(
                '/All_Data/CrIS-SDR-GEO_All/SatelliteZenithAngle')[:]
            self.satRange = h5File_R.get(
                '/All_Data/CrIS-SDR-GEO_All/SatelliteRange')[:]
            self.sunAzimuth = h5File_R.get(
                '/All_Data/CrIS-SDR-GEO_All/SolarAzimuthAngle')[:]
            self.sunZenith = h5File_R.get(
                '/All_Data/CrIS-SDR-GEO_All/SolarZenithAngle')[:]
            real_lw = h5File_R.get('/All_Data/CrIS-SDR_All/ES_RealLW')[:]
            real_mw = h5File_R.get('/All_Data/CrIS-SDR_All/ES_RealMW')[:]
            real_sw = h5File_R.get('/All_Data/CrIS-SDR_All/ES_RealSW')[:]
            Times = h5File_R.get('/All_Data/CrIS-SDR-GEO_All/FORTime')[:]

        except Exception as e:
            print str(e)
            return

        finally:
            h5File_R.close()

        # 记录探元号
        pixel_num = np.full(self.Lons.shape, 0, 'i1')
        for i in xrange(9):
            pixel_num[:, :, i] = i + 1

        self.pixel_num = pixel_num
        self.pixel_num = self.pixel_num.reshape(self.Lons.size, 1)

        Times2 = np.full_like(self.Lons, -999.)
        for i in xrange(Times2.shape[-1]):
            Times2[:, :, i] = Times
        v_ymd2seconds = np.vectorize(npp_ymd2seconds)
        T1 = v_ymd2seconds(Times2)

        self.Lons = self.Lons.reshape(self.Lons.size, 1)
        self.Lats = self.Lats.reshape(self.Lats.size, 1)
        self.satAzimuth = self.satAzimuth.reshape(self.satAzimuth.size, 1)
        self.satZenith = self.satZenith.reshape(self.satZenith.size, 1)
        self.satRange = self.satRange.reshape(self.satRange.size, 1)
        self.sunAzimuth = self.sunAzimuth.reshape(self.sunAzimuth.size, 1)
        self.sunZenith = self.sunZenith.reshape(self.sunZenith.size, 1)

        self.Time = T1.reshape(T1.size, 1)

        # 切趾计算 w0*n-1 + w1*n + w2*n+1 当作n位置的修正值
        # 开头和结尾不参与计算
        real_lw[:, :, :, 1:-1] = w0 * real_lw[:, :, :, :-2] + \
            w1 * real_lw[:, :, :, 1:-1] + w2 * real_lw[:, :, :, 2:]
        real_mw[:, :, :, 1:-1] = w0 * real_mw[:, :, :, :-2] + \
            w1 * real_mw[:, :, :, 1:-1] + w2 * real_mw[:, :, :, 2:]
        real_sw[:, :, :, 1:-1] = w0 * real_sw[:, :, :, :-2] + \
            w1 * real_sw[:, :, :, 1:-1] + w2 * real_sw[:, :, :, 2:]

        real_lw = real_lw[:, :, :, 2:-2]
        real_mw = real_mw[:, :, :, 2:-2]
        real_sw = real_sw[:, :, :, 2:-2]
#         print u'删除开头和结尾的俩个光谱 ', real_lw.shape

        # 波数范围和步长
        wave_lw = np.arange(650., 1095.0 + 0.625, 0.625)
        wave_mw = np.arange(1210.0, 1750 + 1.25, 1.25)
        wave_sw = np.arange(2155.0, 2550.0 + 2.5, 2.5)

        self.wavenumber = np.concatenate((wave_lw, wave_mw, wave_sw))
        self.radiance = np.concatenate((real_lw, real_mw, real_sw), axis=3)
        lens = self.radiance.shape[-1]
        self.radiance = self.radiance.reshape(self.radiance.size / lens, lens)

    def gapFilling(self):
        #         print 'gapFilling before:'
        #         print self.wavenumber_old.shape
        #         print self.radiance_old.shape
        gapFile = os.path.join(MainPath, 'COEFF', 'cris_fs.GapCoeff.h5')
        h5File_R = h5py.File(gapFile, 'r')
        c0 = h5File_R.get('C0')[:]
        p0 = h5File_R.get('P0')[:]
        gapNum = h5File_R.get('GAP_NUM')[:]
        radiance_new = np.dot(self.radiance_old, p0)
#         print 'radiance_new', radiance_new.shape
        radiance_new = radiance_new + c0
        ch_part1 = gapNum[0]
        ch_part2 = gapNum[0] + gapNum[1]
        ch_part3 = gapNum[0] + gapNum[1] + gapNum[2]
        real_lw_e = radiance_new[:, 0:ch_part1]
        real_mw_e = radiance_new[:, ch_part1:ch_part2]
        real_sw_e = radiance_new[:, ch_part2:ch_part3]
        self.wavenumber = np.arange(650., 2755.0 + 0.625, 0.625)
        self.radiance = np.concatenate(
            (self.real_lw, real_lw_e, self.real_mw, real_mw_e, self.real_sw, real_sw_e), axis=1)
        print '1', self.wavenumber.shape
        print self.radiance.shape
#         real_lw_e = np.full_like(real_lw_e, np.nan)
#         real_mw_e = np.full_like(real_mw_e, np.nan)
#         real_sw_e = np.full_like(real_sw_e, np.nan)
#         self.radiance_old = np.concatenate((self.real_lw, real_lw_e, self.real_mw, real_mw_e, self.real_sw, real_sw_e), axis=1)
#         print 'gapFilling after:'
#         print self.wavenumber.shape
#         print self.radiance.shape
#         self.radiance = np.insert(self.radiance, [1], radiance_new, axis=1)

    def get_G_P_L(self):

        # 增加G矢量计算
        # 第一组经纬度（成像仪）的ECEF坐标系下的值
        G_pos = np.zeros(np.append(self.Lons.shape, 3))
        high = np.zeros_like(self.Lons)
        G_pos[:, :, 0], G_pos[:, :, 1], G_pos[
            :, :, 2] = pb_space.LLA2ECEF(self.Lons, self.Lats, high)
        self.G_pos = G_pos

        # compute CrIS LOS Vector1   局地球面坐标系 RAE--->东-北-天坐标系ENU
        cris_east, cris_north, cris_up = pb_space.RAE2ENU(
            self.satAzimuth, self.satZenith, self.satRange)
        print cris_east.shape
        # compute CrIS LOS Vector2   东-北-天坐标系ENU--->地球中心地球固定坐标系ECEF
        L_pos = np.zeros(np.append(self.Lons.shape, 3))
        L_pos[:, :, 0], L_pos[:, :, 1], L_pos[:, :, 2] = \
            pb_space.ENU2ECEF(
                cris_east, cris_north, cris_up, self.Lons, self.Lats)

        L_pos = L_pos * -1.0
        self.L_pos = L_pos
        self.P_pos = G_pos - L_pos

    def get_rad_tbb(self, D1, bandLst):
        '''
        D1是目标类的实例
        '''
        # iasi 的光谱波数范围
        WaveNum2 = self.wavenumber
        for Band in bandLst:
            WaveNum1 = D1.waveNum[Band]
            WaveRad1 = D1.waveRad[Band]
            WaveRad2 = pb_sat.spec_interp(WaveNum1, WaveRad1, WaveNum2)
            newRad = pb_sat.spec_convolution(WaveNum2, WaveRad2, self.radiance)
            tbb = pb_sat.planck_r2t(newRad, D1.WN[Band])
            tbb = tbb * D1.TeA[Band] + D1.TeB[Band]
            self.Tbb[Band] = tbb.reshape(tbb.size, 1)
            self.Rad[Band] = newRad.reshape(newRad.size, 1)

if __name__ == '__main__':
    T1 = datetime.now()

    BandLst = ['CH_20', 'CH_21', 'CH_22', 'CH_23', 'CH_24', 'CH_25']

    L1File = 'D:/data/CRIS/GCRSO-SCRIF-SCRIS_npp_d20180303_t0016319_e0024297_b32881_c20180308030857410779_noac_ops.h5'
#     L1File = 'D:/data/npp_virrs_cris/GCRSO-SCRIS_npp_d20170201_t0741439_e0749417_b27282_c20180524085728187103_noaa_ops.h5'
    cris = CLASS_CRIS_L1(BandLst)
    cris.LoadFull(L1File)
#     cris.Load(L1File)
    cris.get_G_P_L()
#     print '1', cris.G_pos
#     print '2', cris.L_pos
#     print '3', cris.P_pos
    cris.gapFilling()
#     cris.get_rad_tbb(D1 BandLst)
    T2 = datetime.now()
    print 'times:', (T2 - T1).total_seconds()
    print cris.wavenumber.shape
    print cris.wavenumber_old.shape
    print cris.radiance.shape
    print cris.radiance_old.shape
    print np.nanmin(cris.radiance), np.nanmax(cris.radiance)

    print time.gmtime(cris.Time[0, 0])
    print time.gmtime(cris.Time[-1, -1])
    p = dv_plt.dv_scatter(figsize=(7, 5))
    p.xlim_min = 650
    p.xlim_max = 2755

    p.easyplot(cris.wavenumber, cris.radiance[
               0], 'b', 'after', marker='o', markersize=5)
    p.easyplot(cris.wavenumber_old, cris.radiance_old[
               0], 'r', 'before', marker='o', markersize=5)

    p.title = u'20180415 17:46:39 NPP FULL CRIS'
    p.xlabel = u'波长'
    p.ylabel = u'radince'
    ofile = 'D:/data/CRIS/gapFilling_after1.png'
    p.savefig(ofile, dpi=300)
    pass
