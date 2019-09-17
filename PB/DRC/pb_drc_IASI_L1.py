# coding: utf-8

'''
Created on 2017年9月7日

@author: wangpeng
'''
# 配置文件信息，设置为全局

from datetime import datetime
import os
import sys

import beatl2
import coda

from DV.dv_map import dv_map
from PB import pb_sat
from PB.pb_time import metop_ymd2seconds
import numpy as np


MainPath, MainFile = os.path.split(os.path.realpath(__file__))


class CLASS_IASI_L1():

    def __init__(self, BandLst):

        # 字典类型物理量
        self.Tbb = {}
        self.Rad = {}

        # 二维矩阵
        self.Lons = []
        self.Lats = []
        self.Time = []

        self.satAzimuth = []
        self.satZenith = []
        self.sunAzimuth = []
        self.sunZenith = []

        # 光谱信息
        self.wavenumber = []
        self.radiance = []

    def Load(self, L1File):

        print u'读取 LEO所有数据信息......'
        if not os.path.isfile(L1File):
            print 'Error: %s not found' % L1File
            sys.exit(1)

        try:
            fp = coda.open(L1File)
        except Exception, e:
            print 'Open file error<%s> .' % (e)
            return

        try:
            # EPS = EUMETSAT Polar System atmospheric products (GOME-2 and IASI)
            # EPS = EUMETSAT极地大气系统产品（GOME-2和IASI）'
            # 获取文件头信息
            product_class = coda.get_product_class(fp)
            product_type = coda.get_product_type(fp)
            product_version = coda.get_product_version(fp)
            product_format = coda.get_product_format(fp)
            product_size = coda.get_product_file_size(fp)
            print 'product_class ', product_class
            print 'product_type', product_type
            print 'product_version', product_version
            print 'product_format', product_format
            print 'product_size', product_size
            record = beatl2.ingest(L1File)
            print record

            SAT_angle = coda.fetch(fp, 'MDR', -1, 'MDR', 'GGeoSondAnglesMETOP')
            SUN_angle = coda.fetch(fp, 'MDR', -1, 'MDR', 'GGeoSondAnglesSUN')
            all_sun_angle = []
            all_sat_angle = []

            for i in xrange(len(SAT_angle)):
                tmp_sat = SAT_angle[i].reshape(-1)
                tmp_sun = SUN_angle[i].reshape(-1)
                if len(all_sat_angle) == 0:
                    all_sat_angle = tmp_sat
                    all_sun_angle = tmp_sun
                else:
                    all_sat_angle = np.concatenate((all_sat_angle, tmp_sat), 0)
                    all_sun_angle = np.concatenate((all_sun_angle, tmp_sun), 0)

            iasiLen = len(record.longitude)
            self.satZenith = (all_sat_angle[0::2]).reshape(iasiLen, 1)
            self.satAzimuth = (all_sat_angle[1::2]).reshape(iasiLen, 1)
            self.sunZenith = (all_sun_angle[0::2]).reshape(iasiLen, 1)
            self.sunAzimuth = (all_sun_angle[1::2]).reshape(iasiLen, 1)

            self.Lons = (record.longitude).reshape(iasiLen, 1)
            self.Lats = (record.latitude).reshape(iasiLen, 1)

            self.radiance = record.spectral_radiance * 10 ** 7

            # 暂时取一个观测的光谱波数
            self.wavenumber = record.wavenumber[0, :]

            v_ymd2seconds = np.vectorize(metop_ymd2seconds)
            T1 = v_ymd2seconds(record.time)
            self.Time = T1.reshape(iasiLen, 1)

        except Exception as e:
            print str(e)
            sys.exit(1)
        finally:
            coda.close(fp)

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
            tbb = pb_sat.planck_r2t(
                newRad, D1.WN[Band], D1.TeA[Band], D1.TeB[Band])

            self.Tbb[Band] = tbb.reshape(tbb.size, 1)
            self.Rad[Band] = newRad.reshape(newRad.size, 1)

if __name__ == '__main__':
    T1 = datetime.now()
    BandLst = ['CH_20', 'CH_21', 'CH_22', 'CH_23', 'CH_24', 'CH_25']
    L1File = 'D:/data/IASI/IASI_xxx_1C_M02_20180502060857Z_20180502061152Z_N_O_20180502072426Z__20180502072755'
    iasi1 = CLASS_IASI_L1(BandLst)
    iasi1.Load(L1File)
    L1File = 'D:/data/IASI/IASI_xxx_1C_M02_20180502061153Z_20180502061456Z_N_O_20180502072518Z__20180502072850'
    iasi2 = CLASS_IASI_L1(BandLst)
    iasi2.Load(L1File)
    L1File = 'D:/data/IASI/IASI_xxx_1C_M02_20180502061457Z_20180502061752Z_N_O_20180502072608Z__20180502073251'
    iasi3 = CLASS_IASI_L1(BandLst)
    iasi3.Load(L1File)
#
#     lons = np.concatenate((iasi1.Lons, iasi2.Lons, iasi3.Lons))
#     lats = np.concatenate((iasi1.Lats, iasi2.Lats, iasi3.Lats))
#
# #     iasi.get_rad_tbb('FY3D', 'MERSI2', BandLst)
#     T2 = datetime.now()
#     print 'times:', (T2 - T1).total_seconds()
#     value = np.full(lons.shape, 111)
#     p = dv_map(figsize=(6, 5))
#     p.easyplot(
#         lats, lons, value, vmin=0, vmax=300, markersize=1.5, marker='.')
#     p.savefig('test.png')
#     pass
