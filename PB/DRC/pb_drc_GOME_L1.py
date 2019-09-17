# coding: utf-8
'''
Created on 2018年4月13日

@author: wangpeng
'''
# 配置文件信息，设置为全局

from datetime import datetime
import os
import sys

import beatl2
import coda

from DV import dv_map, dv_plt
from PB import pb_sat
from PB.pb_time import metop_ymd2seconds
import numpy as np


MainPath, MainFile = os.path.split(os.path.realpath(__file__))


class CLASS_GOME_L1():

    def __init__(self, BandLst):

        self.k = 1.98644746103858e-9

        # 字典类型物理量
        self.Ref = {}

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

            WAVE_3 = coda.fetch(fp, 'MDR', -1, 'Earthshine', 'WAVELENGTH_3')
            WAVE_4 = coda.fetch(fp, 'MDR', -1, 'Earthshine', 'WAVELENGTH_4')

            LAMBDA_SMR = coda.fetch(fp, 'VIADR_SMR', -1, 'LAMBDA_SMR')
            SMR = coda.fetch(fp, 'VIADR_SMR', -1, 'SMR')

            print 'gome data is:'
            print record

            self.rec = record
            SUN_Z = coda.fetch(
                fp, 'MDR', -1, 'Earthshine', 'GEO_EARTH', 'SOLAR_ZENITH')
            SUN_A = coda.fetch(
                fp, 'MDR', -1, 'Earthshine', 'GEO_EARTH', 'SOLAR_AZIMUTH')
            SAT_Z = coda.fetch(
                fp, 'MDR', -1, 'Earthshine', 'GEO_EARTH', 'SAT_ZENITH')
            SAT_A = coda.fetch(
                fp, 'MDR', -1, 'Earthshine', 'GEO_EARTH', 'SAT_AZIMUTH')

            print '太阳方位角度长度', SUN_Z.shape, SUN_Z[0].shape

            # 数据观测总长度
            dataLen = record.latitude.size
            dataCol = SUN_Z[0].shape[1]
            dataRow = SUN_Z.shape[0]

            # 角度存放内存
            self.satZenith = np.full((dataLen, 1), -999.)
            self.satAzimuth = np.full((dataLen, 1), -999.)
            self.sunZenith = np.full((dataLen, 1), -999.)
            self.sunAzimuth = np.full((dataLen, 1), -999.)

            # 开始赋值
            for i in xrange(dataLen):
                row, col = np.unravel_index(i, (dataRow, dataCol))
                self.satAzimuth[i] = SAT_A[row][1][col]
                self.satZenith[i] = SAT_Z[row][1][col]
                self.sunAzimuth[i] = SUN_A[row][1][col]
                self.sunZenith[i] = SUN_Z[row][1][col]

            self.Lons = (record.longitude).reshape(dataLen, 1)
            self.Lats = (record.latitude).reshape(dataLen, 1)

            # 计算gome的辐亮度
            self.radiance = record.spectral_radiance[:, 2048:]
            for m in xrange(dataLen):
                row, col = np.unravel_index(m, (dataRow, dataCol))
                for i in xrange(2048):
                    if i < 1024:
                        self.radiance[m, i] = self.radiance[
                            m, i] * self.k / WAVE_3[row][i]
                    else:
                        self.radiance[m, i] = self.radiance[
                            m, i] * self.k / WAVE_4[row][i - 1024]

            # 计算太阳辐亮度
            self.vec_Solar_L = np.zeros((2048,))  # 太阳辐亮度
            self.vec_Solar_WL = np.zeros((2048,))  # 太阳辐亮度对应的波长
            for i in xrange(2048):
                if i < 1024:
                    self.vec_Solar_L[i] = (
                        SMR[0][2][i] * self.k) / LAMBDA_SMR[0][2][i]
                    self.vec_Solar_WL[i] = LAMBDA_SMR[0][2][i]
                else:
                    self.vec_Solar_L[i] = (
                        SMR[0][3][i - 1024] * self.k) / LAMBDA_SMR[0][3][i - 1024]
                    self.vec_Solar_WL[i] = LAMBDA_SMR[0][3][i - 1024]

            print 'GOME数据观测长度 %d' % dataLen
            print '太阳辐亮度波长最小最大值'
            print np.min(self.vec_Solar_WL), self.vec_Solar_WL[0:3]
            print np.max(self.vec_Solar_WL)
            # 暂时取一个观测的光谱波数
            self.wavenumber = record.wavelength[0, 2048:]
            print 'GOME辐亮度波长最小最大值,取第一个观测点'
            print np.min(self.wavenumber), self.wavenumber[0:3]
            print np.max(self.wavenumber)
            self.wavenumber = record.wavelength[9, 2048:]
            print 'GOME辐亮度波长最小最大值，取第十个观测点'
            print np.min(self.wavenumber), self.wavenumber[0:3]
            print np.max(self.wavenumber)

            v_ymd2seconds = np.vectorize(metop_ymd2seconds)
            T1 = v_ymd2seconds(record.time)
            self.Time = T1.reshape(dataLen, 1)
#             print time.gmtime(self.Time[0, 0])

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
            # gome 不需要用波数，使用波长即可
            WaveNum1 = 10 ** 7 / D1.waveNum[Band][::-1]
            WaveRad1 = D1.waveRad[Band][::-1]
            WaveRad2 = pb_sat.spec_interp(WaveNum1, WaveRad1, WaveNum2)
            newRad = pb_sat.spec_convolution(WaveNum2, WaveRad2, self.radiance)
            newRad = newRad.reshape(newRad.size, 1)

            # 把数据1光谱插值到gome的太阳光谱
            WaveRad2_solar = pb_sat.spec_interp(WaveNum1, WaveRad1, WaveNum2)
#             WaveRad2_solar = D1.spec_interp(WaveNum1, WaveRad1, self.vec_Solar_WL)
            newRad_solar = pb_sat.spec_convolution(
                self.vec_Solar_WL, WaveRad2_solar, self.vec_Solar_L)

#             ref = newRad / newRad_solar * np.pi / np.cos(np.deg2rad(self.sunZenith))
            ref = np.pi * newRad / newRad_solar

            self.Ref[Band] = ref.reshape(ref.size, 1)

    def get_rad_tbb_test(self, D1, bandLst):
        '''
        D1是目标类的实例
        '''
        # iasi 的光谱波数范围
        WaveNum2 = self.wavenumber
        for Band in bandLst:
            # 把gome的通道做成390-800波段间隔1纳米的响应
            solar_x = np.arange(390, 801, 1)

            WaveNum1 = 10 ** 7 / D1.waveNum[Band][::-1]
            WaveRad1 = D1.waveRad[Band][::-1]
            y = pb_sat.spec_interp(WaveNum1, WaveRad1, solar_x)
#             newRad = D1.spec_convolution(WaveNum2, WaveRad2, self.radiance)
#             newRad = newRad.reshape(newRad.size, 1)
#             print newRad.shape
            # 太阳光谱插值
            WaveRad2_solar = pb_sat.spec_interp(
                WaveNum1, WaveRad1, self.vec_Solar_WL)
            # 太阳光谱插值
            # 把gome的通道做成390-800波段间隔1纳米的响应
            solar_x = np.arange(390, 801, 1)
            solar_y1 = pb_sat.spec_interp(
                self.vec_Solar_WL[0:1024], self.vec_Solar_L[0:1024], solar_x)
            solar_y2 = pb_sat.spec_interp(
                self.vec_Solar_WL[1024:], self.vec_Solar_L[1024:], solar_x)
            solar_y = solar_y1 + solar_y2
            solar_rad = pb_sat.spec_convolution(solar_x, y, solar_y)
            print solar_y
            print np.min(WaveRad2_solar)
            print np.max(WaveRad2_solar)
#             print WaveRad2_solar
#             np.savetxt('testall.txt', WaveRad2_solar)
#             np.savetxt('test.txt', WaveRad2_solar)
#             WaveRad2_solar = D1.spec_interp(WaveNum1, WaveRad1, self.vec_Solar_WL)
#             newRad_solar = D1.spec_convolution(self.vec_Solar_WL, WaveRad2_solar, self.vec_Solar_L)

            gome_rad1 = self.radiance[:, 0:1024]
            gome_rad2 = self.radiance[:, 1024:]
            print gome_rad1.shape
            print gome_rad2.shape
            gome_y1_lst = []
            gome_y2_lst = []
            for i in xrange(self.radiance.shape[0]):
                gome_y1 = pb_sat.spec_interp(
                    self.wavenumber[0:1024], self.radiance[i, 0:1024], solar_x)
                gome_y2 = pb_sat.spec_interp(
                    self.wavenumber[1024:], self.radiance[i, 1024:], solar_x)
                gome_y1_lst.append(gome_y1)
                gome_y2_lst.append(gome_y2)
            gome_y = np.array(gome_y1_lst) + np.array(gome_y2_lst)
            gome_rad = pb_sat.spec_convolution(solar_x, y, gome_y)
            gome_rad = gome_rad.reshape(gome_rad.size, 1)

            ref = gome_rad / solar_rad * np.pi / \
                np.cos(np.deg2rad(self.sunZenith))

            self.Ref[Band] = ref.reshape(ref.size, 1)


if __name__ == '__main__':
    T1 = datetime.now()
    BandLst = ['CH_01', 'CH_02', 'CH_03', 'CH_08', 'CH_09',
               'CH_10', 'CH_11', 'CH_12', 'CH_13', 'CH_14']
    L1File = 'D:/data/GOME/GOME_xxx_1B_M02_20160109122059Z_20160109122359Z_N_O_20160109134314Z__20160109135951'
#     L1File = 'D:/data/METOP/GOME_xxx_1B_M02_20160109122059Z_20160109122359Z_N_O_20160109134314Z__20160109135951'
#     L1File = 'D:/data/METOP/GOME_xxx_1B_M02_20160110102058Z_20160110102358Z_N_O_20160110114353Z__20160110115057'
    gome = CLASS_GOME_L1(BandLst)
    gome.Load(L1File)
#     gome.get_rad_tbb(D1, BandLst)
    T2 = datetime.now()
    print 'times:', (T2 - T1).total_seconds()

#     oFile = 'C:/Users/wangpeng/Desktop/gome.png'
#     box = [-60., -90., -110., -30.]
#     p = dv_plt.dv_scatter(figsize=(6, 5))
#     p.easyplot(gome.wavenumber, gome.radiance[222], None, None, marker='o', markersize=5)
#     p.savefig(oFile, dpi=300)
    pass
