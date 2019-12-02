# coding: utf-8

'''
Created on 2017年9月7日

@author: wangpeng
'''

from datetime import datetime
import os
import re
import sys

from pyhdf.SD import SD, SDC

from PB import pb_name, pb_space
from PB.pb_sat import sun_earth_dis_correction
import numpy as np


class CLASS_MODIS_L1():

    def __init__(self):

        self.Tbb = {}
        self.Rad = {}
        self.Ref = {}
        self.satAzimuth = []
        self.satZenith = []
        self.sunAzimuth = []
        self.sunZenith = []
        self.Lons = []
        self.Lats = []
        self.Time = []

    def Load(self, L1File):
        print (u'读取 L1 %s' % L1File)
        iname = os.path.basename(L1File)
        try:
            h4File = SD(L1File, SDC.READ)
            # 读取1-2通道数据
            in_data_r250 = h4File.select('EV_250_Aggr1km_RefSB').get()
            in_data_r250_s = h4File.select(
                'EV_250_Aggr1km_RefSB').attributes()['reflectance_scales']
            in_data_r250_o = h4File.select(
                'EV_250_Aggr1km_RefSB').attributes()['reflectance_offsets']
            # 读取 3-7通道数据
            in_data_r500 = h4File.select('EV_500_Aggr1km_RefSB').get()
            in_data_r500_s = h4File.select(
                'EV_500_Aggr1km_RefSB').attributes()['reflectance_scales']
            in_data_r500_o = h4File.select(
                'EV_500_Aggr1km_RefSB').attributes()['reflectance_offsets']
            # 读取8-20通道， 包含26通道
            in_data_r1km = h4File.select('EV_1KM_RefSB').get()
            in_data_r1km_s = h4File.select(
                'EV_1KM_RefSB').attributes()['reflectance_scales']
            in_data_r1km_o = h4File.select(
                'EV_1KM_RefSB').attributes()['reflectance_offsets']
            # 读取20-36通道 不包含26通道
            in_data_t1km = h4File.select('EV_1KM_Emissive').get()
            in_data_t1km_s = h4File.select(
                'EV_1KM_Emissive').attributes()['radiance_scales']
            in_data_t1km_o = h4File.select(
                'EV_1KM_Emissive').attributes()['radiance_offsets']
        except Exception as e:
            print str(e)
        finally:
            h4File.end()

        # 处理定位文件，默认和L1同一目录
        geoFile = find_modis_geo_file(L1File)
        print (u'读取geo %s' % geoFile)
        try:
            h4File = SD(geoFile, SDC.READ)
            in_data_lon = h4File.select('Longitude').get()
            in_data_lat = h4File.select('Latitude').get()
# in_data_LandSeaMask = h4File.select('Land/SeaMask').get()  #
# 多个/不知道是bug还是就这样
            in_data_satz = h4File.select('SensorZenith').get()
            in_data_satz_s = h4File.select(
                'SensorZenith').attributes()['scale_factor']
            in_data_sata = h4File.select('SensorAzimuth').get()
            in_data_sata_s = h4File.select(
                'SensorAzimuth').attributes()['scale_factor']
            in_data_sunz = h4File.select('SolarZenith').get()
            in_data_sunz_s = h4File.select(
                'SolarZenith').attributes()['scale_factor']
            in_data_suna = h4File.select('SolarAzimuth').get()
            in_data_suna_s = h4File.select(
                'SolarAzimuth').attributes()['scale_factor']

        except Exception as e:
            print (str(e))

        finally:
            h4File.end()

        # 角度信息赋值
        self.satZenith = in_data_satz * in_data_satz_s
        self.satAzimuth = in_data_sata * in_data_sata_s
        self.sunZenith = in_data_sunz * in_data_sunz_s
        self.sunAzimuth = in_data_suna * in_data_suna_s
        self.Lons = in_data_lon
        self.Lats = in_data_lat

        # 1-2通道
        modis_shape2 = in_data_r250.shape[1:]
        modis_shape3 = (38,) + modis_shape2

        data_ch38 = np.full(modis_shape3, np.nan)

        for i in range(0, 2, 1):
            # 过滤 无效值
            condition = np.logical_and(
                in_data_r250[i] < 32767, in_data_r250[i] > 0)
            idx = np.where(condition)
            data_ch38[i][idx] = (
                in_data_r250[i][idx] - in_data_r250_o[i]) * in_data_r250_s[i]
        # 3-7通道
        for i in range(2, 7, 1):
            # 过滤 无效值
            condition = np.logical_and(
                in_data_r500[i - 2] < 32767, in_data_r500[i - 2] > 0)
            idx = np.where(condition)
            data_ch38[i][idx] = (
                in_data_r500[i - 2][idx] - in_data_r500_o[i - 2]) * in_data_r500_s[i - 2]
        # 8-19 外加一个 26通道
        for i in range(7, 22, 1):
            # 过滤 无效值
            condition = np.logical_and(
                in_data_r1km[i - 7] < 32767, in_data_r1km[i - 7] > 0)
            idx = np.where(condition)
            data_ch38[i][idx] = (
                in_data_r1km[i - 7][idx] - in_data_r1km_o[i - 7]) * in_data_r1km_s[i - 7]
        # 20-36通道  不包括26通道
        for i in range(22, 38, 1):
            # 过滤 无效值
            condition = np.logical_and(
                in_data_t1km[i - 22] < 32767, in_data_t1km[i - 22] > 0)
            idx = np.where(condition)
            data_ch38[i][idx] = (
                in_data_t1km[i - 22][idx] - in_data_t1km_o[i - 22]) * in_data_t1km_s[i - 22]

        # 删除通道13hi 和  14 hi
        data_ch36 = np.delete(data_ch38, (13, 15), 0)
        newRad = np.full((16,) + modis_shape2, np.nan)

        # 对时间进行赋值
        self.Time = np.full(modis_shape2, -999.)
        nameClass = pb_name.nameClassManager()
        info = nameClass.getInstance(iname)
        ymd = info.dt_s.strftime('%Y%m%d')
        print ymd
        dsol = sun_earth_dis_correction(ymd)
#         print info.dt_s
        secs = int((info.dt_s - datetime(1970, 1, 1, 0, 0, 0)).total_seconds())
        self.Time[:] = secs

        # 把modis的rad转换和FY的单位一致
        # 20,21, 22,23,24,25,27,28,29,30,31,32,33,34,35,36
        cwn = [2.647409E+03, 2.511760E+03, 2.517908E+03, 2.462442E+03,
               2.248296E+03, 2.209547E+03, 1.474262E+03, 1.361626E+03,
               1.169626E+03, 1.028740E+03, 9.076813E+02, 8.308411E+02,
               7.482978E+02, 7.307766E+02, 7.182094E+02, 7.035007E+02]

        for i in xrange(16):
            newRad[i] = data_ch36[i + 20] * \
                ((10000 / cwn[i]) ** 2) / 10. * dsol
            if i <= 5:
                self.Rad['CH_%02d' % (i + 20)] = newRad[i]
            else:
                self.Rad['CH_%02d' % (i + 21)] = newRad[i]

        # 把rad转亮温
        radiance2tbb(data_ch36)

        # 整理通道顺序，把26通道放在对应的26通道
#         del26_data_ch35 = np.delete(data_ch36, 19, 0)
#         order_data_ch36 = np.insert(del26_data_ch35, 25, data_ch36[19], 0)

        for i in range(0, 36, 1):
            #             self.CA['CH_%02d' % (i + 1)] = order_data_ch36[i]
            if i < 19:
                self.Ref['CH_%02d' % (i + 1)] = data_ch36[i]
            elif i == 19:  # 26
                self.Ref['CH_%02d' % (i + 7)] = data_ch36[i]
            elif i > 19 and i <= 25:
                self.Tbb['CH_%02d' % (i)] = data_ch36[i]
            elif i > 25:
                self.Tbb['CH_%02d' % (i + 1)] = data_ch36[i]


def find_modis_geo_file(L1File):

    # 根据输入的L1文件自动拼接GEO文件
    ipath = os.path.dirname(L1File)
    iname = os.path.basename(L1File)

    part1 = iname.split('.')[1]
    part2 = iname.split('.')[2]
    pat = u'\w{5}.%s.%s.\d{3}.\d+.hdf$' % (part1, part2)

    if os.path.isdir(ipath):
        Lst = sorted(os.listdir(ipath), reverse=False)
        for line in Lst:
            m = re.match(pat, line)
            if m:
                geoFile = os.path.join(ipath, line)
                break
    else:
        print '%s not found' % ipath
        sys.exit(-1)

    return geoFile


def radiance2tbb(r):
    '''
    function radiance2tbb: convert radiance data into brightness temperature (i.e., equivalent blackbody temperature)
    r: spectral radiance data in w/m2/sr/um
    w: wavelength in micro
    return: reture value, brightness temperature in K (absolute temperature)
    '''

    cwn = [2.647409E+03, 2.511760E+03, 2.517908E+03, 2.462442E+03,
           2.248296E+03, 2.209547E+03, 1.474262E+03, 1.361626E+03,
           1.169626E+03, 1.028740E+03, 9.076813E+02, 8.308411E+02,
           7.482978E+02, 7.307766E+02, 7.182094E+02, 7.035007E+02]

    tcs = [9.993363E-01, 9.998626E-01, 9.998627E-01, 9.998707E-01,
           9.998737E-01, 9.998770E-01, 9.995694E-01, 9.994867E-01,
           9.995270E-01, 9.997382E-01, 9.995270E-01, 9.997271E-01,
           9.999173E-01, 9.999070E-01, 9.999198E-01, 9.999233E-01]

    tci = [4.818401E-01, 9.426663E-02, 9.458604E-02, 8.736613E-02,
           7.873285E-02, 7.550804E-02, 1.848769E-01, 2.064384E-01,
           1.674982E-01, 8.304364E-02, 1.343433E-01, 7.135051E-02,
           1.948513E-02, 2.131043E-02, 1.804156E-02, 1.683156E-02]

    h = 6.62606876e-34  # Planck constant (Joule second)
    c = 2.99792458e+8
    k = 1.3806503e-23

    c1 = 2.0 * h * c * c
    c2 = (h * c) / k

#     ws = 1.0e-6 * w  # Convert wavelength to meters
#     tbb = np.full_like(r, -999.)
    # Compute brightness temperature
    for i in range(20, 36, 1):

        #         index = np.where(r[i] > 0.)
        index = np.where(~np.isnan(r[i]))
        w = 1.0e+4 / cwn[i - 20]
        ws = 1.0e-6 * w
#         r[i] = c2 / (ws * np.log(c1 / (1.0e+6 * r[i] * ws ** 5) + 1.0))
#         r[i] = (r[i] - tci[i - 20]) / tcs[i - 20]
        r[i, index[0], index[1]] = c2 / \
            (ws *
             np.log(c1 / (1.0e+6 * r[i, index[0], index[1]] * ws ** 5) + 1.0))
        r[i, index[0], index[1]] = (
            r[i, index[0], index[1]] - tci[i - 20]) / tcs[i - 20]


if __name__ == '__main__':
    L1File = 'D:/data/MODIS/MYD021KM.A2017003.0750.006.2017004153232.hdf'
    modis = CLASS_MODIS_L1()
    modis.Load(L1File)

    for key in sorted(modis.Ref.keys()):
        print "%s, %0.6f %0.6f" % (key, np.nanmin(modis.Ref[key]), np.nanmax(modis.Ref[key]))

    for key in sorted(modis.Rad.keys()):
        print "%s, %0.6f %0.6f" % (key, np.nanmin(modis.Rad[key]), np.nanmax(modis.Rad[key]))

    for key in sorted(modis.Tbb.keys()):
        print "%s, %0.6f %0.6f" % (key, np.nanmin(modis.Tbb[key]), np.nanmax(modis.Tbb[key]))
