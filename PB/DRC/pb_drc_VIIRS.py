# -*- coding: utf-8 -*-

from datetime import datetime
import os
import re
import time

import h5py

from PB.pb_io import attrs2dict
from PB.pb_sat import sun_earth_dis_correction
from pb_drc_base import ReadL1
import numpy as np


__description__ = u'MERSI传感器读取类'
__author__ = 'wangpeng'
__date__ = '2018-08-28'
__version__ = '1.0.0_beat'


g_main_path, g_main_file = os.path.split(os.path.realpath(__file__))


class ReadViirsL1(ReadL1):
    """
    读取 VIIRS 传感器的 L1 数据
    分辨率：750m
    卫星： [NPP]
    通道数量：20
    可见光通道：1,2,3,4,6~20
    红外通道：5
    """

    def __init__(self, in_file):
        sensor = 'VIIRS'
        super(ReadViirsL1, self).__init__(in_file, sensor)

    def set_resolution(self):
        """
        use filename set self.resolution
        :return:
        """
        file_name = os.path.basename(self.in_file)
        if 'SVM01' in file_name:
            self.resolution = 750
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def set_satellite(self):
        """
        use filename set self.satellite
        :return:
        """

        file_name = os.path.basename(self.in_file)
        if 'npp' in file_name:
            self.satellite = 'NPP'
        elif 'j01' in file_name:
            self.satellite = 'JPSS-1'

    def set_ymd_hms(self):
        """
        use filename  set self.ymd self.hms
        """
        file_name = os.path.basename(self.in_file)
        pat = u'(GDNBO|GMODO|GMTCO|SVM\d{2}).*_\w{3}_d(\d{8})_t(\d{7})_e(\d{7})_b\d{5}_c\d{20}_\w{4}_ops.h5$'
        g = re.match(pat, file_name)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3)[:-1]  # 舍弃秒的小数点以后位
        else:
            raise ValueError('Cant get the ymdhms from file name.')

    def set_file_attr(self):
        """
        use hdf5 file set self.file_attr
        :return:
        """
        if self.resolution == 750:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                with h5py.File(self.in_file, 'r') as h5r:
                    self.file_attr = attrs2dict(h5r.attrs)
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                "Cant handle this resolution: ".format(self.resolution))

    def set_data_shape(self):
        """
        use file dataset dims set self.data_shape
        :return:
        """
        # 如果分辨率是 1000 米
        if self.resolution == 750:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                self.data_shape = (3072, 3200)
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        # elif self.resolution == 250:
        else:
            raise ValueError(
                "Cant handle this resolution: ".format(self.resolution))

    def set_channels(self):
        """
        return sensor channels
        """
        if self.resolution == 750:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                self.channels = 16
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def get_central_wave_number(self):
        '''
        return 中心波数
        central_wave_number
        wn(cm-1) = 10 ^ 7 / wave_length(nm)
        '''
        if self.resolution == 750:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                #                 data = {'CH_08': 8064.516, 'CH_09': 7256.894, 'CH_10': 6211.180, 'CH_11': 4444.444,
                #                         'CH_12': 2702.702, 'CH_13': 2469.135, 'CH_14': 1169.590, 'CH_15': 929.108, 'CH_16': 832.431}
                data = {'CH_12': 2702.702, 'CH_13': 2469.135,
                        'CH_14': 1169.590, 'CH_15': 929.108, 'CH_16': 832.431}
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

#     def get_dn(self):
#         """
#         return DN
#         """
#         pass

    def get_ref(self):
        """
        return Ref
        """

        data = dict()
        if self.resolution == 750:  # 分辨率为 1000
            satellite_type1 = ['NPP', 'JPSS-1']
            # NPP
            if self.satellite in satellite_type1:
                data_file = self.in_file
                with h5py.File(data_file, 'r') as h5r:
                    # 前12个通道是ref
                    # 由于不确定性 比较了14和15年的数据值一致，定值。偶尔出现-999.33导致计算错
                    ref_a = 2.4417415E-5
                    vmin = 0
                    vmax = 60000
                    sunz = self.get_solar_zenith()
                    dsl = sun_earth_dis_correction(self.ymd)
                    # 逐个通道处理
                    for i in xrange(1, 12, 1):
                        band = 'CH_{:02d}'.format(i)
                        sds_name = '/All_Data/VIIRS-M%d-SDR_All/Reflectance' % i
                        ary_ref = h5r.get(sds_name)[:]

                        data_pre = ary_ref
                        data_pre = data_pre.astype(np.float32)
                        invalid_index = np.logical_or(
                            data_pre <= vmin, data_pre > vmax)
                        data_pre[invalid_index] = np.nan
                        # 修正，徐娜提供
                        data[band] = data_pre * ref_a * \
                            np.cos(np.deg2rad(sunz)) * dsl

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

#     def get_ref_test(self):
#         """
#         return Ref
#         """
#
#         data = dict()
#         if self.resolution == 750:  # 分辨率为 1000
#             satellite_type1 = ['NPP']
#             # NPP
#             if self.satellite in satellite_type1:
#                 data_file = self.in_file
#                 with h5py.File(data_file, 'r') as h5r:
#                     # 前12个通道是ref
#                     # 由于不确定性 比较了14和15年的数据值一致，定值。偶尔出现-999.33导致计算错
#                     ref_a = 2.4417415E-5
#                     vmin = 0
#                     vmax = 60000
#                     sunz = self.get_sensor_zenith()
#                     dsl = sun_earth_dis_correction(self.ymd)
#                     # 逐个通道处理
#                     for i in xrange(1, 12, 1):
#                         band = 'CH_{:02d}'.format(i)
#                         sds_name = '/All_Data/VIIRS-M%d-SDR_All/Reflectance' % i
#                         ary_ref = h5r.get(sds_name)[:]
#
#                         data_pre = ary_ref
#                         data_pre = data_pre.astype(np.float32)
#                         invalid_index = np.logical_or(
#                             data_pre <= vmin, data_pre > vmax)
#                         data_pre[invalid_index] = np.nan
#                         # 修正，徐娜提供
#                         data[band] = data_pre * ref_a * \
#                             np.cos(np.deg2rad(sunz))
#
#             else:
#                 raise ValueError(
#                     'Cant read this satellite`s data.: {}'.format(self.satellite))
#         else:
#             raise ValueError(
#                 'Cant read this data, please check its resolution: {}'.format(self.in_file))
#         return data
    def get_rad_test(self):
        """
        return rad
        """

        data = dict()
        if self.resolution == 750:  # 分辨率为 1000
            satellite_type1 = ['NPP', 'JPSS-1']
            # NPP
            if self.satellite in satellite_type1:
                data_file = self.in_file
                with h5py.File(data_file, 'r') as h5r:
                    vmin = 0
                    vmax = 65000
                    center_wn = self.get_central_wave_number()
                    # 逐个通道处理
                    for i in xrange(1, 12, 1):
                        band = 'CH_{:02d}'.format(i)
                        sds_name1 = '/All_Data/VIIRS-M%d-SDR_All/Radiance' % i
                        ary_rad = h5r.get(sds_name1)[:]

                        if i == 3 or i == 4 or i == 5 or i == 7:
                            k1 = 1.
                            k0 = 0.
                        else:
                            sds_name2 = '/All_Data/VIIRS-M%d-SDR_All/RadianceFactors' % i
                            print sds_name2
                            ary_coeff = h5r.get(sds_name2)[:]
                            k1 = ary_coeff[0]
                            k0 = ary_coeff[1]

                        data_pre = ary_rad
                        data_pre = data_pre.astype(np.float32)
                        invalid_index = np.logical_or(
                            data_pre <= vmin, data_pre > vmax)
                        data_pre[invalid_index] = np.nan
                        # 修正，徐娜提供
#                         data_pre = data_pre * \
#                             ((10000 / center_wn[band]) ** 2) / 10.
                        data[band] = data_pre * k1 + k0

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_rad(self):
        """
        return rad
        """

        data = dict()
        if self.resolution == 750:  # 分辨率为 1000
            satellite_type1 = ['NPP', 'JPSS-1']
            # NPP
            if self.satellite in satellite_type1:
                data_file = self.in_file
                with h5py.File(data_file, 'r') as h5r:
                    vmin = 0
                    vmax = 60000
                    center_wn = self.get_central_wave_number()
                    # 逐个通道处理
                    for i in xrange(12, 17, 1):
                        band = 'CH_{:02d}'.format(i)
                        sds_name1 = '/All_Data/VIIRS-M%d-SDR_All/Radiance' % i
                        ary_rad = h5r.get(sds_name1)[:]

                        if i == 13:
                            k1 = 1.
                            k0 = 0.
                        else:
                            sds_name2 = '/All_Data/VIIRS-M%d-SDR_All/RadianceFactors' % i
                            ary_coeff = h5r.get(sds_name2)[:]
                            k1 = ary_coeff[0]
                            k0 = ary_coeff[1]

                        data_pre = ary_rad
                        data_pre = data_pre.astype(np.float32)
                        invalid_index = np.logical_or(
                            data_pre <= vmin, data_pre > vmax)
                        data_pre[invalid_index] = np.nan
                        # 修正，徐娜提供
                        data_pre = data_pre * \
                            ((10000 / center_wn[band]) ** 2) / 10.
                        data[band] = data_pre * k1 + k0

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_tbb(self):
        """
        return tbb
        """

        data = dict()
        if self.resolution == 750:  # 分辨率为 1000
            satellite_type1 = ['NPP', 'JPSS-1']
            # NPP
            if self.satellite in satellite_type1:
                data_file = self.in_file
                with h5py.File(data_file, 'r') as h5r:
                    vmin = 0
                    vmax = 65000
                    # 逐个通道处理
                    for i in xrange(12, 17, 1):
                        band = 'CH_{:02d}'.format(i)
                        sds_name1 = '/All_Data/VIIRS-M%d-SDR_All/BrightnessTemperature' % i
                        ary_tbb = h5r.get(sds_name1)[:]

                        if i == 13:
                            k1 = 1.
                            k0 = 0.
                        else:
                            sds_name2 = '/All_Data/VIIRS-M%d-SDR_All/BrightnessTemperatureFactors' % i
                            ary_coeff = h5r.get(sds_name2)[:]
                            k1 = ary_coeff[0]
                            k0 = ary_coeff[1]

                        data_pre = ary_tbb
                        data_pre = data_pre.astype(np.float32)
                        invalid_index = np.logical_or(
                            data_pre <= vmin, data_pre > vmax)
                        data_pre[invalid_index] = np.nan
                        data[band] = data_pre * k1 + k0

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_longitude(self):
        """
        return longitude
        """
        if self.resolution == 750:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/All_Data/VIIRS-MOD-GEO_All/Longitude')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < -180, data_pre > 180)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre

        return data

    def get_latitude(self):
        """
        return latitude
        """
        if self.resolution == 750:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/All_Data/VIIRS-MOD-GEO_All/Latitude')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < -90, data_pre > 90)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre

        return data

    def get_sensor_azimuth(self):
        """
        return sensor_azimuth
        """
        if self.resolution == 750:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/All_Data/VIIRS-MOD-GEO_All/SatelliteAzimuthAngle')[:]
                vmin = -180.
                vmax = 180.
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre

        return data

    def get_sensor_zenith(self):
        """
        return sensor_zenith
        """
        if self.resolution == 750:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                vmin = 0.
                vmax = 180.
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/All_Data/VIIRS-MOD-GEO_All/SatelliteZenithAngle')[:]

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre

        return data

    def get_solar_azimuth(self):
        """
        return solar_azimuth
        """
        if self.resolution == 750:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/All_Data/VIIRS-MOD-GEO_All/SolarAzimuthAngle')[:]

                vmin = -180.
                vmax = 180.

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre

        return data

    def get_solar_zenith(self):
        """
        return solar_zenith
        """
        if self.resolution == 750:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                vmin = 0.
                vmax = 180.
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/All_Data/VIIRS-MOD-GEO_All/SolarZenithAngle')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre

        return data

    def get_timestamp(self):
        """
        return from 1970-01-01 00:00:00 seconds
        """
        if self.resolution == 750:
            satellite_type1 = ['NPP', 'JPSS-1']
            s = self.data_shape
            if self.satellite in satellite_type1:
                with h5py.File(self.in_file, 'r') as h5r:
                    sds_name = '/All_Data/VIIRS-MOD-GEO_All/StartTime'
                    ary_time = h5r.get(sds_name)[:]
                    ary_time = ary_time / 1000000.
                    # npp VIIRS和 CRIS 数据的时间单位是距离 1958年1月1日 UTC时间的microseconds 微秒
                    # ymdhms/1000000 = 秒  （距离1958年1月1日 UTC时间）
                    secs = int(
                        (datetime(1970, 1, 1, 0, 0, 0) - datetime(1958, 1, 1, 0, 0, 0)).total_seconds())
                    # 返回距离1970-01-01的秒
                    ary_time = ary_time - secs

                    data = np.repeat(ary_time, 16 * s[1])
                    data = data.reshape(s)
                    data = data.astype(np.int32)

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

if __name__ == '__main__':
    T1 = datetime.now()

    L1File = 'D:/data/VIIRS/GMODO-SVM01-SVM02-SVM03-SVM04-SVM05-SVM06-SVM07-SVM08-SVM09-SVM10-SVM11-SVM12-SVM13-SVM14-SVM15-SVM16_npp_d20170201_t0746499_e0752303_b27282_c20180524090019614137_noaa_ops.h5'
    viirs = ReadViirsL1(L1File)
    print viirs.satellite  # 卫星名
    print viirs.sensor  # 传感器名
    print viirs.ymd  # L1 文件年月日 YYYYMMDD
    print viirs.hms  # L1 文件时分秒 HHMMSS
    print viirs.resolution  # 分辨率
    print viirs.channels  # 通道数量
    print viirs.data_shape
#     print viirs.file_attr

    def print_data_status(datas, name=None):
        data_shape = datas.shape
        data_min = np.nanmin(datas)
        data_max = np.nanmax(datas)
        data_mean = np.nanmean(datas)
        data_median = np.nanmedian(datas)
        print "{}: shape: {}, min: {}, max: {}, mean: {}, median: {}".format(
            name, data_shape, data_min, data_max, data_mean, data_median)

    def print_channel_data(datas):
        if not isinstance(datas, dict):
            return
        keys = list(datas.viewkeys())
        keys.sort()
        for t_channel_name in keys:
            channel_data = datas[t_channel_name]
            print_data_status(channel_data, name=t_channel_name)
#     print 'tbb:'
#     t_data = viirs.get_tbb()
#     print_channel_data(t_data)
#     print 'timestamp:'
#     t_data = viirs.get_timestamp()
#     print type(t_data)
#     print type(t_data[0, 0])
#     print time.gmtime(t_data[0, 0])
#     print time.gmtime(t_data[-1, -1])
#     print_data_status(t_data)
#
#     print 'longitude:'
#     t_data = viirs.get_longitude()
#     print_data_status(t_data)
#
#     print 'latitude:'
#     t_data = viirs.get_latitude()
#     print_data_status(t_data)

#     print 'sensor_azimuth:'
#     t_data1 = viirs.get_sensor_azimuth()
#     print_data_status(t_data1)
#     print 'sensor_zenith:'
#     t_data2 = viirs.get_sensor_zenith()
#     print_data_status(t_data2)
#     print 'solar_azimuth:'
#     t_data3 = viirs.get_solar_azimuth()
#     print_data_status(t_data3)
#     print 'solar_zenith:'
#     t_data4 = viirs.get_solar_zenith()
#     print_data_status(t_data4)

    print 'ref:'
    ref = viirs.get_ref()
    print_channel_data(ref)

    print 'rad:'
    rad = viirs.get_rad_test()
    print_channel_data(rad)
    src_band = ['CH_02', 'CH_05', 'CH_06', 'CH_07', 'CH_08',
                'CH_09', 'CH_10', 'CH_12', 'CH_14', 'CH_15']
    des_band = ['CH_04', 'CH_09', 'CH_10', 'CH_11', 'CH_01',
                'CH_02', 'CH_03', 'CH_05', 'CH_06', 'CH_07']

    dict_value = {}
    for key1, key2 in zip(src_band, des_band):
        #         print key1, key2
        tt = rad[key2] / ref[key2]
#         print rad[key][1000, 1000], ref[key][1000, 1000]
        print 'FY3D/MERSI band: %s' % key1, tt[1000, 1000]
