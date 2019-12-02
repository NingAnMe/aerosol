# -*- coding: utf-8 -*-

from datetime import datetime
import os
import re
import time

import h5py

from DV import dv_plt, dv_map
from PB.pb_io import attrs2dict
from PB.pb_sat import planck_r2t, spec_interp, spec_convolution
from pb_drc_MERSI import ReadMersiL1
from pb_drc_base import ReadL1
import numpy as np


__description__ = u'CRIS传感器读取'
__author__ = 'wangpeng'
__date__ = '2018-09-03'
__version__ = '1.0.0_beat'

g_main_path, g_main_file = os.path.split(os.path.realpath(__file__))


class ReadCrisL1(ReadL1):
    """
    读取 IASI 传感器的 L1 数据
    分辨率：16KM | 3 x 3 14 km IFOV covering a 48 x 48 km2 cell (average sampling distance: 16 km)
    卫星： [NPP]
    通道数量：
    红外通道：2211(no gapfilling) or 3369(this gapfilling)
    """

    def __init__(self, in_file):
        sensor = 'CRIS'
        super(ReadCrisL1, self).__init__(in_file, sensor)

    def set_resolution(self):
        """
        use filename set self.resolution
        """
        file_name = os.path.basename(self.in_file)
        if 'CRIS' in file_name:
            self.resolution = 16000
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def set_satellite(self):
        """
        set satellite name self.satellite
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
        pat = u'\w{5}-.*_\w{3}_d(\d{8})_t(\d{7})_e(\d{7})_b(\d{5})_c(\d{20})_\w{4}_ops.h5$'
        g = re.match(pat, file_name)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2)[:-1]  # 舍弃秒的小数点以后位
        else:
            raise ValueError('Cant get the ymdhms from file name.')

    def set_file_attr(self):
        """
        get hdf5 file attrs self.file_attr
        :return:
        """
        if self.resolution == 16000:
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
        if self.resolution == 16000:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                self.data_shape = (16200, 1)
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
        if self.resolution == 16000:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                self.channels = 3369
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def get_spectral_response(self):
        #         if int(self.ymd) >= 20180423:
        if int(self.ymd) >= 20180201:
            print 'high........'
            # 临时测试
#             wave_nums2, response = self.get_spectral_response_low()
            # 业务
            wave_nums2, response = self.get_spectral_response_high()
        else:
            print 'low........'
            wave_nums2, response = self.get_spectral_response_low()
        return wave_nums2, response

    def get_spectral_response_high(self):
        """
        return 光谱波数和响应值，1维，2维
        """

        if self.resolution == 16000:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:

                s = self.data_shape
                # 增加切趾计算
                w0 = 0.23
                w1 = 0.54
                w2 = 0.23
                data_file = self.in_file
                with h5py.File(data_file, 'r') as h5r:
                    sds_name = '/All_Data/CrIS-FS-SDR_All/ES_RealLW'
                    real_lw = h5r.get(sds_name)[:]

                    sds_name = '/All_Data/CrIS-FS-SDR_All/ES_RealMW'
                    real_mw = h5r.get(sds_name)[:]

                    sds_name = '/All_Data/CrIS-FS-SDR_All/ES_RealSW'
                    real_sw = h5r.get(sds_name)[:]

                # 切趾计算 w0*n-1 + w1*n + w2*n+1 当作n位置的修正值
                # 开头和结尾不参与计算
                real_lw[:, :, :, 1:-1] = w0 * real_lw[:, :, :, :-2] + \
                    w1 * real_lw[:, :, :, 1:-1] + w2 * real_lw[:, :, :, 2:]
                real_mw[:, :, :, 1:-1] = w0 * real_mw[:, :, :, :-2] + \
                    w1 * real_mw[:, :, :, 1:-1] + w2 * real_mw[:, :, :, 2:]
                real_sw[:, :, :, 1:-1] = w0 * real_sw[:, :, :, :-2] + \
                    w1 * real_sw[:, :, :, 1:-1] + w2 * real_sw[:, :, :, 2:]
                print real_lw.shape
                real_lw = real_lw[:, :, :, 2:-2]
                real_mw = real_mw[:, :, :, 2:-2]
                real_sw = real_sw[:, :, :, 2:-2]
                print real_lw.shape

                # 波数范围和步长
                wave_number = np.arange(650., 2755.0 + 0.625, 0.625)
#                 wave_lw = np.arange(650., 1095.0 + 0.625, 0.625)
#                 wave_mw = np.arange(1210.0, 1750 + 0.625, 0.625)
#                 wave_sw = np.arange(2155.0, 2550.0 + 0.625, 0.625)
#                 wave_number_old = np.concatenate(
#                     (wave_lw, wave_mw, wave_sw))
                response_old = np.concatenate(
                    (real_lw, real_mw, real_sw), axis=3)
                print response_old.shape
                last_s = response_old.shape[-1]
                # 16200*最后一个光谱维度
                response_old = response_old.reshape(s[0], last_s)
#                 self.test_w = wave_number_old
#                 self.test_r = response_old
#                 print '23', response_old.shape
                data_file = os.path.join(
                    g_main_path, 'COEFF', 'cris_fs.GapCoeff.h5')

                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as h5r:
                    c0 = h5r.get('C0')[:]
                    p0 = h5r.get('P0')[:]
                    gapNum = h5r.get('GAP_NUM')[:]

                response_new = np.dot(response_old, p0)
                response_new = response_new + c0
                print '4', response_new.shape
                ch_part1 = gapNum[0]
                ch_part2 = gapNum[0] + gapNum[1]
                ch_part3 = gapNum[0] + gapNum[1] + gapNum[2]
                real_lw_e = response_new[:, 0:ch_part1]
                real_mw_e = response_new[:, ch_part1:ch_part2]
                real_sw_e = response_new[:, ch_part2:ch_part3]

                # 把原响应值 维度转成2维
                real_lw = real_lw.reshape(s[0], real_lw.shape[-1])
                real_mw = real_mw.reshape(s[0], real_mw.shape[-1])
                real_sw = real_sw.reshape(s[0], real_sw.shape[-1])
                response = np.concatenate(
                    (real_lw, real_lw_e, real_mw, real_mw_e, real_sw, real_sw_e), axis=1)
                s = response.shape
                response = response.reshape(s[0], 1, s[1])
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return wave_number, response

    def get_spectral_response_low(self):
        """
        return 光谱波数和响应值，1维，2维, 处理低分辨率的cris
        """

        if self.resolution == 16000:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:

                s = self.data_shape
                # 增加切趾计算
                w0 = 0.23
                w1 = 1 - 2 * w0
                w2 = w0
                data_file = self.in_file
                with h5py.File(data_file, 'r') as h5r:
                    sds_name = '/All_Data/CrIS-SDR_All/ES_RealLW'
                    real_lw = h5r.get(sds_name)[:]

                    sds_name = '/All_Data/CrIS-SDR_All/ES_RealMW'
                    real_mw = h5r.get(sds_name)[:]

                    sds_name = '/All_Data/CrIS-SDR_All/ES_RealSW'
                    real_sw = h5r.get(sds_name)[:]

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

                # 波数范围和步长
                wave_lw = np.arange(650., 1095.0 + 0.625, 0.625)
                wave_mw = np.arange(1210.0, 1750 + 1.25, 1.25)
                wave_sw = np.arange(2155.0, 2550.0 + 2.5, 2.5)

                wave_number = np.concatenate((wave_lw, wave_mw, wave_sw))
                response = np.concatenate((real_lw, real_mw, real_sw), axis=3)
                last_shape = response.shape[-1]
                response = response.reshape(s[0], 1, last_shape)

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return wave_number, response

    def get_rad(self, wave_nums, wave_spec, band_list, center_wave_nums, a, b, lut):
        """
        wave_nums: 波数cm-1（字典形式，key是通道信息，针对FY数据）
        wave_spec: 响应0-1（字典形式，key是通道信息，针对FY数据）
        band_list： CH_20-CH_25
        center_wave_nums: 目前未用，只是为了和get_tbb统一
        a: 目前未用，只是为了和get_tbb统一
        b: 目前未用，只是为了和get_tbb统一
        return channel rad
        """
        data = dict()
        if self.resolution == 16000:
            satellite_type1 = ['NPP', 'JPSS-1']
#             print a, b
#             print lut

            if self.satellite in satellite_type1:
                # 获取iasi的光谱响应 服务器上这个日期开始下载全分辨率cris
                wave_nums2, response = self.get_spectral_response()
                for band in sorted(band_list):
                    wave_nums1 = wave_nums[band]
                    wave_spec1 = wave_spec[band]
                    # 插值 把FY插值到IASI上
                    wave_spec2 = spec_interp(
                        wave_nums1, wave_spec1, wave_nums2)
                    # 对应FY的响应值，大小一致就可以和FY比较了，响应是IASI的
#                     print wave_spec2
                    rads = spec_convolution(wave_nums2, wave_spec2, response)
#                     data[band] = rads
                    # 过滤<=0的结果
#                     print rads
                    idx = np.where(rads <= 0.)
                    rads[idx] = np.nan
#                     data[band] = rads.reshape(rads.size, 1)
                    data[band] = rads  # .reshape(rads.size, 1)
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_tbb(self, wave_nums, wave_spec, band_list, center_wave_nums, a, b, lut):
        """
        use 卫星1的光谱和响应wave_nums,wave_spec（字典形式，key是通道信息，针对FY数据）
        a,b 分别是红外tbb修正系数，字典 ，key是通道信息
        return channel rad
        """
        data = dict()
        if self.resolution == 16000:
            satellite_type1 = ['NPP', 'JPSS-1']

            if self.satellite in satellite_type1:
                # 目标数据通道信息
                # 获取通道的rads
                rads = self.get_rad(
                    wave_nums, wave_spec, band_list, center_wave_nums, a, b, lut)
                for band in band_list:
                    rad = rads[band]
                    if lut:
                        tbb = np.interp(rad, lut['rad'][band], lut['tbb'])
                        data[band] = tbb
                    else:
                        central_wave_number = center_wave_nums[band]
                        tbb = planck_r2t(rad, central_wave_number)
                        if a is not None:
                            k1 = a[band]
                            k0 = b[band]
                            data[band] = tbb * k1 + k0
                        else:
                            data[band] = tbb
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
        if self.resolution == 16000:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/All_Data/CrIS-SDR-GEO_All/Longitude')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < -180, data_pre > 180)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre.reshape(self.data_shape)

        return data

    def get_latitude(self):
        """
        return latitude
        """
        if self.resolution == 16000:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/All_Data/CrIS-SDR-GEO_All/Latitude')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < -90, data_pre > 90)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre.reshape(self.data_shape)

        return data

    def get_sensor_azimuth(self):
        """
        return sensor_azimuth
        """
        if self.resolution == 16000:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/All_Data/CrIS-SDR-GEO_All/SatelliteAzimuthAngle')[:]
                vmin = -180.
                vmax = 180.
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre.reshape(self.data_shape)

        return data

    def get_sensor_zenith(self):
        """
        return sensor_zenith
        """
        if self.resolution == 16000:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                vmin = 0.
                vmax = 180.
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/All_Data/CrIS-SDR-GEO_All/SatelliteZenithAngle')[:]

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre.reshape(self.data_shape)

        return data

    def get_solar_azimuth(self):
        """
        return solar_azimuth
        """
        if self.resolution == 16000:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/All_Data/CrIS-SDR-GEO_All/SolarAzimuthAngle')[:]

                vmin = -180.
                vmax = 180.

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre.reshape(self.data_shape)

        return data

    def get_solar_zenith(self):
        """
        return solar_zenith
        """
        if self.resolution == 16000:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                vmin = 0.
                vmax = 180.
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/All_Data/CrIS-SDR-GEO_All/SolarZenithAngle')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre.reshape(self.data_shape)

        return data

    def get_height(self):
        """
        return hight
        """
        if self.resolution == 16000:
            satellite_type1 = ['NPP', 'JPSS-1']
            if self.satellite in satellite_type1:
                vmin = 0
                vmax = 3000000  # uni m  一般极轨卫星高度840km 左右，这里最大值设置3000km
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/All_Data/CrIS-SDR-GEO_All/SatelliteRange')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre.reshape(self.data_shape)

        return data

    def get_timestamp(self):
        """
        return from 1970-01-01 00:00:00 seconds
        """
        if self.resolution == 16000:
            satellite_type1 = ['NPP', 'JPSS-1']
            s = self.data_shape
            if self.satellite in satellite_type1:
                with h5py.File(self.in_file, 'r') as h5r:
                    sds_name = '/All_Data/CrIS-SDR-GEO_All/FORTime'
                    ary_time = h5r.get(sds_name)[:]
                    ary_time = ary_time / 1000000.
                    # npp cris 数据的时间单位是距离 1958年1月1日 UTC时间的microseconds 微秒
                    # ymdhms/1000000 = 秒  （距离1958年1月1日 UTC时间）
                    secs = (datetime(1970, 1, 1, 0, 0, 0) -
                            datetime(1958, 1, 1, 0, 0, 0)).total_seconds()
                    # 返回距离1970-01-01的秒
                    ary_time = ary_time - secs
                    data = np.repeat(ary_time, 9, axis=1)
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

    L1File = 'D:/data/CRIS/GCRSO-SCRIF-SCRIS_npp_d20180201_t1332319_e1340297_b32463_c20190305050241052556_noac_ops.h5'
#     L1File = 'D:/data/CRIS/GCRSO-SCRIS_npp_d20180415_t1746399_e1754377_b33502_c20180416014630729252_nobc_ops.h5'
    cris = ReadCrisL1(L1File)
    print cris.satellite  # 卫星名
    print cris.sensor  # 传感器名
    print cris.ymd  # L1 文件年月日 YYYYMMDD
    print cris.hms  # L1 文件时分秒 HHMMSS
    print cris.resolution  # 分辨率
    print cris.channels  # 通道数量
    print cris.data_shape
#     print cris.file_attr

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

    L1File = 'D:/data/MERSI/FY3D_MERSI_GBAL_L1_20180201_1405_1000M_MS.HDF'
    mersi = ReadMersiL1(L1File)
    lats = mersi.get_latitude()
    lons = mersi.get_longitude()
    tbb = mersi.get_tbb()
#     band_list = ['CH_20', 'CH_21', 'CH_22', 'CH_23', 'CH_24', 'CH_25']
#     wave_nums, wave_spec = mersi.get_spectral_response()
#     central_wave_numbers = mersi.get_central_wave_number()
#     a = mersi.get_tbb_k1()
#     b = mersi.get_tbb_k0()
#     lut_bt = mersi.get_lut_bt()
#     rad = cris.get_rad(
#         wave_nums, wave_spec, band_list, central_wave_numbers, a, b, lut_bt)
#     tbb = cris.get_tbb(
#         wave_nums, wave_spec, band_list, central_wave_numbers, a, b, lut_bt)
#
#     print rad['CH_21'].shape

    print 'get_spectral_response:'
#     wavenums, response = cris.get_spectral_response()
# #     wavenums, response = cris.get_spectral_response_low()
#     print_data_status(wavenums)
#     print_data_status(response)
#     print 'get_spectral_response1111:'
#     wavenums1, response1 = cris.get_spectral_response_high()
#     print_data_status(wavenums1)
#     print_data_status(response1)
    print 'longitude:'
    t_data1 = cris.get_longitude()

    print 'latitude:'
    t_data2 = cris.get_latitude()
    value = np.full_like(t_data2, 100.)
    tbbs = tbb['CH_21']
    p = dv_map.dv_map(figsize=(6, 5))
    p.easyplot(
        t_data2, t_data1, tbbs, vmin=180, vmax=320, markersize=1.5, marker='.')
    p.savefig('D:/data/CRIS/test1.png')
    pass
# #
#     print 'sensor_azimuth:'
#     t_data = cris.get_sensor_azimuth()
#     print_data_status(t_data)
#     print 'sensor_zenith:'
#     t_data = cris.get_sensor_zenith()
#     print_data_status(t_data)
#     print 'solar_azimuth:'
#     t_data = cris.get_solar_azimuth()
#     print_data_status(t_data)
#     print 'solar_zenith:'
#     t_data = cris.get_solar_zenith()
#     print_data_status(t_data)
#
#     print 'hight:'
#     t_data = cris.get_height()
#     print_data_status(t_data)
#
#     print 'time'
#     t_data = cris.get_timestamp()
#     print_data_status(t_data)
#
#     print 'rad:'
#     t_data = cris.get_rad()
#     print_channel_data(t_data)
#
#     print 'tbb:'
#     t_data = cris.get_tbb()
#     print_channel_data(t_data)

#     p = dv_plt.dv_scatter(figsize=(7, 5))
#     p.xlim_min = 650
#     p.xlim_max = 2755
#     print len(cris.test_w)
#     p.easyplot(
#         wavenums1, response1[0, 0], 'b', 'after', marker='o', markersize=5)
#     p.easyplot(
#         cris.test_w, cris.test_r[0], 'r', 'before', marker='o', markersize=5)
#
#     p.title = u'20180415 17:46:39 NPP FULL CRIS'
#     p.xlabel = u'波长'
#     p.ylabel = u'radince'
#     ofile = 'D:/data/CRIS/gapFilling_after.png'
#     p.savefig(ofile, dpi=300)
