# -*- coding: utf-8 -*-

from datetime import datetime
import os
import re
import time

from pyhdf.SD import SD, SDC

from PB.pb_io import attrs2dict
from PB.pb_sat import planck_r2t, spec_interp, spec_convolution
from pb_drc_base import ReadL1
import numpy as np


__description__ = u'CRIS传感器读取'
__author__ = 'wangpeng'
__date__ = '2018-09-03'
__version__ = '1.0.0_beat'

g_main_path, g_main_file = os.path.split(os.path.realpath(__file__))


class ReadAirsL1(ReadL1):
    """
     读取 AIRS 传感器的 L1 数据
     分辨率：13.5 km IFOV for the spectrometer; 2.3 km IFOV for VIS/NIR channels
     卫星：AQUA TERRA
     通道数量：2378
     可见光通道：
     红外通道：
    """

    def __init__(self, in_file):
        sensor = 'AIRS'
        super(ReadAirsL1, self).__init__(in_file, sensor)

    def set_resolution(self):
        """
        use filename set self.resolution
        :return:
        """
        file_name = os.path.basename(self.in_file)
        if 'AIRS' in file_name:
            self.resolution = 13500
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def set_satellite(self):
        """
        use filename set self.satellite
        :return:
        """
        self.satellite = "AQUA"
#         if "MYD" in self.in_file:
#             self.satellite = "AQUA"
#         elif "MOD" in self.in_file:
#             self.satellite = "TERRA"
#         else:
#             raise ValueError('Cant get the satellite name from file name.')

    def set_ymd_hms(self):
        """
        use filename  set self.ymd self.hms
        """
        file_name = os.path.basename(self.in_file)
        pat = u'AIRS.(\d{4}).(\d{2}).(\d{2}).(\d{3}).L1B.AIRS_Rad.v5.0.(0|\d{2}).0.G(\d{11}).hdf$'
        g = re.match(pat, file_name)
        if g:
            self.ymd = g.group(1) + g.group(2) + g.group(3)
            # 074 表示一天中产生的第几个文件，每6分钟产生一个文件。60取证为小时，取余为分钟
            hour = (int(g.group(4)) - 1) * 6 / 60
            mina = (int(g.group(4)) - 1) * 6 % 60
            hm = "%02d%02d" % (hour, mina)
            self.hms = hm + '00'
        else:
            raise ValueError('Cant get the ymdhms from file name.')

    def set_file_attr(self):
        """
        get hdf5 file attrs self.file_attr
        :return:
        """
        if self.resolution == 13500:
            satellite_type = ['AQUA', 'TERRA']
            if self.satellite in satellite_type:
                h4r = SD(self.in_file, SDC.READ)
                self.file_attr = attrs2dict(h4r.attributes())
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                "Cant handle this resolution: ".format(self.resolution))

    def set_data_shape(self):
        """
        use dataset set self.data_shape
        :return:
        """
        # 如果分辨率是 13500 米
        if self.resolution == 13500:
            satellite_type = ['AQUA', 'TERRA']
            if self.satellite in satellite_type:
                h4r = SD(self.in_file, SDC.READ)
                in_data_r250 = h4r.select('Latitude').get()
                self.data_shape = in_data_r250.shape
                h4r.end()
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                "Cant handle this resolution: ".format(self.resolution))

    def set_channels(self):
        """
        return sensor channels
        :return:
        """
        if self.resolution == 13500:
            self.channels = 2378
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def get_spectral_response(self):
        """
        return 光谱波数和响应值，1维，2维
        """
        if self.resolution == 13500:
            satellite_type1 = ['AQUA', 'TERRA']
            if self.satellite in satellite_type1:
                try:
                    h4r = SD(self.in_file, SDC.READ)
                    data_pre = h4r.select('radiances').get()
                    h4r.end()

                    # 非法值变成0 卷积时不贡献
                    condition = np.logical_or(data_pre < 0, data_pre > 200)
                    idx = np.where(condition)
#                     print len(idx[0])
                    if len(idx[0] > 0):
                        data_pre[idx] = 0
                    # 单位转换
                except Exception as e:
                    print(str(e))
                response = data_pre
                # 暂时取一个观测的光谱波数
                file_name = '{}_{}_SRF_Pub.txt'.format(
                    self.satellite, self.sensor)
                data_file = os.path.join(g_main_path, 'SRF', file_name)
                dtype = {
                    'names': ('wave_length', 'response'), 'formats': ('f4', 'f4')}
                if os.path.isfile(data_file):
                    datas = np.loadtxt(data_file, dtype=dtype)
                    # 波长转波数
                    wave_number = datas['wave_length']
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return wave_number, response

    def get_rad(self, wave_nums, wave_spec, band_list, central_wave_nums, a, b, lut):
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
        if self.resolution == 13500:
            satellite_type1 = ['AQUA', 'TERRA']

            if self.satellite in satellite_type1:
                # 获取iasi的光谱响应
                wave_nums2, response = self.get_spectral_response()
                for band in sorted(band_list):
                    wave_nums1 = wave_nums[band]
                    wave_spec1 = wave_spec[band]

                    # 插值 把FY插值到AIRS上
                    wave_spec2 = spec_interp(
                        wave_nums1, wave_spec1, wave_nums2)
                    # 对应FY的响应值，大小一致就可以和FY比较了，响应是IASI的

                    rads = spec_convolution(wave_nums2, wave_spec2, response)
#                     print band, np.allclose(rads, np.nan, equal_nan=True)
                    # 过滤<=0的结果
#                     print rads
                    idx = np.where(rads <= 0.)
                    if len(idx[0] > 0):
                        rads[idx] = np.nan

                    data[band] = rads  # .reshape(rads.size, 1)

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_tbb(self, wave_nums, wave_spec, band_list, central_wave_nums, a, b, lut):
        """
        use 卫星1的光谱和响应wave_nums,wave_spec（字典形式，key是通道信息，针对FY数据）
        a,b 分别是红外tbb修正系数，字典 ，key是通道信息
        return channel rad
        """
        data = dict()
        if self.resolution == 13500:
            satellite_type1 = ['AQUA', 'TERRA']

            if self.satellite in satellite_type1:
                # 目标数据通道信息
                # 获取通道的rads
                rads = self.get_rad(
                    wave_nums, wave_spec, band_list, central_wave_nums, a, b, lut)
                for band in band_list:
                    rad = rads[band]
                    if lut:
                        print 'lut'
                        tbb = np.interp(rad, lut['rad'][band], lut['tbb'])
                        data[band] = tbb
                    else:
                        central_wave_number = central_wave_nums[band]
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
        if self.resolution == 13500:
            satellite_type = ['AQUA', 'TERRA']
            if self.satellite in satellite_type:
                h4r = SD(self.in_file, SDC.READ)
                data_pre = h4r.select('Longitude').get()
                h4r.end()
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < -180., data_pre > 180.)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre

        return data

    def get_latitude(self):
        """
        return latitude
        """
        if self.resolution == 13500:
            satellite_type = ['AQUA', 'TERRA']
            if self.satellite in satellite_type:
                h4r = SD(self.in_file, SDC.READ)
                data_pre = h4r.select('Latitude').get()
                h4r.end()
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < -90., data_pre > 90.)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre

        return data

    def get_sensor_azimuth(self):
        """
        return sensor_azimuth
        """
        if self.resolution == 13500:
            satellite_type = ['AQUA', 'TERRA']
            if self.satellite in satellite_type:
                vmin = -180.
                vmax = 180.
                h4r = SD(self.in_file, SDC.READ)
                data_pre = h4r.select('satazi').get()
                h4r.end()
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
        if self.resolution == 13500:
            satellite_type = ['AQUA', 'TERRA']
            if self.satellite in satellite_type:
                vmin = 0.
                vmax = 180.
                h4r = SD(self.in_file, SDC.READ)
                data_pre = h4r.select('satzen').get()
                h4r.end()
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
        if self.resolution == 13500:
            satellite_type = ['AQUA', 'TERRA']
            if self.satellite in satellite_type:
                vmin = -180.
                vmax = 180.
                h4r = SD(self.in_file, SDC.READ)
                data_pre = h4r.select('solazi').get()
                h4r.end()
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
        if self.resolution == 13500:
            satellite_type = ['AQUA', 'TERRA']
            if self.satellite in satellite_type:
                vmin = 0.
                vmax = 180.
                h4r = SD(self.in_file, SDC.READ)
                data_pre = h4r.select('solzen').get()
                h4r.end()
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
        if self.resolution == 13500:
            satellite_type1 = ['AQUA', 'TERRA']
            if self.satellite in satellite_type1:
                h4r = SD(self.in_file, SDC.READ)
                ary_time = h4r.select('Time').get()
                h4r.end()
                ary_time = ary_time  # / 1000000.
                # npp cris 数据的时间单位是距离 1958年1月1日 UTC时间的microseconds 微秒
                # ymdhms/1000000 = 秒  （距离1958年1月1日 UTC时间）
                secs = (datetime(1993, 1, 1, 0, 0, 0) -
                        datetime(1970, 1, 1, 0, 0, 0)).total_seconds()
                # 返回距离1970-01-01的秒
                data = ary_time + secs
                data = data.astype(np.int32)

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data


if __name__ == '__main__':
    L1File = 'D:/data/AIRS/AIRS.2013.11.30.095.L1B.AIRS_Rad.v5.0.22.0.G13334123505.hdf'
    L1File = 'D:/data/AIRS/AIRS.2005.01.01.003.L1B.AIRS_Rad.v5.0.0.0.G07085070901.hdf'

    modis = ReadAirsL1(L1File)
    print modis.satellite  # 卫星名
    print modis.sensor  # 传感器名
    print modis.ymd  # L1 文件年月日 YYYYMMDD
    print modis.hms  # L1 文件时分秒 HHMMSS
    print modis.resolution  # 分辨率
    print modis.channels  # 通道数量
    print modis.data_shape
#     print modis.file_attr  # L1 文件属性

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

#     print 'dn:'
#     t_data = modis.get_dn()
#     print_channel_data(t_data)

#     print 'ref:'
#     t_data = modis.get_ref()
#     print_channel_data(t_data)

#     print 'rad:'
#     t_data = modis.get_rad()
#     print_channel_data(t_data)

#     print 'tbb:'
#     t_data = modis.get_tbb()
#     print_channel_data(t_data)
    print 'get_spectral_response:'
    wavenums, response = modis.get_spectral_response()
    print_data_status(wavenums)
    print_data_status(response)
    print response

#     print 'longitude:'
#     t_data = modis.get_longitude()
#     print_data_status(t_data)
#     print 'latitude:'
#     t_data = modis.get_latitude()
#     print_data_status(t_data)
#     print 'sensor_azimuth:'
#     t_data = modis.get_sensor_azimuth()
#     print_data_status(t_data)
#     print 'sensor_zenith:'
#     t_data = modis.get_sensor_zenith()
#     print_data_status(t_data)
#     print 'solar_azimuth:'
#     t_data = modis.get_solar_azimuth()
#     print_data_status(t_data)
#     print 'solar_zenith:'
#     t_data = modis.get_solar_zenith()
#     print_data_status(t_data)
#     print 'timestamp:'
#     t_data = modis.get_timestamp()
#     print_data_status(t_data)
#     print time.gmtime(t_data[0, 0])
#     print time.gmtime(t_data[-1, -1])
#     print datetime.utcfromtimestamp(t_data[0][0])
#     print datetime.utcfromtimestamp(t_data[-1][-1])
