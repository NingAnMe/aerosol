# -*- coding: utf-8 -*-

from datetime import datetime
import os
import re
import sys

import h5py

from PB.pb_io import attrs2dict
from PB.pb_sat import planck_r2t
from pb_drc_base import ReadL1
import numpy as np


__description__ = u'TOU传感器读取类'
__author__ = 'wangpeng'
__date__ = '2018-11-29'
__version__ = '1.0.0_beat'


g_main_path, g_main_file = os.path.split(os.path.realpath(__file__))


class ReadTouL1(ReadL1):

    """
    读取 TOU 传感器的 L1 数据
    分辨率：50000m
    卫星： [FY3A FY3B FY3C]
    通道数量：6
    可见光通道：1,2,3,4,5,6
    红外通道：5

    分辨率：250
    卫星：
    通道数量：
    可见光通道：
    红外通道：
    """

    def __init__(self, in_file):
        sensor = 'TOU'
        super(ReadTouL1, self).__init__(in_file, sensor)

    def set_resolution(self):
        """
        use filename set self.resolution
        :return:
        """
        file_name = os.path.basename(self.in_file)
        if '50KM' in file_name:
            self.resolution = 50000
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def set_satellite(self):
        """
        use filename set self.satellite
        :return:
        """
        file_name = os.path.basename(self.in_file)
        pattern = r'([A-Z0-9]+)_%s.*' % self.sensor
        m = re.match(pattern, file_name)
        if m:
            self.satellite = m.groups()[0]
        else:
            raise ValueError('Cant get the satellite name from file name.')

    def set_ymd_hms(self):
        """
        use filename  set self.ymd self.hms
        """
        file_name = os.path.basename(self.in_file)
        pat = u'\w{4}_\w{5}_\w{4}_L1_(\d{8})_(\d{4})_\w{5}_MS.HDF$'
        g = re.match(pat, file_name)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2) + '00'
        else:
            raise ValueError('Cant get the ymdhms from file name.')

    def set_file_attr(self):
        """
        get hdf5 file attrs self.file_attr
        :return:
        """
        if self.resolution == 50000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
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
        use dataset set self.data_shape
        :return:
        """
        # 如果分辨率是 50000 米
        if self.resolution == 50000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            if self.satellite in satellite_type1:
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get('Latitude')[:]
                    self.data_shape = data_pre.shape
            elif self.satellite in satellite_type2:
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get('/Geolocation/Latitude')[:]
                    self.data_shape = data_pre.shape
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
        if self.resolution == 50000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                self.channels = 6
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def get_dn(self):
        """
        return DN
        """
        data = dict()
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            satellite_type3 = ['FY3D']

            if self.satellite in satellite_type1:
                data_file = self.in_file
                with h5py.File(data_file, 'r') as h5r:
                    ary_ch1 = h5r.get('/EV_250_Aggr.1KM_RefSB').value
                    ary_ch5 = h5r.get('/EV_250_Aggr.1KM_Emissive').value
                    ary_ch6 = h5r.get('/EV_1KM_RefSB').value
                    vmin = 0
                    vmax = 10000

                    # 逐个通道处理
                    for i in xrange(self.channels):
                        band = 'CH_{:02d}'.format(i + 1)

                        if i < 4:
                            k = i
                            data_pre = ary_ch1[k]
                            # 开始处理
                        elif i == 4:
                            data_pre = ary_ch5
                        else:
                            k = i - 5
                            data_pre = ary_ch6[k]

                        data_pre = data_pre.astype(np.float32)
                        invalid_index = np.logical_or(
                            data_pre <= vmin, data_pre > vmax)
                        data_pre[invalid_index] = np.nan
                        data[band] = data_pre

            elif self.satellite in satellite_type2:
                data_file = self.in_file
                with h5py.File(data_file, 'r') as h5r:
                    ary_ch1 = h5r.get('/Data/EV_250_Aggr.1KM_RefSB').value
                    ary_ch5 = h5r.get('/Data/EV_250_Aggr.1KM_Emissive').value
                    ary_ch6 = h5r.get('/Data/EV_1KM_RefSB').value
                    vmin = 0
                    vmax = 10000

                    # 逐个通道处理
                    for i in xrange(self.channels):
                        band = 'CH_{:02d}'.format(i + 1)

                        if i < 4:
                            k = i
                            data_pre = ary_ch1[k]
                            # 开始处理
                        elif i == 4:
                            data_pre = ary_ch5
                        else:
                            k = i - 5
                            data_pre = ary_ch6[k]

                        data_pre = data_pre.astype(np.float32)
                        invalid_index = np.logical_or(
                            data_pre <= vmin, data_pre > vmax)
                        data_pre[invalid_index] = np.nan
                        data[band] = data_pre

            elif self.satellite in satellite_type3:
                data_file = self.in_file
                with h5py.File(data_file, 'r') as h5r:
                    ary_ch1_4 = h5r.get('/Data/EV_250_Aggr.1KM_RefSB')[:]
                    ary_ch5_19 = h5r.get('/Data/EV_1KM_RefSB')[:]
                    ary_ch20_23 = h5r.get('/Data/EV_1KM_Emissive')[:]
                    ary_ch24_25 = h5r.get('/Data/EV_250_Aggr.1KM_Emissive')[:]
                    vmin = 0
                    vmax = 4095
                    # 逐个通道处理
                    for i in xrange(self.channels):
                        band = 'CH_{:02d}'.format(i + 1)
                        if i < 4:
                            k = i
                            data_pre = ary_ch1_4[k]
                            # 开始处理
                        elif i >= 4 and i < 19:
                            k = i - 4
                            data_pre = ary_ch5_19[k]
                        elif i >= 19 and i < 23:
                            k = i - 19
                            data_pre = ary_ch20_23[k]
                        else:
                            k = i - 23
                            data_pre = ary_ch24_25[k]

                        data_pre = data_pre.astype(np.float32)
                        invalid_index = np.logical_or(
                            data_pre <= vmin, data_pre > vmax)
                        data_pre[invalid_index] = np.nan
                        data[band] = data_pre
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_ref(self):
        """
        return Ref
        """

        data = dict()
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            satellite_type2 = ['FY3D']

            # FY3A/B/C
            if self.satellite in satellite_type1:
                dn = self.get_dn()
                k0 = self.get_k0()
                k1 = self.get_k1()
                k2 = self.get_k2()
                if 'FY3B' in self.satellite:
                    if int(self.ymd + self.hms) <= 20130306001500:
                        scales = 100.
                    else:
                        scales = 10000.
                else:
                    scales = 100.

                # 逐个通道处理
                for i in xrange(self.channels):
                    band = 'CH_{:02d}'.format(i + 1)
                    if 'CH_05' in band:
                        continue

                    channel_data = dn[band]**2 * k2[band] + dn[band] * \
                        k1[band] + k0[band]
                    pre_data = channel_data / scales

                    idx = np.where(pre_data < 0.)
                    if len(idx[0] > 0):
                        pre_data[idx] = np.nan
                    data[band] = pre_data

            # FY3D
            elif self.satellite in satellite_type2:
                dn = self.get_dn()
                k0 = self.get_k0()
                k1 = self.get_k1()
                k2 = self.get_k2()

                # 逐个通道处理
                for i in xrange(self.channels):
                    band = 'CH_{:02d}'.format(i + 1)
                    if i < 19:

                        pre_data = dn[band]**2 * k2[band] + dn[band] * \
                            k1[band] + k0[band]

                        idx = np.where(pre_data < 0.)
                        if len(idx[0] > 0):
                            pre_data[idx] = np.nan
                        data[band] = pre_data / 100.
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
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            satellite_type2 = ['FY3D']
            if self.satellite in satellite_type1:
                dn = self.get_dn()
                # 逐个通道处理
                for i in xrange(self.channels):
                    band = 'CH_{:02d}'.format(i + 1)
                    if 'CH_05' in band:
                        data[band] = dn[band] / 100.

            elif self.satellite in satellite_type2:
                dn = self.get_dn()
                with h5py.File(self.in_file, 'r') as h5r:
                    ary_a1 = h5r.get('/Data/EV_1KM_Emissive').attrs['Slope']
                    ary_b1 = h5r.get(
                        '/Data/EV_1KM_Emissive').attrs['Intercept']

                    ary_a2 = h5r.get(
                        '/Data/EV_250_Aggr.1KM_Emissive').attrs['Slope']
                    ary_b2 = h5r.get(
                        '/Data/EV_250_Aggr.1KM_Emissive').attrs['Intercept']

                    a = np.concatenate((ary_a1, ary_a2))
                    b = np.concatenate((ary_b1, ary_b2))

                # 逐个通道处理
                for i in xrange(self.channels):
                    band = 'CH_{:02d}'.format(i + 1)

                    if i >= 19:
                        k = i - 19
                        data[band] = dn[band] * a[k] + b[k]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_tbb_k1(self):
        """
        return tbb_k1  dict one value
        """

        data = dict()
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            satellite_type2 = ['FY3D']

            if self.satellite in satellite_type1:
                data['CH_05'] = 1
            elif self.satellite in satellite_type2:
                data['CH_20'] = 1.00103
                data['CH_21'] = 1.00085
                data['CH_22'] = 1.00125
                data['CH_23'] = 1.00030
                data['CH_24'] = 1.00133
                data['CH_25'] = 1.00065

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_tbb_k0(self):
        """
        return tbb_k0  dict one value
        """
        data = dict()
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            satellite_type2 = ['FY3D']

            if self.satellite in satellite_type1:
                data['CH_05'] = 0
            elif self.satellite in satellite_type2:
                data['CH_20'] = -0.4759
                data['CH_21'] = -0.3139
                data['CH_22'] = -0.2662
                data['CH_23'] = -0.0513
                data['CH_24'] = -0.0734
                data['CH_25'] = 0.0875
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
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C', 'FY3D']

            if self.satellite in satellite_type1:
                # rad转tbb的修正系数，所有时次都是固定值
                tbb_k0 = self.get_tbb_k0()
                tbb_k1 = self.get_tbb_k1()
                rads = self.get_rad()
                central_wave_numbers = self.get_central_wave_number()
                # 逐个通道处理
                for i in xrange(self.channels):
                    band = 'CH_{:02d}'.format(i + 1)
                    if band in rads.keys():
                        k0 = tbb_k0[band]
                        k1 = tbb_k1[band]
                        central_wave_number = central_wave_numbers[band]
                        rad = rads[band]
                        tbb = planck_r2t(rad, central_wave_number)
                        data[band] = tbb * k1 + k0

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
        if self.resolution == 50000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            if self.satellite in satellite_type1:
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get('/Longitude').value
            elif self.satellite in satellite_type2:
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get('/Geolocation/Longitude').value

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
        if self.resolution == 50000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            if self.satellite in satellite_type1:
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get('/Latitude').value
            elif self.satellite in satellite_type2:
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get('/Geolocation/Latitude').value

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < -90, data_pre > 90)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre

        return data

    def get_land_sea_mask(self):
        """
        return land_sea_mask
        """
        if self.resolution == 50000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            if self.satellite in satellite_type1:
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get('/Land_sea_mask').value
            elif self.satellite in satellite_type2:
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get('/Geolocation/Land_sea_mask').value

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < 0, data_pre > 7)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre

        return data

    def get_sensor_azimuth(self):
        """
        return sensor_azimuth
        """
        if self.resolution == 50000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            vmin = -18000
            vmax = 18000
            if self.satellite in satellite_type1:
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get('/Satellite_azimuth_angle').value

            elif self.satellite in satellite_type2:
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/Geolocation/Satellite_azimuth_angle').value

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre / 100.

        return data

    def get_sensor_zenith(self):
        """
        return sensor_zenith
        """
        if self.resolution == 50000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            vmin = 0
            vmax = 18000
            if self.satellite in satellite_type1:
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get('/Satellite_zenith_angle').value

            elif self.satellite in satellite_type2:
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/Geolocation/Satellite_zenith_angle').value
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre / 100.

        return data

    def get_solar_azimuth(self):
        """
        return solar_azimuth
        """
        if self.resolution == 50000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            vmin = -18000
            vmax = 18000
            if self.satellite in satellite_type1:
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get('/Solar_azimuth_angle').value

            elif self.satellite in satellite_type2:
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get(
                        '/Geolocation/Solar_azimuth_angle').value

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre / 100.

        return data

    def get_solar_zenith(self):
        """
        return solar_zenith
        """
        if self.resolution == 50000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            vmin = 0
            vmax = 18000
            if self.satellite in satellite_type1:
                # s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get('/Solar_zenith_angle').value

            elif self.satellite in satellite_type2:
                with h5py.File(self.in_file, 'r') as h5r:
                    data_pre = h5r.get('/Geolocation/Solar_zenith_angle').value

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre / 100.

        return data

    def get_timestamp(self):
        """
        return from 1970-01-01 00:00:00 seconds
        """
        if self.resolution == 50000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                seconds_of_file = 6300  # 一个时次持续 300 秒
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
            file_date = datetime.strptime(self.ymd + self.hms, '%Y%m%d%H%M%S')
            timestamp = (
                file_date - datetime(1970, 1, 1, 0, 0, 0)).total_seconds()
            row_length = self.data_shape[0]
            delta = np.linspace(0, seconds_of_file - 1, row_length)
            data = np.full(self.data_shape, np.nan, dtype=np.float64)
            data[:] = (delta + timestamp).reshape(-1, 1)
            data = data.astype(np.int32)
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_central_wave_length(self):
        '''
        #return central_wave_number wn(cm-1) = 10 ^ 7 / wave_length(nm)
        return central_wave_length, unit nm
        '''
        if self.resolution == 50000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                data = {'CH_01': 308.727,
                        'CH_02': 312.638,
                        'CH_03': 317.652,
                        'CH_04': 322.464,
                        'CH_05': 331.375,
                        'CH_06': 360.253}
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_central_wave_width(self):
        '''
        #return central_wave_number wn(cm-1) = 10 ^ 7 / wave_length(nm)
        return central_wave_width (半高波宽)
        '''
        if self.resolution == 50000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                data = {'CH_01': 1.164,
                        'CH_02': 1.152,
                        'CH_03': 1.171,
                        'CH_04': 1.156,
                        'CH_05': 1.159,
                        'CH_06': 1.140}
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_spectral_response(self):
        """
        return 光谱波数和响应值，两个字典
        """
        center_wave_length = self.get_central_wave_length()
        center_wave_width = self.get_central_wave_width()
        data1 = dict()
        data2 = dict()
        if self.resolution == 50000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                for i in xrange(self.channels):
                    k = i + 1
                    band = "CH_{:02d}".format(k)
                    # 采样频率
                    step = center_wave_width[band] / 25.
                    print band, step
                    # 波长开始
                    wave_start = center_wave_length[
                        band] - center_wave_width[band]
                    # 波长结束
                    wave_end = center_wave_length[
                        band] + center_wave_width[band]
                    # 中心波长/半高波宽
                    wave_length_range = np.arange(wave_start, wave_end, step)
                    print len(wave_length_range)
                    center_wn_ww = center_wave_length[
                        band] / center_wave_width[band]

                    print center_wn_ww
                    srf = wave_length_range / \
                        center_wave_width[band] + 1 - center_wn_ww
                    print srf
#                     data1[band] = wave_number
#                     data2[band] = response
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data1, data2

if __name__ == '__main__':
    L1File = 'D:/data/TOU/FY3A_TOUXX_GBAL_L1_20130101_1018_050KM_MS.HDF'
    L1File = 'D:/data/TOU/FY3B_TOUXX_GBAL_L1_20181001_1654_050KM_MS.HDF'
    L1File = 'D:/data/TOU/FY3C_TOUXX_GBAL_L1_20181001_1600_050KM_MS.HDF'
    mersi = ReadTouL1(L1File)
    print mersi.satellite  # 卫星名
    print mersi.sensor  # 传感器名
    print mersi.ymd  # L1 文件年月日 YYYYMMDD
    print mersi.hms  # L1 文件时分秒 HHMMSS
    print mersi.resolution  # 分辨率
    print mersi.channels  # 通道数量
    print mersi.data_shape
    print type(mersi.file_attr)

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

#     print 'longitude:'
#     t_data = mersi.get_longitude()
#     print_data_status(t_data)
#     print 'latitude:'
#     t_data = mersi.get_latitude()
#     print_data_status(t_data)
#     print 'land_sea_mask:'
#     t_data = mersi.get_land_sea_mask()
#     print_data_status(t_data)
#     print 'sensor_azimuth:'
#     t_data = mersi.get_sensor_azimuth()
#     print_data_status(t_data)
#     print 'sensor_zenith:'
#     t_data = mersi.get_sensor_zenith()
#     print_data_status(t_data)
#     print 'solar_azimuth:'
#     t_data = mersi.get_solar_azimuth()
#     print_data_status(t_data)
#     print 'solar_zenith:'
#     t_data = mersi.get_solar_zenith()
#     print_data_status(t_data)
#     print 'timestamp:'
#     t_data = mersi.get_timestamp()
#     print_data_status(t_data)
#     print datetime.utcfromtimestamp(t_data[0, 0])
#     print datetime.utcfromtimestamp(t_data[-1, -1])

    print 'get_spectral_response:'
    wavenums, wave_spec = mersi.get_spectral_response()
#     print_channel_data(wavenums)
#     print_channel_data(wave_spec)
