# -*- coding: utf-8 -*-

from datetime import datetime
import os
import re
import time

import h5py

from PB.pb_io import attrs2dict
from PB.pb_sat import planck_r2t, planck_t2r
from pb_drc_base import ReadL1
import numpy as np


__description__ = u'FY4的  传感器读取类'
__author__ = 'wangpeng'
__date__ = '2018-08-28'
__version__ = '1.0.0_beat'

g_main_path, g_main_file = os.path.split(os.path.realpath(__file__))


class ReadAgriL1(ReadL1):

    """
    读取  传感器的 L1 数据
    分辨率：4000m
    卫星： [FY4A]
    通道数量：20
    可见光通道：1,2,3,4,6~20
    红外通道：5

    分辨率：2000m
    卫星：
    通道数量：
    可见光通道：
    红外通道：
    """

    def __init__(self, in_file):
        sensor = 'AGRI'
        super(ReadAgriL1, self).__init__(in_file, sensor)

    def set_resolution(self):
        """
        use filename set self.resolution
        :return:
        """
        file_name = os.path.basename(self.in_file)
        if '4000M' in file_name:
            self.resolution = 4000
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def set_satellite(self):
        """
        use filename set self.satellite
        :return:
        """
        file_name = os.path.basename(self.in_file)
        pattern = r'([A-Z0-9]+)-_%s.*' % self.sensor
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
        pat = u'FY4[A-Z]-_AGRI--_N_DISK_\d{4}E_L1-_\w{3}-_MULT_NOM_(\d{8})(\d{6})_(\d{14})_4000M_V0001.HDF\Z'
        g = re.match(pat, file_name)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2)
        else:
            raise ValueError('Cant get the ymdhms from file name.')

    def set_file_attr(self):
        """
        get hdf5 file attrs self.file_attr
        :return:
        """
        if self.resolution == 4000:
            satellite_type1 = ['FY4A']
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
        if self.resolution == 4000:
            satellite_type1 = ['FY4A']
            if self.satellite in satellite_type1:
                self.data_shape = (2748, 2748)
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                "Cant handle this resolution: ".format(self.resolution))

    def set_channels(self):
        """
        return sensor channels
        """
        if self.resolution == 4000:
            satellite_type1 = ['FY4A']
            if self.satellite in satellite_type1:
                self.channels = 14
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def __get_geo_file(self):
        """
        return 定位文件
        """
        if self.resolution == 4000:
            satellite_type1 = ['FY4A']
            if self.satellite in satellite_type1:
                geo_file = self.in_file.replace('FDI', 'GEO')
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                "Cant handle this resolution: ".format(self.resolution))
        return geo_file

    def __get_lonlat_file(self):
        """
        return 定位文件
        """
        if self.resolution == 4000:
            satellite_type1 = ['FY4A']
            if self.satellite in satellite_type1:
                geo_file = os.path.join(
                    g_main_path, 'GEO', 'FY4X_LON_LAT_LUT.H5')
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                "Cant handle this resolution: ".format(self.resolution))
        return geo_file

    def get_lut_bt(self):
        """
        return  lut 字典， key值包括tbb和rad，其中rad是字典，key值是通道 CH_01 CH_02 ...
        """
        lut = {}
        if self.resolution == 4000:
            satellite_type1 = ['FY4A']
            if self.satellite in satellite_type1:
                lut_file = os.path.join(
                    g_main_path, 'LUT', 'FY4A_RADA_TBB.txt')

                if os.path.isfile(lut_file):

                    lut_ary = np.loadtxt(lut_file)
                    if 'tbb' not in lut.keys():
                        lut['tbb'] = lut_ary[:, 0]
                    if 'rad' not in lut.keys():
                        lut['rad'] = {}

                    for i in xrange(8, 15):
                        band = 'CH_{:02d}'.format(i)
                        if band not in lut['rad'].keys():
                            lut['rad'][band] = lut_ary[:, i - 7]
                else:
                    lut = None

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                "Cant handle this resolution: ".format(self.resolution))
        return lut

    def get_dn(self):
        """
        return DN
        """
        data = dict()
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']

            if self.satellite in satellite_type1:
                data_file = self.in_file
                vmin = 0
                vmax = 4095
                with h5py.File(data_file, 'r') as h5r:
                    for i in xrange(self.channels):
                        band = 'CH_{:02d}'.format(i + 1)
                        ary_dn = h5r.get('/NOMChannel%02d' % (i + 1))[:]

                        data_pre = ary_dn.astype(np.float32)
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
        return ref
        """
        data = dict()
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']
            dn = self.get_dn()

            if self.satellite in satellite_type1:

                data_file = self.in_file

                with h5py.File(data_file, 'r') as h5r:
                    for i in xrange(self.channels):
                        if i <= 5:
                            band = 'CH_{:02d}'.format(i + 1)
                            ary_lut = h5r.get(
                                '/CALChannel%02d' % (i + 1))[:]
                            data_pre = np.full(self.data_shape, np.nan)
                            valid_index = np.logical_and(
                                True, np.isfinite(dn[band]))
                            lut_idx = dn[band][valid_index].astype(np.int16)
                            data_pre[valid_index] = ary_lut[lut_idx]
                            data[band] = data_pre
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_rad_test(self):
        """
        return rad
        """
        data = dict()
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']
            tbbs = self.get_tbb()
            lut = self.get_lut_bt()

            if self.satellite in satellite_type1:

                for i in xrange(self.channels):
                    band = 'CH_{:02d}'.format(i + 1)
                    if i >= 7:
                        tbb = 250
                        rad = np.interp(tbb, lut['tbb'], lut['rad'][band])
                        data[band] = rad
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_rad_test2(self):
        """
        return rad
        """
        data = dict()
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']
            tbbs = self.get_tbb()
            central_wave_numbers = self.get_central_wave_number()

            if self.satellite in satellite_type1:

                for i in xrange(self.channels):
                    band = 'CH_{:02d}'.format(i + 1)
                    if i >= 7:
                        tbb = 250
                        central_wave_number = central_wave_numbers[band]
                        rad = planck_t2r(tbb, central_wave_number)
                        data[band] = rad
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
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']
            tbbs = self.get_tbb()
            central_wave_numbers = self.get_central_wave_number()
            lut = self.get_lut_bt()
#             lut = None
            if self.satellite in satellite_type1:

                for i in xrange(self.channels):
                    band = 'CH_{:02d}'.format(i + 1)
                    if i >= 7:
                        tbb = tbbs[band]
                        central_wave_number = central_wave_numbers[band]
                        if lut:
                            print 'f4 lut'
                            rad = np.interp(tbb, lut['tbb'], lut['rad'][band])
                        else:
                            print 'f4 plank'
                            rad = planck_t2r(tbb, central_wave_number)
                        data[band] = rad
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_tbb_test(self):
        """
        return rad
        """
        data = dict()
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']
            tbbs = self.get_tbb()
            lut = self.get_lut_bt()

            if self.satellite in satellite_type1:

                for i in xrange(self.channels):
                    band = 'CH_{:02d}'.format(i + 1)
                    if i >= 7:
                        rad = 36
                        tbb = np.interp(rad, lut['rad'][band], lut['tbb'])
                        data[band] = tbb
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_tbb_test2(self):
        """
        return tbb
        """
        data = dict()
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']
#             dn = self.get_dn()

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
                        rad = 36
                        tbb = planck_r2t(rad, central_wave_number)
                        data[band] = tbb * k1 + k0
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
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']
            dn = self.get_dn()

            if self.satellite in satellite_type1:
                tbb_k0 = self.get_tbb_k0()
                tbb_k1 = self.get_tbb_k1()
                data_file = self.in_file

                with h5py.File(data_file, 'r') as h5r:
                    for i in xrange(self.channels):
                        if i >= 7:
                            band = 'CH_{:02d}'.format(i + 1)
                            k0 = tbb_k0[band]
                            k1 = tbb_k1[band]
                            ary_lut = h5r.get(
                                '/CALChannel%02d' % (i + 1))[:]
                            data_pre = np.full(self.data_shape, np.nan)
                            valid_index = np.logical_and(
                                True, np.isfinite(dn[band]))
                            lut_idx = dn[band][valid_index].astype(np.int16)
                            data_pre[valid_index] = ary_lut[lut_idx]
                            data[band] = data_pre * k1 + k0
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_tbb_k1(self):
        """
        return K1
        """
        data = dict()
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']
            if self.satellite in satellite_type1:
                data = {'CH_08': 1., 'CH_09': 1., 'CH_10': 1.,
                        'CH_11': 1., 'CH_12': 1., 'CH_13': 1., 'CH_14': 1.}
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_tbb_k0(self):
        """
        return K0
        """

        data = dict()
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']
            if self.satellite in satellite_type1:
                data = {'CH_08': 0., 'CH_09': 0., 'CH_10': 0.,
                        'CH_11': 0., 'CH_12': 0., 'CH_13': 0., 'CH_14': 0.}

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
        data = dict()
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']

            if self.satellite in satellite_type1:
                center_lon = self.file_attr['NOMCenterLon']
                data_file = self.__get_lonlat_file()

                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('Longitude')[:]

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < -180, data_pre > 180)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre + center_lon

        return data

    def get_latitude(self):
        """
        return latitude
        """
        data = dict()
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']

            if self.satellite in satellite_type1:

                data_file = self.__get_lonlat_file()

                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('Latitude')[:]

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
        data = dict()
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']

            if self.satellite in satellite_type1:
                data_file = self.__get_geo_file()

                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/NOMSatelliteAzimuth')[:]
                vmin = 0.
                vmax = 360.

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
        data = dict()
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']
            if self.satellite in satellite_type1:
                data_file = self.__get_geo_file()

                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/NOMSatelliteZenith')[:]

                vmin = 0.
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

    def get_solar_azimuth(self):
        """
        return solar_azimuth
        """
        data = dict()
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']
            if self.satellite in satellite_type1:
                data_file = self.__get_geo_file()

                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/NOMSunAzimuth')[:]

                vmin = 0.
                vmax = 360.

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
        data = dict()
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']
            if self.satellite in satellite_type1:
                data_file = self.__get_geo_file()

                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/NOMSunZenith')[:]

                vmin = 0.
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

    def get_timestamp(self):
        """
        return from 1970-01-01 00:00:00 seconds
        """
        if self.resolution == 4000:
            satellite_type1 = ['FY4A']
            if self.satellite in satellite_type1:
                seconds_of_file = 15 * 60  # 一个时次持续 300 秒
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

    def get_spectral_response(self):
        """
        return 光谱波数和响应值，两个字典
        """
        data1 = dict()
        data2 = dict()
        if self.resolution == 4000:
            satellite_type1 = ['FY4A']
            if self.satellite in satellite_type1:
                dtype = {
                    'names': ('wave_length', 'response'), 'formats': ('f4', 'f4')}
                for i in xrange(self.channels):
                    k = i + 1
                    band = "CH_{:02d}".format(k)
                    file_name = '{}_{}_SRF_CH{:02d}_Pub.txt'.format(
                        self.satellite, self.sensor, k)
                    data_file = os.path.join(g_main_path, 'SRF', file_name)
                    if not os.path.isfile(data_file):
                        continue
                    datas = np.loadtxt(data_file, dtype=dtype)
                    # 波长转波数
                    wave_length = datas['wave_length'][::-1]
                    wave_number = 10 ** 4 / wave_length
                    # 响应
                    response = datas['response'][::-1]

                    data1[band] = wave_number
                    data2[band] = response
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data1, data2

    def get_central_wave_number(self):
        '''
        return 中心波数
        central_wave_number
        wn(cm-1) = 10 ^ 7 / wave_length(nm)
        '''
        data = dict()
        if self.resolution == 4000:  # 分辨率为 1000
            satellite_type1 = ['FY4A']
            if self.satellite in satellite_type1:
                #                 data['CH_07'] = 2666.666
                data['CH_08'] = 2666.666
                data['CH_09'] = 1600.000
                data['CH_10'] = 1408.450
                data['CH_11'] = 1176.470
                data['CH_12'] = 934.579
                data['CH_13'] = 833.333
                data['CH_14'] = 740.740
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

if __name__ == '__main__':
    L1File = 'D:/data/AGRI/FY4A-_AGRI--_N_DISK_1047E_L1-_FDI-_MULT_NOM_20180101000000_20180101001459_4000M_V0001.HDF'
    agri = ReadAgriL1(L1File)
    print agri.satellite  # 卫星名
    print agri.sensor  # 传感器名
    print agri.ymd  # L1 文件年月日 YYYYMMDD
    print agri.hms  # L1 文件时分秒 HHMMSS
    print agri.resolution  # 分辨率
    print agri.channels  # 通道数量
    print agri.data_shape
#     print agri.file_attr

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
#     t_data = agri.get_dn()
#     print_channel_data(t_data)
#     print 'ref:'
#     t_data = agri.get_ref()
#     print_channel_data(t_data)
#
#     print 'tbb:'
#     t_data = agri.get_tbb()
#     print_channel_data(t_data)

    print 'rad:'
    t_data = agri.get_rad()
    print_channel_data(t_data)
#
#     print 'tbb->rad_test (tbb=250) lut:'
#     t_data = agri.get_rad_test()
#     for key in sorted(t_data.keys()):
#         print key, t_data[key]
#
#     print 'tbb->rad_test2 (tbb=250) planck:'
#     t_data = agri.get_rad_test2()
#     for key in sorted(t_data.keys()):
#         print key, t_data[key]
#
#     print 'rad->tbb_test (rad=36) lut:'
#     t_data = agri.get_tbb_test()
#     for key in sorted(t_data.keys()):
#         print key, t_data[key]
#
#     print 'rad->tbb_test2 (rad=36) planck:'
#     t_data = agri.get_tbb_test2()
#     for key in sorted(t_data.keys()):
#         print key, t_data[key]
#
#     print 'longitude:'
#     t_data = agri.get_longitude()
#     print_data_status(t_data)
#     print 'latitude:'
#     t_data = agri.get_latitude()
#     print_data_status(t_data)
#     print 'sensor_azimuth:'
#     t_data = agri.get_sensor_azimuth()
#     print_data_status(t_data)
#     print 'sensor_zenith:'
#     t_data = agri.get_sensor_zenith()
#     print_data_status(t_data)
#     print 'solar_azimuth:'
#     t_data = agri.get_solar_azimuth()
#     print_data_status(t_data)
#     print 'solar_zenith:'
#     t_data = agri.get_solar_zenith()
#     print_data_status(t_data)
#     print 'timestamp:'
    t_data = agri.get_timestamp()
    print_data_status(t_data)
    print time.gmtime(t_data[0, 0])
    print time.gmtime(t_data[-1, -1])

#     print 'get_spectral_response:'
#     wavenums, wave_spec = agri.get_spectral_response()
#     print_channel_data(wavenums)
#     print_channel_data(wave_spec)
