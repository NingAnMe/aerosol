# -*- coding:utf-8 -*-
from datetime import datetime
import os
import re
import sys
import h5py

from PB import pb_sat
from PB.pb_io import attrs2dict
from PB.pb_time import get_ymd, get_hm
from pb_drc_base import ReadL1
import numpy as np

__description__ = u'IRAS传感器读取类'
__author__ = 'wangpeng'
__date__ = '2018-10-22'
__version__ = '1.0.0_beat'

"""
读取处理 L1 数据，1000m 和 250m
处理原则
1 过滤原数据无效值和填充值，过滤后无效数据统一使用 NaN 填充
2 统一 shape
3 统一数据 dtype
4 统一通道相关和通道无关数据的存放格式
"""

g_main_path, g_main_file = os.path.split(os.path.realpath(__file__))


class ReadIrasL1(ReadL1):
    """
     读取 VIRR 传感器的 L1 数据
     分辨率：17000
     卫星： FY3A FY3B FY3C
     通道数量：26
     可见光通道：1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
     红外通道：21 22 23 24 25 26
    """

    def __init__(self, in_file):
        sensor = 'IRAS'
        super(ReadIrasL1, self).__init__(in_file, sensor)

    def set_resolution(self):
        """
        根据L1文件名 set self.resolution 分辨率
        :return:
        """
        file_name = os.path.basename(self.in_file)
        if '017KM' in file_name:
            self.resolution = 17000
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def set_satellite(self):
        """
        根据L1文件名 set self.satellite 卫星名
        :return:
        """
        file_name = os.path.basename(self.in_file)
        pattern = r'([A-Z0-9]+)_%s.*' % self.sensor
        m = re.match(pattern, file_name)
        if m:
            self.satellite = m.groups()[0]
        else:
            raise ValueError('Cant get the satellite name from file name.')

    def __get_obc_file(self):
        """
        返回 OBC 文件
        :return:
        """
        if self.resolution == 17000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                obc_file = self.in_file[:-12] + 'OBCXX_MS.HDF'
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                "Cant handle this resolution: ".format(self.resolution))
        return obc_file

    def set_ymd_hms(self):
        """
        根据根据L1文件名 set self.level_1_ymd 和 self.level_1_ymd
        :return:
        """
        file_name = os.path.basename(self.in_file)
        self.ymd = get_ymd(file_name)
        self.hms = get_hm(file_name) + '00'

    def set_file_attr(self):
        """
        根据 self.file_level_1 获取 L1 文件的属性
        set self.level_1_attr
        储存格式是字典
        :return:
        """
        if self.resolution == 17000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                with h5py.File(self.in_file, 'r') as hdf5_file:
                    self.file_attr = attrs2dict(hdf5_file.attrs)
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
        # 如果分辨率是 17000 米
        data_file = self.in_file
        if self.resolution == 17000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            if self.satellite in satellite_type1:
                with h5py.File(data_file, 'r') as h5r:
                    lats = h5r.get('Latitude')[:]
                    self.data_shape = lats.shape
            elif self.satellite in satellite_type2:
                with h5py.File(data_file, 'r') as h5r:
                    lats = h5r.get('/Geolocation/Latitude')[:]
                    self.data_shape = lats.shape
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
        if self.resolution == 17000:
            self.channels = 26
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def get_dn(self):
        """
        return DN
        """
        data = dict()
        if self.resolution == 17000:  # 分辨率为 17000
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            data_file = self.in_file
            vmin = -4095
            vmax = 4095
            if not os.path.isfile(data_file):
                raise ValueError(
                    'Data file is not exist. {}'.format(data_file))
            if self.satellite in satellite_type1:
                with h5py.File(data_file, 'r') as h5r:
                    ary_ch26_dn = h5r.get('/FY3A_IRAS_DN')[:]
            elif self.satellite in satellite_type2:
                with h5py.File(data_file, 'r') as h5r:
                    ary_ch26_dn = h5r.get('/Data/IRAS_DN')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            for i in xrange(self.channels):
                band = 'CH_{:02d}'.format(i + 1)
                data_pre = ary_ch26_dn[i]

                invalid_index = np.logical_or(
                    data_pre <= vmin, data_pre > vmax)
                data_pre = data_pre.astype(np.float32)
                data_pre[invalid_index] = np.nan
                data[band] = data_pre
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_tbb_k1(self):
        """
        return tbb_k1  dict one value
        """

        data = dict()
        if self.resolution == 17000:  # 分辨率为 1000
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']

            if self.satellite in satellite_type1:
                data['CH_01'] = 1.
                data['CH_02'] = 1.
                data['CH_03'] = 1.
                data['CH_04'] = 1.
                data['CH_05'] = 1.
                data['CH_06'] = 1.
                data['CH_07'] = 1.
                data['CH_08'] = 1.
                data['CH_09'] = 1.
                data['CH_10'] = 1.
                data['CH_11'] = 1.
                data['CH_12'] = 1.
                data['CH_13'] = 1.
                data['CH_14'] = 1.
                data['CH_15'] = 1.
                data['CH_16'] = 1.
                data['CH_17'] = 1.
                data['CH_18'] = 1.
                data['CH_19'] = 1.
                data['CH_20'] = 1.

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_tbb_k0(self):
        """
        return tbb_k1  dict one value
        """

        data = dict()
        if self.resolution == 17000:  # 分辨率为 1000
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']

            if self.satellite in satellite_type1:
                data['CH_01'] = 0
                data['CH_02'] = 0
                data['CH_03'] = 0
                data['CH_04'] = 0
                data['CH_05'] = 0
                data['CH_06'] = 0
                data['CH_07'] = 0
                data['CH_08'] = 0
                data['CH_09'] = 0
                data['CH_10'] = 0
                data['CH_11'] = 0
                data['CH_12'] = 0
                data['CH_13'] = 0
                data['CH_14'] = 0
                data['CH_15'] = 0
                data['CH_16'] = 0
                data['CH_17'] = 0
                data['CH_18'] = 0
                data['CH_19'] = 0
                data['CH_20'] = 0

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_tbb(self):
        """
              从数据文件中获取 DNTBB值, set self.tbb
        :return:
        """
        data = dict()
        if self.resolution == 17000:  # 分辨率为 17000
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            data_file = self.in_file
            vmin = 150.
            vmax = 350.
            if not os.path.isfile(data_file):
                raise ValueError(
                    'Data file is not exist. {}'.format(data_file))
            if self.satellite in satellite_type1:
                with h5py.File(data_file, 'r') as h5r:
                    ary_ch26_tb = h5r.get('/FY3A_IRAS_TB')[:]
            elif self.satellite in satellite_type2:
                with h5py.File(data_file, 'r') as h5r:
                    ary_ch26_tb = h5r.get('/Data/IRAS_TB')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
            for i in xrange(self.channels):
                band = 'CH_{:02d}'.format(i + 1)
                if i > 19:
                    continue
                data_pre = ary_ch26_tb[i]
                invalid_index = np.logical_or(
                    data_pre <= vmin, data_pre > vmax)
                data_pre = data_pre.astype(np.float32)
                data_pre[invalid_index] = np.nan
                data[band] = data_pre
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_rad(self):
        """
           经非线性校正后的RAD
        :return:
        """
        data = dict()
        if self.resolution == 17000:  # 分辨率为 17000
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                tbbs = self.get_tbb()
                central_wave_numbers = self.get_central_wave_number()

                for i in xrange(self.channels):
                    band = 'CH_{:02d}'.format(i + 1)
                    if band in tbbs:
                        tbb = tbbs[band]
                        central_wave_number = central_wave_numbers[band]
                        rad = pb_sat.planck_t2r(tbb, central_wave_number)
                        data[band] = rad
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_height(self):
        """
              从数据文件中获取 DNTBB值, set self.height
        :return:
        """
        if self.resolution == 17000:  # 分辨率为 17000
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            data_file = self.in_file

            if not os.path.isfile(data_file):
                raise ValueError(
                    'Data file is not exist. {}'.format(data_file))
            if self.satellite in satellite_type1:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/Height')[:]
            elif self.satellite in satellite_type2:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/Geolocation/DEM')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            vmin = -400
            vmax = 10000
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre

        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_latitude(self):
        """
            从数据文件中获取 纬度值, set self.latitude
        """
        if self.resolution == 17000:  # 分辨率为 17000
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            data_file = self.in_file

            if not os.path.isfile(data_file):
                raise ValueError(
                    'Data file is not exist. {}'.format(data_file))
            if self.satellite in satellite_type1:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/Latitude')[:]
            elif self.satellite in satellite_type2:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/Geolocation/Latitude')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
            # 过滤无效值
            vmin = -90.
            vmax = 90.
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre

        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_longitude(self):
        """
            从数据文件中获取 经度值, set self.longitude
        """
        if self.resolution == 17000:  # 分辨率为 17000
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            data_file = self.in_file
            if not os.path.isfile(data_file):
                raise ValueError(
                    'Data file is not exist. {}'.format(data_file))
            if self.satellite in satellite_type1:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/Longitude')[:]
            elif self.satellite in satellite_type2:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/Geolocation/Longitude')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
            # 过滤无效值
            vmin = -180.
            vmax = 180.
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre

        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_land_sea_mask(self):
        """
            从数据文件中获取海陆类型, set self.land_sea_mask
        """
        if self.resolution == 17000:  # 分辨率为 17000
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            data_file = self.in_file
            if not os.path.isfile(data_file):
                raise ValueError(
                    'Data file is not exist. {}'.format(data_file))
            if self.satellite in satellite_type1:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/LandSeaMask')[:]
            elif self.satellite in satellite_type2:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/Geolocation/LandSeaMask')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
            # 过滤无效值
            vmin = 0
            vmax = 7
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre

        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_land_cover(self):
        """
            从数据文件中获取土地覆盖, set self.land_cover
        """
        if self.resolution == 17000:  # 分辨率为 17000
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            data_file = self.in_file
            if not os.path.isfile(data_file):
                raise ValueError(
                    'Data file is not exist. {}'.format(data_file))
            if self.satellite in satellite_type1:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/LandCover')[:]
            elif self.satellite in satellite_type2:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/Geolocation/LandCover')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
            # 过滤无效值
            vmin = 0
            vmax = 17
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre

        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_sensor_azimuth(self):
        """
            从数据文件中获取卫星方位角 , set self.sensor_azimuth
        """
        if self.resolution == 17000:  # 分辨率为 17000
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            data_file = self.in_file
            if not os.path.isfile(data_file):
                raise ValueError(
                    'Data file is not exist. {}'.format(data_file))
            if self.satellite in satellite_type1:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/SensorAzimuth')[:]
            elif self.satellite in satellite_type2:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/Geolocation/SensorAzimuth')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
            # 过滤无效值
            vmin = -18000
            vmax = 18000
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre / 100.

        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_sensor_zenith(self):
        """
            从数据文件中获取卫星天顶角 , set self.sensor_zenith
        """
        if self.resolution == 17000:  # 分辨率为 17000
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            data_file = self.in_file
            if not os.path.isfile(data_file):
                raise ValueError(
                    'Data file is not exist. {}'.format(data_file))
            if self.satellite in satellite_type1:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/SensorZenith')[:]
            elif self.satellite in satellite_type2:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/Geolocation/SensorZenith')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
            # 过滤无效值
            vmin = 0
            vmax = 18000
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre / 100.
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_solar_azimuth(self):
        """
            从数据文件中获取太阳的方位角 , set self.solar_azimuth
        """
        if self.resolution == 17000:  # 分辨率为 17000
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            data_file = self.in_file
            if not os.path.isfile(data_file):
                raise ValueError(
                    'Data file is not exist. {}'.format(data_file))
            if self.satellite in satellite_type1:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/SolarAzimuth')[:]
            elif self.satellite in satellite_type2:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/Geolocation/SolarAzimuth')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
            # 过滤无效值
            vmin = -18000
            vmax = 18000
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre / 100.

        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_solar_zenith(self):
        """
            从数据文件中获取太阳的天顶角 , set self.solar_zenith
        """
        if self.resolution == 17000:  # 分辨率为 17000
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            data_file = self.in_file
            if not os.path.isfile(data_file):
                raise ValueError(
                    'Data file is not exist. {}'.format(data_file))
            if self.satellite in satellite_type1:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/SolarZenith')[:]
            elif self.satellite in satellite_type2:
                with h5py.File(data_file, 'r') as h5r:
                    data_pre = h5r.get('/Geolocation/SolarZenith')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
            # 过滤无效值
            vmin = 0
            vmax = 18000
            invalid_index = np.logical_or(data_pre < vmin, data_pre > vmax)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre / 100.

        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_timestamp(self):
        if self.resolution == 17000:  # 分辨率为 17000
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            data_file = self.in_file
            if not os.path.isfile(data_file):
                raise ValueError(
                    'Data file is not exist. {}'.format(data_file))
            if self.satellite in satellite_type1:
                with h5py.File(data_file, 'r') as h5r:
                    scnlin_mscnt = h5r.get('/Scnlin_mscnt')[:]
            elif self.satellite in satellite_type2:
                with h5py.File(data_file, 'r') as h5r:
                    scnlin_mscnt = h5r.get('/Data/Scnlin_mscnt')[:]
            else:
                raise ValueError(
                    'Cant read this data, please check its resolution: {}'.format(self.in_file))
            seconds_of_file = (scnlin_mscnt[-1] - scnlin_mscnt[0]) / 1000

            file_date = datetime.strptime(self.ymd + self.hms, '%Y%m%d%H%M%S')
            timestamp = (file_date - datetime(1970, 1,
                                              1, 0, 0, 0)).total_seconds()
            row_length = self.data_shape[0]
            delta = np.linspace(0, seconds_of_file - 1, row_length)
            data = np.full(self.data_shape, np.nan, dtype=np.float64)
            data[:] = (delta + timestamp).reshape(-1, 1)
            data = data.astype(np.int32)
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_central_wave_number(self):
        """
        return 中心波数
        central_wave_number
        wn(cm-1) = 10 ^ 7 / wave_length(nm)
        """
        if self.resolution == 17000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                data = {'CH_01': 669.976914, 'CH_02': 680.162001, 'CH_03': 691.391561,
                        'CH_04': 702.858560, 'CH_05': 715.270436, 'CH_06': 732.203858,
                        'CH_07': 749.383836, 'CH_08': 801.671379, 'CH_09': 899.414299,
                        'CH_10': 1032.591246, 'CH_11': 1343.617931, 'CH_12': 1364.298075,
                        'CH_13': 1529.295554, 'CH_14': 2191.007796, 'CH_15': 2209.606615,
                        'CH_16': 2237.159430, 'CH_17': 2242.434450, 'CH_18': 2387.507219,
                        'CH_19': 2517.407819, 'CH_20': 2667.944995, 'CH_21': 14431.029680,
                        'CH_22': 11265.161110, 'CH_23': 10601.633020, 'CH_24': 10607.324440,
                        'CH_25': 8098.870570, 'CH_26': 6061.054448}
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
        data1 = dict()
        data2 = dict()
        dtype = {'names': ('wave_length', 'response'), 'formats': ('f4', 'f4')}
        if self.resolution == 17000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
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
                    wave_number = 10 ** 7 / wave_length
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


if __name__ == '__main__':
    t_in_file = 'd:/data/IRAS/FY3C_IRASX_GBAL_L1_20150101_0141_017KM_MS.HDF'
#     t_in_file = 'd:/data/IRAS/FY3C_IRASX_GBAL_L1_20180310_1514_017KM_MS.HDF'
    t_read_l1 = ReadIrasL1(t_in_file)
    print t_read_l1.satellite  # 卫星名
    print t_read_l1.sensor  # 传感器名
    print t_read_l1.ymd  # L1 文件年月日 YYYYMMDD
    print t_read_l1.hms  # L1 文件时分秒 HHMMSS
    print t_read_l1.resolution  # 分辨率
    print t_read_l1.channels  # 通道数量
    print t_read_l1.data_shape
#     print t_read_l1.file_attr  # L1 文件属性

    print 'Channel', '-' * 50

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
#     t_data = t_read_l1.get_dn()
#     print_channel_data(t_data)

#     print 'tbb:'
#     t_data = t_read_l1.get_tbb()
#     print_channel_data(t_data)

#     print 'rad'
#     t_data = t_read_l1.get_rad()
#     print_channel_data(t_data)

#     t_data = t_read_l1.get_central_wave_number()
#     print 'central_wave_number:', t_data
#
#     t1_data, t2_data = t_read_l1.get_spectral_response()
#     print 'wave_number:'
#     print (t1_data.keys())
#     print (t2_data.keys())

#     print 'height'
#     t_data = t_read_l1.get_height()
#     print_data_status(t_data)

#     t_data = t_read_l1.get_latitude()
#     print 'latitude:'
#     print_data_status(t_data)
#
#     t_data = t_read_l1.get_longitude()
#     print 'longitude:'
#     print_data_status(t_data)

#     t_data = t_read_l1.get_land_sea_mask()
#     print 'land_sea_mask:'
#     print_data_status(t_data)
#
#     t_data = t_read_l1.get_land_cover()
#     print 'land_cover:'
#     print_data_status(t_data)

#     t_data = t_read_l1.get_sensor_azimuth()
#     print 'sensor_azimuth:'
#     print_data_status(t_data)
#
#     t_data = t_read_l1.get_sensor_zenith()
#     print 'sensor_zenith:'
#     print_data_status(t_data)
#
#     t_data = t_read_l1.get_solar_azimuth()
#     print 'solar_azimuth:'
#     print_data_status(t_data)
#
#     t_data = t_read_l1.get_solar_zenith()
#     print 'solar_zenith:'
#     print_data_status(t_data)

    t_data = t_read_l1.get_timestamp()
    print 'timestamp:'
    print_data_status(t_data)
    print datetime.utcfromtimestamp(t_data[0][0])
    print datetime.utcfromtimestamp(t_data[-1][-1])
#     print datetime_timestamp
#     datetime_file = datetime.strptime(
#         t_read_l1.ymd + t_read_l1.hms, '%Y%m%d%H%M%S')
#     if datetime_timestamp != datetime_file:
#         print 'Error', '-' * 100
#         print t_data[0][0], datetime_timestamp
#         print t_read_l1.ymd + t_read_l1.hms, datetime_file
#         raise ValueError('Please check the get_timestamp')
