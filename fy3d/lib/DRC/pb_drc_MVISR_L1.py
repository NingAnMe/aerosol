#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
@Time    : 2018/7/6 9:41
@Author  : AnNing
"""
from datetime import datetime, timedelta

import numpy as np
from PB.pb_time import time_block
from congrid import congrid
from pyhdf.SD import SD, SDC
from pb_drc_hdf import ReadHDF4


class CLASS_MVISR_L1(ReadHDF4):

    def __init__(self, in_file=None):
        super(CLASS_MVISR_L1, self).__init__()
        self.error = False
        # 定标使用
        self.sat = 'FY1C'
        self.sensor = 'MVISR'
        self.res = 1100
        self.Band = 4  # 通道数
        self.obrit_direction = []
        self.obrit_num = []
        self.file_attr = {}

        self.data_shape = None  # 数据集shape
        self.in_file = in_file

        self.Dn = {}
        self.Ref = {}
        self.Rad = {}
        self.Tbb = {}

        self.satAzimuth = []
        self.satZenith = []
        self.sunAzimuth = []
        self.sunZenith = []
        self.Lons = []
        self.Lats = []
        self.Time = []
        self.SV = {}
        self.BB = {}
        self.LandSeaMask = []
        self.LandCover = []

        # 新添加
        self.k0 = {}
        self.k1 = {}
        self.RelativeAzimuth = []

        # 其他程序使用
        self.LutFile = []
        self.IR_Coeff = []
        self.VIS_Coeff = []

        # 红外通道的中心波数，固定值，MERSI_Equiv Mid_wn (cm-1)
        self.WN = {}
        # 红外转tbb的修正系数，固定值
        self.TeA = {}
        self.TeB = {}

        # 所有通道的中心波数和对应的响应值 ，SRF
        self.waveNum = {}
        self.waveRad = {}

        self.set_band()

        self.extract_data = {}

    def Load(self, in_file):
        if self.error:
            return
        # try:
        hdf4 = SD(in_file, SDC.READ)
        # 过滤无效值
        year_dataset = hdf4.select('Year_Count')[:]
        idx_valid = np.where(year_dataset != 0)[0]

        year = hdf4.select('Year_Count')[:]
        day = hdf4.select('Day_Count')[:]
        msec_dataset = hdf4.select('Msec_Count')[:]
        year = year[idx_valid][0]
        day = day[idx_valid][0]
        msec_dataset = msec_dataset[idx_valid]

        dn_dataset = hdf4.select('Earth_View')[:]
        sv_dataset = hdf4.select('Space_View')[:]
        bb_dataset = hdf4.select('Black_Body_View')[:]

        dn_dataset = dn_dataset[:, idx_valid, :]
        sv_dataset = sv_dataset[:, idx_valid, :]
        bb_dataset = bb_dataset[:, idx_valid, :]

        sensor_zenith_dataset = hdf4.select('Sensor_Zenith')[:]
        solar_zenith_dataset = hdf4.select('Solar_Zenith')[:]
        relative_azimuth = hdf4.select('Relative_Azimuth')[:]
        sensor_zenith_dataset = sensor_zenith_dataset[idx_valid, :]
        solar_zenith_dataset = solar_zenith_dataset[idx_valid, :]
        relative_azimuth = relative_azimuth[idx_valid, :]

        longitude_dataset = hdf4.select('Longitude')[:]
        latitude_dataset = hdf4.select('Latitude')[:]
        longitude_dataset = longitude_dataset[idx_valid, :]
        latitude_dataset = latitude_dataset[idx_valid, :]

        coeff_dataset = hdf4.select('Calibration_coeff')[:]
        coeff_dataset = coeff_dataset[idx_valid, :]

        time = self.create_time(year, day, msec_dataset)

        self.data_shape = dn_dataset[0].shape
        shape = self.data_shape
        cols_count = shape[1]

        self.Time = self.extend_matrix_2d(time, 1, cols_count)
        self.Lats = congrid(latitude_dataset, shape, method='spline')
        self.Lons = congrid(longitude_dataset, shape, method='spline')
        self.satZenith = congrid(sensor_zenith_dataset, shape, method='spline')
        self.sunZenith = congrid(solar_zenith_dataset, shape, method='spline')
        self.RelativeAzimuth = congrid(relative_azimuth, shape, method='spline')

        for i in xrange(self.Band):
            channel_name = 'CH_{:02d}'.format(i + 1)
            self.Dn[channel_name] = dn_dataset[i, :]
            self.SV[channel_name] = congrid(sv_dataset[i, :], shape, method='spline')
            self.BB[channel_name] = congrid(bb_dataset[i, :], shape, method='spline')

            k0_dataset = self.change_1d_to_2d(coeff_dataset[:, i * 2 + 1])
            k1_dataset = self.change_1d_to_2d(coeff_dataset[:, i * 2])
            self.k0[channel_name] = self.extend_matrix_2d(k0_dataset, 1, cols_count)
            self.k1[channel_name] = self.extend_matrix_2d(k1_dataset, 1, cols_count)
            self.Ref[channel_name] = self.Dn[channel_name] * self.k0[channel_name] + \
                self.k1[channel_name]
        hdf4.end()
        # except Exception as why:
        #     print "{}.{}: {}".format(self.__class__, 'Load', why)
        #     self.error = True

        # 复制文件属性
        self.file_attr = self.read_file_attr(in_file)

    def set_extract_data(self):
        """
        提取程序使用的数据
        :return:
        """
        for i in xrange(self.Band):
            channel_name = 'CH_{:02}'.format(i + 1)
            self.extract_data[channel_name] = dict()
            if channel_name in self.Dn:
                self.extract_data[channel_name]['DN'] = self.filter_invalid_data(
                    self.Dn[channel_name])
            if channel_name in self.Ref:
                self.extract_data[channel_name]['REF'] = self.filter_invalid_data(
                    self.Ref[channel_name])
            if channel_name in self.Ref:
                self.extract_data[channel_name]['SV'] = self.filter_invalid_data(
                    self.SV[channel_name])
            if channel_name in self.Ref:
                self.extract_data[channel_name]['BB'] = self.filter_invalid_data(
                    self.BB[channel_name])
            if channel_name in self.Ref:
                self.extract_data[channel_name]['k0'] = self.filter_invalid_data(
                    self.k0[channel_name])
            if channel_name in self.Ref:
                self.extract_data[channel_name]['k1'] = self.filter_invalid_data(
                    self.k1[channel_name])

        self.extract_data['Longitude'] = self.filter_invalid_data(self.Lons)
        self.extract_data['Latitude'] = self.filter_invalid_data(self.Lats)
        self.extract_data['SensorZenith'] = self.filter_invalid_data(self.satZenith)
        self.extract_data['SolarZenith'] = self.filter_invalid_data(self.sunZenith)
        self.extract_data['RelativeAzimuth'] = self.filter_invalid_data(self.RelativeAzimuth)
        self.extract_data['Time'] = self.filter_invalid_data(self.Time)

    def get_extract_data(self):
        self.set_extract_data()
        return self.extract_data

    def create_time(self, year, day, msec_dataset):
        time = []
        for msec in msec_dataset:
            timestamp = self.year_days_msec_to_timestamp(year, day, msec)
            time.append(timestamp)
        time = self.change_1d_to_2d(time)
        return time

    @staticmethod
    def year_days_msec_to_timestamp(year, day_in_year, msec):
        """
        第几天，第几天，当天第多少毫秒，转为距离1970年的时间戳
        :param year:
        :param day_in_year:
        :param msec:
        :return:
        """
        year_date = datetime.strptime(str(year), '%Y')
        date_1970 = datetime.strptime('1970', '%Y')
        day_delta = timedelta(days=int(day_in_year) - 1)
        second_delata = timedelta(seconds=(int(msec) / 1000))
        delta_since_1970 = year_date + day_delta + second_delata - date_1970
        timestamp = delta_since_1970.total_seconds()
        return timestamp

    @staticmethod
    def change_1d_to_2d(data):
        return np.array(data).reshape(-1, 1)

    @staticmethod
    def interpolate_lat_lon(data, cols_data, cols_count):
        """
        传入的数据必须是 2 维
        原数据每两列按线性比例插值
        :param data: 2D [[]]
        :param cols_data: int
        :param cols_count: int
        :return: 2D [[]]
        """
        if cols_data == 1:
            data_one = data[:].reshape(len(data), -1)
            data_extend = np.tile(data_one, cols_count)
        else:
            data_extend = None
            times = int(np.ceil(float(cols_count) / cols_data))
            for col_data in xrange(int(cols_data) - 1):
                data_times = None
                data_one = data[:, col_data]
                data_next = data[:, col_data + 1]
                if col_data < (int(cols_data) - 2):
                    times = times
                else:
                    times = cols_count - data_extend.shape[1]
                for row_data, value_one in enumerate(data_one):
                    data_inter = np.linspace(data_one[row_data], data_next[row_data], times)
                    data_inter = data_inter.reshape(1, -1)
                    if data_times is None:
                        data_times = data_inter
                    else:
                        data_times = np.concatenate((data_times, data_inter), axis=0)
                if data_extend is None:
                    data_extend = data_times
                else:
                    data_extend = np.concatenate((data_extend, data_times), axis=1)

            if data_extend is not None:
                data_extend = data_extend[:, :cols_count]
        return data_extend

    @staticmethod
    def extend_matrix_2d(data, cols_data, cols_count):
        """
        传入的数据必须是 2 维
        原数据每一列按一定比例扩展到多少列
        :param data:
        :param cols_data:
        :param cols_count:
        :return:
        """
        data_extend = None
        times = int(np.ceil(float(cols_count) / cols_data))
        for i in xrange(cols_data):
            data_one = data[:, i].reshape(len(data), -1)
            data_times = np.tile(data_one, times)
            if data_extend is None:
                data_extend = data_times
            else:
                data_extend = np.concatenate((data_extend, data_times), axis=1)
        if data_extend is not None:
            data_extend = data_extend[:, :cols_count]
        return np.array(data_extend)

    def set_band(self):
        if self.error:
            return
        if self.in_file:
            try:
                hdf4 = SD(self.in_file, SDC.READ)
                self.Band = hdf4.select('Earth_View').shape[0]
                print self.Band
            except Exception as why:
                print "{}.{}: {}".format(self.__class__, 'set_band', why)
                self.error = True
                return

    @staticmethod
    def filter_invalid_data(data):
        """
        过滤无效值，将无效值赋值为nan，
        数据集的dtype改为np.float32
        :param data:
        :return:
        """
        data = data.astype(np.float32)
        idx_invalid = np.where(~np.isfinite(data))
        data[idx_invalid] = np.nan
        return data


if __name__ == '__main__':
    with time_block('all'):
        t_in_file = r'D:\nsmc\fix_data\FY1CD\FY1D_L1_GDPT_20020518_1458.HDF'
        t_mvisr = CLASS_MVISR_L1()
        t_mvisr.Load(t_in_file)
        t_data = t_mvisr.get_extract_data()
        t_channel_name = 'CH_'
        for k in t_data:
            if t_channel_name in k:
                for j in t_data[k]:
                    print k, j, t_data[k][j].shape, np.nanmin(t_data[k][j]), np.nanmax(t_data[k][j])
            else:
                print k, t_data[k].shape, np.nanmin(t_data[k]), np.nanmax(t_data[k])
        print t_mvisr.file_attr
        # mvisr.Load(in_file)
        # print mvisr.ir_coeff_k0['CH_01'].shape
        # print mvisr.ir_coeff_k1['CH_01'].shape
        # print mvisr.Time.shape
        # print mvisr.Lats.shape
        # print mvisr.Lons.shape
    # lat_0 = 28.550000
    # lon_0 = 23.390000
    # lat_max = lat_0 + 3
    # lat_min = lat_0 - 3
    # lon_max = lon_0 + 3
    # lon_min = lon_0 - 3
    #
    # box = [lat_max, lat_min, lon_min, lon_max]
    # # box = [60, 10, 70, 150]
    # p = dv_map()
    # p.easyplot(mvisr.Lats, mvisr.Lons, mvisr.Dn['CH_01'], vmin=None, vmax=None,
    #            ptype=None, markersize=0.1, marker='o', box=box)
    # p.savefig('test070701_01.png')
