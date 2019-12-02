#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
@Time    : 2018/9/6 14:28
@Author  : AnNing
"""
import os
import re
from datetime import datetime

import numpy as np
import scipy.interpolate
import scipy.ndimage
from pyhdf.SD import SD, SDC

from PB.pb_io import attrs2dict
from PB.pb_time import get_ymd
from pb_drc_base import ReadL1


g_main_path, g_main_file = os.path.split(os.path.realpath(__file__))


class ReadMvisrL1(ReadL1):
    """
    读取 MVISR 传感器的 L1 数据
    分辨率：1000
        卫星： FY1C FY1D
        通道数量：4
        可见光通道：1 2 3 4
        红外通道：
    分辨率：250
        卫星：
        通道数量：
        可见光通道：
        红外通道：
    """

    def __init__(self, in_file):
        sensor = 'MVISR'
        super(ReadMvisrL1, self).__init__(in_file, sensor)

    def set_resolution(self):
        """
        根据L1文件名 set self.resolution 分辨率
        :return:
        """
        # FY1C FY1D MVISR L1 数据的文件名中没有注明分辨率
        satellite_type1 = ['FY1C', 'FY1D']
        file_name = os.path.basename(self.in_file)
        for satellite in satellite_type1:
            if satellite in file_name:
                self.resolution = 1000

        if self.resolution is None:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def set_satellite(self):
        """
        根据L1文件名 set self.satellite 卫星名
        :return:
        """
        file_name = os.path.basename(self.in_file)
        pattern = r'([A-Z0-9]+)_L1.*'
        m = re.match(pattern, file_name)
        if m:
            self.satellite = m.groups()[0]
        else:
            raise ValueError('Cant get the satellite name from file name.')

    def set_ymd_hms(self):
        """
        根据根据L1文件名 set self.level_1_ymd 和 self.level_1_ymd
        :return:
        """
        file_name = os.path.basename(self.in_file)
        self.ymd = get_ymd(file_name)
        self.hms = self.__get_hm(file_name) + '00'

    def set_file_attr(self):
        """
        根据 self.file_level_1 获取 L1 文件的属性
        set self.level_1_attr
        储存格式是字典
        :return:
        """
        if self.resolution == 1000:
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                hdf4 = SD(self.in_file, SDC.READ)
                self.file_attr = attrs2dict(hdf4.attributes())
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                "Cant handle this resolution: ".format(self.resolution))

    def set_data_shape(self):
        """
        根据 self.satellite set self.dataset_shape
        :return:
        """
        # 如果分辨率是 1000 米
        if self.resolution == 1000:
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                hdf4 = SD(self.in_file, SDC.READ)
                dn = hdf4.select('Earth_View')[0]
                self.data_shape = dn.shape
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        # elif self.resolution == 250:
        else:
            raise ValueError(
                "Cant handle this resolution: ".format(self.resolution))

    def set_channels(self):
        """
        根据 self.satellite set self.sensor_channel_amount
        :return:
        """
        if self.resolution == 1000:
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                self.channels = 10
        # elif self.resolution == 250:
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def get_dn(self):
        """
        从数据文件中获取 DN 值, set self.dn
        :return:
        """
        data = dict()
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                hdf4 = SD(data_file, SDC.READ)
                ref = hdf4.select('Earth_View')[:]
                valid_range_ref = (0, 32767)
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 1 2 3 4 为可见光通道
            for i in xrange(self.channels):
                channel_name = 'CH_{:02d}'.format(i + 1)
                v = valid_range_ref
                data_pre = ref[i].astype(np.float32)
                invalid_index = np.logical_or(data_pre <= v[0], data_pre > v[1])
                data_pre[invalid_index] = np.nan

                channel_data = data_pre
                data[channel_name] = channel_data
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_k0(self):
        """
        0次项
        vis: 可见光缩写
        ir: 红外缩写
        :return:
        """
        data = dict()
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                hdf4 = SD(data_file, SDC.READ)
                coeff_dataset = hdf4.select('Calibration_coeff')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 1 2 3 4 为可见光通道
            for i in xrange(self.channels):
                channel_name = 'CH_{:02d}'.format(i + 1)
                data_pre = coeff_dataset[:, i * 2 + 1].astype(np.float32)
                data_pre = data_pre.reshape(-1, 1)
                # 过滤无效值
                invalid_index = np.logical_and(np.isnan(data_pre), np.isinf(data_pre))
                data_pre[invalid_index] = np.nan

                channel_data = np.zeros(self.data_shape, dtype=np.float32)
                channel_data[:] = data_pre
                data[channel_name] = channel_data
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_k1(self):
        """
        1次项
        vis: 可见光缩写
        ir: 红外缩写
        :return:
        """
        data = dict()
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                hdf4 = SD(data_file, SDC.READ)
                coeff_dataset = hdf4.select('Calibration_coeff')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 1 2 3 4 为可见光通道
            for i in xrange(self.channels):
                channel_name = 'CH_{:02d}'.format(i + 1)
                data_pre = coeff_dataset[:, i * 2].astype(np.float32)
                data_pre = data_pre.reshape(-1, 1)
                # 过滤无效值
                invalid_index = np.logical_and(np.isnan(data_pre), np.isinf(data_pre))
                data_pre[invalid_index] = np.nan

                channel_data = np.zeros(self.data_shape, dtype=np.float32)
                channel_data[:] = data_pre
                data[channel_name] = channel_data
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_ref(self):
        data = dict()
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY1C', 'FY1D']
            ref_channels = ['CH_{:02d}'.format(i) for i in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]]
            if self.satellite in satellite_type1:
                dn = self.get_dn()
                k0 = self.get_k0()
                k1 = self.get_k1()
                for channel_name in dn:
                    if channel_name not in ref_channels:
                        continue
                    channel_data = dn[channel_name] * k1[channel_name] + k0[channel_name]
                    data[channel_name] = channel_data
            else:
                raise ValueError('Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_sv(self):
        """
        :return:
        """
        data = dict()
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                hdf4 = SD(data_file, SDC.READ)
                sv = hdf4.select('Space_View')[:]
                valid_range = (1, 1023)
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            for i in xrange(self.channels):
                channel_name = 'CH_{:02d}'.format(i + 1)
                v = valid_range
                data_pre = sv[i].astype(np.float32)
                data_pre = extend_matrix_2d(data_pre, len(data_pre[0]), self.data_shape[1])
                invalid_index = np.logical_or(data_pre <= v[0], data_pre > v[1])
                data_pre[invalid_index] = np.nan

                channel_data = data_pre
                data[channel_name] = channel_data
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_bb(self):
        """
        :return:
        """
        data = dict()
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                hdf4 = SD(data_file, SDC.READ)
                bb = hdf4.select('Black_Body_View')[:]
                valid_range = (1, 1023)
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            for i in xrange(self.channels):
                channel_name = 'CH_{:02d}'.format(i + 1)
                v = valid_range
                data_pre = bb[i].astype(np.float32)
                data_pre = extend_matrix_2d(data_pre, len(data_pre[0]), self.data_shape[1])
                invalid_index = np.logical_or(data_pre <= v[0], data_pre > v[1])
                data_pre[invalid_index] = np.nan

                channel_data = data_pre
                data[channel_name] = channel_data
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_latitude(self):
        """
        :return:
        """
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                hdf4 = SD(data_file, SDC.READ)
                data_pre = hdf4.select('Latitude')[:]
                valid_range = (-180, 180)
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            v = valid_range
            data_pre = data_pre.astype(np.float32)
            data_pre = congrid(data_pre, self.data_shape, method='spline')
            invalid_index = np.logical_or.reduce((data_pre <= v[0], data_pre > v[1],
                                                  data_pre == 0.))
            data_pre[invalid_index] = np.nan

            data = data_pre
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_longitude(self):
        """
        :return:
        """
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                hdf4 = SD(data_file, SDC.READ)
                data_pre = hdf4.select('Longitude')[:]
                valid_range = (-180, 180)
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            v = valid_range
            data_pre = data_pre.astype(np.float32)
            data_pre = congrid(data_pre, self.data_shape, method='spline')
            invalid_index = np.logical_or.reduce((data_pre <= v[0], data_pre > v[1],
                                                  data_pre == 0.))
            data_pre[invalid_index] = np.nan

            data = data_pre
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_sensor_zenith(self):
        """
        :return:
        """
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                hdf4 = SD(data_file, SDC.READ)
                data_pre = hdf4.select('Latitude')[:]
                valid_range = (-180, 180)
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            v = valid_range
            data_pre = data_pre.astype(np.float32)
            data_pre = congrid(data_pre, self.data_shape, method='spline')
            invalid_index = np.logical_or.reduce((data_pre <= v[0], data_pre > v[1],
                                                  data_pre == 0.))
            data_pre[invalid_index] = np.nan

            data = data_pre
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_solar_zenith(self):
        """
        :return:
        """
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                hdf4 = SD(data_file, SDC.READ)
                data_pre = hdf4.select('Solar_Zenith')[:]
                valid_range = (-180, 180)
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            v = valid_range
            data_pre = data_pre.astype(np.float32)
            data_pre = congrid(data_pre, self.data_shape, method='spline')
            invalid_index = np.logical_or.reduce((data_pre <= v[0], data_pre > v[1],
                                                  data_pre == 0.))
            data_pre[invalid_index] = np.nan

            data = data_pre
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_relative_azimuth(self):
        """
        :return:
        """
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                hdf4 = SD(data_file, SDC.READ)
                data_pre = hdf4.select('Relative_Azimuth')[:]
                valid_range = (0, 360)
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            v = valid_range
            data_pre = data_pre.astype(np.float32)
            data_pre = congrid(data_pre, self.data_shape, method='spline')
            invalid_index = np.logical_or.reduce((data_pre <= v[0], data_pre > v[1],
                                                  data_pre == 0.))
            data_pre[invalid_index] = np.nan

            data = data_pre
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_timestamp(self):
        """
        :return:
        """
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                hdf4 = SD(data_file, SDC.READ)
                data_pre = hdf4.select('Msec_Count')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            data_pre = data_pre.astype(np.float32)

            invalid_data = data_pre <= 0
            data_pre[invalid_data] = np.nan
            time_min = np.nanmin(data_pre)
            time_max = np.nanmax(data_pre)

            seconds_of_file = float((time_max - time_min) / 1000)

            file_date = datetime.strptime(self.ymd + self.hms, '%Y%m%d%H%M%S')
            timestamp = (file_date - datetime(1970, 1, 1, 0, 0, 0)).total_seconds()
            row_length = self.data_shape[0]
            delta = np.linspace(0, seconds_of_file - 1, row_length)
            data = np.full(self.data_shape, np.nan, dtype=np.float64)
            data[:] = (delta + timestamp).reshape(-1, 1)
            data = data.astype(np.int32)
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    @staticmethod
    def __get_hm(file_name):
        """
        从输入文件中获取 hm
        :return:
        """
        if not isinstance(file_name, str):
            return
        m = re.match(r".*_(\d{4})", file_name)

        if m is None:
            return
        else:
            return m.groups()[0]

    def get_latitude_originality(self):
        """
        :return:
        """
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                hdf4 = SD(data_file, SDC.READ)
                data_pre = hdf4.select('Latitude')[:]
                valid_range = (-180, 180)
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            v = valid_range
            data_pre = data_pre.astype(np.float32)
            invalid_index = np.logical_or.reduce((data_pre <= v[0], data_pre > v[1],
                                                  data_pre == 0.))
            data_pre[invalid_index] = np.nan

            data = data_pre
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_longitude_originality(self):
        """
        :return:
        """
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY1C', 'FY1D']
            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                hdf4 = SD(data_file, SDC.READ)
                data_pre = hdf4.select('Longitude')[:]
                valid_range = (-180, 180)
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            v = valid_range
            data_pre = data_pre.astype(np.float32)
            invalid_index = np.logical_or.reduce((data_pre <= v[0], data_pre > v[1],
                                                  data_pre == 0.))
            data_pre[invalid_index] = np.nan

            data = data_pre
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data


def congrid(a, newdims, method='linear', centre=False, minusone=False):
    """Arbitrary resampling of source array to new dimension sizes.
    Currently only supports maintaining the same number of dimensions.
    To use 1-D arrays, first promote them to shape (x,1).

    Uses the same parameters and creates the same co-ordinate lookup points
    as IDL''s congrid routine, which apparently originally came from a VAX/VMS
    routine of the same name.

    method:
    neighbour - closest value from original data
    nearest and linear - uses n x 1-D interpolations using
                         scipy.interpolate.interp1d
    (see Numerical Recipes for validity of use of n 1-D interpolations)
    spline - uses ndimage.map_coordinates

    centre:
    True - interpolation points are at the centres of the bins
    False - points are at the front edge of the bin

    minusone:
    For example- inarray.shape = (i,j) & new dimensions = (x,y)
    False - inarray is resampled by factors of (i/x) * (j/y)
    True - inarray is resampled by(i-1)/(x-1) * (j-1)/(y-1)
    This prevents extrapolation one element beyond bounds of input array.
    """
    if a.dtype not in [np.float64, np.float32]:
        a = np.cast[float](a)

    m1 = np.cast[int](minusone)
    ofs = np.cast[int](centre) * 0.5
    old = np.array(a.shape)
    ndims = len(a.shape)
    if len(newdims) != ndims:
        print "[congrid] dimensions error. " \
              "This routine currently only support " \
              "rebinning to the same number of dimensions."
        return None
    newdims = np.asarray(newdims, dtype=float)
    dimlist = []

    if method == 'neighbour':
        for i in range(ndims):
            base = np.indices(newdims)[i]
            dimlist.append((old[i] - m1) / (newdims[i] - m1) * (base + ofs) - ofs)
        cd = np.array(dimlist).round().astype(int)
        newa = a[list(cd)]
        return newa

    elif method in ['nearest', 'linear']:
        # calculate new dims
        for i in range(ndims):
            base = np.arange(newdims[i])
            dimlist.append((old[i] - m1) / (newdims[i] - m1) * (base + ofs) - ofs)
        # specify old dims
        olddims = [np.arange(i, dtype=np.float) for i in list(a.shape)]

        # first interpolation - for ndims = any
        mint = scipy.interpolate.interp1d(olddims[-1], a, kind=method)
        newa = mint(dimlist[-1])

        trorder = [ndims - 1] + range(ndims - 1)
        for i in range(ndims - 2, -1, -1):
            newa = newa.transpose(trorder)

            mint = scipy.interpolate.interp1d(olddims[i], newa, kind=method)
            newa = mint(dimlist[i])

        if ndims > 1:
            # need one more transpose to return to original dimensions
            newa = newa.transpose(trorder)

        return newa
    elif method in ['spline']:
        # oslices = [slice(0, j) for j in old]
        # oldcoords = np.ogrid[oslices]
        nslices = [slice(0, j) for j in list(newdims)]
        newcoords = np.mgrid[nslices]

        newcoords_dims = range(newcoords.ndim)
        # make first index last
        newcoords_dims.append(newcoords_dims.pop(0))
        newcoords_tr = newcoords.transpose(newcoords_dims)
        # makes a view that affects newcoords

        newcoords_tr += ofs

        deltas = (np.asarray(old) - m1) / (newdims - m1)
        newcoords_tr *= deltas

        newcoords_tr -= ofs

        newa = scipy.ndimage.map_coordinates(a, newcoords)
        return newa
    else:
        print "Congrid error: Unrecognized interpolation type.\n", \
            "Currently only \'neighbour\', \'nearest\',\'linear\',", \
            "and \'spline\' are supported."
        return None


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


if __name__ == '__main__':
    t_in_file = r'D:\nsmc\fix_data\FY1CD\MVISR\FY1C_L1_GDPT_20030601_0016.HDF'
    t_read_l1 = ReadMvisrL1(t_in_file)
    print 'attribute', '-' * 50
    print t_read_l1.satellite  # 卫星名
    print t_read_l1.sensor  # 传感器名
    print t_read_l1.ymd  # L1 文件年月日 YYYYMMDD
    print t_read_l1.hms  # L1 文件时分秒 HHMMSS
    print t_read_l1.resolution  # 分辨率
    print t_read_l1.channels  # 通道数量
    print t_read_l1.data_shape
    print t_read_l1.file_attr  # L1 文件属性

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


    # print 'dn:'
    # t_data = t_read_l1.get_dn()
    # print_channel_data(t_data)
    #
    # print 'k0:'
    # t_data = t_read_l1.get_k0()
    # print_channel_data(t_data)
    #
    # print 'k1:'
    # t_data = t_read_l1.get_k1()
    # print_channel_data(t_data)
    #
    # print 'k2:'
    # t_data = t_read_l1.get_k2()
    # print_channel_data(t_data)

    # print 'ref:'
    # t_data = t_read_l1.get_ref()
    # print_channel_data(t_data)

    # print 'rad'
    # t_data = t_read_l1.get_rad()
    # print_channel_data(t_data)
    #
    # print 'tbb:'
    # t_data = t_read_l1.get_tbb()
    # print_channel_data(t_data)

    print 'sv:'
    t_data = t_read_l1.get_sv()
    print_channel_data(t_data)

    print 'bb:'
    t_data = t_read_l1.get_bb()
    print_channel_data(t_data)

    # t_data = t_read_l1.get_central_wave_number()
    # print 'central_wave_number:'
    # print t_data
    #
    # t_data1, t_data2 = t_read_l1.get_spectral_response()
    # print 'wave_number:'
    # print_channel_data(t_data1)
    # print 'response:'
    # print_channel_data(t_data2)
    #
    # print 'No channel', '-' * 50
    #
    # t_data = t_read_l1.get_height()
    # print 'height'
    # print_data_status(t_data)

    # t_data = t_read_l1.get_latitude()
    # print 'latitude:'
    # print_data_status(t_data)
    #
    # t_data = t_read_l1.get_longitude()
    # print 'longitude:'
    # print_data_status(t_data)

    # t_data = t_read_l1.get_land_cover()
    # print 'land_cover:'
    # print_data_status(t_data)
    #
    # t_data = t_read_l1.get_land_sea_mask()
    # print 'land_sea_mask:'
    # print_data_status(t_data)

    # t_data = t_read_l1.get_sensor_azimuth()
    # print 'sensor_azimuth:'
    # print_data_status(t_data)

    # t_data = t_read_l1.get_sensor_zenith()
    # print 'sensor_zenith:'
    # print_data_status(t_data)

    # t_data = t_read_l1.get_solar_azimuth()
    # print 'solar_azimuth:'
    # print_data_status(t_data)

    # t_data = t_read_l1.get_solar_zenith()
    # print 'solar_zenith:'
    # print_data_status(t_data)

    # t_data = t_read_l1.get_relative_azimuth()
    # print 'relative_azimuth:'
    # print_data_status(t_data)

    # t_data = t_read_l1.get_timestamp()
    # print 'timestamp:'
    # print_data_status(t_data)
    # datetime_timestamp = datetime.utcfromtimestamp(t_data[0][0])
    # datetime_file = datetime.strptime(
    #     t_read_l1.ymd + t_read_l1.hms, '%Y%m%d%H%M%S')
    # if datetime_timestamp != datetime_file:
    #     print 'Error', '-' * 100
    #     print t_data[0][0], datetime_timestamp
    #     print t_read_l1.ymd + t_read_l1.hms, datetime_file
    #     raise ValueError('Please check the get_timestamp')
