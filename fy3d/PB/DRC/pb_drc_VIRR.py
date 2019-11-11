#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
@Time    : 2018/8/16 14:12
@Author  : AnNing
"""
from datetime import datetime
import os
import re

import h5py

from PB.pb_io import attrs2dict
from PB.pb_sat import planck_r2t, is_ad_orbit
from PB.pb_time import get_ymd, get_hm
from pb_drc_base import ReadL1
import numpy as np

g_main_path, g_main_file = os.path.split(os.path.realpath(__file__))


class ReadVirrL1(ReadL1):
    """
    读取 VIRR 传感器的 L1 数据
    分辨率：1000
        卫星： FY3A FY3B FY3C
        通道数量：10
        可见光通道：1 2 6 7 8 9 10
        红外通道：3 4 5
    分辨率：250
        卫星：
        通道数量：
        可见光通道：
        红外通道：
    """

    def __init__(self, in_file):
        sensor = 'VIRR'
        super(ReadVirrL1, self).__init__(in_file, sensor)

    def set_resolution(self):
        """
        根据L1文件名 set self.resolution 分辨率
        :return:
        """
        file_name = os.path.basename(self.in_file)
        if '1000M' in file_name:
            self.resolution = 1000
        # elif '0250M' in file_name:
        #     self.resolution = 250
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

    def set_ymd_hms(self):
        """
        根据根据L1文件名 set self.level_1_ymd 和 self.level_1_ymd
        :return:
        """
        self.file_name = os.path.basename(self.in_file)
        self.ymd = get_ymd(self.file_name)
        self.hms = get_hm(self.file_name) + '00'

    def set_file_attr(self):
        """
        根据 self.file_level_1 获取 L1 文件的属性
        set self.level_1_attr
        储存格式是字典
        :return:
        """
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                # L1文件属性
                with h5py.File(self.in_file, 'r') as hdf5_file:
                    file_attr_l1 = attrs2dict(hdf5_file.attrs)

                self.file_attr = file_attr_l1
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                "Cant handle this resolution: ".format(self.resolution))
        return self.file_attr

    def set_file_attr_obc(self):
        """
        根据 self.file_level_1 获取 L1 文件的属性
        set self.level_1_attr
        储存格式是字典
        :return:
        """
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                # OBC 文件属性
                data_file = self.__get_obc_file()
                with h5py.File(data_file, 'r') as hdf5_file:
                    file_attr_obc = attrs2dict(hdf5_file.attrs)

                self.file_attr_obc = file_attr_obc
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                "Cant handle this resolution: ".format(self.resolution))
        return self.file_attr_obc

    def set_data_shape(self):
        """
        根据 self.satellite set self.dataset_shape
        :return:
        """
        # 如果分辨率是 1000 米
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                self.data_shape = (1800, 2048)
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
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(data_file, 'r') as hdf5_file:
                    emissive = hdf5_file.get('/EV_Emissive')[:, :s[0], :s[1]]
                    valid_range_emissive = (0, 50000)
                    ref_sb = hdf5_file.get('/EV_RefSB')[:, :s[0], :s[1]]
                    valid_range_ref = (0, 32767)
            elif self.satellite in satellite_type2:
                data_file = self.in_file
                with h5py.File(data_file, 'r') as hdf5_file:
                    emissive = hdf5_file.get('/Data/EV_Emissive')[:]
                    valid_range_emissive = (0, 32767)
                    ref_sb = hdf5_file.get('/Data/EV_RefSB')[:]
                    valid_range_ref = (0, 32767)
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 1 2 6 7 8 9 10 为可见光通道，dn 值为 ref_sb
            # 3 4 5 为红外通道，dn 值为 emissive
            for i in xrange(self.channels):
                channel_name = 'CH_{:02d}'.format(i + 1)
                if i < 2:
                    k = i
                    data_pre = ref_sb[k]
                    # 开始处理
                    v = valid_range_ref
                    data_pre = data_pre.astype(np.float32)
                    invalid_index = np.logical_or(
                        data_pre <= v[0], data_pre > v[1])
                    data_pre[invalid_index] = np.nan
                    channel_data = data_pre
                elif 2 <= i <= 4:
                    k = i - 2
                    data_pre = emissive[k]
                    # 开始处理
                    v = valid_range_emissive
                    data_pre = data_pre.astype(np.float32)
                    invalid_index = np.logical_or(
                        data_pre <= v[0], data_pre > v[1])
                    data_pre[invalid_index] = np.nan
                    channel_data = data_pre
                else:
                    k = i - 3
                    data_pre = ref_sb[k]
                    # 开始处理
                    v = valid_range_ref
                    data_pre = data_pre.astype(np.float32)
                    invalid_index = np.logical_or(
                        data_pre <= v[0], data_pre > v[1])
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
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            if self.satellite in satellite_type1:
                s = self.data_shape
                k0_k1_vis = self.file_attr['RefSB_Cal_Coefficients']
                infile_k0k1 = os.path.join('./k0k1/v_20190730/%s' % self.ymd, self.file_name)
                infile_k0k1 = self.in_file
                with h5py.File(infile_k0k1, 'r') as hdf5_file:
                    # k1_ir = hdf5_file.get('Emissive_Radiance_Scales')[0: s[0]]
                    k0_ir = hdf5_file.get('Emissive_Radiance_Offsets')[0: s[0]]
            elif self.satellite in satellite_type2:
                k0_k1_vis = self.file_attr['RefSB_Cal_Coefficients']
                with h5py.File(self.in_file, 'r') as hdf5_file:
                    # k1_ir = hdf5_file.get('/Data/Emissive_Radiance_Scales')[:]
                    k0_ir = hdf5_file.get(
                        '/Data/Emissive_Radiance_Offsets')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
            for i in xrange(self.channels):
                channel_name = 'CH_{:02d}'.format(i + 1)
                if i < 2:
                    k = i * 2
                    # k1 = k0_k1_vis[k]
                    k0 = k0_k1_vis[k + 1]
                    channel_data = np.full(
                        self.data_shape, k0, dtype = np.float32)
                    data[channel_name] = channel_data
                elif 2 <= i <= 4:
                    k = i - 2
                    # data_pre = k1_ir[:, k].reshape(-1, 1)
                    data_pre = k0_ir[:, k].reshape(-1, 1)
                    channel_data = np.full(
                        self.data_shape, np.nan, dtype = np.float32)
                    channel_data[:] = data_pre
                    data[channel_name] = channel_data
                else:
                    k = (i - 3) * 2
                    # k1 = k0_k1_vis[k]
                    k0 = k0_k1_vis[k + 1]
                    channel_data = np.full(
                        self.data_shape, k0, dtype = np.float32)
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
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            if self.satellite in satellite_type1:
                s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                k0_k1_vis = self.file_attr['RefSB_Cal_Coefficients']
                infile_k0k1 = os.path.join('./k0k1/v_20190730/%s' % self.ymd, self.file_name)
                infile_k0k1 = self.in_file
                with h5py.File(infile_k0k1, 'r') as hdf5_file:
                    k1_ir = hdf5_file.get('Emissive_Radiance_Scales')[0: s[0]]
                    # k0_ir = hdf5_file.get('Emissive_Radiance_Offsets')[0: s[0]]
            elif self.satellite in satellite_type2:
                k0_k1_vis = self.file_attr['RefSB_Cal_Coefficients']
                with h5py.File(self.in_file, 'r') as hdf5_file:
                    k1_ir = hdf5_file.get(
                        '/Data/Emissive_Radiance_Scales')[:]
                    # k0_ir = hdf5_file.get('/Data/Emissive_Radiance_Offsets')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
            for i in xrange(self.channels):
                channel_name = 'CH_{:02d}'.format(i + 1)
                if i < 2:
                    k = i * 2
                    k1 = k0_k1_vis[k]
                    # k0 = k0_k1_vis[k + 1]
                    channel_data = np.full(
                        self.data_shape, k1, dtype = np.float32)
                    data[channel_name] = channel_data
                elif 2 <= i <= 4:
                    k = i - 2
                    data_pre = k1_ir[:, k].reshape(-1, 1)
                    # data_pre = k0_ir[:, k].reshape(-1, 1)
                    channel_data = np.full(
                        self.data_shape, np.nan, dtype = np.float32)
                    channel_data[:] = data_pre
                    data[channel_name] = channel_data
                else:
                    k = (i - 3) * 2
                    k1 = k0_k1_vis[k]
                    # k0 = k0_k1_vis[k + 1]
                    channel_data = np.full(
                        self.data_shape, k1, dtype = np.float32)
                    data[channel_name] = channel_data
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_ref(self):
        data = dict()
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            ref_channels = ['CH_{:02d}'.format(i)
                            for i in [1, 2, 6, 7, 8, 9, 10]]
            if self.satellite in satellite_type1:
                dn = self.get_dn()
                k0 = self.get_k0()
                k1 = self.get_k1()
                for channel_name in dn:
                    if channel_name not in ref_channels:
                        continue
                    channel_data = dn[channel_name] * \
                        k1[channel_name] + k0[channel_name]
                    # wangpeng add 20190109 过滤<0的ref
                    invalid_index = np.ma.where(channel_data <= 0.)
                    if len(invalid_index[0] > 0):
                        channel_data[invalid_index] = np.nan
                    data[channel_name] = channel_data / 100.
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_rad(self):
        data = dict()
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                dn = self.get_dn()
                k0 = self.get_k0()
                k1 = self.get_k1()
                b0_b1_b2_nonlinear = self.file_attr.get(
                    'Prelaunch_Nonlinear_Coefficients')
                # 通道 3 4 5
                for i in xrange(3):
                    channel_name = 'CH_{:02d}'.format(i + 3)
#                     b0 = b0_b1_b2_nonlinear[i]
#                     b1 = b0_b1_b2_nonlinear[3 + i]
#                     b2 = b0_b1_b2_nonlinear[6 + i]
                    b0 = b0_b1_b2_nonlinear[3 * i]
                    b1 = b0_b1_b2_nonlinear[3 * i + 1]
                    b2 = b0_b1_b2_nonlinear[3 * i + 2]
#                     print b0, b1, b2
                    rad_pre = dn[channel_name] * \
                        k1[channel_name] + k0[channel_name]
                    rad_nonlinear = rad_pre ** 2 * b2 + rad_pre * b1 + b0
                    rad = rad_pre + rad_nonlinear
                    # 过滤无效值

                    invalid_index = np.ma.where(rad <= 0.)
                    if len(invalid_index[0] > 0):
                        rad[invalid_index] = np.nan
                    data[channel_name] = rad
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_tbb(self):
        data = dict()
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            # 红外转tbb的修正系数，所有时次都是固定值
            tbb_k0 = self.get_tbb_k0()
            tbb_k1 = self.get_tbb_k1()
            if self.satellite in satellite_type1:
                lut = self.get_lut_bt()
                rads = self.get_rad()
                central_wave_numbers = self.get_central_wave_number()
                for band in rads:
                    rad = rads[band]
                    central_wave_number = central_wave_numbers[band]
                    if lut:
                        print 'f3 lut'
                        tbb = np.interp(rad, lut['rad'][band], lut['tbb'])
                    else:
                        print 'f3 plank'
                        tbb = planck_r2t(rad, central_wave_number)
                        k0 = tbb_k0[band]
                        k1 = tbb_k1[band]
                        tbb = tbb * k1 + k0
                    data[band] = tbb
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_tbb_k0(self):
        """
        0次项
        # TBB 非线性校正系数
        :return:
        """
        data = dict()
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                if 'Emmisive_BT_Coefficients' in self.file_attr:
                    coeffs_name = 'Emmisive_BT_Coefficients'  # TBB 非线性校正系数
                else:
                    coeffs_name = 'Emissive_BT_Coefficients'  # TBB 非线性校正系数
                coeffs = self.file_attr.get(coeffs_name)
                for i in xrange(3):
                    channel_name = 'CH_{:02d}'.format(i + 3)
                    # k1 = coeffs[i * 2 + 1]
                    k0 = coeffs[i * 2]
                    data[channel_name] = k0
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_tbb_k1(self):
        """
        1次项
        # TBB 非线性校正系数
        :return:
        """
        data = dict()
        if self.resolution == 1000:  # 分辨率为 1000
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                if 'Emmisive_BT_Coefficients' in self.file_attr:
                    coeffs_name = 'Emmisive_BT_Coefficients'  # TBB 非线性校正系数
                else:
                    coeffs_name = 'Emissive_BT_Coefficients'  # TBB 非线性校正系数
                coeffs = self.file_attr.get(coeffs_name)
                for i in xrange(3):
                    channel_name = 'CH_{:02d}'.format(i + 3)
                    k1 = coeffs[i * 2 + 1]
                    # k0 = coeffs[i * 2]
                    data[channel_name] = k1
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_lut_bt(self):
        """
        return  lut 字典， key值包括tbb和rad，其中rad是字典，key值是通道 CH_01 CH_02 ...
        """
        lut = {}
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                lut_file = os.path.join(
                    g_main_path, 'LUT', '%s_%s_LUT_TB_RB.txt' % (self.satellite, self.sensor))
                if os.path.isfile(lut_file):

                    lut_ary = np.loadtxt(lut_file)
                    if 'tbb' not in lut.keys():
                        lut['tbb'] = lut_ary[:, 0]
                    if 'rad' not in lut.keys():
                        lut['rad'] = {}

                    for i in xrange(3, 6):
                        band = 'CH_{:02d}'.format(i)
                        if band not in lut['rad'].keys():
                            lut['rad'][band] = lut_ary[:, i - 2]
                else:
                    lut = None

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                "Cant handle this resolution: ".format(self.resolution))
        return lut

    def get_sv(self):
        data = dict()
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            if self.satellite in satellite_type1:
                data_file = self.__get_obc_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get('Space_View')[:, :s[0], :s[1]]
            elif self.satellite in satellite_type2:
                data_file = self.__get_obc_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get('/Calibration/Space_View')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < 1, data_pre > 1023)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan

            # 最后10列取平均
            data_pre = np.nanmean(data_pre, axis = 2)

            for i in xrange(self.channels):
                channel_name = 'CH_{:02d}'.format(i + 1)
                channel_data = np.full(
                    self.data_shape, np.nan, dtype = np.float32)

                channel_data[:] = data_pre[i, :].reshape(-1, 1)
                data[channel_name] = channel_data
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_bb(self):
        data = dict()
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']
            if self.satellite in satellite_type1:
                data_file = self.__get_obc_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                s = self.data_shape  # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get('Blackbody_View')[:, :s[0], :s[1]]
            elif self.satellite in satellite_type2:
                data_file = self.__get_obc_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get(
                        '/Calibration/Blackbody_View')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < 1, data_pre > 1023)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan

            # 最后10列取平均
            data_pre = np.nanmean(data_pre, axis = 2)

            for i in xrange(self.channels):
                channel_name = 'CH_{:02d}'.format(i + 1)
                channel_data = np.full(
                    self.data_shape, np.nan, dtype = np.float32)
                channel_data[:] = data_pre[i, :].reshape(-1, 1)
                data[channel_name] = channel_data
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_height(self):
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']

            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                    s = self.data_shape
                    data_pre = hdf5_file.get('Height')[:s[0], :s[1]]
            elif self.satellite in satellite_type2:
                data_file = self.__get_geo_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get('/Geolocation/DEM')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < -1000, data_pre > 10000)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_latitude(self):
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']

            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                    s = self.data_shape
                    data_pre = hdf5_file.get('Latitude')[:s[0], :s[1]]
            elif self.satellite in satellite_type2:
                data_file = self.__get_geo_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get('/Geolocation/Latitude')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < -180, data_pre > 180)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_longitude(self):
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']

            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                    s = self.data_shape
                    data_pre = hdf5_file.get('Longitude')[:s[0], :s[1]]
            elif self.satellite in satellite_type2:
                data_file = self.__get_geo_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get('/Geolocation/Longitude')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < -180, data_pre > 180)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_land_sea_mask(self):
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']

            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                    s = self.data_shape
                    data_pre = hdf5_file.get('LandSeaMask')[:s[0], :s[1]]
            elif self.satellite in satellite_type2:
                data_file = self.__get_geo_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get('/Geolocation/LandSeaMask')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < 0, data_pre > 7)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_land_cover(self):
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']

            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                    s = self.data_shape
                    data_pre = hdf5_file.get('LandCover')[:s[0], :s[1]]
            elif self.satellite in satellite_type2:
                data_file = self.__get_geo_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get('/Geolocation/LandCover')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < 0, data_pre > 17)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_sensor_azimuth(self):
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']

            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                    s = self.data_shape
                    data_pre = hdf5_file.get('SensorAzimuth')[:s[0], :s[1]]
            elif self.satellite in satellite_type2:
                data_file = self.__get_geo_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get(
                        '/Geolocation/SensorAzimuth')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < -18000, data_pre > 18000)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre / 100.
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_sensor_zenith(self):
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']

            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                    s = self.data_shape
                    data_pre = hdf5_file.get('SensorZenith')[:s[0], :s[1]]
            elif self.satellite in satellite_type2:
                data_file = self.__get_geo_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get('/Geolocation/SensorZenith')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < 0, data_pre > 18000)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre / 100.
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_solar_azimuth(self):
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']

            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                    s = self.data_shape
                    data_pre = hdf5_file.get('SolarAzimuth')[:s[0], :s[1]]
            elif self.satellite in satellite_type2:
                data_file = self.__get_geo_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get('/Geolocation/SolarAzimuth')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < -18000, data_pre > 18000)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre / 100.
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_solar_zenith(self):
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']

            if self.satellite in satellite_type1:
                data_file = self.in_file
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                    s = self.data_shape
                    data_pre = hdf5_file.get('SolarZenith')[:s[0], :s[1]]
            elif self.satellite in satellite_type2:
                data_file = self.__get_geo_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get('/Geolocation/SolarZenith')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < 0, data_pre > 18000)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data = data_pre / 100.
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_timestamp(self):
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                seconds_of_file = 300  # 一个时次持续 300 秒
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
            file_date = datetime.strptime(self.ymd + self.hms, '%Y%m%d%H%M%S')
            timestamp = (
                file_date - datetime(1970, 1, 1, 0, 0, 0)).total_seconds()
            row_length = self.data_shape[0]
            delta = np.linspace(0, seconds_of_file - 1, row_length)
            data = np.full(self.data_shape, np.nan, dtype = np.float64)
            data[:] = (delta + timestamp).reshape(-1, 1)
            data = data.astype(np.int32)
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_central_wave_number(self):
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                # 固定值
                # 中心波数: wn(cm-1) = 10 ^ 7 / wave_length(nm)
                data = {'CH_03': 2673.796, 'CH_04': 925.925, 'CH_05': 833.333}
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_spectral_response(self):
        wave_number_dict = dict()
        response_dict = dict()
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B', 'FY3C']
            if self.satellite in satellite_type1:
                dtype = {
                    'names': ('wave_length', 'response'), 'formats': ('f4', 'f4')}
                for i in xrange(self.channels):
                    k = i + 1
                    channel_name = "CH_{:02d}".format(k)
                    file_name = '{}_{}_SRF_CH{:02d}_Pub.txt'.format(
                        self.satellite, self.sensor, k)
                    data_file = os.path.join(g_main_path, 'SRF', file_name)
                    if not os.path.isfile(data_file):
                        continue
                    datas = np.loadtxt(data_file, dtype = dtype)
                    wave_length = datas['wave_length'][::-1]
                    wave_number_channel = 10 ** 7 / wave_length
                    wave_number_dict[channel_name] = wave_number_channel
                    response_channel = datas['response'][::-1]
                    response_dict[channel_name] = response_channel
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return wave_number_dict, response_dict

    def __get_geo_file(self):
        """
        返回 GEO 文件
        :return:
        """
        if self.resolution == 1000:
            satellite_type1 = ['FY3C']
            if self.satellite in satellite_type1:
                geo_file = self.in_file[:-12] + 'GEOXX_MS.HDF'
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                "Cant handle this resolution: ".format(self.resolution))
        return geo_file

    def __get_obc_file(self):
        """
        返回 OBC 文件
        :return:
        """
        if self.resolution == 1000:
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

    # 徐寒列 需要增加VIRR的OBC相关内容
    def get_packet_count(self):
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']

            if self.satellite in satellite_type1:
                data_file = self.__get_obc_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                    s = self.data_shape
                    data_pre = hdf5_file.get('Packet_Count')[:s[0]]
            elif self.satellite in satellite_type2:
                data_file = self.__get_obc_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get('/Calibration/Packet_Count')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < 0, data_pre > 16383)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            print 'data_pre', data_pre.shape
            data = np.full(self.data_shape, np.nan, dtype = np.float32)
            data[:] = data_pre.reshape(-1, 1)
        return data

    def get_prt1_count(self):
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']

            if self.satellite in satellite_type1:
                data_file = self.__get_obc_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                    s = self.data_shape
                    data_pre = hdf5_file.get('PRT1_Count')[:s[0]]
            elif self.satellite in satellite_type2:
                data_file = self.__get_obc_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get('/Calibration/PRT1_Count')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < 0, data_pre > 1023)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data_pre = np.nanmean(data_pre, axis = 1)
            data = np.full(self.data_shape, np.nan, dtype = np.float32)
            data[:] = data_pre.reshape(-1, 1)
        return data

    def get_prt2_count(self):
        if self.resolution == 1000:
            satellite_type1 = ['FY3A', 'FY3B']
            satellite_type2 = ['FY3C']

            if self.satellite in satellite_type1:
                data_file = self.__get_obc_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    # FY3A数据不规整，存在 1810,2048 的数据，取 1800,2048
                    s = self.data_shape
                    data_pre = hdf5_file.get('PRT2_Count')[:s[0]]
            elif self.satellite in satellite_type2:
                data_file = self.__get_obc_file()
                if not os.path.isfile(data_file):
                    raise ValueError(
                        'Data file is not exist. {}'.format(data_file))
                with h5py.File(data_file, 'r') as hdf5_file:
                    data_pre = hdf5_file.get('/Calibration/PRT2_Count')[:]
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

            # 过滤无效值
            invalid_index = np.logical_or(data_pre < 0, data_pre > 1023)
            data_pre = data_pre.astype(np.float32)
            data_pre[invalid_index] = np.nan
            data_pre = np.nanmean(data_pre, axis = 1)
            data = np.full(self.data_shape, np.nan, dtype = np.float32)
            data[:] = data_pre.reshape(-1, 1)
        return data


if __name__ == '__main__':
    t_in_file = r'D:\data\VIRR\FY3C_VIRRX_GBAL_L1_20140103_0240_1000M_MS.HDF'
    t_in_file = r'D:\data\VIRR\FY3B_VIRRX_GBAL_L1_20120102_0220_1000M_MS.HDF'
    t_in_file = r'D:\data\VIRR\FY3A_VIRRX_GBAL_L1_20090405_0020_1000M_MS.HDF'
    t_read_l1 = ReadVirrL1(t_in_file)
    print 'attribute', '-' * 50
    print t_read_l1.satellite  # 卫星名
    print t_read_l1.sensor  # 传感器名
    print t_read_l1.ymd  # L1 文件年月日 YYYYMMDD
    print t_read_l1.hms  # L1 文件时分秒 HHMMSS
    print t_read_l1.resolution  # 分辨率
    print t_read_l1.channels  # 通道数量
    print t_read_l1.data_shape
#     print t_read_l1.file_attr  # L1 文件属性
#     print t_read_l1.file_attr['PRT_Temperature_Coefficients']
    print t_read_l1.file_attr['Emmisive_BT_Coefficients']
    print 'Channel', '-' * 50

    def print_data_status(datas, name = None):
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
            print_data_status(channel_data, name = t_channel_name)

#     t_data = t_read_l1.get_lut_bt()
#     print t_data

#     t_data = t_read_l1.get_dn()
#     print 'dn: CH_03 100行 101列', t_data['CH_03'][99, 100]
#     print 'dn: CH_04 100行 101列', t_data['CH_04'][99, 100]
#     print 'dn: CH_05 100行 101列', t_data['CH_05'][99, 100]
#     print_channel_data(t_data)
#     print np.where(np.isclose(t_data['CH_04'], 997.))
#
#     print 'k0:'
#     t_data = t_read_l1.get_k0()
#     print_channel_data(t_data)
#
#     print 'k1:'
#     t_data = t_read_l1.get_k1()
#     print_channel_data(t_data)
#
#     print 'k2:'
#     t_data = t_read_l1.get_k2()
#     print_channel_data(t_data)
#
    print 'ref:'
    t_data = t_read_l1.get_ref()
    print_channel_data(t_data)

#     t_data = t_read_l1.get_k1()
#     print_channel_data(t_data)
#     print 'rad: CH_03 100行 101列', t_data['CH_03'][99, 100]
#     print 'rad: CH_04 100行 101列', t_data['CH_04'][99, 100]
#     print 'rad: CH_05 100行 101列', t_data['CH_05'][99, 100]
#     print 'tbb:'
#     t_data = t_read_l1.get_tbb()
#     print_channel_data(t_data)
#
#     print 'sv:'
#     t_data = t_read_l1.get_sv()
#     print_channel_data(t_data)

#     print 'get_packet_count'
#     t_data = t_read_l1.get_packet_count()
#     print print_data_status(t_data)
#
#     print 'bb:'
#     t_data = t_read_l1.get_bb()
#     print_channel_data(t_data)
#
#     t_data = t_read_l1.get_central_wave_number()
#     print 'central_wave_number:'
#     print t_data
#
#     t_data1, t_data2 = t_read_l1.get_spectral_response()
#     print 'wave_number:'
#     print_channel_data(t_data1)
#     print 'response:'
#     print_channel_data(t_data2)
#
#     print 'No channel', '-' * 50
#
#     t_data = t_read_l1.get_height()
#     print 'height'
#     print_data_status(t_data)
#
#     t_data = t_read_l1.get_latitude()
#     print 'latitude:'
#     print_data_status(t_data)
#     print is_ad_orbit(t_data)
#
#     t_data = t_read_l1.get_longitude()
#     print 'longitude:'
#     print_data_status(t_data)
#
#     t_data = t_read_l1.get_land_cover()
#     print 'land_cover:'
#     print_data_status(t_data)
#
#     t_data = t_read_l1.get_land_sea_mask()
#     print 'land_sea_mask:'
#     print_data_status(t_data)
#
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
#
#     t_data = t_read_l1.get_timestamp()
#     print 'timestamp:'
#     print_data_status(t_data)
#     datetime_timestamp = datetime.utcfromtimestamp(t_data[0][0])
#     datetime_file = datetime.strptime(
#         t_read_l1.ymd + t_read_l1.hms, '%Y%m%d%H%M%S')
#     if datetime_timestamp != datetime_file:
#         print 'Error', '-' * 100
#         print t_data[0][0], datetime_timestamp
#         print t_read_l1.ymd + t_read_l1.hms, datetime_file
#         raise ValueError('Please check the get_timestamp')
