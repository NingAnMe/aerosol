#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
@Time    : 2018/8/21 17:51
@Author  : AnNing
"""
import os
"""
类名：
ReadSensorL1

# 初始化
self.__init__(in_file)
self.in_file = in_file  # L1 文件的完整路径 （str）

# 数据外部描述信息
self.satellite = None # 卫星名  （str）
self.sensor = sensor # 传感器名  （str）
self.ymd = None # L1 文件年月日 YYYYMMDD  （str）
self.hms = None # L1 文件时分秒 HHMMSS  （str）
self.resolution = None # 传感器分辨率  （int）
self.channels = None # 通道数量，（int）
self.file_attr = None # L1 文件属性，（dict）
self.data_shape = None # 处理以后的数据 np.shape （tuple）

# 初始化执行的方法
self.set_resolution()  # 设置 self.resolution
self.set_satellite()  # 设置  self.satellite
self.set_ymd_hms()  # 设置  self.ymd 和 self.hms
self.set_file_attr()  # 设置  self.file_attr
self.set_data_shape()  # 设置  self.data_shape
self.set_channels()  # 设置  self.channels

数据获取方法实现原则

需要完成的功能：
1 过滤原数据无效值和填充值，过滤后无效数据统一使用 np.nan 填充
2 统一 shape 为 self.data_shape 大小
3 统一数据 dtype 为 np.float32
4 统一通道相关和通道无关数据的存放格式，通道相关使用字典类型储存，键名为‘CH_01’ ‘CH_02’等
分通道数据格式：
channel_data = np.full(self.data_shape, np.nan, dtype=float32)
data = {"CH_01":channel_data}
不分通道数据格式：
data = np.full(self.data_shape, np.nan, dtype=float32)
5 返回的数据使用 np.ndarray 类

# 1通道相关数据获取方法
self.get_dn(self)  # 太阳反射通道地球观测值和发射通道地球观测值
self.get_k0(self)  # 定标系数
self.get_k1(self)  # 定标系数
self.get_k2(self)  # 定标系数
self.get_ref(self)  # 反射率
self.get_rad(self)  # 辐射值
self.get_tbb(self)  # 亮温值
self.get_sv(self)  # 空间观测值
self.get_bb(self)  # 黑体观测值

# 2通道相关，但数据的 shape 与 self.data_shape 不同
self.get_central_wave_number(self)  # 中心波数，单位 cm^-1，shape = （1，）
self.get_spectral_response(self)  # 波数和波数对应的响应值（波数从小到大排列，波数单位 cm^-1），shape = （n，1）
self.get_tbb_k0(self)  # 亮温修正系数，shape = （1，）
self.get_tbb_k1(self)  # 亮温修正系数，shape = （1，）

# 3非通道数据获取方法
self.get_cloudmask(self) # 云检测结果
self.get_height(self)  # 高度
self.get_latitude(self)  # 纬度
self.get_longitude(self)  # 经度
self.get_land_sea_mask(self)  # 海陆类型
self.get_land_cover(self)  # 地表覆盖类型
self.get_sensor_azimuth(self)  # 卫星方位角
self.get_sensor_zenith(self)  # 卫星天顶角
self.get_solar_azimuth(self)  # 太阳方位角
self.get_solar_zenith(self)  # 太阳高度角
self.get_relative_azimuth(self)  # 相对方位角
self.get_timestamp(self)  # UTC 时间戳（距离1970年1月1日0时的秒数）

# 4非通道数据，但数据的 shape 与 self.data_shape 不同
"""


class ReadL1(object):
    """
    读取处理 L1 数据
    传感器：
    分辨率：
        目前支持卫星：
        通道数量：
        可见光通道：
        红外通道：
    """

    def __init__(self, in_file, sensor):
        if not os.path.isfile(in_file):
            raise ValueError('{} is not exist.'.format(in_file))
#         else:
#             print 'Read {} L1 : <<< {}'.format(sensor, in_file)

        self.in_file = in_file

        self.satellite = None  # 卫星名
        self.sensor = sensor  # 传感器名
        self.ymd = None  # L1 文件年月日 YYYYMMDD
        self.hms = None  # L1 文件时分秒 HHMMSS
        self.resolution = None  # 分辨率
        self.channels = None  # 通道数量
        self.file_attr = None  # L1 文件属性
        self.data_shape = None  # 处理以后的数据 np.shape
        # 中心波数: wn(cm-1) = 10 ^ 7 / wave_length(nm)
        self.central_wave_number = None

        # 执行初始化相关方法
        self.set_resolution()  # 设置 self.resolution
        self.set_satellite()  # 设置  self.satellite
        self.set_ymd_hms()  # 设置  self.ymd 和 self.hms
        self.set_file_attr()  # 设置  self.file_attr
        self.set_data_shape()  # 设置  self.data_shape
        self.set_channels()  # 设置  self.channels

    def set_satellite(self):
        pass

    def set_resolution(self):
        pass

    def set_ymd_hms(self):
        pass

    def set_file_attr(self):
        pass

    def set_data_shape(self):
        pass

    def set_channels(self):
        pass

    def get_dn(self):
        return

    def get_ref(self):
        return

    def get_rad(self):
        return

    def get_tbb(self):
        return

    def get_lut_bt(self):
        return

    def get_tbb_k0(self):
        return

    def get_tbb_k1(self):
        return

    def get_sv(self):
        return

    def get_bb(self):
        return

    def get_k0(self):
        return

    def get_k1(self):
        return

    def get_k2(self):
        return

    def get_k3(self):
        return

    def get_height(self):
        return

    def get_latitude(self):
        return

    def get_longitude(self):
        return

    def get_land_sea_mask(self):
        return

    def get_land_cover(self):
        return

    def get_sensor_azimuth(self):
        return

    def get_sensor_zenith(self):
        return

    def get_solar_azimuth(self):
        return

    def get_solar_zenith(self):
        return

    def get_relative_azimuth(self):
        return

    def get_timestamp(self):
        return

    def get_central_wave_number(self):
        return

    def get_spectral_response(self):
        return

    def get_cloudmask(self):
        return
