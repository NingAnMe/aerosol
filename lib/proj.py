#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
@Time    : 2019/8/8
@Author  : AnNing
"""
import numpy as np
from pyproj import Proj, transform

# 角度 -> 弧度
DEGREES_TO_RADIANS = np.pi / 180.
# 弧度 -> 角度
RADIANS_TO_DEGREES = 180. / np.pi
# 地球平均半径
EARTH_MEAN_RADIUS_KM = 6371.009
# 地球极半径
EARTH_POLAR_RADIUS_KM = 6356.752
# 地球赤道半径
EARTH_EQUATOR_RADIUS_KM = 6378.137

WGS84_A = 6378137.0
WGS84_F = 1.0 / 298.257223563
WGS84_B = WGS84_A * (1.0 - WGS84_F)
WGS84_E2 = 2 * WGS84_F - WGS84_F ** 2


# Rotational angular velocity of Earth in radians/sec from IERS
#   Conventions (2003).
ANGVEL = 7.2921150e-5


def degree2meter(degree):
    return degree * np.pi * EARTH_EQUATOR_RADIUS_KM * 1000. / 180.


def meter2degree(meter):
    return (meter * 180) / (np.pi * EARTH_EQUATOR_RADIUS_KM * 1000)


class ProjCore:
    """
    投影公共类
    """
    def __init__(self, projstr, res, unit,
                 row=None, col=None, pt_tl=None, pt_br=None,):
        """
        [args]:
        projstr proj4投影参数字符串 
        res     分辨率
        unit    分辨率单位，支持 m km deg, 确保单位与投影方式一致
        row     行数
        col     列数
        pt_tl   左上角经纬度元组， 形式如 (lon, lat)
        pt_br   右下角经纬度元组， 形式如 (lon, lat)

        row、 col 和  pt_tl、 pt_br 两对里必须传一对，用以确定网格大小， 不能都是None

        projstr 样例：
        1. 等经纬
           "+init=epsg:4326" or "+proj=longlat +datum=WGS84 +no_defs"
#           "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +x_0=-half_res +y_0=half_res"
        2. 极射赤面
           "+proj=stere +ellps=clrk66 +lat_0=90 +lat_ts=70 +lon_0=0 +k_0=0.969858730377 +a=6371000 +units=m"
        3. 兰勃特等面积
           "+proj=laea +lat_0=-74.180000 +lon_0=-146.620000 +x_0=0 +y_0=0 +ellps=WGS84"
        4. 阿伯斯  (常用于中国区域)
           "+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +a=6378245.0 +b=6356863.0"
        5. 待补充   

        """
        self.proj4str = projstr
        self.pfunc = Proj(self.proj4str)  # 转换函数
        print(self.pfunc)

        if unit == "km":
            self.res = res * 1000
            self.unit = "m"
        elif unit == "deg":
            # self.res = np.deg2rad(res)  # pyproj < 2.0使用
            self.res = res  # pyproj >= 2.0使用
            self.unit = unit
        else:
            self.unit = unit
            self.res = res

        if row is not None and col is not None:
            self.row = row
            self.col = col
            self.x_tl = -(self.col - 1) / 2 * self.res
            self.y_tl = (self.row - 1) / 2 * self.res

        elif pt_tl is not None and pt_br is not None:
            self.x_tl, self.y_tl = self.pfunc(*pt_tl)
            x_br, y_br = self.pfunc(*pt_br)

            self.row = int(round((self.y_tl - y_br) / self.res)) + 1
            self.col = int(round((x_br - self.x_tl) / self.res)) + 1
        else:
            raise ValueError("row、 col 和  pt_tl、 pt_br 两对里必须传一对，用以确定网格大小， 不能都是None")

        self.lons = None
        self.lats = None

        print("投影后的网络大小： ({},{})".format(self.row, self.col))

    def lonslats2ij(self, lons, lats):
        """
        '经纬度转行列号 lons,lats -> i,j
        '参数是n维数组 经纬度
        '返回值是n维数组 行列号
        """
        if isinstance(lons, (list, tuple)):
            lons = np.array(lons)
        if isinstance(lats, (list, tuple)):
            lats = np.array(lats)

        if isinstance(lons, np.ndarray):
            assert lons.shape == lats.shape, \
                "lons and lats must have same shape."

            args_shape = lons.shape
            # 转成1维，因为proj只接收1维参数
            lons = lons.reshape((-1))
            lats = lats.reshape((-1))
            # 通过平面坐标系计算投影后的行和列
            x, y = self.pfunc(lons, lats)
            i = self.__y2i(y)
            j = self.__x2j(x)
            return i.reshape(args_shape), j.reshape(args_shape)
        else:
            x, y = self.pfunc(lons, lats)
            i = self.__y2i(y)
            j = self.__x2j(x)
            return i, j

    def __y2i(self, y):
        """
        y 转 行号
        """
        if isinstance(y, (list, tuple)):
            y = np.array(y)
        return np.rint((self.y_tl - y) / self.res).astype(int)

    def __x2j(self, x):
        """
        x 转 列号
        """
        if isinstance(x, (list, tuple)):
            x = np.array(x)
        return np.rint((x - self.x_tl) / self.res).astype(int)

    def grid_lonslats(self):
        """
        '生成投影后网格 各格点的经纬度
        """
        # 制作一个2维的矩阵
        i, j = np.mgrid[0:self.row:1, 0:self.col:1]
        y = self.__i2y(i)
        x = self.__j2x(j)

        # 把二维的x,y 转成1维，因为proj只接收1维参数
        x = x.reshape((-1))
        y = y.reshape((-1))

        lons, lats = self.pfunc(x, y, inverse=True)
        # 转回2维

        self.lons = lons.reshape((self.row, self.col))
        self.lats = lats.reshape((self.row, self.col))
        return self.lons, self.lats

    def __i2y(self, i):
        """
        '行号 转 y
        """
        if isinstance(i, (list, tuple)):
            i = np.array(i)

        y = self.y_tl - i * self.res
        return y

    def __j2x(self, j):
        """
        '列号 转 x
        """
        if isinstance(j, (list, tuple)):
            j = np.array(j)
        x = j * self.res + self.x_tl
        return x

    def create_lut(self, lons, lats):
        """
        '创建投影查找表, （字典）
        '即 源数据经纬度位置与投影后位置的对应关系
        """
        if isinstance(lons, (list, tuple)):
            lons = np.array(lons)
        if isinstance(lats, (list, tuple)):
            lats = np.array(lats)
        assert lons.shape == lats.shape, "Lons and Lats must have same shape."

        # 投影后必是2维的，行列 proj1_i,proj1_j
        prj_i, prj_j = self.lonslats2ij(lons, lats)

        valid_index = np.logical_and.reduce((prj_i >= 0, prj_i < self.row,
                                             prj_j >= 0, prj_j < self.col))

        if lons.ndim == 1:
            pre_n = np.arange(0, lons.size, 1, "i4")

            # 投影方格以外的数据过滤掉
            prj_i = prj_i[valid_index]
            prj_j = prj_j[valid_index]
            pre_n = pre_n[valid_index]
            return {"pre_n": pre_n, "prj_i": prj_i, "prj_j": prj_j}

        elif lons.ndim == 2:
            pre_row, pre_col = lons.shape
            pre_i, pre_j = np.mgrid[0:pre_row:1, 0:pre_col:1]

            # 投影方格以外的数据过滤掉
            prj_i = prj_i[valid_index]
            prj_j = prj_j[valid_index]
            pre_i = pre_i[valid_index]
            pre_j = pre_j[valid_index]

            return {"pre_i": pre_i, "pre_j": pre_j, "prj_i": prj_i, "prj_j": prj_j}

    def transform2ij(self, proj_str1, x1, y1):
        """
        '不同投影方式之间转换
        '返回值是整数
        """
        args_shape = x1.shape
        x1 = np.array(x1).reshape((-1))  # 转成1维
        y1 = np.array(y1).reshape((-1))
        p1 = Proj(proj_str1)
        x2, y2 = transform(p1, self.pfunc, x1, y1)
        i = self.__y2i(y2)
        j = self.__x2j(x2)
        return i.reshape(args_shape), j.reshape(args_shape)


class ProjGLL:
    """
    等经纬度区域类
    """

    def __init__(self, nlat=90., slat=-90., wlon=-180., elon=180., res_lat=None, res_lon=None, row_max=None,
                 col_max=None):
        """
        nlat, slat, wlon, elon: 北纬, 南纬, 西经, 东经
        resLat: 纬度分辨率（度）
        resLon: 经度分辨率（度）
        """
        self.nlat = float(nlat)  # 北纬
        self.slat = float(slat)  # 南纬
        self.wlon = float(wlon)  # 西经
        self.elon = float(elon)  # 东经

        if res_lat is None and row_max is None:
            raise ValueError("resLat and rowMax must set one")

        if res_lon is None and col_max is None:
            raise ValueError("resLon and colMax must set one")

        if res_lat is None:
            self.rowMax = int(row_max)
            self.resLat = (self.nlat - self.slat) / self.rowMax
        else:
            self.resLat = float(res_lat)
            self.rowMax = int(
                round((self.nlat - self.slat) / self.resLat))  # 最大行数

        if res_lon is None:
            self.colMax = int(col_max)
            self.resLon = (self.elon - self.wlon) / self.colMax
        else:
            self.resLon = float(res_lon)
            self.colMax = int(
                round((self.elon - self.wlon) / self.resLon))  # 最大列数

    def generate_lats_lons(self):
        lats, lons = np.mgrid[
            self.nlat - self.resLat / 2.: self.slat + self.resLat * 0.1:-self.resLat,
            self.wlon + self.resLon / 2.: self.elon - self.resLon * 0.1: self.resLon]
        return lats, lons

    def lonslats2ij(self, lons, lats):
        j = self.lons2j(lons)
        i = self.lats2i(lats)
        return i, j

    def lons2j(self, lons):
        """
        lons: 输入经度
        ret: 返回 输入经度在等经纬度网格上的列号，以左上角为起点0,0
        """
        if isinstance(lons, (list, tuple)):
            lons = np.array(lons)
        if isinstance(lons, np.ndarray):
            idx = np.isclose(lons, 180.)
            lons[idx] = -180.
        return np.floor((lons - self.wlon) / self.resLon).astype(int)  # 列号

    def lats2i(self, lats):
        """
        lats: 输入纬度
        ret: 返回 输入纬度在等经纬度网格上的行号，以左上角为起点0,0
        """
        if isinstance(lats, (list, tuple)):
            lats = np.array(lats)
        return np.floor((self.nlat - lats) / self.resLat).astype(int)  # 行号


def fill_2d(array2d, mask, use_from):
    """
    2维矩阵无效值补点
    array2d  2维矩阵
    mask     无效值掩模矩阵
    useFrom  u/d/l/r, 用上/下/左/右的点来补点
    """
    assert len(array2d.shape) == 2, \
        "array2d must be 2d array."
    assert array2d.shape == mask.shape, \
        "array2d and musk must have same shape."

    condition = np.empty_like(mask)
    # 用上方的有效点补点
    if use_from == 'up' or use_from == 'u':
        condition[1:, :] = mask[1:, :] * (~mask)[:-1, :]
        condition[0, :] = False
        index = np.where(condition)
        array2d[index[0], index[1]] = array2d[index[0] - 1, index[1]]

    # 用右方的有效点补点
    elif use_from == 'right' or use_from == 'r':
        condition[:, :-1] = mask[:, :-1] * (~mask)[:, 1:]
        condition[:, -1] = False
        index = np.where(condition)
        array2d[index[0], index[1]] = array2d[index[0], index[1] + 1]

    # 用下方的有效点补点
    elif use_from == 'down' or use_from == 'd':
        condition[:-1, :] = mask[:-1, :] * (~mask)[1:, :]
        condition[-1, :] = False
        index = np.where(condition)
        array2d[index[0], index[1]] = array2d[index[0] + 1, index[1]]

    # 用左方的有效点补点
    elif use_from == 'left' or use_from == 'l':
        condition[:, 1:] = mask[:, 1:] * (~mask)[:, :-1]
        condition[:, 0] = False
        index = np.where(condition)
        array2d[index[0], index[1]] = array2d[index[0], index[1] - 1]


def fill_points_2d(array2d, invalid_value=0):
    """
    2维矩阵无效值补点
    array2d  2维矩阵
    invalidValue  无效值
    """
    # 用右方的有效点补点
    mask = np.isclose(array2d, invalid_value)
    fill_2d(array2d, mask, 'r')

    # 用左方的有效点补点
    mask = np.isclose(array2d, invalid_value)
    fill_2d(array2d, mask, 'l')

    # 用上方的有效点补点
    mask = np.isclose(array2d, invalid_value)
    fill_2d(array2d, mask, 'u')

    # 用下方的有效点补点
    mask = np.isclose(array2d, invalid_value)
    fill_2d(array2d, mask, 'd')


def fill_points_2d_nan(array2d):
    """
    2维矩阵无效值补点
    array2d  2维矩阵
    invalidValue  无效值
    """
    # 用右方的有效点补点
    mask = np.isnan(array2d)
    fill_2d(array2d, mask, 'r')

    # 用左方的有效点补点
    mask = np.isnan(array2d)
    fill_2d(array2d, mask, 'l')

    # 用上方的有效点补点
    mask = np.isnan(array2d)
    fill_2d(array2d, mask, 'u')

    # 用下方的有效点补点
    mask = np.isnan(array2d)
    fill_2d(array2d, mask, 'd')


if __name__ == '__main__':
    # print('创建投影查找表: +units=m +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
    # res_degree = meter2degree(10000)  # 分辨率，10km
    # print(res_degree)
    # projstr_ = "+units=m +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    # proj = ProjCore(projstr_, res_degree, unit="deg", pt_tl=(69.995, 55.995), pt_br=(139.995, 14.995))  # 角点也要放在格点中心位置

    print('创建投影查找表: +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
    res_degree = meter2degree(10000)  # 分辨率，10km
    print(res_degree)
    projstr_ = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    proj = ProjCore(projstr_, res_degree, unit="deg", pt_tl=(69.995, 55.995), pt_br=(139.995, 14.995))  # 角点也要放在格点中心位置

    print('创建投影查找表: +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
    ps = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    r = meter2degree(4000)
    print(r)
    p = ProjCore(ps, r, unit="deg", pt_tl=(-179.5, 89.5), pt_br=(179.5, -89.5))  # 角点也要放在格点中心位置
