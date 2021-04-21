#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-12-23 17:30
# @Author  : NingAnMe <ninganme@qq.com>
import numpy as np
from lib.proj import ProjCore


# 创建投影查找表
def proj_china(data, lons, lats, data_min=None, data_max=None):
    print('创建投影查找表')
    res_degree = 0.01  # 分辨率，1km
    projstr = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    proj = ProjCore(projstr, res_degree, unit="deg", pt_tl=(69.995, 54.995),
                    pt_br=(139.995, 9.995))  # 角点也要放在格点中心位置
    ii, jj = proj.lonslats2ij(lons, lats)

    data_new = np.full((proj.row, proj.col), -999, dtype=np.float)

    if data_min is not None and data_max is not None:
        valid = np.logical_and.reduce((ii >= 0, ii < proj.row,
                                       jj >= 0, jj < proj.col,
                                       data > data_min, data < data_max,
                                       ))
    else:
        valid = np.logical_and.reduce((ii >= 0, ii < proj.row,
                                       jj >= 0, jj < proj.col,
                                       ))
    ii = ii[valid]
    jj = jj[valid]
    data = data[valid]
    data_new[ii, jj] = data
    lons_grid, lats_grid = proj.grid_lonslats()
    return data_new, lons_grid, lats_grid
