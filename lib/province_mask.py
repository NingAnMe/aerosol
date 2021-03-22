#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-12-23 17:43
# @Author  : NingAnMe <ninganme@qq.com>

import numpy as np
from lib.hdf5 import get_hdf5_data

PROVINCE_MASK_FILE = 'lib/province_mask.hdf'


PROVINCE_MASK = {'北京市': 2, '湖南省': 14,
                 '天津市': 27, '广东省': 6,
                 '河北省': 10, '广西': 7,
                 '山西省': 25, '海南省': 9,
                 '内蒙古': 19, '重庆市': 3,
                 '辽宁省': 18, '四川省': 26,
                 '吉林省': 17, '贵州省': 8,
                 '黑龙江省': 11, '云南省': 30,
                 '上海市': 24, '西藏': 29,
                 '江苏省': 15, '陕西省': 22,
                 '浙江省': 31, '甘肃省': 5,
                 '安徽省': 1, '青海省': 21,
                 '福建省': 4, '宁夏': 20,
                 '江西省': 16, '新疆': 28,
                 '山东省': 23, '台湾省': 32,
                 '河南省': 12, '香港': 33,
                 '湖北省': 13, '澳门': 34, }


def get_province_mask(provinces):
    pro_mask_data = get_hdf5_data(PROVINCE_MASK_FILE, 'province_mask', 1, 0, [0, 255], 0)
    mask = None
    for pro in provinces:
        if pro == '中国':
            mask = pro_mask_data != 0
        else:
            valid = pro_mask_data == PROVINCE_MASK[pro]
            if mask is None:
                mask = valid
            else:
                mask = np.logical_or(mask, valid)
    return mask
