#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-06-03 14:42
# @Author  : NingAnMe <ninganme@qq.com>
"""

MODIS 数据：/nas03/AQUA/MODIS/L1/ORBIT/2020/20200428
MODIS 与 FY3D 交叉匹配：/nas03/CMA_GSICS_TEST/CommonData/sno/FENGYUN-3D_AQUA
"""

import os

import numpy as np
import seaborn as sns
import matplotlib.pylab as plt

from lib.load_mersi import ReadMersiL1

in_files = ['FY3D_MERSI_GBAL_L1_20200428_0940_1000M_MS.HDF',
            'FY3D_MERSI_GBAL_L1_20200428_1125_1000M_MS.HDF',
            ]
geo_files = ['FY3D_MERSI_GBAL_L1_20200428_0940_GEO1K_MS.HDF',
             'FY3D_MERSI_GBAL_L1_20200428_1125_GEO1K_MS.HDF',
             ]

ch01_data = None
ch03_data = None
ch07_data = None


def get_sea_index(data):
    return np.logical_and(data > 5, data < 8)


def get_land_index(data):
    return data == 1


def get_ndvi(nir, red):
    return (nir - red) / (nir + red)


for in_file, geo_file in zip(in_files, geo_files):
    in_file = os.path.join('test_data', 'DEBUG', 'FY3D', in_file)
    geo_file = os.path.join('test_data', 'DEBUG', 'FY3D', geo_file)
    modis_loader = ReadMersiL1(in_file, geo_file)
    ref = modis_loader.get_ref()
    land_sea = modis_loader.get_land_sea_mask()
    index_land = get_land_index(land_sea)

    ch01 = ref.get('CH_01')[index_land]
    ch03 = ref.get('CH_03')[index_land]  # RED
    ch04 = ref.get('CH_04')[index_land]  # NIR
    ch07 = ref.get('CH_07')[index_land]

    ndvi = get_ndvi(ch04, ch03)
    ndvi_less_01 = ndvi > 0.6
    index = np.logical_and(ch03 > 0, ch03 < 0.2)
    index = np.logical_and(ndvi_less_01, index)

    if ch01_data is None:
        ch01_data = ch01[index]
    else:
        ch01_data = np.append(ch01_data, ch01[index])

    if ch03_data is None:
        ch03_data = ch03[index]
    else:
        ch03_data = np.append(ch03_data, ch03[index])

    if ch07_data is None:
        ch07_data = ch07[index]
    else:
        ch07_data = np.append(ch07_data, ch07[index])

    print(ch01_data.size)
    print(ch03_data.size)
    print(ch07_data.size)

sns.set(style="whitegrid")


def plot_reg(x, y, out_file):
    f, ax = plt.subplots(figsize=(6.5, 6.5))
    sns.despine(f, left=True, bottom=True)
    i = np.logical_and(np.isfinite(x), np.isfinite(y))
    x = x[i]
    y = y[i]
    sns.regplot(x, y, ax=ax)
    r = np.polyfit(x, y, 1)
    print(i.sum())
    print(r)
    filename = os.path.basename(out_file)
    plt.title(filename[:-4])
    plt.figtext(0.1, 0.95, 'a = {:0.4f}'.format(r[0]))
    plt.figtext(0.1, 0.90, 'b = {:0.4f}'.format(r[1]))
    plt.savefig(out_file)


ch01_data = ch01_data[:10000]
ch03_data = ch03_data[:10000]
ch07_data = ch07_data[:10000]


plot_reg(ch07_data, ch01_data, os.path.join('test_data', 'DEBUG', 'fy3d_fig', 'ch07_ch01.png'))
plot_reg(ch07_data, ch03_data, os.path.join('test_data', 'DEBUG', 'fy3d_fig', 'ch07_ch03.png'))
plot_reg(ch01_data, ch03_data, os.path.join('test_data', 'DEBUG', 'fy3d_fig', 'ch01_ch03.png'))
