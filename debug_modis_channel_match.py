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
import pandas as pd
import matplotlib.pylab as plt

from lib.load_modis import ReadModisL1

in_files = ['MYD021KM.A2020119.0715.061.2020119191034.hdf',
            'MYD021KM.A2020119.0740.061.2020119191249.hdf',
            'MYD021KM.A2020119.0850.061.2020119191031.hdf',
            ]
geo_files = ['MYD03.A2020119.0715.061.2020119153511.hdf',
             'MYD03.A2020119.0740.061.2020119154816.hdf',
             'MYD03.A2020119.0850.061.2020119164249.hdf',
             ]

ch05_data = None
ch02_data = None
ch06_data = None
ch26_data = None


def get_sea_index(data):
    return np.logical_and(data > 5, data < 8)


for in_file, geo_file in zip(in_files, geo_files):
    in_file = os.path.join('test_data', 'DEBUG', 'MODIS', in_file)
    geo_file = os.path.join('test_data', 'DEBUG', 'MODIS', geo_file)
    modis_loader = ReadModisL1(in_file, geo_file)
    ref = modis_loader.get_ref()
    land_sea = modis_loader.get_land_sea_mask()
    index_sea = get_sea_index(land_sea)

    ch05 = ref.get('CH_05')[index_sea]
    ch02 = ref.get('CH_02')[index_sea]
    ch06 = ref.get('CH_06')[index_sea]
    ch26 = ref.get('CH_26')[index_sea]

    ch01 = ref.get('CH_01')[index_sea]

    index = np.logical_and(ch01 > 0, ch01 < 0.2)
    if ch05_data is None:
        ch05_data = ch05[index]
    else:
        ch05_data = np.append(ch05_data, ch05[index])

    if ch02_data is None:
        ch02_data = ch02[index]
    else:
        ch02_data = np.append(ch02_data, ch02[index])

    if ch06_data is None:
        ch06_data = ch06[index]
    else:
        ch06_data = np.append(ch06_data, ch06[index])

    if ch26_data is None:
        ch26_data = ch26[index]
    else:
        ch26_data = np.append(ch26_data, ch26[index])
    print(ch05_data.size)
    print(ch02_data.size)
    print(ch06_data.size)
    print(ch26_data.size)

sns.set(style="whitegrid")

df_data = pd.DataFrame({'CH_02': ch02_data,
                        'CH_05': ch05_data,
                        'CH_06': ch06_data,
                        'CH_26': ch05_data,
                        })


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


plot_reg(ch02_data, ch05_data, os.path.join('test_data', 'DEBUG', 'modis_fig', 'ch02_ch05.png'))
plot_reg(ch06_data, ch05_data, os.path.join('test_data', 'DEBUG', 'modis_fig', 'ch06_ch05.png'))
plot_reg(ch26_data, ch05_data, os.path.join('test_data', 'DEBUG', 'modis_fig', 'ch26_ch05.png'))
