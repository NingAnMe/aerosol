#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2019/11/12 14:20
# @Author  : NingAnMe <ninganme@qq.com>
"""
目前有问题：跑出来的结果。除了经纬度数据集，其他的数据集全部都是无效值
1、将MODIS的数据，使用FY3D的格式化方法，转为envi格式，跑程序
2、
"""
import argparse
from datetime import datetime

from schedule import product_fy3d_aerosol


functions = {
    'product_fy3d_aerosol',
}

sat_sensors = {
    'FY4A_AGRI',
    'FY3D_MERSI',
}

frequencys = {
    'Orbit',
    'Daily',
    'Monthly'
    'Yearly',
}

if __name__ == '__main__':

    parser = argparse.ArgumentParser(description='Main')
    parser.add_argument('--function', '-f', help='程序名', required=True)
    parser.add_argument('--datetime_start', '-d', help='开始时间，YYYYmmddHHMMSS(20190101000000)', required=True)
    parser.add_argument('--datetime_end', '-a', help='结束时间，YYYYmmddHHMMSS(20190101235959)', required=True)
    parser.add_argument('--sat_sensor', '-s', help='卫星传感器，FY4A_AGRI', required=False)
    parser.add_argument('--frequency', '-e', help='时间分辨率，Orbit', required=False)
    args = parser.parse_args()

    assert args.function in functions, '{}'.format(functions)

    datetime_start = datetime.strptime(args.datetime_start, '%Y%m%d%H%M%S')
    datetime_end = datetime.strptime(args.datetime_end, '%Y%m%d%H%M%S')

    eval(args.function)(datetime_start=datetime_start, datetime_end=datetime_end,
                        frequency=args.frequency, sat_sensor=args.sat_sensor)
