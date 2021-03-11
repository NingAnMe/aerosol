#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2021-02-03 9:48
# @Author  : NingAnMe <ninganme@qq.com>
import os
from datetime import datetime
from dateutil.relativedelta import relativedelta
from aerosol_a01_orbit_fy3d import aerosol_orbit
from lib.load_mersi import ReadMersiL1

FY3D_L1_PATH = ''
FY3D_GEO_PATH = ''
FY3D_CLOUD_PATH = ''
FY3D_AOD_PATH = ''

TMP_PATH = 'TMP_DIR'


def get_l1_files(l1_dir, dt_now):
    l1_files = list()
    return l1_files


def get_geo_file(l1_file):
    return geo_file


def get_cloud_file(l1_file):
    return cloud_file


def one_day(dt):
    satellite = 'FY3D'
    sensor = 'MERSI'
    l1_files = get_l1_files(FY3D_L1_PATH, dt_now=dt)
    for l1_1000m in l1_files:
        l1_geo = get_geo_file(l1_1000m)
        l1_cloudmask = get_cloud_file(l1_1000m)
        l1_reader = ReadMersiL1(l1_1000m)
        ymdhms = l1_reader.ymd + l1_reader.hms
        dir_temp = TMP_PATH
        out_dir = os.path.join(FY3D_AOD_PATH, 'Orbit', 'ymd')
        aerosol_orbit(l1_1000m, l1_cloudmask, l1_geo, ymdhms, dir_temp, out_dir, satellite, sensor, rewrite=False)
