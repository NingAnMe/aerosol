#!/usr/bin/env python
# -*- coding: utf-8 -*-
# @Time    : 2021-02-03 9:48
# @Author  : NingAnMe <ninganme@qq.com>
import os
from datetime import datetime
from dateutil.relativedelta import relativedelta
import argparse

import numpy as np
import pickledb

from aerosol_a01_orbit_fy3d import aerosol_orbit
from lib.load_mersi import ReadMersiL1

FY3D_L1_PATH = '/home/aodo3/CIMISS_DATA/fy3d/L1'
FY3D_GEO_PATH = '/home/aodo3/CIMISS_DATA/fy3d/L1'
FY3D_CLOUD_PATH = '/home/aodo3/CIMISS_DATA/fy3d/L2'
FY3D_AOD_PATH = '/home/aodo3/FY3D_AEROSOL_DATA/FY3D_MERSI_1KM'

TMP_PATH = '/home/aodo3/FY3D_AEROSOL_DATA/TMP'

LAT_RANGE = (17, 54)
LON_RANGE = (73, 136)

NOT_CHINA_CACHE = 'db.db'

# Z_SATE_C_BAWX_20210317025647_P_FY3D_MERSI_GBAL_L1_20210317_0240_1000M_MS.HDF
# Z_SATE_C_BAWX_20210317025648_P_FY3D_MERSI_GBAL_L1_20210317_0240_GEO1K_MS.HDF
# Z_SATE_C_BAWX_20210317004425_P_FY3D_MERSI_ORBT_L2_CLM_MLT_NUL_20210317_0000_1000M_MS.HDF

db = pickledb.load(NOT_CHINA_CACHE, False)


def get_files(dt_now: datetime, data_path: str, key_word: str):
    files = dict()
    dt_yes = dt_now - relativedelta(days=1)
    for dt in (dt_now, dt_yes):
        ymd = dt.strftime("%Y%m%d")
        y = ymd[:4]
        m = ymd[4:6]
        d = ymd[6:8]
        path_dt = os.path.join(data_path, y, m, d)
        if not os.path.isdir(path_dt):
            continue
        for filename in os.listdir(path_dt):
            hm = filename.split('_')[-3]
            ymdhm = ymd + hm
            if key_word in filename:
                files[ymdhm] = os.path.join(path_dt, filename)
    return files


def check_china(l1_file: str, geo_file: str):
    """
    检查数据是否经过中国区
    """
    rd = ReadMersiL1(l1_file, geo_file=geo_file)
    lons = rd.get_longitude()
    lats = rd.get_latitude()
    china = np.logical_and.reduce(lats > LAT_RANGE[0], lats < LAT_RANGE[1],
                                  lons > LON_RANGE[0], lons < LON_RANGE[1])
    print(np.sum(china))
    if not np.any(china):
        return False


def get_l1_geo_cloud(dt_now: datetime):
    l1_files = get_files(dt_now, FY3D_L1_PATH, '1000M_MS')
    geo_files = get_files(dt_now, FY3D_GEO_PATH, 'GEO1K_MS')
    cloud_files = get_files(dt_now, FY3D_CLOUD_PATH, '1000M_MS')
    print(f'{dt_now}: l1_files {len(l1_files)} geo_files {len(geo_files)} cloud_files {len(cloud_files)}')
    for ymdhm in l1_files.keys():
        if db.get(ymdhm) == 'notchina':  # 检测是否经过中国区
            continue
        if ymdhm not in geo_files or ymdhm not in cloud_files:  # 检测三个源文件是否同时存在
            print(f'Warning: {ymdhm} 这个时刻，1000m geo cloud 文件不同时存在')
            continue
        l1_file = l1_files[ymdhm]
        geo_file = geo_files[ymdhm]
        cloud_file = cloud_file[ymdhm]
        if not check_china(l1_file, geo_file):  # 检测是否经过中国区
            db.put(ymdhm, 'notchina')
            db.dump()
            continue
        else:
            yield l1_file, geo_file, cloud_file, ymdhm


def one_day(dt: datetime):
    satellite = 'FY3D'
    sensor = 'MERSI'
    for l1_1000m, l1_geo, l1_cloudmask, ymdhm in get_l1_geo_cloud(dt):
        dir_temp = TMP_PATH
        out_dir = os.path.join(FY3D_AOD_PATH, 'Orbit', 'ymd')
        aerosol_orbit(l1_1000m,
                      l1_cloudmask,
                      l1_geo,
                      ymdhm + '00',
                      dir_temp,
                      out_dir,
                      satellite,
                      sensor,
                      rewrite=False)


def parse_args():
    parser = argparse.ArgumentParser(description='Train a segmentor')
    parser.add_argument('--date-start', help='start date  YYYYMMDD')
    return parser.parse_args()


def main():
    args = parse_args()
    if args.date_start is not None:
        dt = datetime.strptime(args.date_start, "%Y%m%d")
        one_day(dt)
    else:
        dt = datetime.now()
        one_day(dt)


if __name__ == '__main__':
    main()
