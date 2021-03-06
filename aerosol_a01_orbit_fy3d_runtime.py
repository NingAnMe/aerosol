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

from aerosol_a01_orbit_fy3d_csj import aerosol_orbit
from lib.load_mersi import ReadMersiL1
from aod_h01_combine import combine_fy3d_1km_daily
from aod_p01_plot_map_combine import plot_map

DEPLOY = False

if DEPLOY:
    FY3D_L1_PATH = '/home/aodo3/CIMISS_DATA/fy3d/L1'
    FY3D_GEO_PATH = '/home/aodo3/CIMISS_DATA/fy3d/L1'
    FY3D_CLOUD_PATH = '/home/aodo3/CIMISS_DATA/fy3d/L2'
    FY3D_AOD_PATH = '/home/aodo3/FY3D_AEROSOL_DATA/FY3D_MERSI_1KM'
    FY3D_TMP_PATH = '/home/aodo3/FY3D_AEROSOL_DATA/TMP'
# else:
#     FY3D_L1_PATH = '/DISK/DATA02/PROJECT/SourceData/FY3D/1000M'
#     FY3D_GEO_PATH = '/DISK/DATA02/PROJECT/SourceData/FY3D/GEO1K'
#     FY3D_CLOUD_PATH = '/DISK/DATA02/PROJECT/SourceData/FY3D/CLM'
#     FY3D_AOD_PATH = '/DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/IMP_MERSI_1KM'
#     FY3D_TMP_PATH = '/DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/TMP'
else:
    FY3D_L1_PATH = '/DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/2FY3D-MERSI-L1-ORBT-1000M'
    FY3D_GEO_PATH = '/DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/2FY3D-MERSI-L1-ORBT-GEO1K'
    FY3D_CLOUD_PATH = '/DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/2FY3D_MERSI_L2_CLM-ORBT-1000M'
    FY3D_AOD_PATH = '/DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/IMP_MERSI_1KM'
    FY3D_TMP_PATH = '/DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/TMP'

LAT_RANGE = (17, 54)
LON_RANGE = (73, 136)

NOT_CHINA_CACHE = 'db.db'

# Z_SATE_C_BAWX_20210317025647_P_FY3D_MERSI_GBAL_L1_20210317_0240_1000M_MS.HDF
# Z_SATE_C_BAWX_20210317025648_P_FY3D_MERSI_GBAL_L1_20210317_0240_GEO1K_MS.HDF
# Z_SATE_C_BAWX_20210317004425_P_FY3D_MERSI_ORBT_L2_CLM_MLT_NUL_20210317_0000_1000M_MS.HDF

db = pickledb.load(NOT_CHINA_CACHE, False)


def get_files(dt_now: datetime, data_path: str, key_word: str):
    files = dict()
    dt = dt_now
    ymd = dt.strftime("%Y%m%d")
    y = ymd[:4]
    m = ymd[4:6]
    d = ymd[6:8]
    if DEPLOY:
        path_dt = os.path.join(data_path, y, m, d)  # 上海气象局归档路径格式
    else:
        # path_dt = os.path.join(data_path, y, ymd)  # 北京气象局归档路径格式
        # path_dt = data_path  # 上海提供的格式
        # 验证数据的数据格式
        if '2FY3D-MERSI-L1-ORBT-GEO1K' in data_path or '2FY3D-MERSI-L1-ORBT-1000M' in data_path:
            path_dt = os.path.join(data_path, ymd)
        elif '2FY3D_MERSI_L2_CLM-ORBT-1000M' in data_path:
            path_dt = data_path
        else:
            raise KeyError('数据目录和数据获取错误')
    print(f'INFO: get {key_word} path_dt: {path_dt}')
    if not os.path.isdir(path_dt):
        return files
    filenames = os.listdir(path_dt)
    filenames.sort()
    for filename in filenames:
        hm = str(filename.split('_')[-3])
        ymdhm = ymd + hm
        if key_word in filename and filename[-3:].lower() == 'hdf':
            files[ymdhm] = os.path.join(path_dt, filename)
    return files


def check_china_day(l1_file: str, geo_file: str, ymdhm: str):
    """
    检查数据是否有经过中国区的白天数据
    """
    cache = db.get(ymdhm + 'china_day')
    if cache:
        return cache
    try:
        rd = ReadMersiL1(l1_file, geo_file=geo_file)
        lons = rd.get_longitude()
        lats = rd.get_latitude()
        sz = rd.get_solar_zenith()

        china_day = np.logical_and.reduce((lats > LAT_RANGE[0], lats < LAT_RANGE[1],
                                           lons > LON_RANGE[0], lons < LON_RANGE[1],
                                           sz > 0, sz < 75,))
        china_day_sum = np.sum(china_day)
        print(f"{l1_file} china_day： {china_day_sum}")
        if china_day_sum == 0:
            db.set(ymdhm + 'china_day', 'False')
            db.dump()
            return 'False'
        else:
            db.set(ymdhm + 'china_day', 'True')
            db.dump()

            # 判断是否经过长三角
            yrd_day = np.logical_and.reduce((lats > 26, lats < 36,
                                             lons > 114, lons < 123,
                                             sz > 0, sz < 75,))
            yrd_day_sum = np.sum(yrd_day)
            print(f"{l1_file} yrd_day {yrd_day_sum}")
            if china_day_sum == 0:
                db.set(ymdhm + 'yrd_day', 'False')
                db.dump()
            else:
                db.set(ymdhm + 'yrd_day', 'True')
                db.dump()

            return 'True'

    except OSError as why:
        print(why)
        return 'False'


def get_l1_geo_cloud(dt_now: datetime):
    l1_files = get_files(dt_now, FY3D_L1_PATH, '1000M_MS')
    geo_files = get_files(dt_now, FY3D_GEO_PATH, 'GEO1K_MS')
    cloud_files = get_files(dt_now, FY3D_CLOUD_PATH, '1000M_MS')
    print(
        f'{dt_now}: l1_files {len(l1_files)} geo_files {len(geo_files)} cloud_files {len(cloud_files)}'
    )
    for ymdhm in l1_files.keys():
        if ymdhm not in geo_files or ymdhm not in cloud_files:  # 检测三个源文件是否同时存在
            print(f'Warning: 1000m geo cloud 文件不同时存在，跳过 {ymdhm}')
            continue
        l1_file = l1_files[ymdhm]
        geo_file = geo_files[ymdhm]
        cloud_file = cloud_files[ymdhm]

        if check_china_day(l1_file, geo_file, ymdhm) == 'True':  # 检测是否经过中国区的白天数据
            yield l1_file, geo_file, cloud_file, ymdhm
        else:
            print(f'INFO：没有经过中国区的白天数据，跳过  {ymdhm}')


def plot_china_map(dt_now: datetime):
    dt = dt_now
    ymd = dt.strftime("%Y%m%d")
    print(f'INFO：开始绘制 {ymd} 的数据')
    orbit_dir = os.path.join(FY3D_AOD_PATH, 'Orbit', ymd)
    if not os.path.isdir(orbit_dir):
        print(f'WARNING: 路径不存在，跳过 {orbit_dir}')
        return
    orbit_files = os.listdir(orbit_dir)
    if len(orbit_files) <= 0:
        print(f'WARNING：数据数量为0，跳过 {orbit_dir}')

    daily_dir = os.path.join(FY3D_AOD_PATH, 'Daily', ymd)
    daily_file = os.path.join(daily_dir, f'FY3D_MERSI_GBAL_L2_AOD_MLT_GLL_{ymd}_POAD_1000M_MS.HDF')
    if os.path.isfile(daily_file) and db.get(f'{ymd}filecount') == len(orbit_files):  # 已经绘图，切无变化
        print(f'INFO: 已经绘图，且无数据变化，跳过 {ymd}')
        return
    print(dt, dt + relativedelta(days=1) - relativedelta(minutes=1), orbit_dir)
    combine_fy3d_1km_daily(datetime_start=dt,
                           datetime_end=dt + relativedelta(days=1) - relativedelta(minutes=1),
                           l1_dir=orbit_dir,
                           geo_dir=None,
                           out_dir=daily_dir)

    plot_map(dt,
             dt + relativedelta(days=1) - relativedelta(minutes=1),
             data_dir=daily_dir,
             out_dir=daily_dir,
             data_type='FY3D_MERSI_1KM',
             date_type="Daily",
             deploy=True)
    db.set(f'{ymd}filecount', len(orbit_files))
    db.dump()


def one_day(dt: datetime):
    satellite = 'FY3D'
    sensor = 'MERSI'
    for l1_1000m, l1_geo, l1_cloudmask, ymdhm in get_l1_geo_cloud(dt):
        dir_temp = FY3D_TMP_PATH
        out_dir = os.path.join(FY3D_AOD_PATH, 'Orbit', ymdhm[:8])
        aerosol_orbit(l1_1000m,
                      l1_cloudmask,
                      l1_geo,
                      ymdhm + '00',
                      dir_temp,
                      out_dir,
                      satellite,
                      sensor,
                      rewrite=False)


def fy3d_statistic(dt: datetime):
    with open('statistic_china.txt', 'w') as f_china:
        with open('statistics_yrd.txt', 'w') as f_yrd:
            for l1_1000m, l1_geo, l1_cloudmask, ymdhm in get_l1_geo_cloud(dt):
                f_china.write('{}\n'.format(ymdhm))
                if db.get(ymdhm + 'yrd_day') == 'True':
                    f_yrd.write('{}\n'.format(ymdhm))


def parse_args():
    parser = argparse.ArgumentParser(description='Train a segmentor')
    parser.add_argument('--date_start', '-s', help='date  YYYYMMDD')
    parser.add_argument('--date_end', '-e', help='date  YYYYMMDD')
    parser.add_argument('--port', '-p', default=54321, help='bind port')
    parser.add_argument('--statistic', default=False, type=bool, help='statistic')
    return parser.parse_args()


def main():
    args = parse_args()
    import socket
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    args.port = int(args.port)
    try:
        s.bind(('127.0.0.1', args.port))
    except OSError:
        print(f"ERROR: 启动失败，端口被占用 {args.port}")
        exit(-1)

    if args.statistic:
        dt_start = datetime.strptime(args.date_start, "%Y%m%d")
        dt_end = datetime.strptime(args.date_end, "%Y%m%d")
        while dt_start <= dt_end:
            fy3d_statistic(dt_start)
            dt_start += relativedelta(days=1)
        exit(0)

    if args.date_start is not None or args.date_end is not None:
        dt_start = datetime.strptime(args.date_start, "%Y%m%d")
        dt_end = datetime.strptime(args.date_end, "%Y%m%d")
        while dt_start <= dt_end:
            one_day(dt_start)
            plot_china_map(dt_start)
            dt_start += relativedelta(days=1)
    else:
        dt_str = datetime.now().strftime('%Y%m%d')
        dt_now = datetime.strptime(dt_str, '%Y%m%d')
        dt_yes = dt_now - relativedelta(days=1)
        for dt in [dt_yes, dt_now]:
            one_day(dt)
            plot_china_map(dt)


if __name__ == '__main__':
    main()
