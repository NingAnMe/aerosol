#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-10-29 10:17
# @Author  : NingAnMe <ninganme@qq.com>
"""
合成数据
"""
import argparse
import os
from datetime import datetime
from dateutil.relativedelta import relativedelta
from collections import defaultdict
import numpy as np

from lib.path import make_sure_path_exists
from lib.aod import AodFy3d1km, AodCombine, AodFy3d5km, AodModis
from lib.hdf5 import write_hdf5_and_compress
from lib.proj import ProjCore

from config import AOD_FY3D_1KM_DIR, GEO_FY3D_1KM_DIR, AOD_COMBINE_DIR, AOD_FY3D_5KM_DIR, AOD_MODIS_3KM_DIR, \
    AOD_MODIS_10KM_DIR

import warnings

warnings.filterwarnings('ignore')


def get_month(dt):
    return dt.strftime("%Y%m") + '01'


def get_season(dt):
    m = dt.strftime("%m")
    if m in {'12', '01', '02'}:
        season = '12'
    elif m in {'03', '04', '05'}:
        season = '03'
    elif m in {'06', '07', '08'}:
        season = '06'
    elif m in {'09', '10', '11'}:
        season = '09'
    else:
        raise ValueError(m)
    if m in {'01', '02'}:
        y = (dt - relativedelta(years=1)).strftime("%y")
    else:
        y = dt.strftime("%y")
    return y + season + '01'


def get_year(dt):
    return dt.strftime("%Y") + '0101'


def get_day_str(dt):
    return dt.strftime("%Y%m%d")


def get_month_str(dt):
    return dt.strftime("%Y-%m")


def get_season_str(dt):
    m = dt.strftime("%m")
    if m in {'12', '01', '02'}:
        season = 'DJF'
    elif m in {'03', '04', '05'}:
        season = 'MAM'
    elif m in {'06', '07', '08'}:
        season = 'JJA'
    elif m in {'09', '10', '11'}:
        season = 'SON'
    else:
        raise ValueError(m)
    if m in {'01', '02'}:
        y = (dt - relativedelta(years=1)).strftime("%y")
    else:
        y = dt.strftime("%y")
    return y + '-' + season


def get_year_str(dt):
    return dt.strftime("%Y")


def get_sum_count(aod_sum, aod_count, aod):
    if aod_sum is None:
        aod_sum = np.zeros_like(aod, dtype=np.float)
    if aod_count is None:
        aod_count = np.zeros_like(aod, dtype=np.int)
    index_valid = aod > 0
    aod_sum[index_valid] += aod[index_valid]
    aod_count[index_valid] += 1
    return aod_sum, aod_count


def get_mean(aod_sum, aod_count):
    index_valid = aod_count > 0
    aod_mean = np.full_like(aod_sum, -999, dtype=np.float)
    aod_mean[index_valid] = aod_sum[index_valid] / aod_count[index_valid]
    return aod_mean


def combine_fy3d_1km_daily(datetime_start=None, datetime_end=None,
                           l1_dir=None, geo_dir=None, out_dir=None):
    # FY3D_MERSI_ORBT_L2_AOD_MLT_NUL_20190916_0910_1000M_MS.HDF
    # FY3D_MERSI_GBAL_L2_AOD_MLT_GLL_20190705_POAD_5000M_MS.HDF
    print('in dir :{}'.format(l1_dir))
    file_dict = defaultdict(list)
    for root, dirs, files in os.walk(l1_dir):
        for name in files:
            if name[-3:].lower() != 'hdf':
                continue
            date_str_file = name.split('_')[7]
            date_ = datetime.strptime(date_str_file, "%Y%m%d")
            if datetime_start is not None and datetime_end is not None:
                if not (datetime_start <= date_ <= datetime_end):
                    continue
            date_str = date_.strftime("%Y%m%d")
            file_dict[date_str].append(os.path.join(root, name))

    if not file_dict:
        print('没有找到数据')
        return

    # 创建投影查找表
    print('创建投影查找表')
    res_degree = 0.01  # 分辨率，1km
    projstr = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    proj = ProjCore(projstr, res_degree, unit="deg", pt_tl=(69.995, 54.995),
                    pt_br=(139.995, 9.995))  # 角点也要放在格点中心位置

    for d_, files in file_dict.items():
        aod_sum = np.zeros((proj.row, proj.col), dtype=np.float)
        aod_count = np.zeros_like(aod_sum, dtype=np.float)
        filename_out = 'FY3D_MERSI_GBAL_L2_AOD_MLT_GLL_{}_POAD_1000M_MS.HDF'.format(d_)

        file_out = os.path.join(out_dir, filename_out)
        # if os.path.isfile(file_out):
        #     print('already exist {}'.format(file_out))
        #     continue

        print('<<< {}'.format(d_))
        for file_ in files:
            print('<<< {}'.format(file_))

            file_loader = AodFy3d1km(file_, geo_file=file_)
            aod = file_loader.get_aod()
            lons, lats = file_loader.get_lon_lat()
            print(np.nanmin(aod), np.nanmax(aod), np.nanmean(aod))
            if aod is None:
                print('aod 为 None: {}'.format(file_))
                continue

            # 投影
            print('投影')
            ii, jj = proj.lonslats2ij(lons, lats)
            valid = np.logical_and.reduce((ii >= 0, ii < proj.row,
                                           jj >= 0, jj < proj.col,
                                           aod > 0, aod < 1.5,
                                           ))
            if valid.sum() == 0:
                print('valid.size == 0, continue')
                continue
            print('valid.sum() == {}'.format(valid.sum()))

            ii = ii[valid]
            jj = jj[valid]
            aod = aod[valid]
            aod_sum[ii, jj] += aod
            aod_count[ii, jj] += 1

            print(np.nanmin(aod), np.nanmax(aod), np.nanmean(aod))
            print(np.nanmin(aod_sum), np.nanmax(aod_sum), np.nanmean(aod_sum))
        if aod_sum is not None and aod_count is not None:
            aod_mean = get_mean(aod_sum, aod_count)
        else:
            continue

        # 新的网格的经纬度
        lons_grid, lats_grid = proj.grid_lonslats()
        make_sure_path_exists(out_dir)
        print((aod_mean != -999).sum())
        data_write = {
            'Optical_Depth_Land_And_Ocean': aod_mean,
            'Longitude': lons_grid,
            'Latitude': lats_grid
        }
        write_hdf5_and_compress(data_write, file_out)


def combine(frequency='Monthly', datetime_start=None, datetime_end=None,
            data_dir=None, out_dir=None, res_type='1KM', data_loader=None,
            satellite_sensor=None):
    print('frequency       ===  {}'.format(frequency))
    print('datetime_start  ===  {}'.format(datetime_start))
    print('datetime_end    ===  {}'.format(datetime_end))
    print('data_dir        ===  {}'.format(data_dir))
    print('out_dir         ===  {}'.format(out_dir))
    print('res_type        ===  {}'.format(res_type))
    print('satellite_sensor        ===  {}'.format(satellite_sensor))
    if data_loader is None:
        data_loader = AodCombine

    file_dict = defaultdict(list)
    for root, dirs, files in os.walk(data_dir):
        for name in files:
            if name[-3:].lower() != 'hdf':
                continue
            in_file = os.path.join(root, name)
            date_ = data_loader(in_file).dt
            if datetime_start is not None and datetime_end is not None:
                if not (datetime_start <= date_ <= datetime_end):
                    continue
            if frequency == 'Monthly':
                date_str = get_month(date_)
            elif frequency == 'Seasonly':
                date_str = get_season(date_)
            elif frequency == 'Yearly':
                date_str = get_year(date_)
            else:
                raise ValueError(frequency)
            file_dict[date_str].append(in_file)
    res_dict = {
        '1KM': '1000M',
        '5KM': '5000M',
    }
    for d_, files in file_dict.items():
        aod_sum = None
        aod_count = None
        lons = None
        lats = None
        res = res_dict[res_type]
        filename_out = '{}_GBAL_L2_AOD_MLT_GLL_{}_POAD_{}_MS.HDF'.format(satellite_sensor, d_, res)
        file_out = os.path.join(out_dir, filename_out)
        if os.path.isfile(file_out):
            print('already exist {}'.format(file_out))
            continue
        for file_ in files:
            print('<<< {}'.format(file_))
            loader = data_loader(file_)
            aod = loader.get_aod()  # 无效值赋值为-999

            if aod is None:
                print('aod 为 None: {}'.format(file_))
                continue

            valid = np.logical_and(aod > 0, aod < 1.5)
            aod[~valid] = -999
            # print(np.nanmin(aod), np.nanmax(aod), np.nanmean(aod))
            aod_sum, aod_count = get_sum_count(aod_sum, aod_count, aod)

            if lons is None and lats is None:
                lons, lats = loader.get_lon_lat()
        if aod_sum is not None and aod_count is not None:
            aod_mean = get_mean(aod_sum, aod_count)
        else:
            continue

        make_sure_path_exists(out_dir)
        print((aod_mean != -999).sum())
        data_write = {
            'Optical_Depth_Land_And_Ocean': aod_mean,
            'Longitude': lons,
            'Latitude': lats
        }
        write_hdf5_and_compress(data_write, file_out)


def combine_aqua_daily(datetime_start=None, datetime_end=None,
                       data_dir=None, out_dir=None):
    # FY3D_MERSI_ORBT_L2_AOD_MLT_NUL_20190916_0910_1000M_MS.HDF
    # FY3D_MERSI_GBAL_L2_AOD_MLT_GLL_20190705_POAD_5000M_MS.HDF
    print('data_dir :{}'.format(data_dir))
    file_dict = defaultdict(list)
    for root, dirs, files in os.walk(data_dir):
        for name in files:
            if name[-3:].lower() != 'hdf':
                continue
            date_str_file = name[10:17]
            date_ = datetime.strptime(date_str_file, "%Y%j")
            if datetime_start is not None and datetime_end is not None:
                if not (datetime_start <= date_ <= datetime_end):
                    continue
            date_str = date_.strftime("%Y%m%d")
            file_dict[date_str].append(os.path.join(root, name))

    if not file_dict:
        print('没有找到数据')
        return

    # 创建投影查找表
    print('创建投影查找表')
    res_degree = 0.01  # 分辨率，1km
    projstr = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    proj = ProjCore(projstr, res_degree, unit="deg", pt_tl=(69.995, 54.995),
                    pt_br=(139.995, 9.995))  # 角点也要放在格点中心位置

    for d_, files in file_dict.items():
        aod_sum = np.zeros((proj.row, proj.col), dtype=np.float)
        aod_count = np.zeros_like(aod_sum, dtype=np.float)
        filename_out = 'AQUA_MODIS_GBAL_L2_AOD_MLT_GLL_{}_POAD_1000M_MS.HDF'.format(d_)

        file_out = os.path.join(out_dir, filename_out)
        if os.path.isfile(file_out):
            print('already exist {}'.format(file_out))
            continue

        print('<<< {}'.format(d_))
        for file_ in files:
            print('<<< {}'.format(file_))

            file_loader = AodModis(file_)
            aod = file_loader.get_aod()
            lons, lats = file_loader.get_lon_lat()
            print(np.nanmin(aod), np.nanmax(aod), np.nanmean(aod))
            if aod is None:
                print('aod 为 None: {}'.format(file_))
                continue

            # 投影
            print('投影')
            ii, jj = proj.lonslats2ij(lons, lats)
            valid = np.logical_and.reduce((ii >= 0, ii < proj.row,
                                           jj >= 0, jj < proj.col,
                                           aod > 0, aod < 1.5,
                                           ))
            if valid.sum() == 0:
                print('valid.size == 0, continue')
                continue
            print('valid.sum() == {}'.format(valid.sum()))

            ii = ii[valid]
            jj = jj[valid]
            aod = aod[valid]
            aod_sum[ii, jj] += aod
            aod_count[ii, jj] += 1

            print(np.nanmin(aod), np.nanmax(aod), np.nanmean(aod))
            print(np.nanmin(aod_sum), np.nanmax(aod_sum), np.nanmean(aod_sum))
        if aod_sum is not None and aod_count is not None:
            aod_mean = get_mean(aod_sum, aod_count)
        else:
            continue

        # 新的网格的经纬度
        lons_grid, lats_grid = proj.grid_lonslats()
        make_sure_path_exists(out_dir)
        print((aod_mean != -999).sum())
        data_write = {
            'Optical_Depth_Land_And_Ocean': aod_mean,
            'Longitude': lons_grid,
            'Latitude': lats_grid
        }
        write_hdf5_and_compress(data_write, file_out)


def main(data_type=None, date_start=None, date_end=None, date_type=None):
    datetime_start = datetime.strptime(date_start, "%Y%m%d")
    datetime_end = datetime.strptime(date_end, "%Y%m%d")

    if data_type == 'FY3D_MERSI_1KM':
        combine_dir = os.path.join(AOD_COMBINE_DIR, 'AOD_COMBINE_{}'.format(data_type))
        if date_type in {'Daily'}:
            out_dir = os.path.join(combine_dir, date_type)
            combine_fy3d_1km_daily(datetime_start=datetime_start, datetime_end=datetime_end,
                                   l1_dir=AOD_FY3D_1KM_DIR, geo_dir=GEO_FY3D_1KM_DIR, out_dir=out_dir)
        elif date_type in {'Monthly', 'Seasonly', 'Yearly'}:
            data_dir = os.path.join(combine_dir, 'Daily')
            out_dir = os.path.join(combine_dir, date_type)
            combine(frequency=date_type, datetime_start=datetime_start, datetime_end=datetime_end,
                    data_dir=data_dir, res_type='1KM', out_dir=out_dir, satellite_sensor='FY3D_MERSI')
        else:
            parser.print_help()
            raise ValueError(date_type)
    elif data_type == 'FY3D_MERSI_5KM':
        combine_dir = os.path.join(AOD_COMBINE_DIR, 'AOD_COMBINE_{}'.format(data_type))
        if date_type in {'Monthly', 'Seasonly', 'Yearly'}:
            out_dir = os.path.join(combine_dir, date_type)
            if date_type == 'Monthly':
                dataLoader = AodFy3d5km
                data_dir = AOD_FY3D_5KM_DIR
            else:
                dataLoader = None
                data_dir = os.path.join(combine_dir, 'Monthly')
            combine(frequency=date_type, datetime_start=datetime_start, datetime_end=datetime_end,
                    data_dir=data_dir, res_type='5KM', out_dir=out_dir,
                    satellite_sensor='FY3D_MERSI', data_loader=dataLoader)
        else:
            parser.print_help()
            raise ValueError(date_type)
    elif data_type in {'AQUA_MODIS_3KM', 'AQUA_MODIS_10KM'}:
        combine_dir = os.path.join(AOD_COMBINE_DIR, 'AOD_COMBINE_{}'.format(data_type))
        if date_type in {'Daily'}:
            out_dir = os.path.join(combine_dir, date_type)
            if '3KM' in data_type:
                data_dir = AOD_MODIS_3KM_DIR
            else:
                data_dir = AOD_MODIS_10KM_DIR
            combine_aqua_daily(datetime_start=datetime_start, datetime_end=datetime_end,
                               data_dir=data_dir, out_dir=out_dir)
        elif date_type in {'Monthly', 'Seasonly', 'Yearly'}:
            data_dir = os.path.join(combine_dir, 'Daily')
            out_dir = os.path.join(combine_dir, date_type)
            combine(frequency=date_type, datetime_start=datetime_start, datetime_end=datetime_end,
                    data_dir=data_dir, res_type='1KM', out_dir=out_dir, satellite_sensor='AQUA_MODIS')
        else:
            parser.print_help()
            raise ValueError(date_type)
    else:
        parser.print_help()
        raise ValueError(data_type)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Schedule')
    parser.add_argument('--dataType', help='数据类型：FY3D_MERSI_1KM、FY3D_MERSI_5KM、AQUA_MODIS_3KM、AQUA_MODIS_5KM',
                        required=True)
    parser.add_argument('--dateType', help='合成的类型日、月、季、年：Daily、Monthly、Seasonly、Yearly', required=True)
    parser.add_argument('--dateStart', help='开始时间（8位时间）：YYYYMMDD(20190101)', required=False)
    parser.add_argument('--dateEnd', help='结束时间（8位时间）：YYYYMMDD(20190102)', required=False)
    args = parser.parse_args()

    dataType = args.dataType
    dateStart = args.dateStart
    dateEnd = args.dateEnd
    dateType = args.dateType

    main(data_type=dataType, date_start=dateStart, date_end=dateEnd, date_type=dateType)

    """
    FY3D_MERSI_1KM 数据合成
    程序目录：
    输入数据
        原始L1数据放在文件夹：“/DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/2FY3D_MERSI_L2_AOD-ORBT-1000M”
        原始GEO数据放在文件夹：“/DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/2FY3D-MERSI-L1-GEO1K”
    输出数据
        合成的数据所在文件夹：“/DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/COMBINE”

    合成 FY3D_MERSI_1KM 日数据
    python3 aod_h01_combine.py --dataType FY3D_MERSI_1KM --dateType Daily --dateStart 20190101 --dateEnd 20190131

    合成 FY3D_MERSI_1KM 月数据
    python3 aod_h01_combine.py --dataType FY3D_MERSI_1KM --dateType Monthly --dateStart 20190101 --dateEnd 20190131

    合成 FY3D_MERSI_1KM 季度数据
    python3 aod_h01_combine.py --dataType FY3D_MERSI_1KM --dateType Seasonly --dateStart 20190101 --dateEnd 20190228

    合成 FY3D_MERSI_1KM 年数据
    python3 aod_h01_combine.py --dataType FY3D_MERSI_1KM --dateType Yearly --dateStart 20190101 --dateEnd 20191231

    合成 FY3D_MERSI_5KM 月数据
    python3 aod_h01_combine.py --dataType FY3D_MERSI_5KM --dateType Monthly --dateStart 20190101 --dateEnd 20190131

    合成 FY3D_MERSI_5KM 季度数据
    python3 aod_h01_combine.py --dataType FY3D_MERSI_5KM --dateType Seasonly --dateStart 20190101 --dateEnd 20190228

    合成 FY3D_MERSI_5KM 年数据
    python3 aod_h01_combine.py --dataType FY3D_MERSI_5KM --dateType Yearly --dateStart 20190101 --dateEnd 20191231

    合成 AQUA_MODIS_3KM 日数据
    python3 aod_h01_combine.py --dataType AQUA_MODIS_3KM --dateType Daily --dateStart 20190101 --dateEnd 20190131

    合成 AQUA_MODIS_3KM 月数据
    python3 aod_h01_combine.py --dataType AQUA_MODIS_3KM --dateType Monthly --dateStart 20190101 --dateEnd 20190131

    合成 AQUA_MODIS_3KM 季度数据
    python3 aod_h01_combine.py --dataType AQUA_MODIS_3KM --dateType Seasonly --dateStart 20190101 --dateEnd 20190228

    合成 AQUA_MODIS_3KM 年数据
    python3 aod_h01_combine.py --dataType AQUA_MODIS_3KM --dateType Yearly --dateStart 20190101 --dateEnd 20191231

    合成 AQUA_MODIS_3KM 日数据
    python3 aod_h01_combine.py --dataType AQUA_MODIS_3KM --dateType Daily --dateStart 20190101 --dateEnd 20190131

    合成 AQUA_MODIS_3KM 月数据
    python3 aod_h01_combine.py --dataType AQUA_MODIS_3KM --dateType Monthly --dateStart 20190101 --dateEnd 20190131

    合成 AQUA_MODIS_3KM 季度数据
    python3 aod_h01_combine.py --dataType AQUA_MODIS_3KM --dateType Seasonly --dateStart 20190101 --dateEnd 20190228

    合成 AQUA_MODIS_3KM 年数据
    python3 aod_h01_combine.py --dataType AQUA_MODIS_3KM --dateType Yearly --dateStart 20190101 --dateEnd 20191231
    """
