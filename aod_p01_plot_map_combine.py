#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-12-23 11:49
# @Author  : NingAnMe <ninganme@qq.com>
import os
from datetime import datetime
import argparse

import numpy as np

from lib.aod import AodCombine
from lib.province_mask import get_province_mask
from lib.proj_aod import proj_china

from config import AOD_COMBINE_DIR, AOD_PICTURE_DIR
from aod_h01_combine import get_day_str, get_month_str, get_season_str, get_year_str
from config import get_area_range, get_areas
from aod_p02_plot_map_origin import plot_map_picture

import warnings
warnings.filterwarnings('ignore')


def plot_map(datetime_start, datetime_end, data_dir=None, out_dir=None, data_loader=AodCombine,
             data_type=None, date_type=None):
    print("plot_map")
    print("datetime_start === {}".format(datetime_start))
    print("datetime_end === {}".format(datetime_end))
    print("data_dir === {}".format(data_dir))
    print("out_dir === {}".format(out_dir))
    print("data_loader === {}".format(data_loader))
    print("data_type === {}".format(data_type))
    print("date_type === {}".format(date_type))
    filelist = list()
    for root, dirs, files in os.walk(data_dir):
        for name in files:
            if name[-3:].lower() != 'hdf':
                continue
            date_ = data_loader(name).dt
            if not (datetime_start <= date_ <= datetime_end):
                continue
            filelist.append(os.path.join(root, name))
    filelist.sort()
    for in_file in filelist:
        print("<<< : {}".format(in_file))
        for area_type in ["China", "YRD"]:
            loader = data_loader(in_file)
            title = get_title(data_type, date_type, loader.dt, area_type)
            filename = "_".join(title.split()) + '.png'
            out_file = os.path.join(out_dir, filename)

            data = loader.get_aod()
            lons, lats = loader.get_lon_lat()

            data, lons, lats = proj_china(data, lons, lats)

            vmin = 0
            vmax = 1.5

            ticks = np.arange(0, 1.51, 0.3)

            if area_type == 'China':
                nanhai = True
            else:
                nanhai = False

            mksize = 5

            areas = get_areas(area_type)
            mask = get_province_mask(areas)

            valid = np.logical_and.reduce((data > vmin, data < vmax, mask))

            data_mask = data[valid]
            lons_mask = lons[valid]
            lats_mask = lats[valid]

            longitude_range, latitude_range = get_area_range(area_type)
            box = [latitude_range[1], latitude_range[0], longitude_range[0], longitude_range[1]]

            plot_map_picture(data_mask, lons_mask, lats_mask, title=title, vmin=vmin, vmax=vmax,
                             areas=areas, box=box, ticks=ticks, file_out=out_file,
                             mksize=mksize, nanhai=nanhai)


get_dt_str = {
    'Daily': get_day_str,
    'Monthly': get_month_str,
    'Seasonly': get_season_str,
    'Yearly': get_year_str,
}


def get_title(data_type, date_type, dt, area_type):
    satellite, sensor = data_type.split('_')[:2]
    date_str = get_dt_str[date_type](dt)

    title = "{} {} {} AOD(550nm) over {}".format(date_str, satellite, sensor, area_type)
    return title


def main(data_type=None, date_start=None, date_end=None, date_type=None):
    datetime_start = datetime.strptime(date_start, "%Y%m%d")
    datetime_end = datetime.strptime(date_end, "%Y%m%d")

    combine_dir = os.path.join(AOD_COMBINE_DIR, 'AOD_COMBINE_{}'.format(data_type))
    picture_dir = os.path.join(AOD_PICTURE_DIR, 'AOD_MAP_COMBINE_{}'.format(data_type))

    if data_type in {'FY3D_MERSI_1KM', 'AQUA_MODIS_3KM', 'AQUA_MODIS_10KM'}:

        if date_type in {'Daily', 'Monthly', 'Seasonly', 'Yearly'}:
            data_dir = os.path.join(combine_dir, date_type)
            out_dir = os.path.join(picture_dir, date_type)
            plot_map(datetime_start, datetime_end, data_dir=data_dir, out_dir=out_dir,
                     data_type=data_type, date_type=date_type)
        else:
            parser.print_help()
            raise ValueError(date_type)
    elif data_type == 'FY3D_MERSI_5KM':
        if date_type in {'Monthly', 'Seasonly', 'Yearly'}:
            data_dir = os.path.join(combine_dir, date_type)
            out_dir = os.path.join(picture_dir, date_type)
            plot_map(datetime_start, datetime_end, data_dir=data_dir, out_dir=out_dir,
                     data_type=data_type, date_type=date_type)
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
    parser.add_argument('--dateStart', help='开始时间（8位时间）：YYYYMMDD(20190101)', required=True)
    parser.add_argument('--dateEnd', help='结束时间（8位时间）：YYYYMMDD(20190102)', required=True)
    parser.add_argument('--dateType', help='合成的类型日、月、季、年：Daily、Monthly、Seasonly、Yearly', required=True)
    args = parser.parse_args()

    dataType = args.dataType
    dateStart = args.dateStart
    dateEnd = args.dateEnd
    dateType = args.dateType

    main(data_type=dataType, date_start=dateStart, date_end=dateEnd, date_type=dateType)

    """
    绘制 FY3D_MERSI_1KM 日分布图
    python3 aod_p01_plot_map_combine.py --dataType FY3D_MERSI_1KM --dateType Daily --dateStart 20190101 --dateEnd 20190131

    绘制 FY3D_MERSI_1KM 月分布图
    python3 aod_p01_plot_map_combine.py --dataType FY3D_MERSI_1KM --dateType Monthly --dateStart 20190101 --dateEnd 20190131

    绘制 FY3D_MERSI_1KM 季度分布图
    python3 aod_p01_plot_map_combine.py --dataType FY3D_MERSI_1KM --dateType Seasonly --dateStart 20190101 --dateEnd 20190228

    绘制 FY3D_MERSI_1KM 年分布图
    python3 aod_p01_plot_map_combine.py --dataType FY3D_MERSI_1KM --dateType Yearly --dateStart 20190101 --dateEnd 20191231

    绘制 FY3D_MERSI_5KM 月分布图
    python3 aod_p01_plot_map_combine.py --dataType FY3D_MERSI_5KM --dateType Monthly --dateStart 20190101 --dateEnd 20190131

    绘制 FY3D_MERSI_5KM 季度分布图
    python3 aod_p01_plot_map_combine.py --dataType FY3D_MERSI_5KM --dateType Seasonly --dateStart 20190101 --dateEnd 20190228

    绘制 FY3D_MERSI_5KM 年分布图
    python3 aod_p01_plot_map_combine.py --dataType FY3D_MERSI_5KM --dateType Yearly --dateStart 20190101 --dateEnd 20191231

    绘制 AQUA_MODIS_3KM 日分布图
    python3 aod_p01_plot_map_combine.py --dataType AQUA_MODIS_3KM --dateType Daily --dateStart 20190101 --dateEnd 20190131

    绘制 AQUA_MODIS_3KM 月分布图
    python3 aod_p01_plot_map_combine.py --dataType AQUA_MODIS_3KM --dateType Monthly --dateStart 20190101 --dateEnd 20190131

    绘制 AQUA_MODIS_3KM 季度分布图
    python3 aod_p01_plot_map_combine.py --dataType AQUA_MODIS_3KM --dateType Seasonly --dateStart 20190101 --dateEnd 20190228

    绘制 AQUA_MODIS_3KM 年分布图
    python3 aod_p01_plot_map_combine.py --dataType AQUA_MODIS_3KM --dateType Yearly --dateStart 20190101 --dateEnd 20191231

    绘制 AQUA_MODIS_3KM 日分布图
    python3 aod_p01_plot_map_combine.py --dataType AQUA_MODIS_3KM --dateType Daily --dateStart 20190101 --dateEnd 20190131

    绘制 AQUA_MODIS_3KM 月分布图
    python3 aod_p01_plot_map_combine.py --dataType AQUA_MODIS_3KM --dateType Monthly --dateStart 20190101 --dateEnd 20190131

    绘制 AQUA_MODIS_3KM 季度分布图
    python3 aod_p01_plot_map_combine.py --dataType AQUA_MODIS_3KM --dateType Seasonly --dateStart 20190101 --dateEnd 20190228

    绘制 AQUA_MODIS_3KM 年分布图
    python3 aod_p01_plot_map_combine.py --dataType AQUA_MODIS_3KM --dateType Yearly --dateStart 20190101 --dateEnd 20191231
    """