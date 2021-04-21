#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-12-23 17:04
# @Author  : NingAnMe <ninganme@qq.com>
import argparse
import os

import numpy as np
import matplotlib.pyplot as plt
from pylab import subplots_adjust

from DV.dv_map import dv_map

from lib.path import make_sure_path_exists
from lib.aod import AodFy3d1km, AodFy3d5km, AodModis, AodImapp1km
from lib.proj_aod import proj_china
from lib.proj import fill_points_2d
from lib.province_mask import get_province_mask

from config import get_areas, get_area_range

import warnings
warnings.filterwarnings('ignore')


def plot_map_picture(value, longitude, latitude, title='', vmin=-np.inf, vmax=np.inf, areas=None, box=None, ticks=None,
                     file_out=None, ptype='pcolormesh', mksize=5, nanhai=False):
    if file_out is None:
        print('没有指定输出文件：file_out is None')
        return

    # 开始画图-----------------------

    fig = plt.figure(figsize=(9, 8))  # 图像大小

    p = dv_map(fig=fig)

    subplots_adjust(left=0.07, right=0.98, top=0.90, bottom=0.15)
    p.show_colorbar = False
    p.show_countries = False
    p.show_coastlines = False
    # p.show_line_of_latlon = False
    # p.show_china = True
    if nanhai:
        p.nanhai_loc = [0.83, 0.25, 0.15, 0.17]
        p.nanhai_minimap()
    p.show_china_province = True
    p.show_inside_china = True
    p.show_inside_china_mini = True

    if box:
        if abs(box[1] - box[0]) < 10:
            p.delat = 2
        else:
            p.delat = 5
        if abs(box[2] - box[3]) < 10:
            p.delon = 2
        elif abs(box[2] - box[3]) < 40:
            p.delon = 5
        else:
            p.delon = 10
    # else:
    #     p.delat = 2  # 纬度刻度线分辨率
    #     p.delon = 2  # 经度刻度线分辨率
    p.color_coast = "#3a3a3a"  # 海岸线颜色
    p.color_contry = "#3a3a3a"  # 国家颜色
    p.fontsize_tick = 15

    # set color map
    p.valmin = vmin
    p.valmax = vmax
    p.colormap = plt.get_cmap('jet')  # mpl.cm.rainbow, summer, jet, bwr
    # p.colorbar_extend = "max"

    # plot
    p.easyplot(latitude, longitude, value, vmin=vmin, vmax=vmax, box=box, markersize=mksize, ptype=ptype)

    if areas is not None and len(areas) > 0:
        print('设置地区 ：{}'.format(areas))
        # aeres = ["江苏省", "安徽省", "浙江省", "上海市"]
        for aere in areas:
            p.city_boundary(aere, linewidth=1.2, shape_name='中国省级行政区')

    # 色标 ---------------------------
    cb_loc = [0.12, 0.07, 0.76, 0.03]
    # unit = r"$\mathregular{(10^{15}\/\/molec/cm^2)}$"
    fontsize = 16
    # p.add_custom_colorbar(cb_loc, p.valmin, p.valmax,
    #                       fmt="%d",
    #                       unit="(1E16 molec/cm^2)",
    #                       fontsize=fontsize)
    c_ax = fig.add_axes(cb_loc)
    # cbar = fig.colorbar(p.cs, cax=c_ax, ticks=np.arange(0, 1.6, 0.3), orientation='horizontal')
    fig.colorbar(p.cs, cax=c_ax, ticks=ticks, orientation='horizontal')
    for l in c_ax.xaxis.get_ticklabels():
        l.set_fontproperties(p.font_mid)
        l.set_fontsize(fontsize)
        l.set_color(p.color_ticker)
    # cbar写单位
    # cbar.ax.set_title(unit, x=1.0382, y=0, color=p.color_ticker,
    #                   ha='left', va='center',
    #                   fontproperties=p.font_mid, fontsize=fontsize)

    # 标题 ---------------------------
    p.w_title = p.suptitle(title, fontsize=14, y=0.97)

    # save
    p.savefig(file_out, dpi=200)
    print(">>> {}".format(file_out))
    p.clean()


def main(data_type, in_path, out_path, geo_path):
    assert data_type in {'FY3D_MERSI_1KM', 'FY3D_MERSI_5KM', 'AQUA_MODIS_3KM', 'AQUA_MODIS_10KM',
                         'IMP_MERSI_1KM'}, "{} 输入错误".format(data_type)
    if os.path.isfile(in_path):
        filelist = [in_path]
    elif os.path.isdir(in_path):
        filelist = list()
        for root, dirs, files in os.walk(in_path):
            for name in files:
                if name[-3:].lower() != 'hdf':
                    continue
                filelist.append(os.path.join(root, name))
    else:
        raise ValueError('{} 不是文件夹也不是有效文件'.format(in_path))

    assert len(filelist) > 0, "输入文件的数量小于1"
    make_sure_path_exists(out_path)

    if data_type in {'FY3D_MERSI_1KM'}:
        data_loader = AodFy3d1km
    elif data_type in {'FY3D_MERSI_5KM'}:
        data_loader = AodFy3d5km
    elif data_type in {'IMP_MERSI_1KM'}:
        data_loader = AodImapp1km
    elif data_type in {'AQUA_MODIS_3KM', 'AQUA_MODIS_10KM'}:
        data_loader = AodModis
    else:
        raise ValueError(data_type)

    for in_file in filelist:
        if geo_path is not None:
            if os.path.isfile(geo_path):
                loader = data_loader(in_file, geo_file=geo_path)
            else:
                loader = data_loader(in_file, geo_path=geo_path)
        else:
            loader = data_loader(in_file)
        dt = loader.dt

        data = loader.get_aod()  # 无效值为-999
        lons, lats = loader.get_lon_lat()

        data, lons, lats = proj_china(data, lons, lats, 0, 1.5)
        if data_type in {"FY3D_MERSI_5KM"}:
            fill_points_2d(data, -999)
            fill_points_2d(data, -999)

        for area_type in ["China", "YRD"]:
            title = get_title(data_type, dt, area_type)
            filename = loader.filename + '.{}.png'.format(area_type)
            out_file = os.path.join(out_path, filename)

            vmin = 0
            vmax = 1.5

            ticks = np.arange(0, 1.51, 0.3)

            if area_type == 'China':
                mksize = 1  # 改为1
                nanhai = True
            else:
                mksize = 2  # 改为1
                nanhai = False

            areas = get_areas(area_type)
            mask = get_province_mask(areas)

            valid = np.logical_and.reduce((data > vmin, data < vmax, mask))
            data_mask = data[valid]
            lons_mask = lons[valid]
            lats_mask = lats[valid]

            count = len(data_mask)
            print('count == {}'.format(count))

            longitude_range, latitude_range = get_area_range(area_type)
            box = [latitude_range[1], latitude_range[0], longitude_range[0], longitude_range[1]]
            plot_map_picture(data_mask, lons_mask, lats_mask, title=title, vmin=vmin, vmax=vmax,
                             areas=areas, box=box, ticks=ticks, file_out=out_file,
                             mksize=mksize, nanhai=nanhai)


def get_title(data_type, dt, area_type):
    satellite, sensor = data_type.split('_')[:2]
    date_str = dt.strftime("%Y%m%d")

    title = "{} {} {} AOD(550nm) over {}".format(date_str, satellite, sensor, area_type)
    return title


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Schedule')
    parser.add_argument('--dataType', help='数据类型：FY3D_MERSI_1KM、FY3D_MERSI_5KM、AQUA_MODIS_3KM、AQUA_MODIS_10KM',
                        required=True)
    parser.add_argument('--inPath', help='输入路径（绝对路径）：可以是一个文件夹，也可以是单个文件', required=True)
    parser.add_argument('--outPath', help='输出路径（绝对路径）：文件夹', required=True)
    parser.add_argument('--geoPath', help='FY3D 1KM的GEO文件路径（绝对路径）：可以是一个文件夹，也可以是单个文件', required=False)
    args = parser.parse_args()

    dataType = args.dataType
    inPath = args.inPath
    outPath = args.outPath
    geoPath = args.geoPath

    main(dataType, inPath, outPath, geoPath)

    """
    单个绘制 FY3D_MERSI_1KM 分布图（输入为单个文件）
    python3 aod_p02_plot_map_origin.py --dataType FY3D_MERSI_1KM --inPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/2FY3D_MERSI_L2_AOD-ORBT-1000M/FY3D_MERSI_ORBT_L2_AOD_MLT_NUL_20200526_2310_1000M_MS.HDF --outPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/test --geoPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/2FY3D-MERSI-L1-GEO1K/FY3D_MERSI_GBAL_L1_20200526_2310_GEO1K_MS.HDF 
    批量绘制 FY3D_MERSI_1KM 分布图（输入为文件夹）
    python3 aod_p02_plot_map_origin.py --dataType FY3D_MERSI_1KM --inPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/2FY3D_MERSI_L2_AOD-ORBT-1000M --outPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/test --geoPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/2FY3D-MERSI-L1-GEO1K
    
    单个绘制 FY3D_MERSI_5KM 分布图（输入为单个文件）
    python3 aod_p02_plot_map_origin.py --dataType FY3D_MERSI_5KM --inPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/1FY3D-MERSI-L2-AOD-DAILY-5000M/2019/FY3D_MERSI_GBAL_L2_AOD_MLT_GLL_20191231_POAD_5000M_MS.HDF  --outPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/test
    批量绘制 FY3D_MERSI_5KM 分布图（输入为文件夹）
    python3 aod_p02_plot_map_origin.py --dataType FY3D_MERSI_5KM --inPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/1FY3D-MERSI-L2-AOD-DAILY-5000M  --outPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/test
    
    单个绘制 AQUA_MODIS_3KM 分布图（输入为单个文件）
    python3 aod_p02_plot_map_origin.py --dataType AQUA_MODIS_3KM --inPath /DISK/DATA02/KTS/SourceData/AQUA/MODIS/MYD04_3K/ORBIT/2019/MYD04_3K.A2019365.0805.061.2019365193244.hdf --outPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/test
    批量绘制 AQUA_MODIS_3KM 分布图（输入为文件夹）
    python3 aod_p02_plot_map_origin.py --dataType AQUA_MODIS_3KM --inPath /DISK/DATA02/KTS/SourceData/AQUA/MODIS/MYD04_3K/ORBIT/2019 --outPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/test
    
    单个绘制 AQUA_MODIS_10KM 分布图（输入为单个文件）
    python3 aod_p02_plot_map_origin.py --dataType AQUA_MODIS_3KM --inPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/3MYD04_L2_MODIS_AOD_10KM/2019/MYD04_L2.A2019001.0420.061.2019001165304.hdf --outPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/test
    批量绘制 AQUA_MODIS_10KM 分布图（输入为文件夹）
    python3 aod_p02_plot_map_origin.py --dataType AQUA_MODIS_3KM --inPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/3MYD04_L2_MODIS_AOD_10KM/2019 --outPath /DISK/DATA02/PROJECT/SourceData/ShangHai/AOD/test
    """