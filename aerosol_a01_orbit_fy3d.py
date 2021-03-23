#!/usr/bin/env python
# -*- coding: utf-8 -*-
# @Time    : 2019/12/2 9:53
# @Author  : NingAnMe <ninganme@qq.com>

import sys
import os
import yaml
import h5py
import matplotlib.pyplot as plt
import numpy as np
from config import *
from lib.path import get_aid_path
from lib.fy3d2envi import fy3d2modis_1km, fy3d2modis_cloudmask, fy3d2modis_cloudmask_qa, fy3d2modis_geo, fy3d2modis_met
from lib.fy3abc2envi import (fy3abc2modis_1km, fy3abc2modis_cloudmask, fy3abc2modis_cloudmask_qa,
                             fy3abc2modis_geo, fy3abc2modis_met)
from lib.utils import format_data
from spectral.io import envi
from datetime import datetime
import matplotlib as mpl
mpl.use('Agg')

aid_dir = get_aid_path()
metadatas = os.path.join(aid_dir, 'metadatas.pickle')

os.system('source ~/.bashrc')


def aerosol_orbit(l1_1000m, l1_cloudmask, l1_geo, yyyymmddhhmmss, dir_temp, out_dir, satellite, sensor, rewrite=True,
                  vis_file=None, ir_file=None):
    print("<<< l1_1000m      : {}".format(l1_1000m))
    print("<<< l1_cloudmask  : {}".format(l1_cloudmask))
    print("<<< l1_geo        : {}".format(l1_geo))
    print("<<< yyyymmddhhmmss: {}".format(yyyymmddhhmmss))
    print("<<< tmp_dir       : {}".format(dir_temp))
    print("<<< satellite     : {}".format(satellite))
    print("<<< sensor        : {}".format(sensor))
    print("<<< rewrite        : {}".format(rewrite))
    print("<<< vis_file        : {}".format(vis_file))
    print("<<< ir_file        : {}".format(ir_file))

    datetime_temp = datetime.strptime(yyyymmddhhmmss, '%Y%m%d%H%M%S')

    yyyymmdd = datetime_temp.strftime('%Y%m%d')
    hhmm = datetime_temp.strftime('%H%M')
    yyjjj = yyyymmdd[2:4] + datetime_temp.strftime('%j')

    out_h5file = os.path.join(out_dir, '%s_%s_ORBT_L2_AOD_MLT_NUL_%s_%s_1000M_MS.HDF' % (satellite, sensor, yyyymmdd, hhmm))
    if (not os.path.isfile(out_h5file)) or rewrite:
        if DEBUG:
            print('yyyymmdd: ', yyyymmdd)
            print('hhmm    : ', hhmm)
            print('yyjjj   : ', yyjjj)

        format_datetime = {
            'yyjjj': yyjjj,
            'hhmm': hhmm,
        }
        out_dir_temp = os.path.join(dir_temp, '{}/{}'.format(yyyymmdd, hhmm))
        if not os.path.isdir(out_dir_temp):
            os.makedirs(out_dir_temp)

        l1_1000m_envi = os.path.join(out_dir_temp, 'a1.{yyjjj}.{hhmm}.1000m.hdr'.format(**format_datetime))
        l1_geo_envi = os.path.join(out_dir_temp, 'a1.{yyjjj}.{hhmm}.geo.hdr'.format(**format_datetime))
        l1_met_envi = os.path.join(out_dir_temp, 'a1.{yyjjj}.{hhmm}.met.hdr'.format(**format_datetime))
        l1_cloudmask_envi = os.path.join(out_dir_temp, 'a1.{yyjjj}.{hhmm}.mod35.hdr'.format(**format_datetime))
        l1_cloudmask_qa_envi = os.path.join(out_dir_temp, 'a1.{yyjjj}.{hhmm}.mod35qa.hdr'.format(**format_datetime))

        if vis_file is not None or ir_file is not None:
            coef_txt_flag = True
        else:
            coef_txt_flag = False

        if sensor == "MERSI":
            if satellite == "FY3D":
                all_night = fy3d2modis_geo(l1_1000m, l1_geo, l1_geo_envi, metadatas)
                if all_night:
                    print("全部是夜晚数据")
                    return
                fy3d2modis_1km(l1_1000m, l1_geo, l1_1000m_envi, metadatas, vis_file, ir_file, coef_txt_flag)
                fy3d2modis_met(l1_1000m, l1_geo, l1_met_envi, metadatas)
                fy3d2modis_cloudmask(l1_cloudmask, l1_cloudmask_envi, metadatas)
                fy3d2modis_cloudmask_qa(l1_cloudmask, l1_cloudmask_qa_envi, metadatas)
            elif satellite in ["FY3A", "FY3B", "FY3C"]:
                all_night = fy3abc2modis_geo(l1_1000m, l1_geo, l1_geo_envi, metadatas)
                if all_night:
                    print("全部是夜晚数据")
                    return
                fy3abc2modis_1km(l1_1000m, l1_geo, l1_1000m_envi, metadatas, vis_file, ir_file, coef_txt_flag)
                fy3abc2modis_met(l1_1000m, l1_geo, l1_met_envi, metadatas)
                fy3abc2modis_cloudmask(l1_cloudmask, l1_cloudmask_envi, metadatas)
                fy3abc2modis_cloudmask_qa(l1_cloudmask, l1_cloudmask_qa_envi, metadatas)
            else:
                raise ValueError(f"不支持的satellite：{satellite}")
        else:
            raise ValueError(f"不支持的sensor：{sensor}")
        
        if DEBUG:
            print('product :{}'.format(l1_1000m_envi))
            print('product :{}'.format(l1_geo_envi))
            print('product :{}'.format(l1_met_envi))
            print('product :{}'.format(l1_cloudmask_envi))
            print('product :{}'.format(l1_cloudmask_qa_envi))
        
        for file_ in [l1_1000m_envi, l1_geo_envi, l1_met_envi, l1_cloudmask_envi, l1_cloudmask_qa_envi]:
            if not os.path.isfile(file_):
                print('ERROR: file found error: {}'.format(file_))
                return {
                    "data": {},
                    "status": ERROR,
                    "statusInfo": {
                        "message": "程序错误",
                        "detail": "程序错误，没有生成：{}".format(file_)
                    }
                }
        
        format_datetime['out_dir'] = out_dir_temp
        cmd = 'cd {out_dir} && run_mersi_aerosol.csh aqua 1 a1.{yyjjj}.{hhmm}.1000m.hdf {out_dir}'.format(
            **format_datetime)
        
        print('cmd :{}'.format(cmd))
        # os.system(cmd)
        print('>>> success: {}'.format(out_dir_temp))

        """
        http://www.spectralpython.net/class_func_ref.html#spectral.io.envi
        add envi format to hdf5
        wangpeng 20191204
        """
        envi_hdr = os.path.join(out_dir_temp, 'a1.%s.%s.mod04.hdr' % (yyjjj, hhmm))
        envi_img = os.path.join(out_dir_temp, 'a1.%s.%s.mod04.img' % (yyjjj, hhmm))
        envi_data = envi.open(envi_hdr, envi_img)
        lats = envi_data.read_band(0)
        lons = envi_data.read_band(1)
        aod_550 = envi_data.read_band(2)

        from lib.utils import fill_points_2d_nan
        aod_550[np.logical_or(aod_550 == 0.001, aod_550 == 0)] = np.nan
        fill_points_2d_nan(aod_550)
        aod_550[np.isnan(aod_550)] = -327.68

        #     dset_name = envi_data.metadata['band names']

        if not os.path.isdir(out_dir):
            os.makedirs(out_dir)

        with h5py.File(out_h5file, 'w') as h5w:
            h5w.create_dataset('/Geolocation/Latitude', data=lats, compression='gzip', compression_opts=5, shuffle=True)
            h5w.create_dataset('/Geolocation/Longitude', data=lons, compression='gzip', compression_opts=5, shuffle=True)
            h5w.create_dataset('AOT_Land', data=aod_550, compression='gzip', compression_opts=5,
                               shuffle=True)
            debug_data = False
            if debug_data:
                h5w.create_dataset('SDS_ratio_small_Land_Ocean', data=envi_data.read_band(3),
                                   compression='gzip', compression_opts=5, shuffle=True)
                h5w.create_dataset('Corrected_Optical_Depth_Land_.47micron', data=envi_data.read_band(4),
                                   compression='gzip', compression_opts=5, shuffle=True)
                h5w.create_dataset('Corrected_Optical_Depth_Land_.55micron', data=envi_data.read_band(5),
                                   compression='gzip', compression_opts=5, shuffle=True)
                h5w.create_dataset('Corrected_Optical_Depth_Land_.66micron', data=envi_data.read_band(6),
                                   compression='gzip', compression_opts=5, shuffle=True)
                h5w.create_dataset('Effective_Optical_Depth_Average_Ocean_.47micron', data=envi_data.read_band(7),
                                   compression='gzip', compression_opts=5, shuffle=True)
                h5w.create_dataset('Effective_Optical_Depth_Average_Ocean_.55micron', data=envi_data.read_band(8),
                                   compression='gzip', compression_opts=5, shuffle=True)
                h5w.create_dataset('Effective_Optical_Depth_Average_Ocean_.66micron', data=envi_data.read_band(9),
                                   compression='gzip', compression_opts=5, shuffle=True)
                h5w.create_dataset('Effective_Optical_Depth_Average_Ocean_.86micron', data=envi_data.read_band(10),
                                   compression='gzip', compression_opts=5, shuffle=True)
                h5w.create_dataset('Effective_Optical_Depth_Average_Ocean_1.2micron', data=envi_data.read_band(11),
                                   compression='gzip', compression_opts=5, shuffle=True)
                h5w.create_dataset('Effective_Optical_Depth_Average_Ocean_1.6micron', data=envi_data.read_band(12),
                                   compression='gzip', compression_opts=5, shuffle=True)
                h5w.create_dataset('Effective_Optical_Depth_Average_Ocean_2.1micron', data=envi_data.read_band(13),
                                   compression='gzip', compression_opts=5, shuffle=True)
            print(">>> : {}".format(out_h5file))
    else:
        print("文件已存在，跳过:{}".format(out_h5file))

    return {
        "data": {"out_dir": [out_dir],
                 "out_file": [out_h5file]},
        "status": SUCCESS,
        "statusInfo": {
            "message": "完成",
        }
    }


def plot_aod_image(hdf_file, out_image):
    with h5py.File(hdf_file, 'r') as hdf:
        aod = hdf.get('Optical_Depth_Land_And_Ocean')
        if aod:
            aod_550 = aod[:]
            aod_550 = np.ma.masked_where(aod_550 < 0, aod_550)
            r = normal255int8(aod_550)
            plot_image_origin(r, out_image)


def plot_image_origin(r, out_file):
    row, col = np.shape(r)
    width = col / 100.
    length = row / 100.
    dpi = 100
    fig = plt.figure(figsize=(width, length), dpi=dpi)  # china

    #     rgb = np.stack([r, g, b], axis = 2)
    #     rgb = np.stack([r], axis = 2)

    plt.imshow(r, cmap='jet')

    plt.axis('off')

    plt.gca().xaxis.set_major_locator(plt.NullLocator())
    plt.gca().yaxis.set_major_locator(plt.NullLocator())
    plt.subplots_adjust(top=1, bottom=0, right=1, left=0, hspace=0, wspace=0)
    plt.margins(0, 0)

    out_dir = os.path.dirname(out_file)

    if not os.path.isdir(out_dir):
        os.makedirs(out_dir)

    fig.savefig(out_file, dpi=dpi)
    fig.clear()
    plt.close()
    print('>>> {}'.format(out_file))


def normal255int8(array):
    """
    小于 0 的值赋值为0，其他值归一化到 0-255
    :param array: ndarray
    :return: ndarray
    """
    data = (array - array.min()) / (array.max() - array.min())
    data = data * 255
    data = data.astype(np.uint8)
    return data


class ReadInYaml:

    def __init__(self, in_file):
        """
        读取yaml格式配置文件
        """
        if not os.path.isfile(in_file):
            sys.exit(-1)

        with open(in_file, 'r') as stream:
            cfg = yaml.load(stream)

        self.ir_file = cfg['CALFILE']['irfile']
        self.vis_file = cfg['CALFILE']['visfile']

        self.jobname = cfg['INFO']['job_name']
        self.ymd = cfg['INFO']['ymd']
        self.hms = cfg['INFO']['hms']
        self.rewrite = cfg['INFO']['rewrite']

        self.ipath_l1b = cfg['PATH']['ipath_l1b']
        self.ipath_geo = cfg['PATH'].get('ipath_geo')
        self.ipath_clm = cfg['PATH']['ipath_clm']
        self.opath = cfg['PATH']['opath']


def main(in_file):
    # 01 ICFG = 输入配置文件类 ##########
    in_cfg = ReadInYaml(in_file)

    vis_file = in_cfg.vis_file
    ir_file = in_cfg.ir_file

    l1b_file = in_cfg.ipath_l1b
    clm_file = in_cfg.ipath_clm
    geo_file = in_cfg.ipath_geo
    outpath = in_cfg.opath

    ymdhms = in_cfg.ymd + in_cfg.hms
    satellite, sensor = in_cfg.jobname.split('_')
    rewrite = in_cfg.rewrite
    result = aerosol_orbit(l1b_file, clm_file, geo_file, ymdhms, outpath, outpath, satellite, sensor, rewrite=rewrite,
                           vis_file=vis_file, ir_file=ir_file)
    if result is not None and result['status'] == SUCCESS:
        out_hdf = result['data']['out_file'][0]
        if not os.path.isfile(out_hdf):
            print('HDF文件不存在：{}'.format(out_hdf))
        out_image = result['data']['out_file'][0] + '.png'
        if (not os.path.isfile(out_image)) or rewrite:
            plot_aod_image(out_hdf, out_image)
        else:
            print('文件已经存在，跳过:{}'.format(out_image))


def t_aerosol_orbit():
    fy3d_dir_in = os.path.join('TEST_DATA_FY3D_MERSI', 'in')
    l1_1000m = os.path.join(fy3d_dir_in, 'FY3D_MERSI_GBAL_L1_20191001_0100_1000M_MS.HDF')
    l1_cloudmask = os.path.join(fy3d_dir_in, 'FY3D_MERSI_ORBT_L2_CLM_MLT_NUL_20191001_0100_1000M_MS.HDF')
    l1_geo = os.path.join(fy3d_dir_in, 'FY3D_MERSI_GBAL_L1_20191001_0100_GEO1K_MS.HDF')
    yyyymmddhhmmss = '20191001010000'
    fy3d_dir_out = os.path.join('TEST_DATA_FY3D_MERSI', 'out')
    dir_temp = fy3d_dir_out
    out_dir = fy3d_dir_out
    satellite = 'FY3D'
    sensor = 'MERSI'

    aerosol_orbit(l1_1000m, l1_cloudmask, l1_geo, yyyymmddhhmmss, dir_temp, out_dir, satellite, sensor, rewrite=True,
                  vis_file=None, ir_file=None)


if __name__ == '__main__':

    t_aerosol_orbit()

    # # 获取python输入参数，进行处理
    # args = sys.argv[1:]
    # if len(args) == 1:  # 跟参数，则处理输入的时段数据
    #     IN_FILE = args[0]
    # else:
    #     print('input args error exit')
    #     sys.exit(-1)
    # main(IN_FILE)
