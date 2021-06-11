#!/usr/bin/env python
# -*- coding: utf-8 -*-
# @Time    : 2019/12/2 9:53
# @Author  : NingAnMe <ninganme@qq.com>

import sys
import shutil
import yaml
import h5py
import matplotlib.pyplot as plt
import numpy as np
from config import *
from lib.path import get_aid_path, get_root_path
from datetime import datetime
import matplotlib as mpl
from lib.utils import run_cmd
from lib.lib_aod_ncep_to_byte import Ncep2Byte
mpl.use('Agg')

aid_dir = get_aid_path()
root_dir = get_root_path()
metadatas = os.path.join(aid_dir, 'metadatas.pickle')

os.system('source ~/.bashrc')


def clear_tmp(tmp_path):
    if os.path.isdir(tmp_path):
        shutil.rmtree(tmp_path)
        print(f'INFO: remove tmp path {tmp_path}')


def get_grib_bin(yyyymmdd):
    """
    获取grib file 的二进制格式文件
    :param yyyymmdd: str
    :return:
    """

    yyyy = yyyymmdd[:4]
    mm = yyyymmdd[4:6]
    stime = yyyymmdd

    out_path = os.path.join(root_dir, 'fnl')
    file_path = os.path.join(out_path, f'{yyyymmdd[:4]}')
    if not os.path.isdir(file_path):
        os.makedirs(file_path)
    grib_file = os.path.join(file_path, f'fnl_{stime}_00_00.grib2')
    if not os.path.isfile(grib_file):
        wget_cmd = f'wget -N  -L --no-check-certificate  -np  --load-cookies auth.rda.ucar.edu.fnl ' \
                   f'https://rda.ucar.edu/data/OS/ds083.2/grib2/{yyyy}/{yyyy}.{mm}/fnl_{stime}_00_00.grib2 -P {file_path}'
        code, msg = run_cmd(wget_cmd)
        print(code, msg.split('\n')[-4:])
        assert code == 0, f'get fnl file error'
    print(f'grib_file : {grib_file}')
    grib_file_bin = grib_file + '.bin'
    if not os.path.isfile(grib_file_bin):
        n2b = Ncep2Byte(grib_file, grib_file_bin, True)
        n2b.ncep2byte()
    print(f'grib_file_bin : {grib_file_bin}')
    return grib_file_bin


def get_lut_path():
    return os.path.join(root_dir, 'lut')


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

        format_datetime['out_dir'] = out_dir_temp
        out_file = os.path.join(out_dir_temp, f'a1.{yyjjj}.{hhmm}.1000m.hdf')
        grib_bin = get_grib_bin(yyyymmdd)
        lut_path = get_lut_path()
        cmd = f'aod_b01_aerosol.exe {l1_1000m} {l1_geo} {l1_cloudmask} {grib_bin} {lut_path} {out_file}'

        print('cmd :{}'.format(cmd))

        code, msg = run_cmd(cmd_string=cmd, timeout=40)
        print(code, msg)
        os.system(cmd)
        print('>>> success: {}'.format(out_dir_temp))
        print('>>> success: {}'.format(out_file))

        try:
            with h5py.File(out_file, 'r') as h5r:
                aod_550 = h5r.get('Land_Aod_550')[:] * 0.001
                lons = h5r.get('Longitude')[:]
                lats = h5r.get('Latitude')[:]
            if not os.path.isdir(out_dir):
                os.makedirs(out_dir)

            with h5py.File(out_h5file, 'w') as h5w:
                h5w.create_dataset('/Geolocation/Latitude', data=lats, compression='gzip', compression_opts=5,
                                   shuffle=True)
                h5w.create_dataset('/Geolocation/Longitude', data=lons, compression='gzip', compression_opts=5,
                                   shuffle=True)
                h5w.create_dataset('AOT_Land', data=aod_550, compression='gzip', compression_opts=5,
                                   shuffle=True)
                print(">>> : {}".format(out_h5file))
                clear_tmp(out_dir_temp)

        except Exception as e:
            print('read ERROR ：{}'.format(e))
            return
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
    fy3d_dir_in = os.path.join('/home/kts_project_v1/shanghai/aod/TEST_DATA_FY3D_MERSI', 'in')
    l1_1000m = os.path.join(fy3d_dir_in, 'FY3D_MERSI_GBAL_L1_20191001_0100_1000M_MS.HDF')
    l1_cloudmask = os.path.join(fy3d_dir_in, 'FY3D_MERSI_ORBT_L2_CLM_MLT_NUL_20191001_0100_1000M_MS.HDF')
    l1_geo = os.path.join(fy3d_dir_in, 'FY3D_MERSI_GBAL_L1_20191001_0100_GEO1K_MS.HDF')
    yyyymmddhhmmss = '20191001010000'
    fy3d_dir_out = os.path.join('/home/kts_project_v1/shanghai/aod/TEST_DATA_FY3D_MERSI', 'out')
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
