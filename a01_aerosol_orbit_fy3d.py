#!/usr/bin/env python
# -*- coding: utf-8 -*-
# @Time    : 2019/12/2 9:53
# @Author  : NingAnMe <ninganme@qq.com>
from datetime import datetime
import os
import sys
import yaml
from config import *
from lib.path import get_aid_path
from lib.fy3d2envi import fy3d2modis_1km, fy3d2modis_cloudmask, fy3d2modis_cloudmask_qa, fy3d2modis_geo, fy3d2modis_met
from lib import utils

aid_dir = get_aid_path()
metadatas = os.path.join(aid_dir, 'metadatas.pickle')


def aerosol_orbit_fy3d(l1_1000m, l1_cloudmask, l1_geo, yyyymmddhhmmss, dir_temp, out_dir):
    print("<<< function      : {}".format(utils.get_function_name()))
    print("<<< l1_1000m      : {}".format(l1_1000m))
    print("<<< l1_cloudmask  : {}".format(l1_cloudmask))
    print("<<< l1_geo        : {}".format(l1_geo))
    print("<<< yyyymmddhhmmss: {}".format(yyyymmddhhmmss))
    print("<<< tmp_dir       : {}".format(dir_temp))

    datetime_temp = datetime.strptime(yyyymmddhhmmss, '%Y%m%d%H%M%S')

    yyyymmdd = datetime_temp.strftime('%Y%m%d')
    hhmm = datetime_temp.strftime('%H%M')
    yyjjj = yyyymmdd[2:4] + datetime_temp.strftime('%j')
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
    fy3d2modis_1km(l1_1000m, l1_geo, l1_1000m_envi, metadatas)

    l1_geo_envi = os.path.join(out_dir_temp, 'a1.{yyjjj}.{hhmm}.geo.hdr'.format(**format_datetime))
    fy3d2modis_geo(l1_1000m, l1_geo, l1_geo_envi, metadatas)

    l1_met_envi = os.path.join(out_dir_temp, 'a1.{yyjjj}.{hhmm}.met.hdr'.format(**format_datetime))
    fy3d2modis_met(l1_1000m, l1_geo, l1_met_envi, metadatas)

    l1_cloudmask_envi = os.path.join(out_dir_temp, 'a1.{yyjjj}.{hhmm}.mod35.hdr'.format(**format_datetime))
    fy3d2modis_cloudmask(l1_cloudmask, l1_cloudmask_envi, metadatas)

    l1_cloudmask_qa_envi = os.path.join(out_dir_temp, 'a1.{yyjjj}.{hhmm}.mod35qa.hdr'.format(**format_datetime))
    fy3d2modis_cloudmask_qa(l1_cloudmask, l1_cloudmask_qa_envi, metadatas)
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
    cmd = 'cd {out_dir} && run_fy3d_aerosol.csh aqua 1 a1.{yyjjj}.{hhmm}.1000m.hdf {out_dir}'.format(
        **format_datetime)
    os.system(cmd)
    print('>>> success: {}'.format(out_dir_temp))
    return {
                "data": {"out_dir": [out_dir]},
                "status": SUCCESS,
                "statusInfo": {
                    "message": "完成",
                    "detail": "结果目录:{}".format(out_dir_temp)
                }
            }


class ReadInYaml:

    def __init__(self, in_file):
        """
        读取yaml格式配置文件
        """
        if not os.path.isfile(in_file):
            sys.exit(-1)

        with open(in_file, 'r') as stream:
            cfg = yaml.load(stream)

        self.jobname = cfg['INFO']['JOBNAME']
        self.ymd = cfg['INFO']['ymd']
        self.hms = cfg['INFO']['hms']

        self.ipath_l1b = cfg['PATH']['ipath_l1b']
        self.ipath_geo = cfg['PATH']['ipath_geo']
        self.ipath_clm = cfg['PATH']['ipath_clm']
        self.opath = cfg['PATH']['opath']


def main(in_file):

    # 01 ICFG = 输入配置文件类 ##########
    in_cfg = ReadInYaml(in_file)

    l1b_file = in_cfg.ipath_l1b
    clm_file = in_cfg.ipath_clm
    geo_file = in_cfg.ipath_geo
    ymdhms = in_cfg.ymd + in_cfg.hms
    outpath = in_cfg.opath
    print(l1b_file)
    print(clm_file)
    print(geo_file)
    aerosol_orbit_fy3d(l1b_file, clm_file, geo_file, ymdhms, outpath, outpath)


if __name__ == '__main__':

    # 获取python输入参数，进行处理
    args = sys.argv[1:]
    if len(args) == 1:  # 跟参数，则处理输入的时段数据
        IN_FILE = args[0]
    else:
        print('input args error exit')
        sys.exit(-1)
    main(IN_FILE)
