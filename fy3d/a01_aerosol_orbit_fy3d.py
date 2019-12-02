#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2019/12/2 9:53
# @Author  : NingAnMe <ninganme@qq.com>
from datetime import datetime
import os

from .config import *
from lib.path import get_aid_path
from lib.fy3d2envi import fy3d2modis_1km, fy3d2modis_cloudmask, fy3d2modis_cloudmask_qa, fy3d2modis_geo, fy3d2modis_met


aid_dir = get_aid_path()
metadatas = os.path.join(aid_dir, 'metadatas.pickle')


def aerosol_orbit_fy3d(l1_1000m, l1_cloudmask, l1_geo, yyyymmddhhmmss, dir_temp, out_file):
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
    fy3d2modis_1km(l1_1000m, l1_1000m_envi, metadatas)

    l1_geo_envi = os.path.join(out_dir_temp, 'a1.{yyjjj}.{hhmm}.geo.hdr'.format(**format_datetime))
    fy3d2modis_geo(l1_1000m, l1_geo_envi, metadatas)

    l1_met_envi = os.path.join(out_dir_temp, 'a1.{yyjjj}.{hhmm}.met.hdr'.format(**format_datetime))
    fy3d2modis_met(l1_1000m, l1_met_envi, metadatas)

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
                "data": {"aerosol": out_file},
                "status": SUCCESS,
                "statusInfo": {
                    "message": "完成",
                    "detail": "结果目录:{}".format(out_dir_temp)
                }
            }
