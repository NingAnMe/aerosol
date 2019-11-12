#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2019/11/11 16:19
# @Author  : NingAnMe <ninganme@qq.com>
from datetime import datetime
from dateutil.relativedelta import relativedelta
import os

from lib.path import get_aid_path
from a02_fy3d2envi import fy3d2modis_1km, fy3d2modis_cloudmask, fy3d2modis_cloudmask_qa, fy3d2modis_geo, fy3d2modis_met


aid_dir = get_aid_path()
metadatas = os.path.join(aid_dir, 'metadatas.pickle')


def product_fy3d_aerosol(datetime_start: datetime = None, datetime_end: datetime = None, **kwargs):
    in_dir = '/mnt/hgfs/Projects/imapp_modisl2/test_data/fy3d_in'
    out_dir = '/mnt/hgfs/Projects/imapp_modisl2/test_data/fy3d_result'
    l1_1000m_filename = 'FY3D_MERSI_GBAL_L1_{yyyymmdd}_{hhmm}_1000M_MS.HDF'
    l1_geo_filename = 'FY3D_MERSI_GBAL_L1_{yyyymmdd}_{hhmm}_GEO1K_MS.HDF'
    l1_cloudmask_filename = 'FY3D_MERSI_ORBT_L2_CLM_MLT_NUL_{yyyymmdd}_{hhmm}_1000M_MS.HDF'

    datetime_temp = datetime_start
    while datetime_temp <= datetime_end:
        yyyymmdd = datetime_temp.strftime('%Y%m%d')
        hhmm = datetime_temp.strftime('%H%M')
        yyjjj = yyyymmdd[2:4] + datetime_temp.strftime('%j')
        print('yyyymmdd: ', yyyymmdd)
        print('hhmm: ', hhmm)
        print('yyjjj: ', yyjjj)

        l1_1000m = l1_1000m_filename.format(yyyymmdd=yyyymmdd, hhmm=hhmm)
        l1_cloudmask = l1_cloudmask_filename.format(yyyymmdd=yyyymmdd, hhmm=hhmm)
        l1_geo = l1_geo_filename.format(yyyymmdd=yyyymmdd, hhmm=hhmm)

        filenames = set(os.listdir(in_dir))
        if l1_1000m not in filenames:
            continue
        else:
            l1_1000m = os.path.join(in_dir, l1_1000m)
            print('l1_1000m: ', l1_1000m)
        if l1_geo not in filenames:
            continue
        else:
            l1_geo = os.path.join(in_dir, l1_geo)
            print('l1_geo: ', l1_geo)
        if l1_cloudmask not in filenames:
            continue
        else:
            l1_cloudmask = os.path.join(in_dir, l1_cloudmask)
            print('l1_cloudmask: ', l1_cloudmask)

        format_datetime = {
            'yyjjj': yyjjj,
            'hhmm': hhmm,
        }
        out_dir_temp = os.path.join(out_dir, '{}_{}'.format(yyyymmdd, hhmm))
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

        flag = True
        for file_ in [l1_1000m_envi, l1_geo_envi, l1_met_envi, l1_cloudmask_envi, l1_cloudmask_qa_envi]:
            if not os.path.isfile(file_):
                print('file found error: {}'.format(file_))
                flag = False

        if flag:
            format_datetime['out_dir'] = out_dir_temp
            cmd = 'cd {out_dir} && run_fy3d_aerosol.csh aqua 1 a1.{yyjjj}.{hhmm}.1000m.hdf {out_dir}'.format(
                **format_datetime)
            os.system(cmd)

        datetime_temp += relativedelta(minutes=5)
