# -*- coding: utf-8 -*-

import os
import sys
import h5py
import yaml
import numpy as np
from qiae_lib.dp_proj import prj_core

'''
Created on 2019年12月20日
@author: wape2
'''
main_path, main_file = os.path.split(os.path.realpath(__file__))


class ReadModeYaml():

    def __init__(self, in_file):
        """
        读取yaml格式配置文件
        """
        if not os.path.isfile(in_file):
            print ('Not Found %s' % in_file)
            sys.exit(-1)

        with open(in_file, 'r') as stream:
            cfg = yaml.load(stream)

        self.area = cfg['a02']['area']
        self.res = cfg['a02']['res']


class ReadYaml():

    def __init__(self, in_file):
        """
        读取yaml格式配置文件
        """
        if not os.path.isfile(in_file):
            print ('Not Found %s' % in_file)
            sys.exit(-1)

        with open(in_file, 'r') as stream:
            cfg = yaml.load(stream)

            self.job_name = cfg['INFO']['job_name']
            self.ymd = cfg['INFO']['ymd']
            self.ipath = cfg['PATH']['ipath']
            self.opath = cfg['PATH']['opath']


class ReadAodData():

    def __init__(self):

        pass

    def load_lonlat(self, infile):

        data_dict = {}
        with h5py.File(infile, 'r') as h5w:
            lons = h5w.get('Longitude')[:]
            lats = h5w.get('Latitude')[:]

        data_dict['lon'] = lons
        data_dict['lat'] = lats
        return data_dict

    def load_aod(self, infile):
        with h5py.File(infile, 'r') as h5w:
            aod = h5w.get('Optical_Depth_Land_And_Ocean')[:]
        return aod

#     def loads(self, infile):
#         """
#         dataset | group/dataset   input dict
#         """
#
#         if not os.path.isfile(infile):
#             raise ValueError('File is not exist: {}'.format(infile))
#
#         data = dict()
#
#         with h5py.File(infile, 'r') as h5w:
#             for root_name in h5w:
#                 h5_name = h5w.get(root_name)
#
#                 if 'Dataset' in type(h5_name).__name__:
#                     sds_name = root_name
#                     print ('-->', sds_name)
#                     data[sds_name] = h5w.get(sds_name)[:]
#
#                 elif 'Group' in type(h5_name).__name__:
#                     grp_name = root_name
#                     if grp_name not in data:
#                         data[grp_name] = {}
#                     for sds_name in h5_name:
#                         print ('-->', grp_name, sds_name)
#                         data[grp_name][sds_name] = h5_name.get(sds_name)[:]
#         return data


def main(yaml_file):

    # 读取接口文件
    yaml1 = ReadYaml(yaml_file)

    mode_file = os.path.join(main_path, 'cfg', '%s.yaml' % (yaml1.job_name))

    yaml2 = ReadModeYaml(mode_file)

    nlat, slat, wlon, elon = yaml2.area
    res = yaml2.res

    if nlat <= 0:
        nlat = nlat + res / 2
    else:
        nlat = nlat - res / 2

    if slat <= 0:
        slat = slat + res / 2
    else:
        slat = slat - res / 2

    if wlon <= 0:
        wlon = wlon + res / 2
    else:
        wlon = wlon - res / 2

    if elon <= 0:
        elon = elon + res / 2
    else:
        elon = elon - res / 2

    cmd = '+proj=longlat +datum=WGS84 +no_defs'
    print (wlon, nlat, elon, slat)
    pp = prj_core(cmd, res, unit = 'deg', pt_tl = (wlon, nlat), pt_br = (elon, slat))
    print (yaml2.res)

    aod_550 = np.full((pp.row, pp.col), -999.)

    proj_lut = {}

    h5data = ReadAodData()
    for infile in yaml1.ipath:
        data = h5data.load_lonlat(infile)
        lons = data['lon']
        lats = data['lat']
        lut = pp.create_lut(lons, lats)
        proj_lut[infile] = lut
    del h5data

    h5data = ReadAodData()
    for infile in proj_lut:
        di = proj_lut[infile]['pre_i']
        dj = proj_lut[infile]['pre_j']
        pi = proj_lut[infile]['prj_i']
        pj = proj_lut[infile]['prj_j']
        aod = h5data.load_aod(infile)
        aod_550[pi, pj] = aod[di, dj]
    del h5data

    outpath = os.path.join(yaml1.opath, yaml1.ymd[:4],)
    outname = '%s_AOD_DAILY_%s.HDF5' % (yaml1.job_name, yaml1.ymd)

    if not os.path.isdir(outpath):
        os.makedirs(outpath)

    outpath = os.path.join(outpath, outname)
    h5w = h5py.File(outpath, 'w')
    h5w.create_dataset('Optical_Depth_Land_And_Ocean', data = aod_550, compression = 'gzip', compression_opts = 5, shuffle = True)
    h5w.close()


if __name__ == '__main__':
#     infile = r'D:\data\aerosol\FY3D_MERSI_AOD_GRANULE_20191001_0020.HDF5'
#     infile = r'D:\data\MERSI\FY3D_MERSI_GBAL_L1_20181001_0020_1000M_MS.HDF'
#     h5 = ReadH5Data()
#     data = h5.loads(infile)
#
#     print (data)
    args = sys.argv[1:]
    yaml_file = args[0]
    main(yaml_file)
