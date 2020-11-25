# -*- coding: utf-8 -*-

import os
import sys
import h5py
import yaml
import numpy as np
from qiae_lib.dp_proj import prj_core
import matplotlib.pyplot as plt
import matplotlib as mpl
import cartopy.crs as ccrs
# import cartopy.feature as cfeature
# from mpl_toolkits.axes_grid1 import make_axes_locatable, axes_size
from cartopy.mpl.ticker import LongitudeFormatter, LatitudeFormatter

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


def combine(yaml_file, outfile):

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

    h5data = ReadAodData()
    for infile in proj_lut:
        di = proj_lut[infile]['pre_i']
        dj = proj_lut[infile]['pre_j']
        pi = proj_lut[infile]['prj_i']
        pj = proj_lut[infile]['prj_j']
        aod = h5data.load_aod(infile)
        aod_550[pi, pj] = aod[di, dj]

    h5w = h5py.File(outfile, 'w')
    h5w.create_dataset('Optical_Depth_Land_And_Ocean', data = aod_550, compression = 'gzip', compression_opts = 5, shuffle = True)
    h5w.close()


def main(yaml_file):

    # 读取接口文件
    yaml1 = ReadYaml(yaml_file)

    outpath = yaml1.opath
    outname = '%s_AOD_DAILY_%s.HDF5' % (yaml1.job_name, yaml1.ymd)

    sat, sensor = yaml1.job_name.split('_')
    ymd = yaml1.ymd

    if not os.path.isdir(outpath):
        os.makedirs(outpath)

    outfile = os.path.join(outpath, outname)
    outpng = outfile + '.png'
    ndsi = None
    if not os.path.isfile(outfile):
        ndsi = combine(yaml_file, outfile)
    if not os.path.isfile(outpng):
        if ndsi is not None:
            plot_map(ndsi, outpng)
        else:
            h5w = h5py.File(outfile)
            data = h5w.get('Optical_Depth_Land_And_Ocean')[:]
            h5w.close()
            plot_map(sat, sensor, ymd, data, outpng)


def plot_map(sat, sensor, ymd, img, outfig):

    fig = plt.figure(figsize = (10, 5))
    ax = fig.add_subplot(111, projection = ccrs.PlateCarree())
#     ax = plt.axes(projection = ccrs.PlateCarree(central_longitude = 0))
#     ax.coastlines()
    ax.coastlines(resolution = '50m', color = 'black', linewidth = 0.3)
#     ax.stock_img()
    ax.set_global()
    print('start')
    # w e n s
#     img_extent = (-179.95, 179.95, -89.95, 89.95)
    img_extent = (-180, 180, -90, 90)

    # 设置色标的最大最小值
    norm = mpl.colors.Normalize(vmin = 0, vmax = 1.2)
    img = np.ma.masked_where(img < 0 , img)
#     cb = ax.scatter(lon, lat, marker = '.', c = value, s = 0.4, cmap = 'jet', lw = 0, norm = norm)
    im = ax.imshow(img, origin = 'upper', cmap = 'jet', norm = norm, extent = img_extent, transform = ccrs.PlateCarree())

    plt.colorbar(im, fraction = 0.046, pad = 0.04)

    # 标注坐标轴
    ax.set_xticks([-180, -120, -60, 0, 60, 120, 180], crs = ccrs.PlateCarree())
    ax.set_yticks([-90, -60, -30, 0, 30, 60, 90], crs = ccrs.PlateCarree())
    # zero_direction_label用来设置经度的0度加不加E和W
    lon_formatter = LongitudeFormatter(zero_direction_label = False)
    lat_formatter = LatitudeFormatter()
    ax.xaxis.set_major_formatter(lon_formatter)
    ax.yaxis.set_major_formatter(lat_formatter)
    ax.grid(linestyle = '--')

    ax.set_title('%s/%s Daily Aod550 %s' % (sat, sensor, ymd))

    # Save the plot by calling plt.savefig() BEFORE plt.show()
#     plt.savefig('coastlines.pdf')
    plt.savefig(outfig)
    print('end')


if __name__ == '__main__':
    args = sys.argv[1:]
    yaml_file = args[0]
    main(yaml_file)
