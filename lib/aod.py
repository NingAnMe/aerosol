#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-07-30 12:36
# @Author  : NingAnMe <ninganme@qq.com>
import os
from datetime import datetime
import h5py
try:
    import gdal
except ImportError:
    from osgeo import gdal
from pyhdf.SD import SD, SDC
import numpy as np


class AodFy3d1km:

    def __init__(self, in_file, geo_file=None, geo_path=None):
        self.in_file = in_file  # FY3D_MERSI_ORBT_L2_AOD_MLT_NUL_20190916_0910_1000M_MS.HDF
        self.filename = os.path.basename(in_file)
        self.geo_file = self.get_geo_file(geo_file, geo_path)  # FY3D_MERSI_GBAL_L1_20190916_0910_GEO1K_MS.HDF
        ymd = self.filename.split('_')[7]
        hm = self.filename.split('_')[8]
        self.dt = datetime.strptime(ymd+hm, '%Y%m%d%H%M')

    @classmethod
    def get_hdf5_data(cls, hdf5_file, data_name, slope=None, intercept=None, valid_range=None):
        with h5py.File(hdf5_file, 'r') as hdf:
            dataset = hdf.get(data_name)
            if slope is None:
                slope = dataset.attrs['Slope']
            if intercept is None:
                intercept = dataset.attrs['Intercept']
            if valid_range is None:
                valid_range = dataset.attrs['valid_range']
            data = dataset[:].astype(np.float)
            valid_index = np.logical_or(data < valid_range[0], data > valid_range[1])
            data = data * slope + intercept
            data[valid_index] = -999
            return data

    def get_geo_file(self, geo_file=None, geo_path=None):
        # FY3D_MERSI_ORBT_L2_AOD_MLT_NUL_20190916_0910_1000M_MS.HDF
        if geo_file is not None:
            return geo_file
        elif geo_path is not None:
            namesplit = self.filename.split('_')
            geo_fy3d_name = 'FY3D_MERSI_GBAL_L1_{}_{}_GEO1K_MS.HDF'.format(namesplit[7], namesplit[8])
            return os.path.join(geo_path, geo_fy3d_name)
        else:
            return None

    def get_lon_lat(self):
        assert self.geo_file is not None, "没有有效的GEO文件"
        lons = self.get_hdf5_data(self.geo_file, '/Geolocation/Longitude')
        lats = self.get_hdf5_data(self.geo_file, '/Geolocation/Latitude')
        return lons, lats

    def get_aod(self):
        return self.get_hdf5_data(self.in_file, 'AOT_Land', 0.001, 0, (0, 1500))[1]


class AodFy3d5km:

    def __init__(self, in_file, geo_file=None):
        self.in_file = in_file
        self.geo_file = geo_file
        self.filename = os.path.basename(in_file)  # FY3D_MERSI_GBAL_L2_AOD_MLT_GLL_20190906_POAD_5000M_MS.HDF
        ymd = self.filename.split('_')[7]
        self.dt = datetime.strptime(ymd, '%Y%m%d')

    @classmethod
    def get_hdf5_data(cls, hdf5_file, data_name, slope=None, intercept=None, valid_range=None):
        with h5py.File(hdf5_file, 'r') as hdf:
            dataset = hdf.get(data_name)
            if dataset is not None:
                if slope is None:
                    slope = dataset.attrs['Slope']
                if intercept is None:
                    intercept = dataset.attrs['Intercept']
                if valid_range is None:
                    valid_range = dataset.attrs['valid_range']
                data = dataset[:].astype(np.float)
                valid_index = np.logical_or(data < valid_range[0], data > valid_range[1])
                data = data * slope + intercept
                data[valid_index] = -999
                return data

    @staticmethod
    def get_lon_lat():
        laty = np.arange(90, -90, -0.05)
        lonx = np.arange(-180, 180, 0.05)
        lons, lats = np.meshgrid(lonx, laty)
        return lons, lats

    def get_aod(self):
        return self.get_hdf5_data(self.in_file, 'AOT_Land_Mean', 0.001, 0, (0, 1500))[1]


class AodModis:
    def __init__(self, in_file, geo_file=None):
        self.in_file = in_file
        self.geo_file = geo_file
        self.filename = os.path.basename(in_file)
        yj = self.filename.split('.')[1][1:]
        hm = self.filename.split('.')[2]
        self.dt = datetime.strptime(yj+hm, "%Y%j%H%M")

    @classmethod
    def get_hdf4_data(cls, hdf4_file, data_name, slope=None, intercept=None, valid_range=None):
        hdf = SD(hdf4_file, SDC.READ)
        dataset = hdf.select(data_name)
        if dataset is not None:
            attrs = dataset.attributes()
            if slope is None:
                slope = attrs['scale_factor']
            if intercept is None:
                intercept = attrs['add_offset']
            if valid_range is None:
                valid_range = attrs['valid_range']
            data = dataset.get().astype(np.float)
            valid_index = np.logical_or(data < valid_range[0], data > valid_range[1])
            data = data * slope + intercept
            data[valid_index] = -999
            return data

    def get_lon_lat(self):
        lons = self.get_hdf4_data(self.in_file, 'Longitude')
        lats = self.get_hdf4_data(self.in_file, 'Latitude')
        return lons, lats

    def get_aod(self):
        return self.get_hdf4_data(self.in_file, 'Optical_Depth_Land_And_Ocean', valid_range=(0, 1500))


class AodFy4a4km:

    def __init__(self, in_file, geo_file='lib/FY4A_4KM_GEO.NC'):
        self.in_file = in_file
        self.geo_file = geo_file
        self.filename = os.path.basename(in_file)
        # ymdhm = "".join(os.path.splitext(self.filename)[0].split('_')[4:6])
        # self.dt = datetime.strptime(ymdhm, '%Y%m%d%H%M')

    @classmethod
    def get_hdf5_data(cls, hdf5_file, data_name, slope=None, intercept=None, valid_range=None):
        with h5py.File(hdf5_file, 'r') as hdf:
            dataset = hdf.get(data_name)
            if slope is None:
                slope = dataset.attrs['Slope']
            if intercept is None:
                intercept = dataset.attrs['Intercept']
            if valid_range is None:
                valid_range = dataset.attrs['valid_range']
            data = dataset[:].astype(np.float)
            invalid_index = np.logical_or(data < valid_range[0], data > valid_range[1])
            data = data * slope + intercept
            data[invalid_index] = -999
            return data

    def get_lon_lat(self):
        laty = self.get_hdf5_data(self.geo_file, 'lat', 1, 0)
        lonx = self.get_hdf5_data(self.geo_file, 'lon', 1, 0)
        lons, lats = np.meshgrid(lonx, laty)
        return lons, lats

    def get_aod(self):
        return self.get_hdf5_data(self.in_file, 'AOD', 1, 0, (0, 1.5))[1]


class AodModis100km:
    def __init__(self, in_file, geo_file=None):
        self.in_file = in_file
        self.geo_file = geo_file
        self.filename = os.path.basename(in_file)

    @classmethod
    def get_hdf4_data(cls, hdf4_file, data_name, slope=None, intercept=None, valid_range=None):
        hdf = SD(hdf4_file, SDC.READ)
        dataset = hdf.select(data_name)
        if dataset is not None:
            attrs = dataset.attributes()
            if slope is None:
                slope = attrs['scale_factor']
            if intercept is None:
                intercept = attrs['add_offset']
            if valid_range is None:
                valid_range = attrs['valid_range']
            data = dataset.get().astype(np.float)
            valid_index = np.logical_or(data < valid_range[0], data > valid_range[1])
            data = data * slope + intercept
            data[valid_index] = -999
            return data

    @staticmethod
    def get_lon_lat():
        laty = np.arange(90, -90, -1)
        lonx = np.arange(-180, 180, 1)
        lons, lats = np.meshgrid(lonx, laty)
        return lons, lats

    @staticmethod
    def get_lon_lat_5km():
        laty = np.arange(90, -90, -0.05)
        lonx = np.arange(-180, 180, 0.05)
        lons, lats = np.meshgrid(lonx, laty)
        return lons, lats

    def get_aod(self):
        return self.get_hdf4_data(self.in_file, 'Aerosol_Optical_Depth_Land_Mean_Mean', valid_range=(0, 1500))


class AodCombine:

    def __init__(self, in_file):
        self.in_file = in_file
        self.filename = os.path.basename(in_file)  # FY3D_MERSI_GBAL_L2_AOD_MLT_GLL_20190906_POAD_1000M_MS.HDF
        ymd = self.filename.split('_')[7]
        self.dt = datetime.strptime(ymd, '%Y%m%d')

    @classmethod
    def get_hdf5_data(cls, hdf5_file, data_name, slope=None, intercept=None, valid_range=None):
        with h5py.File(hdf5_file, 'r') as hdf:
            dataset = hdf.get(data_name)
            if slope is None:
                slope = dataset.attrs['Slope']
            if intercept is None:
                intercept = dataset.attrs['Intercept']
            if valid_range is None:
                valid_range = dataset.attrs['valid_range']
            data = dataset[:].astype(np.float)
            valid_index = np.logical_or(data < valid_range[0], data > valid_range[1])
            data = data * slope + intercept
            data[valid_index] = -999
            return data

    def get_lon_lat(self):
        lons = self.get_hdf5_data(self.in_file, 'Longitude', 1, 0, (-180, 180))
        lats = self.get_hdf5_data(self.in_file, 'Latitude', 1, 0, (-90, 90))
        return lons, lats

    def get_aod(self):
        return self.get_hdf5_data(self.in_file, 'Optical_Depth_Land_And_Ocean', 1, 0, (0, 1.5))


class Dataset:
    def __init__(self, in_file):
        self.in_file = in_file  # Tiff或者ENVI文件

        dataset = gdal.Open(self.in_file)
        self.XSize = dataset.RasterXSize  # 网格的X轴像素数量
        self.YSize = dataset.RasterYSize  # 网格的Y轴像素数量
        self.GeoTransform = dataset.GetGeoTransform()  # 投影转换信息
        self.ProjectionInfo = dataset.GetProjection()  # 投影信息

    def get_data(self, band):
        """
        band: 读取第几个通道的数据
        """
        dataset = gdal.Open(self.in_file)
        band = dataset.GetRasterBand(band)
        data = band.ReadAsArray()
        return data

    def get_lon_lat(self):
        """
        获取经纬度信息
        """
        gtf = self.GeoTransform
        x_range = range(0, self.XSize)
        y_range = range(0, self.YSize)
        x, y = np.meshgrid(x_range, y_range)
        lon = gtf[0] + x * gtf[1] + y * gtf[2]
        lat = gtf[3] + x * gtf[4] + y * gtf[5]
        return lon, lat
