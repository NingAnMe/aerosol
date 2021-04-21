#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-07-23 10:11
# @Author  : NingAnMe <ninganme@qq.com>
import os
import h5py
import numpy as np
from pyhdf.SD import SD, SDC


class CppFy3c:
    def __init__(self, in_file, geo_file=None):
        self.in_file = in_file
        self.geo_file = geo_file
        self.filename = os.path.basename(in_file)

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
            data[np.logical_or(data < valid_range[0], data > valid_range[1])] = np.nan
            data = data * slope + intercept
            return data

    def get_lon_lat(self):
        lons = self.get_hdf5_data(self.geo_file, 'Geolocation/Longitude')
        lats = self.get_hdf5_data(self.geo_file, 'Geolocation/Latitude')
        return lons, lats

    def get_ctop_temperature(self):
        return self.get_hdf5_data(self.in_file, '5-min granule Cloud Top Temperature', intercept=0)

    def get_ctop_hight(self):
        return self.get_hdf5_data(self.in_file, '5-min granule Cloud Top Height')


class CppModis:
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
            data[np.logical_or(data < valid_range[0], data > valid_range[1])] = np.nan
            data = data * slope + intercept
            return data

    def get_lon_lat(self):
        lons = self.get_hdf4_data(self.in_file, 'Longitude')
        lats = self.get_hdf4_data(self.in_file, 'Latitude')
        return lons, lats

    def get_ctop_temperature(self):
        return self.get_hdf4_data(self.in_file, 'Cloud_Top_Temperature', intercept=0)

    def get_ctop_hight(self):
        return self.get_hdf4_data(self.in_file, 'Cloud_Top_Height')


class AodFy3d:
    def __init__(self, in_file, geo_file=None):
        self.in_file = in_file
        self.geo_file = geo_file
        self.filename = os.path.basename(in_file)

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
            data[np.logical_or(data < valid_range[0], data > valid_range[1])] = np.nan
            data = data * slope + intercept
            return data

    def get_lon_lat(self):
        lons = self.get_hdf5_data(self.geo_file, 'Geolocation/Longitude')
        lats = self.get_hdf5_data(self.geo_file, 'Geolocation/Latitude')
        return lons, lats

    def get_ctop_temperature(self):
        return self.get_hdf5_data(self.in_file, '5-min granule Cloud Top Temperature', intercept=0)
