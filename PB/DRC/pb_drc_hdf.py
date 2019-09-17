#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
@Time    : 2018/8/3 9:23
@Author  : AnNing
"""

import h5py
import numpy as np
from PB.pb_io import attrs2dict
from pyhdf.SD import SD, SDC


class ReadHDF5(object):
    """
    读取HDF5文件的一些常用封装方法
    """

    def __init__(self):
        pass

    @staticmethod
    def read_file_attr(in_file):
        try:
            with h5py.File(in_file, 'r') as hdf5_file:
                return attrs2dict(hdf5_file.attrs)
        except Exception as why:
            print why
            return

    def write_hdf5(self, data_dict, out_file, dtype=np.float32):
        # 对数据进行循环处理,结果放在 self.result
        with h5py.File(out_file, 'w') as hdf5_file:
            for data_name in data_dict:
                if 'CH_'.lower() in data_name.lower():
                    group_name = data_name
                    for dataset in data_dict[group_name]:
                        data = data_dict[group_name][dataset]
                        whole_name = '{}{}'.format(group_name, dataset)
                        self.create_dataset(whole_name, data, dtype, hdf5_file)
                else:
                    dataset = data_name
                    data = data_dict[dataset]
                    whole_name = dataset
                    self.create_dataset(whole_name, data, dtype, hdf5_file)

    @staticmethod
    def create_dataset(name, data, dtype, hdf5, compression='gzip', compression_opts=5,
                       shuffle=True):
        dataset = hdf5.create_dataset(name, data=data, dtype=dtype,
                                      compression=compression,
                                      compression_opts=compression_opts,
                                      shuffle=shuffle)
        return dataset

    @staticmethod
    def write_file_attr(out_file, file_attr):
        with h5py.File(out_file, 'r') as hdf5_file:
            for k, v in file_attr.items():
                hdf5_file.attr[k] = v


class ReadHDF4(object):
    """
    """
    def __init__(self):
        pass

    @staticmethod
    def read_file_attr(in_file):
        try:
            hdf4 = SD(in_file, SDC.READ)
            return attrs2dict(hdf4.attributes())
        except Exception as why:
            print 'ReadHDF4.read_file_attr: {}'.format(why)
            return
