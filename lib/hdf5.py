#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-10-29 11:26
# @Author  : NingAnMe <ninganme@qq.com>
import h5py
import numpy as np


def get_hdf5_data(hdf5_file, data_name, slope=None, intercept=None, valid_range=None, full_value=-999):
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
        data[invalid_index] = full_value
        return data


def write_hdf5_and_compress(datas, out_file, compression='gzip', compression_opts=5, shuffle=True):
    """
    :param shuffle: 分块写入
    :param compression_opts: 压缩等级
    :param compression: 压缩类型
    :param datas: 数据，(dict)
    :param out_file: 输出文件 (str)
    :return:
    """
    if not datas:
        return
    compression = compression
    compression_opts = compression_opts
    shuffle = shuffle
    with h5py.File(out_file, 'w') as hdf5:
        for key in datas:
            dataset_name = key
            data = datas[dataset_name]
            # 处理
            hdf5.create_dataset(dataset_name, data=data, compression=compression,
                                compression_opts=compression_opts,
                                shuffle=shuffle)
    print('hdf5 >>> {}'.format(out_file))
