#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-07-23 18:23
# @Author  : NingAnMe <ninganme@qq.com>
import os
import pickle
import numpy as np
from scipy.spatial import cKDTree
# from pykdtree.kdtree import KDTree  # 使用这个库没有办法保存kdtree


def get_kdtree(lons, lats):
    condition = np.logical_and(np.isfinite(lons), np.isfinite(lats))
    idx = np.where(condition)
    lon_new = lons[idx].reshape(-1, 1)
    lat_new = lats[idx].reshape(-1, 1)
    lons_lats = np.concatenate((lon_new, lat_new), axis=1)
    data = lons_lats
    print('start cKDTree')
    ck = cKDTree(data)
    return ck


def make_point_index_lut(lons_data, lats_data, out_file):
    condition = np.logical_and(np.isfinite(lons_data), np.isfinite(lats_data))
    idx = np.where(condition)
    lon_new = lons_data[idx].reshape(-1, 1)
    lat_new = lats_data[idx].reshape(-1, 1)
    # lons_lats = zip(lon_new.reshape(-1, ), lat_new.reshape(-1, ))
    lons_lats = np.concatenate((lon_new, lat_new), axis=1)
    print(lons_lats.shape)
    data = lons_lats
    print('start cKDTree')
    ck = cKDTree(data)

    out_dir = os.path.dirname(out_file)
    if not os.path.isdir(out_dir):
        os.makedirs(out_dir)

    with open(out_file, 'wb') as fp:
        pickle.dump((idx, ck), fp)
        print('生成KDtree查找表:{}'.format(out_file))


def get_point_index(lon, lat, idx, ck, pre_dist=0.04):
    fix_point = (lon, lat)
    dist, index = ck.query([fix_point], 1)
    dist = dist[0]
    index = index[0]
    print('---Query INFO---dist: {}  index: {}'.format(dist, index))

    if dist <= pre_dist:
        fix_point_index = (idx[0][index], idx[1][index])
        print("---INFO---Nearest fix point index={}".format(fix_point_index))
        return fix_point_index
    else:
        print('***WARNING*** dist > {}, Dont extract.'.format(pre_dist))
        return
