#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-07-27 11:06
# @Author  : NingAnMe <ninganme@qq.com>
import numpy as np
from scipy.spatial import cKDTree


class Verification:
    def __init__(self, lons1_kdtree, lats1_kdtree, lons2_query, lats2_query):
        self.lons1_kdtree = lons1_kdtree  # KDtree建模用(分辨率高，数据量大)
        self.lats1_kdtree = lats1_kdtree  # KDtree建模用(分辨率高，数据量大)

        self.lons2_query = lons2_query  # 获取index用
        self.lats2_query = lats2_query  # 获取index用

        self.valid_index1 = np.where(np.logical_and(np.isfinite(lons1_kdtree), np.isfinite(lats1_kdtree)))  # 有效建模数据的index
        self.valid_index2 = np.where(np.logical_and(np.isfinite(lons2_query), np.isfinite(lats2_query)))  # 有效应用数据的index

        print(f'KDtree 数据的有效数量： {len(self.valid_index1[0])}')
        print(f'Query 数据的有效数量： {len(self.valid_index2[0])}')

        if len(self.valid_index1) >= 1:
            self.lons1_kdtree = lons1_kdtree[self.valid_index1]
            self.lats1_kdtree = lats1_kdtree[self.valid_index1]
        else:
            raise ValueError('lons1 lats1没有足够的有效数据')

        if len(self.valid_index2) >= 1:
            self.lons2_query = lons2_query[self.valid_index2]
            self.lats2_query = lats2_query[self.valid_index2]
        else:
            raise ValueError('lons2 lat2没有足够的数据')

        self.kdtree_model = None
        self.dist = None
        self.index_kdtree = None

    @classmethod
    def __format_data(cls, data):
        data = data.reshape(-1, 1)
        return data

    def __get_x(self, d1, d2):
        return np.concatenate((self.__format_data(d1), self.__format_data(d2)), axis=1)

    def get_kdtree(self):
        x = self.__get_x(self.lons1_kdtree, self.lats1_kdtree)
        try:
            print('开始KDtree建模')
            self.kdtree_model = cKDTree(x)
            print('完成KDtree建模')
            return True
        except Exception as why:
            print(why)
            return False

    def get_dist_and_index_kdtree(self):
        """
        获取距离和kdtree数据的index信息
        :return:
        """
        self.dist, self.index_kdtree = self.kdtree_model.query(self.__get_x(self.lons2_query, self.lats2_query))

    def get_index_dist(self, pre_dist=0.01):
        """
        获取符合距离阈值的index信息
        :return:
        """
        print(f'dist ：：min {np.min(self.dist)}  max {np.max(self.dist)} mean {np.mean(self.dist)}')
        index_dist = self.dist < pre_dist
        result_count = index_dist.sum()
        print(f'符合距离阈值的数据量=========：{result_count}')
        return index_dist

    def get_kdtree_data(self, data):
        """
        获取与kdtree建模使用数据同纬度的数据
        :param data:
        :return:
        """
        return data[self.valid_index1][self.index_kdtree]

    def get_query_data(self, data):
        """
        获取与query数据相同的纬度的数据
        :param data:
        :return:
        """
        return data[self.valid_index2]
