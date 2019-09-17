# coding:utf-8
"""
pm_calculate.py
计算相关函数
~~~~~~~~~~~~~~~~~~~
creation time : 2018 1 19
author : anning
~~~~~~~~~~~~~~~~~~~
"""

import numpy as np


def get_centre_point_avg_and_std(dataset, data_range):
    """
    计算数据集的平均值和标准差，二维或者三维
    :param dataset: (np.ndarray)获取的数据集
    :param data_range: (int)截取范围大小
    :return:(list)
    """
    # 获取数据表的维数
    rank = dataset.ndim

    if rank == 2:  # 与通道数无关
        # 获取轴数
        shape = dataset.shape
        dim = int(shape[0])
        avg_and_std = calculate_centre_point_avg_and_std(dataset,
                                                         dim, data_range)
        return avg_and_std

    elif rank == 3:  # 多条通道
        # 获取轴数
        shape = dataset.shape
        channel_num = int(shape[0])  # 通道数
        dim = int(shape[1])  # 每个通道的数据轴数

        # 记录每条通道的均值和标准差
        channels_avg_and_std = []
        for i in xrange(0, channel_num):
            dataset_tem = dataset[i]
            avg_and_std = calculate_centre_point_avg_and_std(dataset_tem,
                                                             dim, data_range)
            channels_avg_and_std.append(avg_and_std)
        return channels_avg_and_std

    else:
        return ['-nan', '-nan']


def calculate_centre_point_avg_and_std(dataset, dim, data_range=3):
    """
    计算一个二维正方形数据集中心点的均值和标准差
    :param dataset: 一个二维数据列表
    :param dim: 轴数
    :param data_range: 范围大小
    :return:
    """
    if len(dataset) != 0:
        # 获取切片的位置坐标
        num_start = int(dim / 2) - int(data_range / 2) - 1
        num_end = int(dim / 2) + int(data_range / 2)

        # 对数据表进行切片
        dataset = dataset[num_start:num_end, num_start:num_end]

        # 计算均值和标准差
        avg = np.mean(dataset)
        std = np.std(dataset)

        if avg == -999:
            return ['-nan', '-nan']
        else:
            return [avg, std]
    else:
        raise ValueError('value error： dataset')


def extract_lines(dataset, probe_count, probe_id):
    """
    提取探头号对应行的数据
    :param dataset: (np.ndarray)二维数据集
    :param probe_count: 探头数量
    :param probe_id: 探头号
    :return:
    """
    probe_count = probe_count  # 探头数量
    line_count = len(dataset)  # 数据集总行数
    probe_id = int(probe_id) - 1  # 将探头号转换为计算机数值

    dataset_index = []
    index = probe_id
    while index < line_count:
        dataset_index.append(index)
        index += probe_count

    dataset_new = dataset[dataset_index, :]
    return dataset_new


def rolling_calculate_avg_std(dataset, rolling_lines):
    """
    对数组进行滚动计算，输出均值和标准差的列表
    :param dataset: (np.ndarray)二维数据集
    :param rolling_lines: 每次滚动的行数
    :return: (np.ndarray)
    """
    line_before = rolling_lines / 2  # 本行数据的前
    line_after = rolling_lines - (rolling_lines / 2)  # 本行数据后
    line_count = len(dataset)  # 数据集的总行数
    # 滚动处理
    avg_std_list = []
    for i in xrange(0, line_count):
        start = i - line_before + 1  # 开始的行号
        start = start if start >= 0 else 0
        end = i + line_after + 1  # 结束的行号
        end = end if end <= line_count else line_count
        tem_dataset = dataset[start: end]
        # 除去填充值 0
        idx = np.where(tem_dataset != 0)
        tem_dataset = tem_dataset[idx]
        if len(tem_dataset) != 0:
            avg = np.mean(tem_dataset)  # 计算均值
            std = np.std(tem_dataset)  # 计算标准差
            avg_std_list.append([avg, std])
        else:
            avg_std_list.append([0, 0])
    avg_std_list = np.array(avg_std_list)
    return avg_std_list


def filter_valid_value(dataset, avg_std_list, multiple):
    """
    过滤标准差范围内的有效值，dataset 和 avg_std_list 行数需要相同
    :param dataset: (np.ndarray)二维数据集
    :param avg_std_list: (np.ndarray or list)二维数据集
    :param multiple: 过滤多少倍 std 范围内的值
    :return: (np.ndarray)一个与原来数组行数相同的二维数组
    """
    dataset_new = []
    if len(dataset) == 0:
        return dataset_new
    else:
        # 一共多少行
        line_count = len(dataset)
        for i in xrange(0, line_count):
            line = dataset[i]
            # 除去无效值
            idx = np.where(line != 0)
            line = line[idx]
            avg, std = avg_std_list[i]

            line = [value for value in line if is_valid(value, avg, std, multiple)]
            if len(line) == 0:
                line.append(0)
            dataset_new.append(line)

    return dataset_new


def is_valid(value, avg, std, multiple):
    """
    判断是否是标准差范围内的有效值
    :param value: 数值
    :param avg: 均值
    :param std: 标准差
    :param multiple: 倍数
    :return:
    """
    if (avg - std * multiple) <= value <= (avg + std * multiple):
        return True
    else:
        return False


def calculate_avg(dataset):
    """
    计算二维数组中每一行数据的均值
    :param dataset: (np.ndarray)二维数组
    :return: 一个与原来数组行数相同的二维数组
    """
    avg_list = []
    if len(dataset) == 0:
        return avg_list
    else:
        # 一共多少行
        line_count = len(dataset)
        for i in xrange(0, line_count):
            line = dataset[i]

            if len(line) == 0:
                avg = 0
            else:
                avg = np.mean(line)

            avg_list.append(avg)
    return avg_list


def expand_dataset_line(dataset, multiple):
    """
    将一个数据集的行数扩大为原来的 N 倍，N 行使用原来一行的数据
    :param dataset: 二维数据集
    :param multiple: 倍数
    :return:
    """
    dataset_new = []
    for line in dataset:
        for i in xrange(0, multiple):
            dataset_new.append(line)
    return dataset_new
