#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2019/12/3 10:47
# @Author  : NingAnMe <ninganme@qq.com>
import inspect
import numpy as np
from PIL import Image
from lib.idw import tree

import os
import signal
import subprocess
import platform


def run_cmd(cmd_string, timeout=20):
    print("命令为：" + cmd_string)
    p = subprocess.Popen(cmd_string, stderr=subprocess.STDOUT, stdout=subprocess.PIPE, shell=True,
                         close_fds=True,
                         start_new_session=True)

    decoding = 'utf-8'
    if platform.system() == "Windows":
        decoding = 'gbk'

    try:
        (msg, errs) = p.communicate(timeout=timeout)
        ret_code = p.poll()
        if ret_code:
            code = 1
            msg = "[Error]Called Error ： " + str(msg.decode(decoding))
        else:
            code = 0
            msg = str(msg.decode(decoding))
    except subprocess.TimeoutExpired:
        # 注意：不能只使用p.kill和p.terminate，无法杀干净所有的子进程，需要使用os.killpg
        p.kill()
        p.terminate()
        os.killpg(p.pid, signal.SIGTERM)

        # 注意：如果开启下面这两行的话，会等到执行完成才报超时错误，但是可以输出执行结果
        # (outs, errs) = p.communicate()
        # print(outs.decode('utf-8'))

        code = 1
        msg = "[ERROR]Timeout Error : Command '" + cmd_string + "' timed out after " + str(timeout) + " seconds"
    except Exception as e:
        code = 1
        msg = "[ERROR]Unknown Error : " + str(e)

    return code, msg


def get_function_name():
    """获取正在运行函数(或方法)名称"""
    return inspect.stack()[1][3]


def _idw(data_grid, lons_grid, lats_grid, lons_origin, lats_origin):
    # index_valid = np.logical_and(data_grid > 0, data_grid < 1.5)
    index_valid = data_grid > 0  # TODO 需要改为这行
    valid_count = index_valid[0].sum()
    print(f'valid count: {valid_count}')
    # if valid_count >= 16:
    if valid_count >= 10:
        value = data_grid[index_valid]
        lons = lons_origin[index_valid]
        lats = lats_origin[index_valid]
        num = lons.shape[0]
        lon_lat = np.zeros((num, 2), dtype=np.float)
        lon_lat[:, 0] = lons
        lon_lat[:, 1] = lats
        idw_tree = tree(lon_lat, value)

        shape = lons_grid.shape
        lons_grid = lons_grid.reshape(-1, 1)
        lats_grid = lats_grid.reshape(-1, 1)
        lons_lats = np.concatenate((lons_grid, lats_grid), axis=1)
        # values = idw_tree(lons_lats, 14)
        values = idw_tree(lons_lats, 10)
        values = values.reshape(shape)
        return values


def format_data(data, lons_grid, lats_grid, lons_origin, lats_origin):
    """
    变为网格数据，无效值填充为0
    :param data:
    :param lons_grid:
    :param lats_grid:
    :param lons_origin:
    :param lats_origin:
    :return:
    """
    try:
        data_new = _idw(data, lons_grid, lats_grid, lons_origin, lats_origin)

        image = Image.fromarray(data)
        image = image.resize((2048, 2000), resample=Image.NEAREST)
        data = np.array(image)
        print(data.shape)

        if data_new is not None:
            # index_invalid = np.logical_or(data < 0, data > 1.5)  # TODO
            index_invalid = np.logical_or(data < 0, data > 999)
            data[index_invalid] = np.nan
            index_valid = np.logical_and(data > 0, data < 999)
            # index_valid = np.logical_and(data > 0, data < 1.5)  # TODO
            data[index_valid] = data_new[index_valid]
        else:
            data[:] = -327.68

        data[np.isnan(data)] = -327.68
    except:
        data[:] = -327.68

    return data


def fill_2d(array2d, mask, use_from):
    """
    2维矩阵无效值补点
    array2d  2维矩阵
    mask     无效值掩模矩阵
    useFrom  u/d/l/r, 用上/下/左/右的点来补点
    """
    assert len(array2d.shape) == 2, \
        "array2d must be 2d array."
    assert array2d.shape == mask.shape, \
        "array2d and musk must have same shape."

    condition = np.empty_like(mask)
    # 用上方的有效点补点
    if use_from == 'up' or use_from == 'u':
        condition[1:, :] = mask[1:, :] * (~mask)[:-1, :]
        condition[0, :] = False
        index = np.where(condition)
        array2d[index[0], index[1]] = array2d[index[0] - 1, index[1]]

    # 用右方的有效点补点
    elif use_from == 'right' or use_from == 'r':
        condition[:, :-1] = mask[:, :-1] * (~mask)[:, 1:]
        condition[:, -1] = False
        index = np.where(condition)
        array2d[index[0], index[1]] = array2d[index[0], index[1] + 1]

    # 用下方的有效点补点
    elif use_from == 'down' or use_from == 'd':
        condition[:-1, :] = mask[:-1, :] * (~mask)[1:, :]
        condition[-1, :] = False
        index = np.where(condition)
        array2d[index[0], index[1]] = array2d[index[0] + 1, index[1]]

    # 用左方的有效点补点
    elif use_from == 'left' or use_from == 'l':
        condition[:, 1:] = mask[:, 1:] * (~mask)[:, :-1]
        condition[:, 0] = False
        index = np.where(condition)
        array2d[index[0], index[1]] = array2d[index[0], index[1] - 1]


def fill_points_2d_nan(array2d):
    """
    2维矩阵无效值补点
    array2d  2维矩阵
    invalidValue  无效值
    """
    # 用右方的有效点补点
    # mask = np.isnan(array2d)
    # fill_2d(array2d, mask, 'r')

    # 用左方的有效点补点
    mask = np.isnan(array2d)
    fill_2d(array2d, mask, 'l')

    # 用上方的有效点补点
    mask = np.isnan(array2d)
    fill_2d(array2d, mask, 'u')

    # 用下方的有效点补点
    mask = np.isnan(array2d)
    fill_2d(array2d, mask, 'd')
