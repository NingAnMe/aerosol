"""
1、aerosol的程序中，MODIS数据在使用的时候没有做日地距离修正，是否去除FY3D的日地距离修正（通过GSISC的MODIS读取类确认的这个问题）
2、aerosol的程序中，需要使用MODIS的5通道和32通道，FY3D中没有对应的通道
3、aerosol程序中使用了cloudmask，对于couldmask程序，没有MODIS的5通道22通道27通道32通道33通道35通道
    如果使用FY3D的cloudmask，需要将FY3D的cloudmask转为MODIS的数据格式和数值（这点需要支持）
4、FY3D的GEO文件中的DEM数据使用

# ######aerosol的程序中，是否需要将FY3D的Radiance转到MODIS的Radiance
"""

import os
import sys
import pickle

from spectral.io import envi
import numpy as np

from PB.DRC import ReadMersiL1


def fy3d2modis_1km(in_file, out_file_1km, metadata_pickle):
    """
    缺少5通道和32通道
    :param in_file:
    :param out_file_1km:
    :param metadata_pickle:  hdr 头信息
    :return:
    """
    datas = np.zeros((2000, 2048, 36), dtype=np.float)
    data_loader = ReadMersiL1(in_file)

    data_map = {
        1: 'CH_03',
        2: 'CH_04',
        3: 'CH_01',
        4: 'CH_02',
        5: 'CH_05',  # 没有5通道的对应通道，暂时使用CH_05
        6: 'CH_06',
        7: 'CH_07',
        8: 'CH_08',
        9: 'CH_09',
        10: 'CH_10',
        12: 'CH_11',
        13: 'CH_12',
        15: 'CH_14',
        16: 'CH_15',
        17: 'CH_16',
        18: 'CH_17',
        19: 'CH_18',
        20: 'CH_20',
        23: 'CH_21',
        26: 'CH_05',
        28: 'CH_22',
        29: 'CH_23',
        31: 'CH_24',
        32: 'CH_24',  # 没有5通道的对应通道，暂时使用CH_24
    }

    refs = data_loader.get_ref()
    for channel in refs:
        _data = refs[data_map[channel]]
        _data[np.isnan(_data)] = -1
        datas[:, :, channel - 1] = _data
    rads = data_loader.get_rad()
    for channel in rads:
        _data = rads[data_map[channel]]
        _data[np.isnan(_data)] = -1
        datas[:, :, channel - 1] = _data

    metadatas = pickle.load(metadata_pickle)
    metadata = metadatas.get('1000m')
    _metadata = metadata.get('metadata')
    _interleave = metadata.get('interleave')
    envi.save_image(out_file_1km, datas, metadata=_metadata, interleave=_interleave, force=True)
    print('>>> {}'.format(out_file_1km))


def fy3d2modis_geo(in_file, out_file_geo, metadata_pickle):
    """
    缺少5通道和32通道
    :param in_file:
    :param out_file_geo:
    :param metadata_pickle:  hdr 头信息
    :return:
    """
    datas = np.zeros((2000, 2048, 8), dtype=np.float)
    data_loader = ReadMersiL1(in_file)

    data_map = {
        0: data_loader.get_latitude,
        1: data_loader.get_longitude,
        2: data_loader.get_sensor_zenith,
        3: data_loader.get_sensor_azimuth,
        4: data_loader.get_solar_zenith,
        5: data_loader.get_solar_azimuth,  # 没有5通道的对应通道，暂时使用CH_05
        6: data_loader.get_hight,
        7: data_loader.get_land_sea_mask,
    }

    for channel in data_map:
        _data = data_map[channel]()
        _data[np.isnan(_data)] = -1
        datas[:, :, channel] = _data

    metadatas = pickle.load(metadata_pickle)
    metadata = metadatas.get('geo')
    _metadata = metadata.get('metadata')
    _interleave = metadata.get('interleave')
    envi.save_image(out_file_geo, datas, metadata=_metadata, interleave=_interleave, force=True)
    print('>>> {}'.format(out_file_geo))


def fy3d2modis_met(in_file, out_file_met, metadata_pickle):
    """
    缺少5通道和32通道
    :param in_file:
    :param out_file_met:
    :param metadata_pickle:  hdr 头信息
    :return:
    """
    datas = np.zeros((200, 2), dtype=np.int8)
    data_loader = ReadMersiL1(in_file)

    flag = data_loader.get_day_night_flag()
    flag[np.isnan(flag)] = 1
    flag[flag == 2] = 1  # Mix 设置为晚上
    datas[:, 0] = flag.astype(np.int8)

    mirror = np.zeros((200, 1), dtype=np.int8)
    mirror[mirror % 2 == 0] = 1
    datas[:, 1] = mirror

    metadatas = pickle.load(metadata_pickle)
    metadata = metadatas.get('met')
    _metadata = metadata.get('metadata')
    _interleave = metadata.get('interleave')
    envi.save_image(out_file_met, datas, metadata=_metadata, interleave=_interleave, force=True)
    print('>>> {}'.format(out_file_met))


# def fy3d2modis_cloud(in_file, out_file_cloud, metadata_pickle):
#     """
#     缺少5通道和32通道
#     :param in_file:
#     :param out_file_cloud:
#     :param metadata_pickle:  hdr 头信息
#     :return:
#     """
#     datas = np.zeros((2000, 2048, 8), dtype=np.float)
#     data_loader = ReadMersiL1(in_file)
#
#     cloud_data = data_loader.get_cloudmask()
#
#     data_map = {
#         0: data_loader.get_latitude,
#         1: data_loader.get_longitude,
#         2: data_loader.get_sensor_zenith,
#         3: data_loader.get_sensor_azimuth,
#         4: data_loader.get_solar_zenith,
#         5: data_loader.get_solar_azimuth,  # 没有5通道的对应通道，暂时使用CH_05
#         6: data_loader.get_hight,
#         7: data_loader.get_land_sea_mask,
#     }
#
#     for channel in data_map:
#         _data = data_map[channel]()
#         _data[np.isnan(_data)] = -1
#         datas[:, :, channel] = _data
#
#     metadatas = pickle.load(metadata_pickle)
#     metadata = metadatas.get('geo')
#     _metadata = metadata.get('metadata')
#     _interleave = metadata.get('interleave')
#     envi.save_image(out_file_cloud, datas, metadata=_metadata, interleave=_interleave, force=True)
#     print('>>> {}'.format(out_file_cloud))
