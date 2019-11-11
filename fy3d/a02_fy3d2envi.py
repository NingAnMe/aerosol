"""
1、aerosol的程序中，MODIS数据在使用的时候没有做日地距离修正，是否去除FY3D的日地距离修正（通过GSISC的MODIS读取类确认的这个问题）
2、aerosol的程序中，需要使用MODIS的5通道和32通道，FY3D中没有对应的通道
3、aerosol程序中使用了cloudmask，对于couldmask程序，没有MODIS的5通道22通道27通道32通道33通道35通道
    如果使用FY3D的cloudmask，需要将FY3D的cloudmask转为MODIS的数据格式和数值
4、aerosol程序中使用了DEM,需要对应FY3D的GEO文件中的DEM数据使用

# ######aerosol的程序中，是否需要将FY3D的Radiance转到MODIS的Radiance
"""

import os
import sys
import pickle

import h5py
from spectral.io import envi
import numpy as np

sys.path.append('..')

from PB.DRC import ReadMersiL1


def fy3d2modis_1km(in_file, out_file, metadata_pickle):
    """
    缺少5通道和32通道
    :param in_file:
    :param out_file:
    :param metadata_pickle:  hdr 头信息
    :return:
    """
    datas = np.zeros((2000, 2048, 36), dtype=np.float32)
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
        32: 'CH_24',  # 没有32通道的对应通道，暂时使用CH_24
    }

    data_map_reverse = {}
    for k, v in data_map.items():
        data_map_reverse[v] = k

    refs = data_loader.get_ref()
    for channel in refs:
        if channel not in data_map_reverse:
            continue
        _data = refs[channel]
        _data[np.isnan(_data)] = -1
        datas[:, :, data_map_reverse[channel] - 1] = _data
    rads = data_loader.get_rad()
    for channel in rads:
        if channel not in data_map_reverse:
            continue
        _data = rads[channel]
        _data[np.isnan(_data)] = -1
        datas[:, :, data_map_reverse[channel] - 1] = _data

    with open(metadata_pickle, 'rb') as f:
        metadatas = pickle.load(f)
    metadata = metadatas.get('1000m')
    _metadata = metadata.get('metadata')
    _interleave = metadata.get('interleave')
    envi.save_image(out_file, datas, metadata=_metadata, interleave=_interleave, force=True)
    print('>>> {}'.format(out_file))


def fy3d2modis_geo(in_file, out_file, metadata_pickle):
    """
    缺少5通道和32通道
    :param in_file:
    :param out_file:
    :param metadata_pickle:  hdr 头信息
    :return:
    """
    datas = np.zeros((2000, 2048, 8), dtype=np.float32)
    data_loader = ReadMersiL1(in_file)

    data_map = {
        0: data_loader.get_latitude,
        1: data_loader.get_longitude,
        2: data_loader.get_sensor_zenith,
        3: data_loader.get_sensor_azimuth,
        4: data_loader.get_solar_zenith,
        5: data_loader.get_solar_azimuth,
        6: data_loader.get_hight,
        7: data_loader.get_land_sea_mask,
    }

    for channel in data_map:
        _data = data_map[channel]()
        _data[np.isnan(_data)] = -1
        datas[:, :, channel] = _data

    with open(metadata_pickle, 'rb') as f:
        metadatas = pickle.load(f)
    print(metadatas.keys())
    metadata = metadatas.get('geo')
    print(metadata.keys())
    _metadata = metadata.get('metadata')
    _interleave = metadata.get('interleave')
    envi.save_image(out_file, datas, metadata=_metadata, interleave=_interleave, force=True)
    print('>>> {}'.format(out_file))


def fy3d2modis_met(in_file, out_file, metadata_pickle):
    """
    缺少5通道和32通道
    :param in_file:
    :param out_file:
    :param metadata_pickle:  hdr 头信息
    :return:
    """
    datas = np.zeros((200, 2), dtype=np.uint8)
    data_loader = ReadMersiL1(in_file)

    flag = data_loader.get_day_night_flag()
    flag[np.isnan(flag)] = 1
    flag[flag == 2] = 1  # Mix 设置为晚上

    # MODIS和FY3的白天晚上好像是反的，在MODIS中，1是白天，0是晚上
    idx0 = flag == 0
    idx1 = flag == 1
    flag[idx0] = 1
    flag[idx1] = 0

    datas[:, 0] = flag.astype(np.int8).reshape(-1)

    mirror = data_loader.get_mirror_side().reshape(-1).astype(np.int8)
    datas[:, 1] = mirror.reshape(-1)

    with open(metadata_pickle, 'rb') as f:
        metadatas = pickle.load(f)
    metadata = metadatas.get('met')
    _metadata = metadata.get('metadata')
    _interleave = metadata.get('interleave')
    envi.save_image(out_file, datas, metadata=_metadata, interleave=_interleave, force=True)
    print('>>> {}'.format(out_file))


def fy3d2modis_cloudmask(in_file, out_file, metadata_pickle):
    """
    缺少5通道和32通道
    :param in_file:
    :param out_file:
    :param metadata_pickle:  hdr 头信息
    :return:
    """
    datas = np.zeros((2000, 2048, 6), dtype=np.uint8)

    with h5py.File(in_file, 'r') as hdf:
        for i in range(6):
            datas[:, :, i] = hdf.get('Cloud_Mask')[i, :, :]

    with open(metadata_pickle, 'rb') as f:
        metadatas = pickle.load(f)
    metadata = metadatas.get('mod35')
    _metadata = metadata.get('metadata')
    _interleave = metadata.get('interleave')
    envi.save_image(out_file, datas, metadata=_metadata, interleave=_interleave, force=True)
    print('>>> {}'.format(out_file))


def fy3d2modis_cloudmask_qa(in_file, out_file, metadata_pickle):
    """
    缺少5通道和32通道
    :param in_file:
    :param out_file:
    :param metadata_pickle:  hdr 头信息
    :return:
    """
    datas = np.zeros((2000, 2048, 10), dtype=np.uint8)

    with h5py.File(in_file, 'r') as hdf:
        for i in range(10):
            datas[:, i] = hdf.get('Cloud_Mask_QA')[i, :]

    with open(metadata_pickle, 'rb') as f:
        metadatas = pickle.load(f)
    metadata = metadatas.get('mod35qa')
    _metadata = metadata.get('metadata')
    _interleave = metadata.get('interleave')
    envi.save_image(out_file, datas, metadata=_metadata, interleave=_interleave, force=True)
    print('>>> {}'.format(out_file))


if __name__ == '__main__':

    metadatas_ = 'metadatas.pickle'

    indir = '../test_data/FY3D_MERSI'
    outdir = '../test_data/fy3d_l2_test_input'

    l1_1000m = os.path.join(indir, 'FY3D_MERSI_GBAL_L1_20191022_1620_1000M_MS.HDF')
    l1_geo = os.path.join(indir, 'FY3D_MERSI_GBAL_L1_20191022_1620_GEO1K_MS.HDF')
    l1_couldmask = os.path.join(indir, 'FY3D_MERSI_ORBT_L2_CLM_MLT_NUL_20191022_1620_1000M_MS.HDF')

    l1_1000m_nevi = os.path.join(outdir, 'a1.17299.1910.1000m.hdr')
    fy3d2modis_1km(l1_1000m, l1_1000m_nevi, metadatas_)

    l1_geo_nevi = os.path.join(outdir, 'a1.17299.1910.geo.hdr')
    fy3d2modis_geo(l1_1000m, l1_geo_nevi, metadatas_)

    l1_met_nevi = os.path.join(outdir, 'a1.17299.1910.met.hdr')
    fy3d2modis_met(l1_1000m, l1_met_nevi, metadatas_)

    l1_cloudmask_nevi = os.path.join(outdir, 'a1.17299.1910.mod35.hdr')
    fy3d2modis_cloudmask(l1_couldmask, l1_cloudmask_nevi, metadatas_)

    l1_cloudmask_qa_nevi = os.path.join(outdir, 'a1.17299.1910.mod35qa.hdr')
    fy3d2modis_cloudmask(l1_couldmask, l1_cloudmask_qa_nevi, metadatas_)
