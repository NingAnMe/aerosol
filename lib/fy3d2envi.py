"""
1、aerosol的程序中，MODIS数据在使用的时候没有做日地距离修正，是否去除FY3D的日地距离修正（通过GSISC的MODIS读取类确认的这个问题）
2、aerosol的程序中，需要使用MODIS的5通道，FY3D中没有对应的通道
3、aerosol程序中使用了cloudmask，对于couldmask程序，没有MODIS的5通道22通道27通道32通道33通道35通道
    如果使用FY3D的cloudmask，需要将FY3D的cloudmask转为MODIS的数据格式和数值
4、aerosol程序中使用了DEM,需要对应FY3D的GEO文件中的DEM数据使用

# ######aerosol的程序中，是否需要将FY3D的Radiance转到MODIS的Radiance
"""
import pickle

import h5py
from spectral.io import envi
import numpy as np

from .load_mersi import ReadMersiL1


def fy3d2modis_1km(in_file, geo_file, out_file, metadata_pickle, vis_file, ir_file, coef_txt_flag):
    """
    缺少5通道和32通道
    :param in_file:
    :param geo_file:
    :param out_file:
    :param metadata_pickle:  hdr 头信息
    :param vis_file:  定标文件
    :param ir_file:  定标文件
    :param coef_txt_flag:  是否使用外部定标
    :return:
    """
    datas = np.zeros((2000, 2048, 36), dtype=np.float32)
    data_loader = ReadMersiL1(in_file, geo_file=geo_file, vis_file=vis_file, ir_file=ir_file,
                              coef_txt_flag=coef_txt_flag)

    data_map = {
        1: 'CH_03',
        2: 'CH_04',
        3: 'CH_01',
        4: 'CH_02',
        5: 'CH_02',  # 没有5通道的对应通道，暂时使用CH_02
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
        32: 'CH_25',
    }

    # 日地距离校正
    solar_zenith = data_loader.get_solar_zenith()
    scale = np.cos(np.deg2rad(solar_zenith))

    refs = data_loader.get_ref()
    for k, v in data_map.items():
        index = k
        channel = data_map[k]
        if channel in refs:
            _data = refs[channel] / scale  # 日地距离校正
            print(channel, np.nanmin(_data), np.nanmax(_data), np.nanmean(_data))
            _data[np.isnan(_data)] = -1
            datas[:, :, index - 1] = _data

    rads = data_loader.get_rad()
    for k, v in data_map.items():
        index = k
        channel = data_map[k]
        if channel in rads:
            _data = rads[channel]
            print(channel, np.nanmin(_data), np.nanmax(_data), np.nanmean(_data))
            _data[np.isnan(_data)] = -1
            datas[:, :, index - 1] = _data

    with open(metadata_pickle, 'rb') as f:
        metadatas = pickle.load(f)
    metadata = metadatas.get('1000m')
    _metadata = metadata.get('metadata')
    _interleave = metadata.get('interleave')
    envi.save_image(out_file, datas, metadata=_metadata, interleave=_interleave, force=True)
    print('>>> {}'.format(out_file))


def fy3d2modis_geo(l1_file, geo_file, out_file, metadata_pickle):
    """
    缺少5通道和32通道
    :param l1_file:
    :param geo_file:
    :param out_file:
    :param metadata_pickle:  hdr 头信息
    :return:
    """
    all_night = False
    datas = np.zeros((2000, 2048, 8), dtype=np.float32)
    data_loader = ReadMersiL1(l1_file, geo_file=geo_file)

    data_map = {
        0: data_loader.get_latitude,
        1: data_loader.get_longitude,
        2: data_loader.get_sensor_zenith,
        3: data_loader.get_sensor_azimuth,
        4: data_loader.get_solar_zenith,
        5: data_loader.get_solar_azimuth,
        6: data_loader.get_height,
        7: data_loader.get_land_sea_mask,
    }

    sz = data_loader.get_solar_zenith()
    sz = sz[np.isfinite(sz)]
    if (sz >= 75).all():
        all_night = True
        return all_night

    for channel in data_map:
        _data = data_map[channel]()
        if channel == 3 or channel == 5:
            index = _data > 180
            _data[index] = _data[index] - 360
        print(channel, np.nanmin(_data), np.nanmax(_data), np.nanmean(_data))
        _data[np.isnan(_data)] = -1
        datas[:, :, channel] = _data

    with open(metadata_pickle, 'rb') as f:
        metadatas = pickle.load(f)
    metadata = metadatas.get('geo')
    _metadata = metadata.get('metadata')
    _interleave = metadata.get('interleave')
    envi.save_image(out_file, datas, metadata=_metadata, interleave=_interleave, force=True)
    print('>>> {}'.format(out_file))
    return all_night


def fy3d2modis_met(l1_file, geo_file, out_file, metadata_pickle):
    """
    缺少5通道和32通道
    :param l1_file:
    :param geo_file:
    :param out_file:
    :param metadata_pickle:  hdr 头信息
    :return:
    """
    datas = np.zeros((200, 2), dtype=np.uint8)
    data_loader = ReadMersiL1(l1_file, geo_file=geo_file)

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

    datas = np.repeat(datas, 10, axis=0)
    print(datas.shape)

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
            print(i, np.nanmin(datas[:, :, i]), np.nanmax(datas[:, :, i]), np.nanmean(datas[:, :, i]))

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
            datas[:, :, i] = hdf.get('Cloud_Mask_QA')[i, :, :]

    with open(metadata_pickle, 'rb') as f:
        metadatas = pickle.load(f)
    metadata = metadatas.get('mod35qa')
    _metadata = metadata.get('metadata')
    _interleave = metadata.get('interleave')
    envi.save_image(out_file, datas, metadata=_metadata, interleave=_interleave, force=True)
    print('>>> {}'.format(out_file))
