"""
1、FY3A没有云数据，不能出结果
# ######aerosol的程序中，是否需要将FY3D的Radiance转到MODIS的Radiance
"""
import os
import pickle
from shutil import copyfile

from spectral.io import envi
import numpy as np

from .load_mersi import ReadMersiL1
from .path import get_aid_path


def fy3abc2modis_1km(in_file, geo_file, out_file, metadata_pickle, vis_file, ir_file, coef_txt_flag):
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
        5: 'CH_02',
        6: 'CH_06',
        7: 'CH_07',
        8: 'CH_08',
        9: 'CH_09',
        10: 'CH_10',
        12: 'CH_12',
        13: 'CH_14',
        15: 'CH_15',
        16: 'CH_16',
        17: 'CH_17',
        18: 'CH_18',
        19: 'CH_19',
        20: 'CH_05',
        23: 'CH_05',
        26: 'CH_05',
        28: 'CH_05',
        29: 'CH_05',
        31: 'CH_05',
        32: 'CH_05',  # 没有32通道的对应通道，暂时使用CH_24
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
            _data[np.isnan(_data)] = -1
            datas[:, :, index - 1] = _data

    with open(metadata_pickle, 'rb') as f:
        metadatas = pickle.load(f)
    metadata = metadatas.get('1000m')
    _metadata = metadata.get('metadata')
    _interleave = metadata.get('interleave')
    envi.save_image(out_file, datas, metadata=_metadata, interleave=_interleave, force=True)
    print('>>> {}'.format(out_file))


def fy3abc2modis_geo(l1_file, geo_file, out_file, metadata_pickle):
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

    sz = data_loader.get_solar_zenith()
    sz = sz[np.isfinite(sz)]
    if (sz >= 75).all():
        all_night = True
        return all_night

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

    for channel in data_map:
        _data = data_map[channel]()
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


def fy3abc2modis_met(l1_file, geo_file, out_file, metadata_pickle):
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

    solar_zenith = data_loader.get_solar_zenith()
    sz_mean = np.nanmean(solar_zenith, axis=1)
    sz_mean.reshape(-1, 1)

    # MODIS和FY3的白天晚上好像是反的，在MODIS中，1是白天，0是晚上
    flag = np.zeros((200,), dtype=np.int8)
    for i in range(0, 200):
        sz_mean_ = np.nanmean(sz_mean[i*10:(i+1)*10])
        if sz_mean_ < 75:
            flag[i] = 1

    datas[:, 0] = flag

    # 因为FY3ABC没有mirror数据集，mirror全都置为1
    datas[:, 1] = 1

    with open(metadata_pickle, 'rb') as f:
        metadatas = pickle.load(f)
    metadata = metadatas.get('met')
    _metadata = metadata.get('metadata')
    _interleave = metadata.get('interleave')
    envi.save_image(out_file, datas, metadata=_metadata, interleave=_interleave, force=True)
    print('>>> {}'.format(out_file))


def fy3abc2modis_cloudmask(in_file, out_file, metadata_pickle):
    """
    缺少5通道和32通道
    :param in_file:
    :param out_file:
    :param metadata_pickle:  hdr 头信息
    :return:
    """
    from lib.fy3d2envi import fy3d2modis_cloudmask
    fy3d2modis_cloudmask(in_file, out_file, metadata_pickle)


def fy3abc2modis_cloudmask_qa(in_file, out_file, metadata_pickle):
    """
    缺少5通道和32通道
    :param in_file:
    :param out_file:
    :param metadata_pickle:  hdr 头信息
    :return:
    """
    if not os.path.isfile(out_file):
        aid_path = get_aid_path()
        hdr_file = os.path.join(aid_path, "fy3abcd.mod35qa.hdr")
        img_file = os.path.join(aid_path, "fy3abcd.mod35qa.img")
        copyfile(hdr_file, out_file)
        copyfile(img_file, out_file.replace('hdr', 'img'))
