# -*- coding: utf-8 -*-
"""
@Time    : 2018/6/29 14:33
@Author  : AnNing
"""
from pb_drc_AGRI import ReadAgriL1
from pb_drc_AIRS import ReadAirsL1
from pb_drc_CRIS import ReadCrisL1
from pb_drc_HIRAS import ReadHirasL1
from pb_drc_IASI import ReadIasiL1
from pb_drc_GOME import ReadGomeL1
from pb_drc_IRAS import ReadIrasL1
from pb_drc_MERSI import ReadMersiL1
from pb_drc_MODIS import ReadModisL1
from pb_drc_MVISR import ReadMvisrL1
from pb_drc_VIIRS import ReadViirsL1
from pb_drc_VIRR import ReadVirrL1
from pb_drc_VISSR import ReadVissrL1


def get_level1_read_class(sensor):
    """
    获取卫星对对应的提取类
    :param sensor:
    :return:
    """
    read_class = {
        'AGRI': ReadAgriL1,
        'CRIS': ReadCrisL1,
        'IASI': ReadIasiL1,
        'IRAS': ReadIrasL1,
        'MERSI': ReadMersiL1,
        'MODIS': ReadModisL1,
        'MVISR': ReadMvisrL1,
        'VIIRS': ReadViirsL1,
        'VIRR': ReadVirrL1,
        'VISSR': ReadVissrL1,
    }

    if sensor in read_class:
        return read_class[sensor]
    else:
        raise 'Dont have the sat+sensor data read class: {}'.format(sensor)


def get_level1_data(read_class, data_name):
    """
    通过数据名，获取对应的数据
    :param read_class:
    :param data_name:
    :return:
    """
    get_data_functions = {
        'DN': read_class.get_dn,
        'K0': read_class.get_k0,
        'K1': read_class.get_k1,
        'K2': read_class.get_k2,
        "REF": read_class.get_ref,
        'RAD': read_class.get_rad,
        'TBB': read_class.get_tbb,
        'SV': read_class.get_sv,
        'BB': read_class.get_bb,
        'Height': read_class.get_height,
        'Latitude': read_class.get_latitude,
        'Longitude': read_class.get_longitude,
        'LandSeaMask': read_class.get_land_sea_mask,
        'LandCover': read_class.get_land_cover,
        'SensorAzimuth': read_class.get_sensor_azimuth,
        'SensorZenith': read_class.get_sensor_zenith,
        'SolarAzimuth': read_class.get_solar_azimuth,
        'SolarZenith': read_class.get_solar_zenith,
        'RelativeAzimuth': read_class.get_relative_azimuth,
        'Timestamp': read_class.get_timestamp,
    }
    if data_name in get_data_functions:
        return get_data_functions[data_name]()
    else:
        raise 'Dont have the data_name in read class: {}'.format(data_name)
