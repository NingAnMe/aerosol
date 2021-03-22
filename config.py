#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2019/11/12 14:45
# @Author  : NingAnMe <ninganme@qq.com>
import os
DEBUG = True

SUCCESS = 0
WARNING = 1
ERROR = 2

LONGITUDE_RANGE_China = [70, 140]
LATITUDE_RANGE_China = [15, 55]

LONGITUDE_RANGE_ChangSanJiao = [114, 123]
LATITUDE_RANGE_ChangSanJiao = [27, 36]

LONGITUDE_RANGE_JingJinJi = [113, 120]
LATITUDE_RANGE_JingJinJi = [36, 43]

LONGITUDE_RANGE_ZhuSanJiao = [110, 116]
LATITUDE_RANGE_ZhuSanJiao = [20, 25]

LONGITUDE_RANGE_FenWei = [105, 115]
LATITUDE_RANGE_FenWei = [32, 40]

AREA_TYPE = {
    "China",  # 中国区
    "YRD",  # 长三角
    "PRD",  # 珠三角
    "FWP",  # 汾渭平原
    "BTH",  # 京津冀
}

PROJ_DATA = '/DISK/DATA02/PROJECT/SourceData/ShangHai/AOD'
# PROJ_DATA = '/home/aodo3/FY3D_AEROSOL_DATA'  # 数据文件根目录


# 结果
AOD_PICTURE_DIR = os.path.join(PROJ_DATA, 'PICTURE')  # 分布图文件夹
AOD_COMBINE_DIR = os.path.join(PROJ_DATA, 'COMBINE')  # 合成数据所在文件夹，日、月、季、年
AOD_MATCH_DIR = os.path.join(PROJ_DATA, 'MATCH')  # 匹配结果所在文件夹

# FY3D 原数据
AOD_FY3D_1KM_DIR = os.path.join(PROJ_DATA, '2FY3D_MERSI_L2_AOD-ORBT-1000M')  # FY3D数据的文件夹路径
GEO_FY3D_1KM_DIR = os.path.join(PROJ_DATA, '2FY3D-MERSI-L1-GEO1K')  # FY3D数据的文件夹路径
AOD_FY3D_5KM_DIR = os.path.join(PROJ_DATA, '1FY3D-MERSI-L2-AOD-DAILY-5000M')  # FY3D数据的文件夹路径

# FY4A 原数据
AOD_FY4A_4KM_DIR = os.path.join(PROJ_DATA, '4FY4A_AGRI_L2_AOD_4000M')  # FY4A数据的文件夹路径

# MODIS 原数据
if PROJ_DATA == '/DISK/DATA02/PROJECT/SourceData/ShangHai/AOD':
    AOD_MODIS_3KM_DIR = '/DISK/DATA02/KTS/SourceData/AQUA/MODIS/MYD04_3K/ORBIT'
    AOD_MODIS_10KM_DIR = '/DISK/DATA02/KTS/SourceData/AQUA/MODIS/MYD04_L2/ORBIT'
else:
    AOD_MODIS_3KM_DIR = os.path.join(PROJ_DATA, '3MYD04_L2_MODIS_AOD_3KM')  # MODIS数据的文件夹路径
    AOD_MODIS_10KM_DIR = os.path.join(PROJ_DATA, '3MYD04_L2_MODIS_AOD_10KM')  # MODIS数据的文件夹路径


def get_area_range(area_type):
    if area_type == 'China':
        return LONGITUDE_RANGE_China, LATITUDE_RANGE_China
    elif area_type == 'YRD':
        return LONGITUDE_RANGE_ChangSanJiao, LATITUDE_RANGE_ChangSanJiao
    elif area_type == "PRD":
        return LONGITUDE_RANGE_ZhuSanJiao, LATITUDE_RANGE_ZhuSanJiao
    elif area_type == "FWP":
        return LONGITUDE_RANGE_FenWei, LATITUDE_RANGE_FenWei
    elif area_type == "BTH":
        return LONGITUDE_RANGE_JingJinJi, LATITUDE_RANGE_JingJinJi
    else:
        raise ValueError(area_type)


def get_areas(area_type):
    if area_type == 'YRD':
        citys = ["江苏省", "安徽省", "浙江省", "上海市"]
    elif area_type == 'BTH':
        citys = ["北京市", "天津市", "河北省"]
    elif area_type == 'FWP':
        citys = ["陕西省", "山西省", "河南省"]
    elif area_type == 'FWP':
        citys = ["广东省"]
    elif area_type == 'China':
        citys = ["中国"]
    else:
        raise ValueError(area_type)
    return citys
