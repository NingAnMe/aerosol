# coding:utf-8 
'''
Created on 2015年8月20日

@author: zhangtao
'''
import os
from pb_drc_base import classManager

PROJECT_ROOT = os.environ['PROJECT_ROOT']
PROJECT_NAME = os.environ['PROJECT_NAME']
pypath = os.path.join(PROJECT_ROOT, 'main', PROJECT_NAME, 'ext')
lst = [e[:-3] for e in os.listdir(pypath) if e[-3:] == '.py' and e.startswith('ext_sate')]
if len(lst) == 0:
    lst = [e[:-4] for e in os.listdir(pypath) if e[-4:] == '.pyc' and e.startswith('ext_sate')]
for each in lst:
    exec('from main.%s.ext.%s import *' % (PROJECT_NAME, each))

def getClassByFileName(fileName):
    """
    通过文件名找到对应的class
    """
    allcls = classManager()
    return allcls.getClass(fileName)

def getInstanceByFileName(fileName):
    """
    通过文件名找到对应的classs实例
    """
    allcls = classManager()
    return allcls.getInstance(fileName)
    
if __name__ == '__main__':
    pass
#     ll = ["FY3A_MERSI_GBAL_L1_20150820_0130_0250M_MS.HDF",
#           "FY3B_MERSI_00H0_L2_ASL_MLT_GLL_20150813_POAD_1000M_MS.HDF",
#             "FY3B_MERSI_10F0_L3_NVI_MLT_HAM_20150731_AOAM_0250M_MS.HDF",
#             "FY3B_MERSI_10H0_L2_ASL_MLT_GLL_20150813_POAD_1000M_MS.HDF",
#             "FY3B_MERSI_30C0_L3_NVI_MLT_HAM_20150810_AOTD_0250M_MS.HDF",
#             "FY3B_MERSI_40B0_L2_ASL_MLT_GLL_20150813_POAD_1000M_MS.HDF",
#             "FY3B_MERSI_GBAL_L1_20150813_0455_0250M_MS.HDF",
#             "FY3B_MERSI_GBAL_L1_20150813_0455_1000M_MS.HDF",
#             "FY3B_MERSI_GBAL_L1_20150813_0455_GEOXX_MS.HDF",
#             "FY3B_MERSI_GBAL_L3_ASL_MLT_GLL_20150731_AOAM_5000M_MS.HDF",
#             "FY3B_MERSI_GBAL_L3_ASL_MLT_GLL_20150810_AOTD_5000M_MS.HDF",
#             "FY3B_MULSS_10U0_L2_SNC_MLT_GLL_20150812_POAD_1000M_MS.HDF",
#             "FY3B_MWRIX_GBAL_L2_VSM_MLT_ESD_20150812_AOAD_025KM_MS.HDF",
#             "FY3B_TOUXX_GBAL_L1_20150813_0534_050KM_MS.HDF",
#             "FY3B_VIRRD_10T0_L2_LST_MLT_HAM_20150813_POAD_1000M_MS.HDF",
#             "FY3B_VIRRX_20Z0_L2_FOG_MLT_GLL_20150813_POAD_1000M_MS.HDF",
#             "FY3B_VIRRX_G080_L2_DST_MLT_GLL_20150813_POAD_1000M_MS.HDF",
#             "FY3B_VIRRX_GBAL_L1_20150812_0000_1000M_MS.HDF",
#             "FY3B_VIRRX_GBAL_L2_GFR_MLT_GLL_20150813_POAD_1000M_MS.HDF",
#             "FY3B_VIRRX_GBAL_L2_TPW_MLT_GLL_20150813_POAD_5000M_MS.HDF",
#             "MOD021KM.A2015225.1500.006.2015226020254.hdf", ]
    ll = ['OMI-Aura_L2-OMNO2_2016m0101t2224-o60983_v003-2016m0102t165503.he5']
    for each in ll:
        print each
        ii = getInstanceByFileName(each)
        print ii.ymd
        print ii.hms

