# coding:UTF-8
'''
Created on 2015年8月21日

@author: zhangtao
'''
import re, sys
from pb_drc_base import L2L3Class
import h5py
import inspect

def getL2L3List():
    return inspect.getmembers(sys.modules[__name__],
                   lambda member: inspect.isclass(member) and member.__module__ == __name__)
 
class FY3B_MULSS_L2_SNC_1KM_AD(L2L3Class):
    def __init__(self):
        '''
        积雪覆盖 日
        '''
        pat = u'(FY3[A-Z])_MULSS_(\w{4})_L2_SNC_MLT_GLL_(\d{8})_POAD_1000M_MS.HDF'
        BandNum = 1
        Row = 1000
        Col = 1000
        Res = 1000
        L2L3Class.__init__(self, pat, BandNum, Row, Col, Res)
        self.BandsName = ['SNC_DAILY']
        self.Sensor = 'MULSS'
        
class FY3B_MULSS_L3_SNC_1KM_AM(L2L3Class):
    def __init__(self):
        '''
        积雪覆盖 月
        '''
        pat = u'(FY3[A-Z])_MULSS_(\w{4})_L3_SNC_MLT_GLL_(\d{8})_POAM_1000M_MS.HDF'
        BandNum = 1
        Row = 1000
        Col = 1000
        Res = 1000
        L2L3Class.__init__(self, pat, BandNum, Row, Col, Res)
        self.BandsName = ['SNC_30DAY']
        self.Sensor = 'MULSS'
        
class FY3B_MWRIX_L2_VSM_25KM_AD(L2L3Class):
    def __init__(self):
        '''
        地表土壤水分 日
        '''
        pat = u'(FY3[A-Z])_MWRIX_(GBAL)_L2_VSM_MLT_ESD_(\d{8})_AOAD_025KM_MS.HDF'
        BandNum = 2
        Row = 586
        Col = 1383
        Res = 25000
        L2L3Class.__init__(self, pat, BandNum, Row, Col, Res)
        self.BandsName = ['Ascending Soil Moisture' ,
                                       'Descending Soil Moisture']
        self.Sensor = 'MWRI'
        
class FY3B_VIRRD_L2_LST_1KM_Day(L2L3Class):
    def __init__(self):
        '''
        陆表温度 白天
        '''
        pat = u'(FY3[A-Z])_VIRRD_(\w{4})_L2_LST_MLT_HAM_(\d{8})_POAD_1000M_MS.HDF'
        BandNum = 1
        Row = 1000
        Col = 1000
        Res = 1000
        L2L3Class.__init__(self, pat, BandNum, Row, Col, Res)
        self.BandsName = ['VIRR_1Km_LST']
        self.Sensor = 'VIRR'
        
class FY3B_VIRRN_L2_LST_1KM_Night(L2L3Class):
    def __init__(self):
        '''
        陆表温度 夜晚
        '''
        pat = u'(FY3[A-Z])_VIRRN_(\w{4})_L2_LST_MLT_HAM_(\d{8})_POAD_1000M_MS.HDF'
        BandNum = 1
        Row = 1000
        Col = 1000
        Res = 1000
        L2L3Class.__init__(self, pat, BandNum, Row, Col, Res)
        self.BandsName = ['VIRR_1Km_LST']
        self.Sensor = 'VIRR'
        
class FY3B_MERSI_L3_NVI_250M_TD(L2L3Class):
    def __init__(self):
        '''
        植被指数 旬
        '''
        pat = u'(FY3[A-Z])_MERSI_(\w{4})_L3_NVI_MLT_HAM_(\d{8})_AOTD_0250M_MS.HDF'
        BandNum = 1
        Row = 4000
        Col = 4000
        Res = 250
        L2L3Class.__init__(self, pat, BandNum, Row, Col, Res)
        self.BandsName = ['250M_10day_NDVI']
        self.SZA = '250M_10day_Solar_Zenith'
        self.Sensor = 'MERSI'

class FY3B_MERSI_L3_NVI_250M_AM(L2L3Class):
    def __init__(self):
        '''
        植被指数 月
        '''
        pat = u'(FY3[A-Z])_MERSI_(\w{4})_L3_NVI_MLT_HAM_(\d{8})_AOAM_0250M_MS.HDF'
        BandNum = 1
        Row = 4000
        Col = 4000
        Res = 250
        L2L3Class.__init__(self, pat, BandNum, Row, Col, Res)
        self.SZA = '250M_Monthly_Solar_Zenith'
        self.BandsName = ['250M_Monthly_NDVI']
        self.Sensor = 'MERSI'
        
class FY3B_VIRRX_L2_GFR_1KM_AD(L2L3Class):
    def __init__(self):
        '''
        火点与秸秆焚烧 日
        '''
        pat = u'(FY3[A-Z])_VIRRX_(GBAL)_L2_GFR_MLT_GLL_(\d{8})_POAD_1000M_MS.HDF'
        BandNum = 1
        Row = 35492  # ????
        Col = 9  # ????
        Res = 1000
        L2L3Class.__init__(self, pat, BandNum, Row, Col, Res)
        self.BandsName = ['FIRES']
        self.Sensor = 'VIRR'
        
class FY3B_VIRRX_L2_DST_1KM_AD(L2L3Class):
    def __init__(self):
        '''
        沙尘监测 日
        '''
        pat = u'(FY3[A-Z])_VIRRX_(\w{4})_L2_DST_MLT_GLL_(\d{8})_POAD_1000M_MS.HDF'
        BandNum = 1
        Row = 1000
        Col = 1000
        Res = 1000
        L2L3Class.__init__(self, pat, BandNum, Row, Col, Res)
        self.BandsName = ['Daily_VDS_DSI']
        self.Sensor = 'VIRR'
        
class FY3B_VIRRX_L2_FOG_1KM_AD(L2L3Class):
    def __init__(self):
        '''
        大雾监测 日
        '''
        pat = u'(FY3[A-Z])_VIRRX_(\w{4})_L2_FOG_MLT_GLL_(\d{8})_POAD_1000M_MS.HDF'
        BandNum = 1
        Row = 1000
        Col = 1000
        Res = 1000
        L2L3Class.__init__(self, pat, BandNum, Row, Col, Res)
        self.BandsName = ['FOGS']
        self.Sensor = 'VIRR'
        
class FY3B_MERSI_L2_ASL_1KM_AD(L2L3Class):
    def __init__(self):
        '''
        陆上气溶胶 日
        '''
        pat = u'(FY3[A-Z])_MERSI_(\w{4})_L2_ASL_MLT_GLL_(\d{8})_POAD_1000M_MS.HDF'
        BandNum = 4
        Row = 1000
        Col = 1000
        Res = 1000        
        L2L3Class.__init__(self, pat, BandNum, Row, Col, Res)
        self.BandsName = ['Aerosol_Optical_Thickness_of_MERSI_470nm',
                          'Aerosol_Optical_Thickness_of_MERSI_550nm',
                          'Aerosol_Optical_Thickness_of_MERSI_650nm',
                          'Aerosol_Small_Particle_Ratio']
        self.SZA = 'Pixel_Sun_Zenit_Angle'
        self.Sensor = 'MERSI'
        
class FY3B_MERSI_L3_ASL_5KM_TD(L2L3Class):
    def __init__(self):
        '''
        陆上气溶胶 旬
        '''
        pat = u'(FY3[A-Z])_MERSI_(GBAL)_L3_ASL_MLT_GLL_(\d{8})_AOTD_5000M_MS.HDF'
        BandNum = 4
        Row = 3600
        Col = 7200
        Res = 5000
        L2L3Class.__init__(self, pat, BandNum, Row, Col, Res)
        self.BandsName = ['Aerosol_Optical_Thickness_of_MERSI_470nm',
                          'Aerosol_Optical_Thickness_of_MERSI_550nm',
                          'Aerosol_Optical_Thickness_of_MERSI_650nm',
                          'Aerosol_Small_Particle_Ratio']
        self.Sensor = 'MERSI'
        
class FY3B_MERSI_L3_ASL_5KM_AM(L2L3Class):
    def __init__(self):
        '''
        陆上气溶胶 月
        '''
        pat = u'(FY3[A-Z])_MERSI_(GBAL)_L3_ASL_MLT_GLL_(\d{8})_AOAM_5000M_MS.HDF'
        BandNum = 4
        Row = 3600
        Col = 7200
        Res = 5000
        L2L3Class.__init__(self, pat, BandNum, Row, Col, Res)
        self.BandsName = ['Aerosol_Optical_Thickness_of_MERSI_470nm',
                          'Aerosol_Optical_Thickness_of_MERSI_550nm',
                          'Aerosol_Optical_Thickness_of_MERSI_650nm',
                          'Aerosol_Small_Particle_Ratio']
        self.Sensor = 'MERSI'
             
class FY3B_TOUXX_L1_50KM_AD(L2L3Class):
    def __init__(self):
        '''
        紫外臭氧总量 日
        '''
        pat = u'(FY3[A-Z])_TOUXX_(GBAL)_L1_(\d{8})_(\d{4})_050KM_MS.HDF'
        BandNum = 1
        Row = 726
        Col = 31
        Res = 50000
        L2L3Class.__init__(self, pat, BandNum, Row, Col, Res)
        self.BandsName = ['Atm_radiance']
        self.SZA = 'Solar_zenith_angle'
        
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.Region = g.group(2)
            self.ymd = g.group(3)
            self.hms = g.group(4) + '00'
            return True
        else:
            return False

    def open(self, Hdf5_file):
        '''
        Hdf5_file:   HDF5文件
        '''
        self.h5file = h5py.File(Hdf5_file, 'r')
        dset = self.h5file['Atm_radiance']
        self.bands = []
        for i in range(6):
            self.bands.append(dset[:, :, i])


class FY3B_VIRRX_L2_TPW_5KM_AD(L2L3Class):
    def __init__(self):
        '''
       晴空大气可降水 日
        '''
        pat = u'(FY3[A-Z])_VIRRX_(GBAL)_L2_TPW_MLT_GLL_(\d{8})_POAD_5000M_MS.HDF'
        BandNum = 2
        Row = 3600
        Col = 7200
        Res = 5000
        L2L3Class.__init__(self, pat, BandNum, Row, Col, Res)
        self.BandsName = ['VIRR_DAY_TPWSDS',
                          'VIRR_NIGHT_TPWSDS']
        self.Sensor = 'VIRR'
                       
if __name__ == '__main__':
    pass
