# coding:utf-8
'''
Created on 2015年8月13日

@author: zhangtao
'''
from abc import ABCMeta, abstractmethod
from datetime import datetime, timedelta
import re, sys
from PB import pb_time


class nameClassManager(object):
    '''
    插件类的装饰器
    '''
    PLUGINS = []

    def getClass(self, text):
        for plugin in self.PLUGINS:
            if plugin().check(text):
                return plugin
        return None

    def getInstance(self, text):
        for plugin in self.PLUGINS:
            inst = plugin()
            if inst.check(text):
                return inst
        return None

    @classmethod
    def plugin(cls, plugin):
        cls.PLUGINS.append(plugin)


class satNameBase(object):  # metaclass=ABCMeta
    ''' hdf base class '''
    __metaclass__ = ABCMeta

    def __init__(self, pat, totalSec):

        self.pat = pat
        self.totalSec = totalSec
        self.ymd = None
        self.hms = None

    @abstractmethod
    def check(self, fileName):
        pass

    @property
    def dt_s(self):
        if self.ymd is None or self.hms is None:
            return None
        return datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")

    @property
    def dt_e(self):
        if self.ymd is None or self.hms is None:
            return None
        return datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S") + timedelta(seconds = (self.totalSec))


@nameClassManager.plugin
class AURA_OMI_L2(satNameBase):
    '''
    AURA OMI L2 NO2 SO2
    OMI-Aura_L2-OMNO2_2016m0101t0059-o60970_v003-2016m0101t202533.he5
    '''

    def __init__(self):
        pat = u'OMI-Aura_L2-\w{5}_(\d{4})m(\d{4})t(\d{4})-o(\d{5})_v003-\w{16}.he5$'
        totalSec = 99 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = '%s%s' % (g.group(1), g.group(2))
            self.hms = g.group(3) + '00'
            return True
        else:
            return False


@nameClassManager.plugin
class AURA_OMI_L3(satNameBase):
    '''
    AURA OMI L3 NO2 SO2 O3 AERUV, AERO
    OMI-Aura_L3-OMAERUVd_2016m0101_v003-2016m0108t111511.he5
    OMI-Aura_L3-OMAEROe_2016m0101_v003-2016m0103t021922.he5
    OMI-Aura_L3-OMSO2e_2016m0101_v003-2016m0108t112606.he5
    '''

    def __init__(self):
        pat = u'OMI-Aura_L3-\w+_(\d{4})m(\d{4})_v003-\w{16}.he5$'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = '%s%s' % (g.group(1), g.group(2))
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class MODDIS_L3(satNameBase):
    '''
    MOD08_M3.A2016001.006.2016034014511.hdf
    MOD08_D3.A2016001.006.2016008061022.hdf
    '''

    def __init__(self):
        pat = u'\w+.A(\d{4})(\d{3}).\d{3}.\d+.hdf$'
        totalSec = 5 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            tt = pb_time.JDay2Datetime(g.group(1), g.group(2), '000000')
            self.ymd = tt.strftime('%Y%m%d')
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class MOPITT_L3_D(satNameBase):
    '''
    MOP03J-20000329-L3V3.1.3.hdf
    '''

    def __init__(self):
        pat = u'MOP03J-(\d{8})-L3V[\.\w]+.hdf$'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class MOPITT_L3_M(satNameBase):
    '''
    MOP03JM-200006-L3V93.1.3.hdf
    MOP03JM-201704-L3V94.6.3.he5
    '''

    def __init__(self):
        #         pat = u'MOP03JM-(\d{6})-L3V93.1.3.hdf$'
        pat = u'MOP03JM-(\d{6})-L3V[\.\w]+.he5$'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1) + '01'
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class METOP_L2(satNameBase):
    '''
    no2track20160301.hdf
    '''

    def __init__(self):
        pat = u'(\w{3})track(\d{8}).hdf$'
        totalSec = 100 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(2)
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class FY1CD_L1(satNameBase):
    '''
    FY1[A-Z]_L1_GDPT_(\d{8})_(\d{4}).HDF\Z
    FY1[A-Z]_L1_GDPT_(\d{8})_(\d{4}).OBC.HDF\Z
    '''

    def __init__(self):
        pat = u'FY1[A-Z]_L1_GDPT_(\d{8})_(\d{4}).*.HDF$'
        totalSec = 100 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2) + '00'
            return True
        else:
            return False


@nameClassManager.plugin
class FY1CD_L1_HRPT(satNameBase):
    '''
    FY1[A-Z]_L1_(\d{8})_(\d{4}).HDF\Z
    FY1[A-Z]_L1_(\d{8})_(\d{4}).OBC.HDF\Z
    '''

    def __init__(self):
        pat = u'FY1[A-Z]_L1_(\d{8})_(\d{4}).*.HDF$'
        totalSec = 100 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2) + '00'
            return True
        else:
            return False


@nameClassManager.plugin
class METOP_L1(satNameBase):
    '''
    IASI_xxx_1C_M02_20160301052958Z_20160301053254Z_N_O_20160301070539Z__20160301070731
    GOME_xxx_1B_M01_20160403080255Z_20160403080555Z_N_O_20160403083330Z__20160403083454
    '''

    def __init__(self):
        pat = u'\w+_(\d{14})Z_(\d{14})Z_N_O_\d{14}Z__\d{14}$'
        totalSec = 3 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)[:8]
            self.hms = g.group(1)[8:]
            return True
        else:
            return False


@nameClassManager.plugin
class NPP_L1(satNameBase):
    '''
    GCRSO-SCRIS_npp_d20160408_t1038089_e1046067_b23041_c20160408205450739096_noaa_ops.h5
    GATMO-SATMS_npp_d20160407_t2102116_e2110113_b23033_c20160408045451138463_noaa_ops.h5
    GCRSO-SCRIS_npp_d20170728_t0501519_e0509497_b29791_c20170729031753478626_noac_ops.h5
    GCRSO-SCRIF-SCRIS_npp_d20180303_t0016319_e0024297_b32881_c20180308030857410779_noac_ops.h5
    '''

    def __init__(self):
        # - 中划线不匹配 不知道为什么
        pat = u'\w{5}-.*_npp_d(\d{8})_t(\d{7})_e(\d{7})_b(\d{5})_c(\d{20})_\w{4}_ops.h5$'
        totalSec = 8 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2)[:-1]  # 舍弃秒的小数点以后位
            return True
        else:
            return False


@nameClassManager.plugin
class GCOM_L1(satNameBase):
    '''
    GW1AM2_201602251839_130D_L1SGBTBR_2210210.h5
    '''

    def __init__(self):
        pat = u'GW1AM2_(\d{12})_\w{4}_L1SGBTBR_\d{7}.(h5$|h5.gz$)'
        totalSec = 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)[:8]
            self.hms = g.group(1)[8:] + '00'
            return True
        else:
            return False


@nameClassManager.plugin
class CALIPSO_L2(satNameBase):
    '''
    CAL_LID_L2_VFM-ValStage1-V3-30.2016-01-01T00-29-33ZN.hdf
    '''

    def __init__(self):
        pat = u'CAL_LID_L2.*(\d{4})-(\d{2})-(\d{2})T(\d{2})-(\d{2})-(\d{2})\w+.hdf$'
        totalSec = 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1) + g.group(2) + g.group(3)
            self.hms = g.group(4) + g.group(5) + g.group(6)
            return True
        else:
            return False

# @nameClassManager.plugin
# class CALIPSO_L2_NEW(satNameBase):
#     '''
#     CAL_LID_L2_05kmALay-Standard-V4-10.2017-01-01T01-27-54ZD.hdf
#     '''
#
#     def __init__(self):
#         pat = u'CAL_LID_L2_05kmALay-Standard-V4-10.(\d{4})-(\d{2})-(\d{2})T(\d{2})-(\d{2})-(\d{2})\w+.hdf$'
#         totalSec = 60 * 60
#         satNameBase.__init__(self, pat, totalSec)
#
#     def check(self, infile):
#         g = re.match(self.pat, infile)
#         if g:
#             self.ymd = g.group(1) + g.group(2) + g.group(3)
#             self.hms = g.group(4) + g.group(5) + g.group(6)
#             return True
#         else:
#             return False


@nameClassManager.plugin
class NPP_VIIRS_L2(satNameBase):
    '''
    NPP_VGAERO_L2.A2016001.0000.P1_03001.2016086032110.hdf
    NPP_VAOT_L2.A2016001.0000.P1_03001.2016086032110.hdf
    '''

    def __init__(self):
        pat = u'NPP_\w+_L2.A(\d{4})(\d{3}).(\d{4}).P1_\d{5}.\d{13}.hdf$'
        totalSec = 5 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            tt = pb_time.JDay2Datetime(
                g.group(1), g.group(2), g.group(3) + '00')
            self.ymd = tt.strftime('%Y%m%d')
            self.hms = g.group(3) + '00'
            return True
        else:
            return False


@nameClassManager.plugin
class IMS_NORTH(satNameBase):
    '''
    ims2016001_24km_v1.3.asc.gz
    '''

    def __init__(self):
        pat = u'ims(\d{4})(\d{3})_\w+_[\.\w]+.asc.gz$'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            tt = pb_time.JDay2Datetime(g.group(1), g.group(2), '000000')
            self.ymd = tt.strftime('%Y%m%d')
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class SIC_NRT_GSFC_D(satNameBase):
    '''
    nt_20160101_f17_nrt_n.bin
    nt_19810101_n07_v1.1_n.bin
    '''

    def __init__(self):
        pat = u'nt_(\d{8})_\w+_[\.\w]+.(bin|png)$'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class SIC_NRT_GSFC_M(satNameBase):
    '''
    nt_198511_n07_v1.1_n.png
    '''

    def __init__(self):
        pat = u'nt_(\d{6})_\w+_[\.\w]+.(bin|png)$'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1) + '01'
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class M10A1(satNameBase):
    '''
    MOD10A1.A2016021.h12v02.005.2016023070638.hdf
    MYD10A1.A2016021.h12v02.005.2016023070638.hdf
    author: huangyejian
    modify: 2016-06-13
    '''

    def __init__(self):
        pat = u'(MOD|MYD)10A1.A(\d{4})(\d{3}).h\d{2}v\d{2}.005.\d{13}.hdf$'
        # pat = u'(MOD13A1).A(\d{4})(\d{3}).h27v05.006.\d{13}.hdf$'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            tt = pb_time.JDay2Datetime(g.group(2), g.group(3), '000000')
            self.ymd = tt.strftime('%Y%m%d')
            self.hms = '000000'
#             print g.group(2), g.group(3)
            return True
        else:
            return False


@nameClassManager.plugin
class MOY08(satNameBase):
    '''
    MYD08_D3.A2014001.051.2014002194328.hdf
    MOD08_D3.A2014001.051.2014002085829.hdf
    author: huangyejian
    modify: 2016-07-23
    '''

    def __init__(self):
        pat = u'(MOD|MYD)08_D3.A(\d{4})(\d{3}).(051|006).\d{13}.hdf$'
        # pat = u'(MOD13A1).A(\d{4})(\d{3}).h27v05.006.\d{13}.hdf$'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            tt = pb_time.JDay2Datetime(g.group(2), g.group(3), '000000')
            self.ymd = tt.strftime('%Y%m%d')
            self.hms = '000000'
#             print g.group(2), g.group(3)
            return True
        else:
            return False


@nameClassManager.plugin
class MCD43C1(satNameBase):
    '''
    MCD43C1.A2014001.005.2014023210532.hdf
    author: huangyejian
    modify: 2016-07-23
    '''

    def __init__(self):
        pat = u'MCD43C1.A(\d{4})(\d{3}).005.\d{13}.hdf$'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            tt = pb_time.JDay2Datetime(g.group(1), g.group(2), '000000')
            self.ymd = tt.strftime('%Y%m%d')
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class vwnd(satNameBase):
    '''
    vwnd.sig995.2009.nc
    author: huangyejian
    modify: 2016-09-01
    '''

    def __init__(self):
        pat = u'vwnd.sig995.(\d{4}).nc'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1) + '0101'
            self.hms = '000000'

            return True
        else:
            return False


@nameClassManager.plugin
class uwnd(satNameBase):
    '''
    uwnd.sig995.2011.nc
    author: huangyejian
    modify: 2016-09-01
    '''

    def __init__(self):
        # pat = u'MCD43C1.A(\d{4})(\d{3}).005.\d{13}.hdf$'
        pat = u'uwnd.sig995.(\d{4}).nc$'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1) + '0101'
            self.hms = '000000'

            return True
        else:
            return False


@nameClassManager.plugin
class pr_wtr(satNameBase):
    '''
    pr_wtr.eatm.2014.nc
    author: huangyejian
    modify: 2016-09-01
    '''

    def __init__(self):
        # pat = u'MCD43C1.A(\d{4})(\d{3}).005.\d{13}.hdf$'
        pat = u'pr_wtr.eatm.(\d{4}).nc$'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            #             tt = pb_time.JDay2Datetime(g.group(1), g.group(2), '000000')
            self.ymd = g.group(1) + '0101'
            self.hms = '000000'

            return True
        else:
            return False


@nameClassManager.plugin
class NECP(satNameBase):
    '''
    NISE_SSMIF13_20000202.HDFEOS
    author: huangyejian
    modify: 2016-07-30
    '''

    def __init__(self):
        pat = u'NISE_SSMIF13_(\d{8}).HDFEOS$'
        # pat = u'(MOD13A1).A(\d{4})(\d{3}).h27v05.006.\d{13}.hdf$'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            #             tt = pb_time.JDay2Datetime(g.group(1) , '000000')
            #             self.ymd = tt.strftime('%Y%m%d')
            self.ymd = g.group(1)
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class oco2(satNameBase):
    '''
    oco2_L2StdGL_12170a_161014_B7302_161016114134.h5
    author: huangyejian
    modify: 2016-10-09
    '''

    def __init__(self):
        pat = u'oco2_\w{7}_\w{6}_(\d{6})_\w{5}_\d{12}.h5\Z'
        # pat = u'(MOD13A1).A(\d{4})(\d{3}).h27v05.006.\d{13}.hdf$'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            #             tt = pb_time.JDay2Datetime(g.group(1) , '000000')
            #             self.ymd = tt.strftime('%Y%m%d')
            self.ymd = "20" + g.group(1)
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class oco2_R(satNameBase):
    '''
    oco2_L2StdGL_12170a_161014_B7302_161016114134.h5
    author: huangyejian
    modify: 2016-10-09
    '''

    def __init__(self):
        pat = u'oco2_\w{7}_\w{6}_(\d{6})_\w{6}_\d{12}.h5\Z'
        # pat = u'(MOD13A1).A(\d{4})(\d{3}).h27v05.006.\d{13}.hdf$'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            #             tt = pb_time.JDay2Datetime(g.group(1) , '000000')
            #             self.ymd = tt.strftime('%Y%m%d')
            self.ymd = "20" + g.group(1)
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class Tansat(satNameBase):
    '''
    20091001.H5
    '''

    def __init__(self):
        pat = u'(\d{8}).H5\Z'
        # pat = u'(MOD13A1).A(\d{4})(\d{3}).h27v05.006.\d{13}.hdf$'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            #             tt = pb_time.JDay2Datetime(g.group(1) , '000000')
            #             self.ymd = tt.strftime('%Y%m%d')
            self.ymd = g.group(1)
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class FY2_DDC(satNameBase):
    '''
    FY2E_FDI_ALL_NOM_20160901_DCC.hdf
    '''

    def __init__(self):
        pat = u'FY2[A-Z]_FDI_ALL_NOM_(\d{8})_\w{3}.hdf\Z'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class FY2E_AMV(satNameBase):
    '''
    FY2E_AMV_IR1_OTG_20161101_1500.AWX
    '''

    def __init__(self):
        pat = u'FY2[A-Z]_AMV_\w{3}_OTG_(\d{8})_(\d{4}).AWX\Z'
        totalSec = 6 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2) + "00"
            return True
        else:
            return False


@nameClassManager.plugin
class HY2A_SM2B(satNameBase):
    '''
    H2A_SM2B20160531_00950.h5
    '''

    def __init__(self):
        pat = u'H2A_SM2B(\d{8})_(\d{5}).h5\Z'
        totalSec = 24 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = "00000"
            return True
        else:
            return False


@nameClassManager.plugin
class FY3C_xishuang(satNameBase):
    '''
    FY3C_MWHSX_ORBT_L2_AHP_MLT_NUL_20161107_2340_015KM_MS.L1c
    FY3C_IRASX_ORBT_L2_AIP_MLT_NUL_20161107_2158_017KM_MS.L1c
    FY3C_MWRIA_ORBT_L2_TPW_MLT_NUL_20161108_0150_025KM_MS.HDF
    FY3C_MWRID_ORBT_L2_TPW_MLT_NUL_20161108_0100_025KM_MS.HDF
    '''

    def __init__(self):
        pat = u'FY3C_\w{5}_ORBT_L2_\w{3}_MLT_NUL_(\d{8})_(\d{4})_\w{5}_MS.(L1c|HDF)\Z'
        totalSec = 60 * 100
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2)
            return True
        else:
            return False


@nameClassManager.plugin
class FY3C_SIC(satNameBase):
    '''
    FY3C_VIRRX_GBAL_L2_SIC_MLT_GLL_20161106_POAD_1000M_MS.HDF
    '''

    def __init__(self):
        pat = u'FY3C_\w{5}_GBAL_L2_\w{3}_MLT_GLL_(\d{8})_POAD_\w{5}_MS.(L1c|HDF)\Z'
        totalSec = 60 * 60 * 24
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = "000000"
            return True
        else:
            return False


@nameClassManager.plugin
class airquality(satNameBase):
    '''
    airquality20161119130000.000
    airquality20161119150000.txt
    '''

    def __init__(self):
        pat = u'airquality(\d{8})(\d{6}).(000|txt)\Z'
        totalSec = 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2)
            return True
        else:
            return False


@nameClassManager.plugin
class Ozone(satNameBase):
    '''
    L3_ozone_omi_20151115.txt
    L3_aersl_omi_20161115.txt
    '''

    def __init__(self):
        pat = u'L3_\w{5}_omi_(\d{8}).txt\Z'
        totalSec = 60 * 60 * 24
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = "000000"
            return True
        else:
            return False


@nameClassManager.plugin
class VIIRSEDR(satNameBase):
    '''
    VIIRS-Cloud-Top-Temperature-EDR
    VIIRS-Cloud-Top-Pressure-EDR

    '''

    def __init__(self):
        totalSec = 60 * 60 * 24
#         pat = u'VIIRS-EDR_VIIRS-Cloud-\w{4}-\w{6}-\w{3}_(\d{8})_\d{5}.tar\Z'
        pat = u'VIIRS-EDR_VIIRS-Cloud-\D*_(\d{8})_\d{5}.tar\Z'
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = "000000"
            return True
        else:
            return False

# @nameClassManager.plugin
# class gdas1(satNameBase):
#     '''
#     gdas1.sep11.w4
#
#     '''
#     def __init__(self):
#         totalSec = 60 * 60 * 24 * 6
#         pat = u'gdas1.(\w{3})(\d{2}).w(\d{1})\Z'
#         satNameBase.__init__(self, pat, totalSec)
#
#     def check(self, infile):
#         g = re.match(self.pat, infile)
#         if g:
#             Mon = pb_time.Shorthand2Mon(g.group(1))
#
#             if g.group(3) == '1':
#                 self.ymd = "20" + g.group(2) + Mon + "01"
#             else:
#                 self.ymd = "20" + g.group(2) + Mon + str(int(g.group(3)) * 7 - 6)
#             print self.ymd
#             self.hms = "000000"
#             return True
#         else :
#             return False


@nameClassManager.plugin
class gdas0p5(satNameBase):
    '''
    20080515_gdas0p5

    '''

    def __init__(self):
        totalSec = 60 * 60 * 24
        pat = u'(\d{8})_gdas0p5\Z'
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = "000000"
            return True
        else:
            return False


@nameClassManager.plugin
class GOME_AAI(satNameBase):
    '''
    envisat project used
    S-O3M_GOME_NAR_02_M02_20171017052357Z_20171017052657Z_N_O_20171017070631Z.hdf5.gz

    '''

    def __init__(self):
        totalSec = 60 * 3
#         u'S-O3M_GOME_NAR_02_\w{3}_(\d{8})(\d{6})Z_\d{14}Z_N_O_\d{14}Z.hdf5.gz\Z'
        pat = u'S-O3M_GOME_NAR_02_M02_(\d{8})(\d{6})Z_\d{14}Z_N_O_\d{14}Z.hdf5.gz\Z'

        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2)
            return True
        else:
            return False


@nameClassManager.plugin
class GOME_NO2_SO2(satNameBase):
    '''
    envisat project used
    S-O3M_GOME_O3-NO2-NO2Tropo-HCHO_L2_20171017050855_001_METOPB_26361_DLR_04.HDF5
    S-O3M_GOME_O3-NO2-NO2Tropo-SO2-HCHO_L2_20170917055654_003_METOPA_56622_DLR_04.HDF5

    '''

    def __init__(self):
        totalSec = 60 * 60 * 1.4
#         u'S-O3M_GOME_NAR_02_\w{3}_(\d{8})(\d{6})Z_\d{14}Z_N_O_\d{14}Z.hdf5.gz\Z'
        pat = u'S-O3M_GOME_O3-NO2-NO2Tropo-.*_(\d{8})(\d{6})\w+.HDF5\Z'

        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2)
            return True
        else:
            return False


@nameClassManager.plugin
class Gomeso2(satNameBase):
    '''
    GOME_O3-NO2-NO2Tropo-BrO-SO2-H2O-HCHO_L2_20170418033736_051_METOPB_23775_DLR_04.HDF5
    GOME_O3-NO2-NO2Tropo-BrO-SO2-H2O-HCHO_L2_20170418060544_050_METOPA_54463_DLR_04.HDF5
    GOME_O3-NO2-NO2Tropo-BrO-SO2-H2O-HCHO-OClO_L2_20161116232745_054_METOPB_21613_DLR_04.HDF5

    '''

    def __init__(self):
        totalSec = 60 * 60 * 2
        pat = u'GOME_O3-NO2-NO2Tropo-BrO-SO2-H2O-HCHO.*_L2_(\d{8})(\d{6})_\d{3}_METOP(\w{1})_\d{5}_DLR_04.HDF5\Z'
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2)
            return True
        else:
            return False


@nameClassManager.plugin
class HIMAWARI_08(satNameBase):
    '''
    AHI8_OBI_2000M_NOM_20171109_0000.hdf
    '''

    def __init__(self):
        totalSec = 60 * 10
        pat = u'AHI8_OBI_2000M_NOM_(\d{8})_(\d{4}).hdf\Z'
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2) + '00'

            return True
        else:
            return False

# tmp


@nameClassManager.plugin
class POLDER3_L1B(satNameBase):
    '''
    POLDER3_L1B-BG1-120055M_2010-02-24T16-22-17_V1-01.h5
    '''

    def __init__(self):
        pat = u'POLDER3_L1B-\w{3}-\w{7}_(\d{4})-(\d{2})-(\d{2})T(\d{2})-(\d{2})-(\d{2})_V1-01.h5$'
#         pat = u'POLDER3_L1B-\w{3}-\w{7}.*.h5$'
        totalSec = 100 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1) + g.group(2) + g.group(3)
            self.hms = g.group(4) + g.group(5) + g.group(6)
            return True
        else:
            return False


# gsics gsics gsics gsics ##############################################
@nameClassManager.plugin
class MODIS_SST(satNameBase):
    """
    20190219012000-STAR-L2P_GHRSST-SSTsubskin-MODIS_A-ACSPO_V2.61B06-v02.0-fv01.0.nc
    """

    def __init__(self):
        pat = u'(\d{8})(\d{6})-STAR-L2P_GHRSST-SSTsubskin-MODIS_A-ACSPO.*.nc$'
        totalSec = 10 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2)
            return True
        else:
            return False


@nameClassManager.plugin
class VIIRS_SST(satNameBase):
    """
    20190615000000-OSPO-L2P_GHRSST-SSTsubskin-VIIRS_NPP-ACSPO_V2.61-v02.0-fv01.0.nc
    """

    def __init__(self):
        pat = u'(\d{8})(\d{6})-OSPO-L2P_GHRSST-SSTsubskin-VIIRS_NPP-ACSPO.*.nc$'
        totalSec = 10 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2)
            return True
        else:
            return False


@nameClassManager.plugin
class VIIRS_OCEAN_COLOR(satNameBase):
    """
    VRSVCW_B2018187_B2018187_F1_WW00_edgemask_chlora.nc
    """

    def __init__(self):
        pat = u'VRSVCW_B(\d{4})(\d{3})_B(\d{4})(\d{3})_F1_WW00_edgemask_chlora.nc$'
        totalSec = 23.99999 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            year = g.group(1)
            jjj = g.group(2)
            ymd = datetime.strptime('%s%s' % (year, jjj), '%Y%j')
            self.ymd = ymd.strftime('%Y%m%d')
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class VIIRS_VBD(satNameBase):
    """
    VBD_npp_d20190519_global-saa_noaa_ops_v23.csv
    """

    def __init__(self):
        pat = u'VBD_npp_d(\d{8})_global-saa_noaa_ops_v23.(kmz|csv|csv.gz)$'
        totalSec = 23.99999 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class MODIS_L2L3(satNameBase):

    def __init__(self):
        pat = u'M\w{6}.A(\d{4})(\d{3}).h\d{2}v\d{2}.006.\d{13}.hdf$'
        totalSec = 23.99999 * 60 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            year = g.group(1)
            jjj = g.group(2)
            ymd = datetime.strptime('%s%s' % (year, jjj), '%Y%j')
            self.ymd = ymd.strftime('%Y%m%d')
            self.hms = '000000'
            return True
        else:
            return False


@nameClassManager.plugin
class HIMAWARI8_AHI_L0(satNameBase):
    '''
    HS_H08_20190509_0000_B01_FLDK_R10_S0110.DAT.bz2
    '''

    def __init__(self):
        pat = u'HS_H08_(\d{8})_(\d{4})_\w{3}_FLDK_\w{3}_\w{5}.DAT.bz2$'
        totalSec = 10 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2) + '00'
            return True
        else:
            return False


@nameClassManager.plugin
class FY1_MVISR_L1(satNameBase):
    '''
    FY1D_L1_GDPT_20031231_1347.HDF
    '''

    def __init__(self):
        pat = u'\w{4}_L1_GDPT_(\d{8})_(\d{4}).HDF$'
        totalSec = 90 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2) + '00'
            return True
        else:
            return False


@nameClassManager.plugin
class FY2_VISSR_L1(satNameBase):
    '''
    FY2C_FDI_ALL_NOM_20160131_0900.hdf
    FY2D_FDI_ALL_NOM_20160131_0900.hdf
    FY2E_FDI_ALL_NOM_20160131_0900.hdf
    FY2F_FDI_ALL_NOM_20160131_0900.hdf
    FY2G_FDI_ALL_NOM_20160131_0900.hdf
    '''

    def __init__(self):
        pat = u'\w{4}_FDI_ALL_NOM_(\d{8})_(\d{4}).hdf$'
        totalSec = 30 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2) + '00'
            return True
        else:
            return False


@nameClassManager.plugin
class FY3_MERSI_VIRR_HIRAS_L1(satNameBase):
    '''
    FY3C_VIRRX_GBAL_L1_20160131_1855_1000M_MS.HDF
    FY3C_MERSI_GBAL_L1_20150530_1015_1000M_MS.HDF
    FY3C_MERSI_GBAL_L1_20150530_1015_GEO1K_MS.HDF
    FY3D_HIRAS_GBAL_L1_20180515_1045_016KM_MS.HDF
    '''

    def __init__(self):
        pat = u'\w{4}_(MERSI|VIRRX|HIRAS)_\w{4}_L1_(\d{8})_(\d{4})_\w{5}_MS.HDF$'
        totalSec = 5 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3) + '00'
            return True
        else:
            return False


@nameClassManager.plugin
class FY3_IRAS_L1(satNameBase):
    '''
    FY3C_IRASX_GBAL_L1_20160129_1215_017KM_MS.HDF
    '''

    def __init__(self):
        pat = u'\w{4}_IRASX_\w{4}_L1_(\d{8})_(\d{4})_\w{5}_MS.HDF$'
        totalSec = 100 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2) + '00'
            return True
        else:
            return False


@nameClassManager.plugin
class FY4_AGRI_L1(satNameBase):
    '''
    FY4A-_AGRI--_N_DISK_0995E_L1-_FDI-_MULT_NOM_20170507031500_20170507032959_4000M_V0001.HDF
    '''

    def __init__(self):
        totalSec = 60 * 15
        pat = u'FY4A-_AGRI--_N_DISK_\d{4}E_L1-_\w{3}-_MULT_NOM_(\d{8})(\d{6})_(\d{14})_4000M_V0001.HDF\Z'
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2)
            return True
        else:
            return False


@nameClassManager.plugin
class METOP_IASI_GOME_L1_NC(satNameBase):
    '''
    W_XX-EUMETSAT-Darmstadt,HYPERSPECT+SOUNDING,MetOpA+IASI_C_EUMP_20181127014334_62814_eps_o_l1.nc
    '''

    def __init__(self):
        pat = u'.*_(\d{14})_(\d{5})_.*.nc$'
        totalSec = 25 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)[:8]
            self.hms = g.group(1)[8:]
            return True
        else:
            return False


@nameClassManager.plugin
class METOP_IASI_GOME_L1(satNameBase):
    '''
    IASI_xxx_1C_M02_20160301052958Z_20160301053254Z_N_O_20160301070539Z__20160301070731
    GOME_xxx_1B_M01_20160403080255Z_20160403080555Z_N_O_20160403083330Z__20160403083454
    '''

    def __init__(self):
        pat = u'\w+_(\d{14})Z_(\d{14})Z_N_O_\d{14}Z__\d{14}$'
        totalSec = 3 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)[:8]
            self.hms = g.group(1)[8:]
            return True
        else:
            return False


@nameClassManager.plugin
class NPP_CRIS_L1(satNameBase):
    '''
    GCRSO-SCRIS_npp_d20160408_t1038089_e1046067_b23041_c20160408205450739096_noaa_ops.h5
    GCRSO-SCRIS_npp_d20170728_t0501519_e0509497_b29791_c20170729031753478626_noac_ops.h5
    GCRSO-SCRIF-SCRIS_npp_d20180303_t0016319_e0024297_b32881_c20180308030857410779_noac_ops.h5
    GCRSO-SCRIF-SCRIS_j01_d20190413_t0721359_e0729337_b07249_c20190414010252589247_noac_ops.h5
    '''

    def __init__(self):
        # - 中划线不匹配 不知道为什么
        pat = u'\w{5}-.*_\w{3}_d(\d{8})_t(\d{7})_e(\d{7})_b(\d{5})_c(\d{20})_\w{4}_ops.h5$'
        totalSec = 8 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(1)
            self.hms = g.group(2)[:-1]  # 舍弃秒的小数点以后位
            return True
        else:
            return False


@nameClassManager.plugin
class NPP_VIIRS_L1(satNameBase):
    '''
    GDNBO_npp_d20160212_t0700199_e0706003_b22245_c20160212130601292266_noaa_ops.h5
    GMODO-SVM01-SVM02-SVM03-SVM04-SVM05-SVM06-SVM07-SVM08-SVM09-SVM10-SVM11-SVM12-SVM13-SVM14-SVM15-SVM16_npp_d20141115_t0001028_e0006432_b15799_c20170810221740745607_noaa_ops.h5
    '''

    def __init__(self):
        pat = u'(GDNBO|GMODO|GMTCO|SVM\d{2}).*_npp_d(\d{8})_t(\d{7})_e(\d{7})_b\d{5}_c\d{20}_\w{4}_ops.h5$'
        totalSec = 6 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3)[:-1]  # 舍弃秒的小数点以后位
            return True
        else:
            return False


@nameClassManager.plugin
class AQUA_MODIS_L1L2(satNameBase):
    '''
    MYD021KM.A2014031.0555.006.2014032173249.hdf
    MYD03.A2015335.0410.006.2015335161644.hdf
    MOD04_L2.A2016059.0040.006.2016099175354.hdf
    MOD04_3K.A2016007.0000.006.2016008152828.hdf
    '''

    def __init__(self):
        pat = u'\w+.A(\d{4})(\d{3}).(\d{4}).\d{3}.\d+.hdf$'
        totalSec = 5 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            tt = pb_time.JDay2Datetime(
                g.group(1), g.group(2), g.group(3) + '00')
            self.ymd = tt.strftime('%Y%m%d')
            self.hms = g.group(3) + '00'
            return True
        else:
            return False


@nameClassManager.plugin
class AQUA_AIRS_L1(satNameBase):
    '''
    AIRS.2012.07.19.074.L1B.AIRS_Rad.v5.0.0.0.G12201105939.hdf
    '''

    def __init__(self):
        #         pat = u'AIRS.(\d{4}).(\d{2}).(\d{2}).(\d{3}).L1B.AIRS_Rad.v5.0.(0|\d{2}).0.G(\d{11}).hdf$'
        pat = u'AIRS.(\d{4}).(\d{2}).(\d{2}).(\d{3}).L1B.AIRS_Rad.*.G(\d{11}).hdf$'
        totalSec = 6 * 60
        satNameBase.__init__(self, pat, totalSec)

    def check(self, infile):
        g = re.match(self.pat, infile)
        if g:
            #             tt = pb_time.JDay2Datetime(g.group(2), g.group(3), '000000')
            self.ymd = g.group(1) + g.group(2) + g.group(3)
            self.hms = '000000'
            # 074 表示一天中产生的第几个文件，每6分钟产生一个文件。60取证为小时，取余为分钟
            hour = (int(g.group(4)) - 1) * 6 / 60
            mina = (int(g.group(4)) - 1) * 6 % 60
            hm = "%02d%02d" % (hour, mina)
            self.hms = hm + '00'

            return True
        else:
            return False


if __name__ == '__main__':
    allcls = nameClassManager()
    a = allcls.getInstance('VBD_npp_d20160715_global-saa_noaa_ops_v23.csv.gz')
    print a.dt_s, a.dt_e
