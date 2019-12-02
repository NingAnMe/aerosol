# coding: utf-8

'''
Created on 2017年9月7日

@author: wangpeng
'''

import os
import time

import h5py

from PB import pb_sat
from PB import pb_space
from PB.pb_time import fy3_ymd2seconds
import numpy as np


# 获取类py文件所在的目录
MainPath, MainFile = os.path.split(os.path.realpath(__file__))


class CLASS_MERSI2_L1():
    '''
    1km的mersi2数据类
    '''

    def __init__(self):

        # 定标使用
        self.sat = 'FY3D'
        self.sensor = 'MERSI'
        self.res = 1000
        self.Band = 25
        self.orbit_direction = []
        self.orbit_num = []
        self.Dn = {}
        self.Ref = {}
        self.Rad = {}
        self.Tbb = {}

        self.CloudMask = []
        self.satAzimuth = []
        self.satZenith = []
        self.sunAzimuth = []
        self.sunZenith = []
        self.Lons = []
        self.Lats = []
        self.Time = []

        self.SV = {}
        self.BB = {}
        self.LandSeaMask = []
        self.LandCover = []

        # wangpeng add k  2018-06-29
        self.cal_coeff1 = {}
        self.cal_coeff2 = {}

        # 增加矢量计算 G,P,L
        self.G_pos = None
        self.L_pos = None
        self.P_pos = None

        # 波长（μm） 转 波数 cm-1  (10000μm=1cm)   cm-1 = 10000/波长（μm）
        # 红外通道的中心波数，固定值，MERSI_Equiv Mid_wn (cm-1)
        self.WN = {'CH_20': 2634.359, 'CH_21': 2471.654, 'CH_22':
                   1382.621, 'CH_23': 1168.182, 'CH_24': 933.364, 'CH_25': 836.941}
        # 红外转tbb的修正系数，固定值
        self.TeA = {'CH_20': 1.00103, 'CH_21': 1.00085, 'CH_22':
                    1.00125, 'CH_23': 1.00030, 'CH_24': 1.00133, 'CH_25': 1.00065}
        self.TeB = {'CH_20': -0.4759, 'CH_21': -0.3139, 'CH_22': -
                    0.2662, 'CH_23': -0.0513, 'CH_24': -0.0734, 'CH_25': 0.0875}
        # 所有通道的中心波数和对应的响应值 ，SRF
        self.waveNum = {}
        self.waveRad = {}

        # 投影使用
        self.VIS_Coeff = []

    def Load(self, L1File):

        ipath = os.path.dirname(L1File)
        iname = os.path.basename(L1File)
        geoFile = os.path.join(ipath, iname[0:-12] + 'GEO1K_MS.HDF')
        clm_name = iname.replace('GBAL_L1', 'ORBT_L2_CLM_MLT_NUL')
        clm_file = os.path.join(ipath, clm_name)
        print (u'读取 L1 %s' % L1File)
        try:
            h5File_R = h5py.File(L1File, 'r')

            orbit_direction = h5File_R.attrs.get('Orbit Direction')
            orbit_num = h5File_R.attrs.get('Orbit Number')
            self.orbit_direction.append(orbit_direction)
            self.orbit_num.append(orbit_num)

            ary_ch1_4 = h5File_R.get('/Data/EV_250_Aggr.1KM_RefSB')[:]
            ary_ch5_19 = h5File_R.get('/Data/EV_1KM_RefSB')[:]
            ary_ch20_23 = h5File_R.get('/Data/EV_1KM_Emissive')[:]
            ary_ch20_23_a = h5File_R.get(
                '/Data/EV_1KM_Emissive').attrs['Slope']
            ary_ch20_23_b = h5File_R.get(
                '/Data/EV_1KM_Emissive').attrs['Intercept']
            ary_ch24_25 = h5File_R.get('/Data/EV_250_Aggr.1KM_Emissive')[:]
            ary_ch24_25_a = h5File_R.get(
                '/Data/EV_250_Aggr.1KM_Emissive').attrs['Slope']
            ary_ch24_25_b = h5File_R.get(
                '/Data/EV_250_Aggr.1KM_Emissive').attrs['Intercept']
            ary_IR_Coeff = h5File_R.get('/Calibration/IR_Cal_Coeff')[:]
            ary_VIS_Coeff = h5File_R.get('/Calibration/VIS_Cal_Coeff')[:]
            ary_sv = h5File_R.get('/Calibration/SV_DN_average')[:]
            ary_bb = h5File_R.get('/Calibration/BB_DN_average')[:]
        except Exception as e:
            print str(e)
            return
        finally:
            h5File_R.close()

        print (u'读取geo %s' % geoFile)
        try:
            # 读取GEO文件
            h5File_R = h5py.File(geoFile, 'r')
            ary_satz = h5File_R.get('/Geolocation/SensorZenith')[:]
            ary_sata = h5File_R.get('/Geolocation/SensorAzimuth')[:]
            ary_sunz = h5File_R.get('/Geolocation/SolarZenith')[:]
            ary_suna = h5File_R.get('/Geolocation/SolarAzimuth')[:]
            ary_lon = h5File_R.get('/Geolocation/Longitude')[:]
            ary_lat = h5File_R.get('/Geolocation/Latitude')[:]
            ary_LandCover = h5File_R.get('/Geolocation/LandCover')[:]
            ary_LandSeaMask = h5File_R.get('/Geolocation/LandSeaMask')[:]
            ary_day = h5File_R.get('/Timedata/Day_Count')[:]
            ary_time = h5File_R.get('/Timedata/Millisecond_Count')[:]
        except Exception as e:
            print str(e)
            return
        finally:
            h5File_R.close()

        print (u'读取 云检测 %s' % clm_file)
        try:
            h5File_R = h5py.File(clm_file, 'r')
            data_pre = h5File_R.get('Cloud_Mask')[0, :, :]

            clm_flag = np.full(data_pre.shape, -999)

            z = data_pre

            # 0 表示无效值
            z0 = z & 0b1
            z12 = (z >> 1) & 0b11
            z4 = (z >> 4) & 0b1
            z67 = (z >> 6) & 0b11

            # Invalid  0
            mask = (z == 0)
            idx = np.where(mask)
            clm_flag[idx] = 0

            # Coastlines
            mask = (z67 == 0b01)
            idx = np.where(mask)
            clm_flag[idx] = 1

            # Uncertain
            mask = (z12 == 0b01) & (z0 == 0b1)
            idx = np.where(mask)
            clm_flag[idx] = 2

            # Cloud
            mask = (z12 == 0b00) & (z0 == 0b1)
            idx = np.where(mask)
            clm_flag[idx] = 3

            # Poss Land Clear
            mask = ((z67 == 0b11) | (z67 == 0b10)) & (
                z12 == 0b10) & (z0 == 0b1)
            idx = np.where(mask)
            clm_flag[idx] = 4

            # Land Clear
            mask = ((z67 == 0b11) | (z67 == 0b10)) & (
                z12 == 0b11) & (z0 == 0b1)
            idx = np.where(mask)
            clm_flag[idx] = 5

            # Poss Sea Clear
            mask = (z67 == 0b00) & (z12 == 0b10) & (z0 == 0b1)
            idx = np.where(mask)
            clm_flag[idx] = 6

            # Sea Clear
            mask = (z67 == 0b00) & (z12 == 0b11) & (z4 == 0b1) & (z0 == 0b1)
            idx = np.where(mask)
            clm_flag[idx] = 7

            # Sun Glint
            mask = (z67 == 0b00) & (z12 == 0b11) & (z4 == 0b0) & (z0 == 0b1)
            idx = np.where(mask)
            clm_flag[idx] = 8
#             print clm_flag
            data = clm_flag
            if self.CloudMask == []:
                self.CloudMask = data
            else:
                self.CloudMask = np.concatenate((self.CloudMask, data))

            h5File_R.close()
        except Exception as e:
            print str(e)
            if hasattr(self, 'CloudMask'):
                delattr(self, 'CloudMask')

        # 通道的中心波数和光谱响应
        for i in xrange(self.Band):
            BandName = 'CH_%02d' % (i + 1)
            srfFile = os.path.join(
                MainPath, 'SRF', '%s_%s_SRF_CH%02d_Pub.txt' % (self.sat, self.sensor, (i + 1)))
            dictWave = np.loadtxt(
                srfFile, dtype={'names': ('num', 'rad'), 'formats': ('f4', 'f4')})
            waveNum = 10 ** 7 / dictWave['num'][::-1]
            waveRad = dictWave['rad'][::-1]
            self.waveNum[BandName] = waveNum
            self.waveRad[BandName] = waveRad

        # 数据大小 使用经度维度
        dshape = ary_lon.shape

        # 1-19通道的可见光数据进行定标
        K = ary_VIS_Coeff
        for i in range(19):
            if i < 4:
                # 统一下标
                j = i
                indata = ary_ch1_4[j]
            else:
                j = i - 4
                indata = ary_ch5_19[j]

            # 初始存放dn数据的结构，初始值 Nan
            DN = np.full(dshape, np.nan)
            idx = np.logical_and(indata < 65500, indata >= 0)
            DN[idx] = indata[idx]
            Ref = ((DN ** 2 * K[i, 2]) + DN * K[i, 1] + K[i, 0]) / 100.

            BandName = 'CH_%02d' % (i + 1)
            if BandName not in self.Dn.keys():
                self.Dn[BandName] = DN
                self.Ref[BandName] = Ref
            else:
                self.Dn[BandName] = np.concatenate((self.Dn[BandName], DN))
                self.Ref[BandName] = np.concatenate((self.Ref[BandName], Ref))

            if BandName not in self.cal_coeff1.keys():
                self.cal_coeff1[BandName] = [K[i, 0], K[i, 1], K[i, 2]]

        # 20-25通道的红外光数据进行定标 ########
        K = ary_IR_Coeff
        a = np.concatenate((ary_ch20_23_a, ary_ch24_25_a))
        b = np.concatenate((ary_ch20_23_b, ary_ch24_25_b))

        for i in range(6):
            if i < 4:
                # 统一下标
                j = i
                indata = ary_ch20_23[j]
            else:
                j = i - 4
                indata = ary_ch24_25[j]

            BandName = 'CH_%02d' % (i + 20)

            # 初始存放dn数据的结构，初始值 Nan
            DN = np.full(dshape, np.nan)
            Rad = np.full(dshape, np.nan)

            # 存放定标系数
            if BandName not in self.cal_coeff1.keys():
                self.cal_coeff1[BandName] = [
                    K[i, 0, 0], K[i, 2, 0], K[i, 3, 0]]
#             k1 = np.full(dshape, np.nan)

            # 把红外定标系数拆成4个数据 2000*2048
            k1 = (np.repeat(K[i, 1, :], 10 * 2048)).reshape(dshape)
            if BandName not in self.cal_coeff2.keys():
                self.cal_coeff2[BandName] = k1

            else:
                self.cal_coeff2[BandName] = np.concatenate(
                    (self.cal_coeff2[BandName], k1))
            # 对无效DN值进行过滤
            idx = np.logical_and(indata < 65500, indata > 0)
            DN[idx] = indata[idx]
            Rad[idx] = DN[idx] * a[i] + b[i]
            Tbb = pb_sat.planck_r2t(Rad, self.WN[BandName])
            Tbb = Tbb * self.TeA[BandName] + self.TeB[BandName]
            # 对类成员进行赋值
            if BandName not in self.Dn.keys():
                self.Dn[BandName] = DN
                self.Rad[BandName] = Rad
                self.Tbb[BandName] = Tbb
            else:
                self.Dn[BandName] = np.concatenate((self.Dn[BandName], DN))
                self.Rad[BandName] = np.concatenate((self.Rad[BandName], Rad))
                self.Tbb[BandName] = np.concatenate((self.Tbb[BandName], Tbb))

            # 定标方法仿真数据已有描述
#             for row_i in range(DN.shape[0]):
#                 # dn数据每10行使用一个定标系数
#                 jj = int(row_i / 10)
#                 k3 = K[i, 3, jj]
#                 k2 = K[i, 2, jj]
#                 k1 = K[i, 1, jj]
#                 k0 = K[i, 0, jj]
# Rad[row_i, :] = (DN[row_i, :] * k3 ** 3 + DN[row_i, :] * k2 ** 2 +
# DN[row_i, :] * k1 + k0)  # / 100.

        # 全局信息赋值 ############################
        # 对时间进行赋值合并
        v_ymd2seconds = np.vectorize(fy3_ymd2seconds)
        T1 = v_ymd2seconds(ary_day, ary_time)

        Time = np.full(dshape, -999)
        for i in xrange(ary_lon.shape[0]):
            Time[i, :] = T1[i / 10, 0]
        if self.Time == []:
            self.Time = Time
        else:
            self.Time = np.concatenate((self.Time, Time))

        # SV, BB
        for i in xrange(self.Band):
            BandName = 'CH_%02d' % (i + 1)
            SV = np.full(dshape, np.nan)
            BB = np.full(dshape, np.nan)
            for j in xrange(ary_lon.shape[0]):
                SV[j, :] = ary_sv[i][j / 10]
                BB[j, :] = ary_bb[i][j / 10]

#             SV = np.ma.masked_where(SV < 0, SV)
#             BB = np.ma.masked_where(BB < 0, BB)
#             np.ma.filled(SV, np.nan)
#             np.ma.filled(BB, np.nan)

            if BandName not in self.SV.keys():
                self.SV[BandName] = SV
                self.BB[BandName] = BB
            else:
                self.SV[BandName] = np.concatenate((self.SV[BandName], SV))
                self.BB[BandName] = np.concatenate((self.BB[BandName], BB))

        # 土地覆盖
        ary_LandCover_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_LandCover >= 0, ary_LandCover <= 254)
        ary_LandCover_idx[condition] = ary_LandCover[condition]

        if self.LandCover == []:
            self.LandCover = ary_LandCover_idx
        else:
            self.LandCover = np.concatenate(
                (self.LandCover, ary_LandCover_idx))

        # 海陆掩码
        ary_LandSeaMask_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_LandSeaMask >= 0, ary_LandSeaMask <= 7)
        ary_LandSeaMask_idx[condition] = ary_LandSeaMask[condition]

        if self.LandSeaMask == []:
            self.LandSeaMask = ary_LandSeaMask_idx
        else:
            self.LandSeaMask = np.concatenate(
                (self.LandSeaMask, ary_LandSeaMask_idx))

        # 经纬度
        ary_lon_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_lon > -180., ary_lon < 180.)
        ary_lon_idx[condition] = ary_lon[condition]
        if self.Lons == []:
            self.Lons = ary_lon_idx
        else:
            self.Lons = np.concatenate((self.Lons, ary_lon_idx))

        ary_lat_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_lat > -90., ary_lat < 90.)
        ary_lat_idx[condition] = ary_lat[condition]
        if self.Lats == []:
            self.Lats = ary_lat_idx
        else:
            self.Lats = np.concatenate((self.Lats, ary_lat_idx))

        # 卫星方位角 天顶角
        ary_sata_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_sata > 0, ary_sata < 36000)
        ary_sata_idx[condition] = ary_sata[condition]

        if self.satAzimuth == []:
            self.satAzimuth = ary_sata_idx / 100.
        else:
            self.satAzimuth = np.concatenate(
                (self.satAzimuth, ary_sata_idx / 100.))

        ary_satz_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_satz > 0, ary_satz < 18000)
        ary_satz_idx[condition] = ary_satz[condition]
        if self.satZenith == []:
            self.satZenith = ary_satz_idx / 100.
        else:
            self.satZenith = np.concatenate(
                (self.satZenith, ary_satz_idx / 100.))

        # 太阳方位角 天顶角
        ary_suna_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_suna > 0, ary_suna < 36000)
        ary_suna_idx[condition] = ary_suna[condition]

        if self.sunAzimuth == []:
            self.sunAzimuth = ary_suna_idx / 100.
        else:
            self.sunAzimuth = np.concatenate(
                (self.sunAzimuth, ary_suna_idx / 100.))

        ary_sunz_idx = np.full(dshape, np.nan)
        condition = np.logical_and(ary_sunz > 0, ary_sunz < 18000)
        ary_sunz_idx[condition] = ary_sunz[condition]

        if self.sunZenith == []:
            self.sunZenith = ary_sunz_idx / 100.
        else:
            self.sunZenith = np.concatenate(
                (self.sunZenith, ary_sunz_idx / 100.))

        # 系数先不合并，暂时未用，数据格式无法统一了
#         self.IR_Coeff = ary_IR_Coeff
        self.VIS_Coeff = ary_VIS_Coeff

    def get_G_P_L(self):

        # 增加G矢量计算
        # 第一组经纬度（成像仪）的ECEF坐标系下的值
        G_pos = np.zeros(np.append(self.Lons.shape, 3))
        high = np.zeros_like(self.Lons)
        G_pos[:, :, 0], G_pos[:, :, 1], G_pos[
            :, :, 2] = pb_space.LLA2ECEF(self.Lons, self.Lats, high)
        self.G_pos = G_pos

if __name__ == '__main__':
    L1File = 'D:/data/MERSI/FY3D_MERSI_GBAL_L1_20181001_0020_1000M_MS.HDF'
    modis = CLASS_MERSI2_L1()
    modis.Load(L1File)
    for key in sorted(modis.Ref.keys()):
        print "%s, %0.6f %0.6f" % (key, np.nanmin(modis.Ref[key]), np.nanmax(modis.Ref[key]))

    for key in sorted(modis.Rad.keys()):
        print "%s, %0.6f %0.6f" % (key, np.nanmin(modis.Rad[key]), np.nanmax(modis.Rad[key]))

    for key in sorted(modis.Tbb.keys()):
        print "%s, %0.6f %0.6f" % (key, np.nanmin(modis.Tbb[key]), np.nanmax(modis.Tbb[key]))
