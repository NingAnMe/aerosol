# coding:utf-8 
'''
Created on 2015年8月13日

@author: zhangtao
'''
import re, os, sys
from pb_drc_base import L1Class
import numpy as np
import h5py
from pyhdf.SD import SD, SDC
from datetime import datetime, timedelta
import inspect
from scipy.interpolate.interpolate import interp2d

def getL1List():
    return inspect.getmembers(sys.modules[__name__],
                   lambda member: inspect.isclass(member) and member.__module__ == __name__)

def getModisDatetime(year, days, hms):
    date1 = datetime(int(year), 1, 1, int(hms[:2]), int(hms[2:4]), int(hms[4:6]))
    return date1 + timedelta(days=(int(days) - 1))

def interpLonLat(lons, lats, multiple):
    
    row, col = lons.shape
    x = multiple * np.arange(col) + (multiple - 1) / 2.
    y = multiple * np.arange(row) + (multiple - 1) / 2.
    
    # set y minus where lons == -999
    index = np.where(lons[:, 0] < -900)
    # set minus value the point will not participate interp, 
    # but each y should not be the same
    y[index] = y[index] * -1.

    # interp
    z = lons
    f = interp2d(x, y, z)
    xx = np.arange(col * multiple)
    yy = np.arange(row * multiple)
    lons1 = f(xx, yy)

    z = lats
    f = interp2d(x, y, z)
    lats1 = f(xx, yy)
    
    return lons1, lats1

class FY3X_L1_MERSI_1KM(L1Class):
    '''
    FY3X MERSI L1 250M
    '''
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'(FY3[A-Z])_MERSI_\w{4}_L1_(\d{8})_(\d{4})_1000M_MS.HDF'
#         BandNum = 5
#         Row = 2000
#         Col = 2048
        Res = 1000
        Region = 'GBAL'
        L1Class.__init__(self, "MERSI", pat, Res, Region)
        self.totalSec = 5 * 60
        self.BandsName = ['Band1',
                          'Band2',
                          'Band3',
                          'Band4',
                          'Band5']
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3) + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            return True
        else:
            return False
                    
    def open(self, Hdf5_file):
        g = re.match(self.pat, os.path.basename(Hdf5_file))

        self.ymd = g.group(2)
        self.hms = g.group(3)
        
        self.h5file = h5py.File(Hdf5_file, 'r')  # open hdf5 file
        self.lons = self.h5file['Longitude']
        self.lats = self.h5file['Latitude']
#         self.solarZenith = self.h5file['SolarZenith']     
        
        bands1234 = self.h5file['EV_250_Aggr.1KM_RefSB']
        bands5 = self.h5file['EV_250_Aggr.1KM_Emissive']
        self.bands = np.concatenate((bands1234, [bands5]), axis=0)  # join bands1234 & bands5
#         bands6plus = self.h5file['EV_1KM_RefSB']
#         self.bands = np.concatenate((self.bands, bands6plus), axis=0)  # join band6plus


class FY3A_L1_MERSI_250M(L1Class):
    '''
    FY3A MERSI L1 250M
    '''
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'(FY3A)_MERSI_GBAL_L1_(\d{8})_(\d{4})_0250M_MS.HDF'
#         BandNum = 5
#         Row = 8000
#         Col = 8192
        Res = 250
        Region = 'GBAL'
        L1Class.__init__(self, "MERSI", pat, Res, Region)
        self.totalSec = 5 * 60
        self.BandsName = ['EV_250_RefSB_b1',
                          'EV_250_RefSB_b2',
                          'EV_250_RefSB_b3',
                          'EV_250_RefSB_b4',
                          'EV_250_Emissive']
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3) + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            self.Region = 'GBAL'
            return True
        else:
            return False

    def open(self, Hdf5_file):       
        self.h5file = h5py.File(Hdf5_file, 'r')  # open hdf5 file
        self.bands = []
        slope = 0.01  # hdf里是错的
        i = 0
        for eachName in self.BandsName:
            self.bands.append(np.array(self.h5file[eachName]) * slope)
            i = i + 1
        
        lons = self.h5file['Longitude']
        lats = self.h5file['Latitude']
        self.lons, self.lats = interpLonLat(lons, lats, 4)       
                            
class FY3B_L1_MERSI_250M(L1Class):
    '''
    FY3B(-Z) MERSI L1 250M
    '''
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'(FY3[B-Z])_MERSI_GBAL_L1_(\d{8})_(\d{4})_0250M_MS.HDF'
#         BandNum = 5
#         Row = 8000
#         Col = 8192
        Res = 250
        Region = 'GBAL'
        L1Class.__init__(self, "MERSI", pat, Res, Region)
        self.totalSec = 5 * 60
        self.BandsName = ['EV_250_RefSB_b1',
                          'EV_250_RefSB_b2',
                          'EV_250_RefSB_b3',
                          'EV_250_RefSB_b4',
                          'EV_250_Emissive']
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3) + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            self.Region = 'GBAL'
            return True
        else:
            return False

    def open(self, Hdf5_file):       
        self.h5file = h5py.File(Hdf5_file, 'r')  # open hdf5 file
        self.bands = []
        slope = 0.01
        i = 0
        for eachName in self.BandsName:
            self.bands.append(np.array(self.h5file[eachName]) * slope)
            i = i + 1
        
        GEO_file = Hdf5_file.replace("_0250M_", "_GEOXX_")
        self.geofile = h5py.File(GEO_file, 'r')  # open hdf5 file
        self.lons = self.geofile['Longitude']
        self.lats = self.geofile['Latitude']
       
class FY3X_L1_VIRR_1kM(L1Class):
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'(FY3[A-Z])_VIRRX_GBAL_L1_(\d{8})_(\d{4})_(\w{5})_MS.HDF'         
#         BandNum = 10
#         Row = 1800
#         Col = 2048
        Res = 1000
        Region = 'GBAL'
        L1Class.__init__(self, "VIRR", pat, Res, Region)
        self.totalSec = 5 * 60
        self.BandsName = ['EV_RefSB',
                          'EV_Emissive']
        self.bandOrder = [1, 2, 6, 7, 8, 9, 10, 3, 4, 5]
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3) + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            return True
        else:
            return False           

    def open(self, Hdf5_file):       
        self.h5file = h5py.File(Hdf5_file, 'r')  # open hdf5 file
        self.bands = []
        refSB = self.h5file['EV_RefSB']
        emissive = self.h5file['EV_Emissive']
        self.bands = np.concatenate((refSB, emissive), axis=0)  # join bands, 和self.bandOrder对应 ???

        self.lons = self.h5file['Longitude']
        self.lats = self.h5file['Latitude']
        self.solarZenith = self.h5file['SolarZenith']
        
class MODIS_L1_1KM(L1Class):
    '''
    MODIS L1 021KM
    '''
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'(M\wD)(021KM).A(\d{4})(\d{3}).(\d{4}).(\d{3}).(\d+).hdf'          
#         BandNum = 10
#         Row = 2030
#         Col = 1354
        Res = 1000
        Region = 'GBAL'
        L1Class.__init__(self, "MODIS", pat, Res, Region)
        self.totalSec = 5 * 60
        
    def check(self, Hdf4_name):
        g = re.match(self.pat, Hdf4_name)
        if g:
            
            self.dt_s = getModisDatetime(g.group(3), g.group(4), g.group(5) + '00')
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            self.ymd = self.dt_s.strftime("%Y%m%d")
            self.hms = g.group(5) + '00'
            return True
        else:
            return False  
                
    def open(self, ModisHdf4):
        '''
        ModisHdf4:         Modis的HDF4文件
        '''
        self.h4file = SD(ModisHdf4, SDC.READ)

        self.bands = []
        self.BandsName = []
        
        # # EV_250_Aggr1km_RefSB
        dset = self.h4file.select('EV_250_Aggr1km_RefSB').get()
        attr = self.h4file.select('EV_250_Aggr1km_RefSB').attributes()
        bandNames = attr['band_names'].split(',')
        scales = [float(e) for e in attr['radiance_scales']]
        offset = [float(e) for e in attr['radiance_offsets']]
        for i in range(len(bandNames)):
            self.BandsName.append(bandNames[i])
            self.bands.append(scales[i] * (dset[i] - offset[i]))
        
        # # EV_500_Aggr1km_RefSB
        dset = self.h4file.select('EV_500_Aggr1km_RefSB').get()
        attr = self.h4file.select('EV_500_Aggr1km_RefSB').attributes()
        bandNames = attr['band_names'].split(',')
        scales = [float(e) for e in attr['radiance_scales']]
        offset = [float(e) for e in attr['radiance_offsets']]
        for i in range(len(bandNames)):
            self.BandsName.append(bandNames[i])
            self.bands.append(scales[i] * (dset[i] - offset[i]))
        
        # # EV_1KM_RefSB 
        dset = self.h4file.select('EV_1KM_RefSB').get()
        attr = self.h4file.select('EV_1KM_RefSB').attributes()
        bandNames = attr['band_names'].split(',')
        scales = [float(e) for e in attr['radiance_scales']]
        offset = [float(e) for e in attr['radiance_offsets']]
        for i in range(len(bandNames)):
            self.BandsName.append(bandNames[i])
            self.bands.append(scales[i] * (dset[i] - offset[i]))      
        
        # # EV_1KM_Emissive 
        dset = self.h4file.select('EV_1KM_Emissive').get()
        attr = self.h4file.select('EV_1KM_Emissive').attributes()
        bandNames = attr['band_names'].split(',')
        scales = [float(e) for e in attr['radiance_scales']]
        offset = [float(e) for e in attr['radiance_offsets']]
        for i in range(len(bandNames)):
            dset0 = scales[i] * (dset[i] - offset[i]) 
            if bandNames[i] == '31':
                K1 = 734.539; K2 = 1306.26
            elif bandNames[i] == '32':
                K1 = 470.570; K2 = 1194.95
            else:
                K1 = 470.570; K2 = 1194.95
            self.BandsName.append(bandNames[i])
            self.bands.append(K2 / np.log(K1 / dset0 + 1))
        
        # 经纬度在MOD03里
        fname = os.path.split(ModisHdf4)[1]
        tmpName = '.'.join(fname.split('.')[:4]).replace("021KM.", "03.")
        for each in self.secondaryFiles:
            if os.path.basename(each).startswith(tmpName):
                self.geofile = SD(each, SDC.READ)
                self.lons = self.geofile.select('Longitude').get()
                self.lats = self.geofile.select('Latitude').get()
#                 self.solarZenith = self.geofile.select('SolarZenith').get()
                break
       
class MOD02QKM(L1Class):
    '''
    MODIS 02QKM
    '''
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'(AQUA|TERRA)_(\d{4})_(\d{2})_(\d{2})_(\d{2})_(\d{2})_GZ.MOD02QKM.hdf$'       
        Res = 250
        Region = 'GBAL'
        L1Class.__init__(self, "MODIS", pat, Res, Region)
        self.totalSec = 8 * 60
        
    def check(self, Hdf4_name):
        g = re.match(self.pat, Hdf4_name)
        if g:
            self.Sat = g.group(1)
            self.ymd = g.group(2) + g.group(3) + g.group(4)
            self.hms = g.group(5) + g.group(6) + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            self.Region = 'GBAL'
            return True
        else:
            return False  
                
    def open(self, ModisHdf4):
        '''
        ModisHdf4:         Modis的HDF4文件
        '''
        self.h4file = SD(ModisHdf4, SDC.READ)

        self.bands = []
        self.BandsName = []
        
        # # EV_250_RefSB
        dset = self.h4file.select('EV_250_RefSB').get()
        attr = self.h4file.select('EV_250_RefSB').attributes()
        bandNames = attr['band_names'].split(',')
        scales = [float(e) for e in attr['radiance_scales']]
        offset = [float(e) for e in attr['radiance_offsets']]
        for i in range(len(bandNames)):
            self.BandsName.append(bandNames[i])
            condition = np.logical_and(dset[i] > 0 , dset[i] < 32767)
            index = np.where(condition)
            dset_calibrated = np.array(dset[i])
            dset_calibrated[index] = scales[i] * (dset[i][index] - offset[i])
            self.bands.append(dset_calibrated)
                
        lons = self.h4file.select('Longitude').get()
        lats = self.h4file.select('Latitude').get()
        
        # lons lats 差值
        self.lons, self.lats = interpLonLat(lons, lats, 4)
                
class MOD02HKM(L1Class):
    '''
    MODIS 02HKM
    '''
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'(AQUA|TERRA)_(\d{4})_(\d{2})_(\d{2})_(\d{2})_(\d{2})_GZ.MOD02HKM.hdf$'       
        Res = 500
        Region = 'GBAL'
        L1Class.__init__(self, "MODIS", pat, Res, Region)
        self.totalSec = 8 * 60
        
    def check(self, Hdf4_name):
        g = re.match(self.pat, Hdf4_name)
        if g:
            self.Sat = g.group(1)
            self.ymd = g.group(2) + g.group(3) + g.group(4)
            self.hms = g.group(5) + g.group(6) + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            self.Region = 'GBAL'
            return True
        else:
            return False  
                
    def open(self, ModisHdf4):
        '''
        ModisHdf4:         Modis的HDF4文件
        '''
        self.h4file = SD(ModisHdf4, SDC.READ)
        self.bands = []
        self.BandsName = []
        
        # # EV_500_RefSB
        dset = self.h4file.select('EV_500_RefSB').get()
        attr = self.h4file.select('EV_500_RefSB').attributes()
        bandNames = attr['band_names'].split(',')
        scales = [float(e) for e in attr['radiance_scales']]
        offset = [float(e) for e in attr['radiance_offsets']]
        for i in range(len(bandNames)):
            self.BandsName.append(bandNames[i])
            condition = np.logical_and(dset[i] > 0 , dset[i] < 32767)
            index = np.where(condition)
            dset_calibrated = np.array(dset[i])
            dset_calibrated[index] = scales[i] * (dset[i][index] - offset[i])
            self.bands.append(dset_calibrated)
                
        lons = self.h4file.select('Longitude').get()
        lats = self.h4file.select('Latitude').get()
        
        # lons lats 差值
        self.lons, self.lats = interpLonLat(lons, lats, 2)

class MOD021KM(L1Class):
    '''
    MODIS 021KM
    '''
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'(AQUA|TERRA)_(\d{4})_(\d{2})_(\d{2})_(\d{2})_(\d{2})_GZ.MOD021KM.hdf$'       
        Res = 1000
        Region = 'GBAL'
        L1Class.__init__(self, "MODIS", pat, Res, Region)
        self.totalSec = 8 * 60
        
    def check(self, Hdf4_name):
        g = re.match(self.pat, Hdf4_name)
        if g:
            self.Sat = g.group(1)
            self.ymd = g.group(2) + g.group(3) + g.group(4)
            self.hms = g.group(5) + g.group(6) + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            self.Region = 'GBAL'
            return True
        else:
            return False  
                
    def open(self, ModisHdf4):
        '''
        ModisHdf4:         Modis的HDF4文件
        '''
        self.h4file = SD(ModisHdf4, SDC.READ)

        self.bands = []
        self.BandsName = []
        
        # EV_1KM_RefSB
        dset = self.h4file.select('EV_1KM_RefSB').get()
        attr = self.h4file.select('EV_1KM_RefSB').attributes()
        bandNames = attr['band_names'].split(',')
        scales = [float(e) for e in attr['radiance_scales']]
        offset = [float(e) for e in attr['radiance_offsets']]
        for i in range(len(bandNames)):
            self.BandsName.append(bandNames[i])
            condition = np.logical_and(dset[i] > 0 , dset[i] < 32767)
            index = np.where(condition)
            dset_calibrated = np.array(dset[i])
            dset_calibrated[index] = scales[i] * (dset[i][index] - offset[i])
            self.bands.append(dset_calibrated)

        # # EV_1KM_Emissive 
        dset = self.h4file.select('EV_1KM_Emissive').get()
        attr = self.h4file.select('EV_1KM_Emissive').attributes()
        bandNames = attr['band_names'].split(',')
        scales = [float(e) for e in attr['radiance_scales']]
        offset = [float(e) for e in attr['radiance_offsets']]
        for i in range(len(bandNames)):
            self.BandsName.append(bandNames[i])
            condition = np.logical_and(dset[i] > 0 , dset[i] < 32767)
            index = np.where(condition)
            dset_calibrated = np.array(dset[i])
            dset_calibrated[index] = scales[i] * (dset[i][index] - offset[i]) 
            
            if bandNames[i] == '31':
                K1 = 734.539; K2 = 1306.26
            elif bandNames[i] == '32':
                K1 = 470.570; K2 = 1194.95
            else:
                K1 = 470.570; K2 = 1194.95
            condition = np.logical_and(dset_calibrated > 0 , dset_calibrated < 60000)  # !=0 !=65535
            index = np.where(condition)
            dset_calibrated[index] = K2 / np.log(K1 / dset_calibrated[index] + 1.)
            self.bands.append(dset_calibrated)
                        
        # 经纬度在MOD03里
        fname = os.path.basename(ModisHdf4)
        tmpName = fname.replace("021KM.", "03.")
        for each in self.secondaryFiles:
            if os.path.basename(each) == tmpName:
                self.geofile = SD(each, SDC.READ)
                self.lons = self.geofile.select('Longitude').get()
                self.lats = self.geofile.select('Latitude').get()
#                 self.solarZenith = self.geofile.select('SolarZenith').get()
                break
    

class MODIS_L1_03(L1Class):
    '''
    MODIS L1 03 定位文件
    '''
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'(M\wD)(03).A(\d{4})(\d{3}).(\d{4}).(\d{3}).(\d+).hdf'          
        Res = 1000
        Region = 'GBAL'
        L1Class.__init__(self, "MODIS", pat, Res, Region)
        self.totalSec = 5 * 60

    def check(self, Hdf4_name):
        g = re.match(self.pat, Hdf4_name)
        if g:
            
            self.dt_s = getModisDatetime(g.group(3), g.group(4), g.group(5) + '00')
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            self.ymd = self.dt_s.strftime("%Y%m%d")
            self.hms = g.group(5) + '00'
            return True
        else:
            return False
        
class MOD03(L1Class):
    '''
    MODIS 03 定位文件
    '''
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'(AQUA|TERRA)_(\d{4})_(\d{2})_(\d{2})_(\d{2})_(\d{2})_GZ.MOD03.hdf$'
        Res = 1000
        Region = 'GBAL'
        L1Class.__init__(self, "MODIS", pat, Res, Region)
        self.totalSec = 8 * 60
        
    def check(self, Hdf4_name):
        g = re.match(self.pat, Hdf4_name)
        if g:
            self.Sat = g.group(1)
            self.ymd = g.group(2) + g.group(3) + g.group(4)
            self.hms = g.group(5) + g.group(6) + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            self.Region = 'GBAL'
            return True
        else:
            return False

class FY3X_L1_MERSI_GEO(L1Class):
    '''
    FY3X 定位文件
    '''
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'(FY3[A-Z])_MERSI_GBAL_L1_(\d{8})_(\d{4})_GEOXX_MS.HDF'
        Res = 250
        Region = 'GBAL'
        L1Class.__init__(self, "MERSI", pat, Res, Region)
        self.totalSec = 5 * 60
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3) + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            self.Region = 'GBAL'
            return True
        else:
            return False

class FY3X_L1_MERSI_OBC(L1Class):
    '''
    FY3X OBC文件
    '''
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'(FY3[A-Z])_MERSI_GBAL_L1_(\d{8})_(\d{4})_OBCXX_MS.HDF'
        Res = 250
        Region = 'GBAL'
        L1Class.__init__(self, "MERSI", pat, Res, Region)
        self.totalSec = 5 * 60
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3) + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            self.Region = 'GBAL'
            return True
        else:
            return False


class FY3X_L1_IRAS(L1Class):
    '''
    FY3X IRAS文件
    '''
    def __init__(self):
        '''
        HDF5文件  FY3B_IRASX_GBAL_L1_20150919_2318_017KM_MS.HDF
        '''
        pat = u'(FY3[A-Z])_IRASX_GBAL_L1_(\d{8})_(\d{4})_017KM_MS.HDF'
        Res = 17000
        Region = 'GBAL'
        L1Class.__init__(self, "IRAS", pat, Res, Region)
        self.totalSec = int(24. * 60. / 14. * 60.)  # 一天14轨
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3) + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            self.Region = 'GBAL'
            return True
        else:
            return False

class FY2X_L1_VISSR(L1Class):
    '''
    FY2X VISSR文件
    '''
    def __init__(self):
        '''
        | 构造函数 
        '''
        pat = u'(FY2[C-Z])_FDI_ALL_NOM_(\d{8})_(\d{4}).hdf'
        Res = 10000  # ????
        Region = 'GBAL'
        L1Class.__init__(self, "VISSR", pat, Res, Region)
        self.totalSec = 30 * 60
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3) + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            self.Region = 'GBAL'
            return True
        else:
            return False

class METOP_IASI(L1Class):
    '''
    FY3X OBC文件
    '''
    def __init__(self):
        '''
        | 构造函数 
        '''
        pat = u'IASI_xxx_1C_(M\d{2})_(\d{14})Z_(\d{14})Z_N_O_(\d{14})Z__(\d{14})'
        Res = 10000
        Region = 'GBAL'
        L1Class.__init__(self, "IASI", pat, Res, Region)
        self.totalSec = 3 * 60
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(2)[:8]
            self.hms = g.group(2)[8:]
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            self.Region = 'GBAL'
            return True
        else:
            return False

class METOP_GOME(L1Class):
    '''
    METOP GOME文件
    '''
    def __init__(self):
        '''
        | 构造函数 
        '''
        pat = u'GOME_xxx_1B_(M\d{2})_(\d{14})Z_(\d{14})Z_N_O_(\d{14})Z__(\d{14})'
        Res = 10000  # ????
        Region = 'GBAL'
        L1Class.__init__(self, "GOME", pat, Res, Region)
        self.totalSec = 3 * 60
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(2)[:8]
            self.hms = g.group(2)[8:]
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            return True
        else:
            return False

class NPP_CRIS(L1Class):
    '''
    NPP CRIS文件
    '''
    def __init__(self):
        '''
        | 构造函数 
        '''
        pat = u'(GCRSO-SCRIS)_npp_d(\d{8})_t(\d{7})_e(\d{7})_b(\d{5})_c(\d{20})_noaa_ops.h5'
        Res = 10  # ????
        Region = 'GBAL'
        L1Class.__init__(self, "CRIS", pat, Res, Region)
        self.totalSec = 8 * 60
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3)[:-1]  # 舍弃秒的小数点以后位
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = datetime.strptime('%s %s' % (self.ymd, g.group(4)[:-1]), "%Y%m%d %H%M%S")
            return True
        else:
            return False

class NPP_ATMS(NPP_CRIS):
    '''
    NPP ATMS文件
    '''
    def __init__(self):
        '''
        | 构造函数 
        '''
        NPP_CRIS.__init__(self)
        self.pat = u'(GATMO-SATMS)_npp_d(\d{8})_t(\d{7})_e(\d{7})_b(\d{5})_c(\d{20})_noaa_ops.h5'
        
class NPP_VIIRS_GCRSO(L1Class):
    '''
    VIIRS_GCRSO文件
    '''
    def __init__(self):
        '''
        | 构造函数 
        '''
        pat = u'(GCRSO)_npp_d(\d{8})_t(\d{7})_e(\d{7})_b(\d{5})_c(\d{20})_noaa_ops.h5'
        Res = 500
        Region = 'GBAL'
        L1Class.__init__(self, "VIIRS", pat, Res, Region)
        self.totalSec = 6 * 60
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3)[:-1]  # 舍弃秒的小数点以后位
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = datetime.strptime('%s %s' % (self.ymd, g.group(4)[:-1]), "%Y%m%d %H%M%S")
            return True
        else:
            return False

class NPP_VIIRS_GMODO(NPP_VIIRS_GCRSO):
    '''
    VIIRS_GMODO文件
    '''
    def __init__(self):
        '''
        | 构造函数 
        '''
        NPP_VIIRS_GCRSO.__init__(self)
        self.pat = u'(GMODO)_npp_d(\d{8})_t(\d{7})_e(\d{7})_b(\d{5})_c(\d{20})_noaa_ops.h5'

class NPP_VIIRS_GDNBO(NPP_VIIRS_GCRSO):
    '''
    VIIRS_GDNBO文件
    '''
    def __init__(self):
        '''
        | 构造函数 
        '''
        NPP_VIIRS_GCRSO.__init__(self)
        self.pat = u'(GDNBO)_npp_d(\d{8})_t(\d{7})_e(\d{7})_b(\d{5})_c(\d{20})_noaa_ops.h5'
        
class NPP_VIIRS_GMTCO(NPP_VIIRS_GCRSO):
    '''
    VIIRS_GMTCO文件
    '''
    def __init__(self):
        '''
        | 构造函数 
        '''
        NPP_VIIRS_GCRSO.__init__(self)
        self.pat = u'(GMTCO)_npp_d(\d{8})_t(\d{7})_e(\d{7})_b(\d{5})_c(\d{20})_noaa_ops.h5'

class NPP_VIIRS_SVM(L1Class):
    '''
    VIIRS_SVM01-16文件
    '''
    def __init__(self):
        '''
        | 构造函数 
        '''
        pat = u'(SVM\d{2})_npp_d(\d{8})_t(\d{7})_e(\d{7})_b(\d{5})_c(\d{20})_noaa_ops.h5'
        Res = 500
        Region = 'GBAL'
        L1Class.__init__(self, "VIIRS", pat, Res, Region)
        self.totalSec = 6 * 60
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3)[:-1]  # 舍弃秒的小数点以后位
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = datetime.strptime('%s %s' % (self.ymd, g.group(4)[:-1]), "%Y%m%d %H%M%S")
            return True
        else:
            return False

class FY3_MWHS_L1(L1Class):
    '''
    FY3 MWHS L1文件
    '''
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'(FY3[A-Z])_MWHSX_GBAL_L1_(\d{8})_(\d{4})_015KM_MS.HDF'
        Res = 15000
        Region = 'GBAL'
        L1Class.__init__(self, "MWHS", pat, Res, Region)
        self.totalSec = 102 * 60
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3) + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            return True
        else:
            return False

class FY3_MWTS_L1(L1Class):
    '''
    FY3 MWTS L1文件
    '''
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'(FY3[A-Z])_MWTSX_GBAL_L1_(\d{8})_(\d{4})_033KM_MS.HDF'
        Res = 33000
        Region = 'GBAL'
        L1Class.__init__(self, "MWTS", pat, Res, Region)
        self.totalSec = 102 * 60
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(2)
            self.hms = g.group(3) + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            return True
        else:
            return False
        
class FY3_MWRI_L1(L1Class):
    '''
    FY3 MWRI L1文件
    '''
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'(FY3[A-Z])_(MWRIA|MWRID)_GBAL_L1_(\d{8})_(\d{4})_010KM_MS.HDF'
        Res = 10000
        Region = 'GBAL'
        L1Class.__init__(self, "MWRI", pat, Res, Region)
        self.totalSec = 102 * 60
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(3)
            self.hms = g.group(4) + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            return True
        else:
            return False

class GCOM_AMSR2_L1(L1Class):
    '''
    GCOM AMSR2 L1文件     GW1AM2_201511050936_034A_L1SGBTBR_2210210.h5.gz
    '''
    def __init__(self):
        '''
        | 构造函数
        '''
        pat = u'GW1AM2_(\d{12})_\w{4}_L1SGBTBR_2210210.(h5$|h5.gz$)'
        Res = 10000
        Region = 'GBAL'
        L1Class.__init__(self, "AMSR2", pat, Res, Region)
        self.totalSec = 60 * 60
        
    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.ymd = g.group(1)[:8]
            self.hms = g.group(1)[8:] + '00'
            self.dt_s = datetime.strptime('%s %s' % (self.ymd, self.hms), "%Y%m%d %H%M%S")
            self.dt_e = self.dt_s + timedelta(seconds=(self.totalSec))
            return True
        else:
            return False                                                                                                                 
# class NOAA18_AVHRR_1KM(L1Class):
#     '''
#     classdocs
#     '''
#     def __init__(self):
#         '''
#         | 构造函数
#         '''
#         pass
#          
#          
#     def check(self, Hdf4_name):
#         pass
#                  
#     def open(self, ModisHdf4):
#         pass
#      
#     def close(self):
#         pass
 
 
class OMI_AURA_L2_OMSO2(L1Class):
    def __init__(self):
        '''
       SO2  NO2
       OMI-Aura_L2-OMSO2_2016m0130t2155-o61405_v003-2016m0131t035312.he5
        '''
        pat = u'OMI-(Aura)_L2-OMSO2_(\d{4})m(\d{4})t(\d{4})-o(\d{5})_(\S+).he5$'
        Res = 1000
        Region = 'GBAL'
        L1Class.__init__(self, "OMI", pat, Res, Region)
        self.totalSec = 100 * 60

    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.Sat = g.group(1)
            self.Region = "GBAL"  # 正则group2 是地区标识
            self.ymd = g.group(2) + g.group(3) + g.group(4)  # 正则group3 是年月日
            
            return True
        else:
            return False

class OCO2_TANSO_L2_CO2(L1Class):
    def __init__(self):
        '''
       CO2
       oco2_L2StdGL_07977a_160101_B7200_160102112351.h5
        '''
        pat = u'oco2_L2Std(\w{2})_(\w{6})_(\d{6})_B(\d{4})_(\d{12}).h5$'
        Res = 1000
        Region = 'GBAL'
        L1Class.__init__(self, "TANSO", pat, Res, Region)
        self.totalSec = 100 * 60

    def check(self, Hdf5_name):
        g = re.match(self.pat, Hdf5_name)
        if g:
            self.Sat = 'OCO2'
            self.Region = "GBAL"  
            self.ymd = '20' + g.group(3)
            return True
        else:
            return False
                                             
if __name__ == '__main__':
    aa = METOP_IASI()
    print aa.check('IASI_xxx_1C_M02_20151021005657Z_20151021005953Z_N_O_20151021023046Z__20151021023749')
    
    

