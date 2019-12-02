# coding: utf-8

'''
Created on 2017年9月7日

@author: wangpeng
'''

import os, h5py
from pyhdf.SD import SD, SDC
import numpy as np
from datetime import datetime
from scipy import interpolate
from PB import  pb_name
from sys import getsizeof

class CLASS_HMW8_L1():

    def __init__(self):

        self.DN = {}
        self.Ref = {}
        self.Rad = {}
        self.Tbb = {}
        self.satAzimuth = []
        self.satZenith = []
        self.sunAzimuth = []
        self.sunZenith = []
        self.Lons = []
        self.Lats = []
        self.Time = []
        self.SV = {}
        self.BB = {}
        self.LandSeaMask = None
        self.LandCover = None

    def Load(self, L1File):

        print L1File
        iname = os.path.basename(L1File)
        i = 0
        dataSahpe = []
        try:
            h5File_R = h5py.File(L1File, 'r')
            for line in  h5File_R.items():
                print line[0]
                i = i + 1
                data = h5File_R.get(line[0])[:]
                dataSahpe = data.shape
                dataT = np.full(data.shape, np.nan)
                condition = np.logical_and(data > 0, data < 65535)
                dataT[condition] = data[condition]

                self.SV['CH_%02d' % i] = None
                self.BB['CH_%02d' % i] = None
                if 'NOMChannelVIS' in line[0]:
                    self.Ref['CH_%02d' % i] = dataT / 10000.
                    print 'CH_%02d' % i

                elif 'NOMChannelIRX' in line[0] :
                    print 'CH_%02d' % i
                    self.Tbb['CH_%02d' % i] = dataT / 100.

                elif 'NOMSunAzimuth' in line[0]:
                    self.sunAzimuth = dataT / 100.
                elif 'NOMSunZenith' in line[0]:
                    self.sunZenith = dataT / 100.
                else:
                    i = i - 1
                    print 'not', i
                data = 0

        except Exception as e:
            print str(e)
            return
        finally:
            h5File_R.close()

        self.Time = np.full(dataSahpe, -999.)
        nameClass = pb_name.nameClassManager()
        info = nameClass.getInstance(iname)
#         print info.dt_s
        secs = int((info.dt_s - datetime(1970, 1, 1, 0, 0, 0)).total_seconds())
        self.Time[:] = secs

    def Loadgeo(self, geoFile):

        try:
            h4File_R = SD(geoFile, SDC.READ)
            Lons = h4File_R.select('pixel_longitude').get()
            Lats = h4File_R.select('pixel_latitude').get()
            sata = h4File_R.select('pixel_satellite_azimuth_angle').get()
            satz = h4File_R.select('pixel_satellite_zenith_angle').get()

            idx = np.where(np.isclose(Lons, -999.))
            Lons[idx] = np.nan
            self.Lons = Lons

            idx = np.where(np.isclose(Lats, -999.))
            Lats[idx] = np.nan
            self.Lats = Lats

            idx = np.where(np.isclose(sata, -999.))
            sata[idx] = np.nan
            self.satAzimuth = sata

            idx = np.where(np.isclose(satz, -999.))
            satz[idx] = np.nan
            self.satZenith = satz

        except Exception as e:
            print str(e)
            return
        finally:
            h4File_R.end()


if __name__ == '__main__':
    L1File = 'D:/nsmc/data/himawari/AHI8_OBI_2000M_NOM_20171212_0340.hdf'
    geoFile = 'D:/nsmc/data/himawari/fygatNAV.Himawari08.xxxxxxx.000001_minmin.hdf'
    mersi = CLASS_HMW8_L1()
#     mersi.LutFile = 'C:\E\py_src\pysrc\OM\FY-3\exe\linux\FY3C-MERSI-LUT-TB-RB.txt'
    mersi.Load(L1File)
#     mersi.Loadgeo(geoFile)
#     pass
