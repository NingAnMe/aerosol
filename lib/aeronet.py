#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Author  : NingAnMe <ninganme@qq.com>
import os
import pandas as pd
"""
AERONET_Site_Name,Site_Latitude(Degrees),Site_Longitude(Degrees)
"""
from datetime import datetime


def get_aod_550nm(aod_675, angstrom_440_675):
    return aod_675 * ((550 / 675) ** (-1 * angstrom_440_675))


class Aeronet:
    def __init__(self, in_file):
        self.site_file = in_file
        self.datas = self.load_Aeronet_site_file(self.site_file)

    def load_Aeronet_site_file(self, in_file):
        datas = pd.read_csv(in_file, skiprows=6, index_col=False)
        return datas

    def get_site_name_lon_lat(self):
        d = self.datas
        return d['AERONET_Site_Name'][0], d['Site_Longitude(Degrees)'][0], d['Site_Latitude(Degrees)'][0]

    def __datetime2datetime(self, row):
        d = row.loc['Date(dd:mm:yyyy)']
        t = row.loc['Time(hh:mm:ss)']
        dt = datetime.strptime(d+t, '%d:%m:%Y%H:%M:%S')
        return dt

    def get_datetime(self):
        """
        "Date(dd:mm:yyyy),Time(hh:mm:ss)"
        :return:
        """
        dts = self.datas.apply(self.__datetime2datetime, axis=1)
        return dts

    def get_aod550(self):
        return get_aod_550nm(self.datas['AOD_675nm'], self.datas['440-675_Angstrom_Exponent'])
