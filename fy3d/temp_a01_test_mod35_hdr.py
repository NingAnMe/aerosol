#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2019/10/31 15:49
# @Author  : AnNing
import gdal

infile = '../test_data/aerosol_test_input/a1.17299.1910.mod35.img'
dataset = gdal.Open(infile)
XSize = dataset.RasterXSize
YSize = dataset.RasterYSize
band = dataset.GetRasterBand(2)
data1 = band.ReadAsArray(0, 0, XSize, YSize)
print(data1.shape)
print(data1[1:10][1:10])
