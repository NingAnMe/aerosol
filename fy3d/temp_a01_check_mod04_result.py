#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2019/11/14 15:54
# @Author  : NingAnMe <ninganme@qq.com>
import os
import gdal
import argparse

parse = argparse.ArgumentParser()
parse.add_argument('in_dir')
args = parse.parse_args()

in_dir = args.in_dir
for root, dirs, filenames in os.walk(in_dir):
    for filename in filenames:
        if 'mod04' in filename and 'img' in filename:
            infile = os.path.join(root, filename)
            print('<<< infile: {}'.format(infile))
            ds = gdal.Open(infile)
            if ds is None:
                continue
            datas = ds.ReadAsArray()
            if datas is None:
                continue
            data = datas[2]
            index = data > 0
            if index.any():
                print('>>>>>>>>> {}'.format(infile))
