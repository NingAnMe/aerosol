#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-05-27 15:32
# @Author  : NingAnMe <ninganme@qq.com>

import os
import numpy as np

from PIL import Image, ImageEnhance
from lib.load_mersi import ReadMersiL1


def normal255int8(array):
    """
    小于 0 的值赋值为0，其他值归一化到 0-255
    :param array: ndarray
    :return: ndarray
    """
    array[np.isnan(array)] = 0
    array[array < 0] = 0
    data = (array - array.min()) / (array.max() - array.min())
    data = data * 255
    data = data.astype(np.uint8)
    return data


def plot_image_image(r, g, b, out_file, brightness=None):
    """
    :param r: ndarray
    :param g: ndarray
    :param b: ndarray
    :param out_file: str
    :param brightness: int 亮度提升的倍数
    :return:
    """
    r = normal255int8(r)
    g = normal255int8(g)
    b = normal255int8(b)

    imr = Image.fromarray(r, 'L')
    img = Image.fromarray(g, 'L')
    imb = Image.fromarray(b, 'L')
    im = Image.merge('RGB', (imr, img, imb))

    if brightness is not None:
        enh_bri = ImageEnhance.Brightness(im)
        im = enh_bri.enhance(brightness)

    im.save(out_file)
    print('>>> {}'.format(out_file))


dir_path = '/RED1BDATA/cma/SourceData/FY3D/1KM/20200428'
ch01 = 'CH_01'
ch02 = 'CH_02'
ch03 = 'CH_03'

in_files = os.listdir(dir_path)
in_files.sort()


def plot_rgb():
    for filename in in_files:
        if os.path.splitext(filename)[1] != '.HDF':
            print(os.path.splitext(filename)[1])
            continue
        in_file = os.path.join(dir_path, filename)
        out_file = in_file + '.png'
        if os.path.isfile(out_file):
            print(out_file)
            continue

        print('<<< {}'.format(in_file))
        try:
            loader = ReadMersiL1(in_file)
        except OSError as why:
            print(why)
            continue
        refs = loader.get_ref()
        r = refs.get(ch03)
        g = refs.get(ch02)
        b = refs.get(ch01)
        plot_image_image(r, g, b, brightness=1.2, out_file=out_file)


def find_file():
    for filename in in_files:
        if os.path.splitext(filename)[1] != '.HDF':
            print(os.path.splitext(filename)[1])
            continue
        in_file = os.path.join(dir_path, filename)
        out_file = in_file + '.png'
        if os.path.isfile(out_file):
            print(out_file)
            continue

        print('<<< {}'.format(in_file))
        try:
            loader = ReadMersiL1(in_file)
        except OSError as why:
            print(why)
            continue
        refs = loader.get_ref()
