#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-04-26 9:44
# @Author  : NingAnMe <ninganme@qq.com>
import os
import re

import numpy as np
import h5py
import yaml
from pylab import *
import matplotlib as mpl
from mpl_toolkits.axes_grid1 import make_axes_locatable
import matplotlib.patches as mpatches

color = dict()
color['Cloudy'] = (1., 1., 1.)  # 生成白色，这个地方是云
color['Uncertain'] = (169 / 255., 169 / 255., 169 / 255.)  # 生成灰色，这个地方不确定
color['Land Clear'] = (0, 128 / 255., 0)  # 生成绿色，这个地方陆地晴空
color['Poss Land Clear'] = (0., 1., 0.)  # 生成亮绿，这个地方陆地也许晴空
color['Sea Clear'] = (25 / 255., 25 / 255., 112 / 255.)  # 生成深蓝，这个地方水面晴空
color['Poss Sea Clear'] = (65 / 255., 105 / 255., 225 / 255.)  # 生成淡蓝，这个地方水面也许晴空
color['Invalid'] = (0., 0., 0.)  # 生成黑色，这个地方数据无效
color['Sun Glint'] = (255 / 255., 0., 0.)  # 生成红色，这个地方水面太阳耀斑
color['Coastlines'] = (255 / 255., 215 / 255., 0)  # 生成黄色，这个是海岸线

colorkeys = ['Cloudy', 'Uncertain', 'Land Clear', 'Poss Land Clear',
             'Sea Clear', 'Poss Sea Clear', 'Invalid', 'Sun Glint', 'Coastlines']


def drawLayer(mask, rgb):
    """
    mask: array of mask
    rgb: color of True in mask
    """
    row, col = mask.shape
    r, g, b = rgb
    z = np.zeros((row, col))
    z = np.ma.masked_where(mask, z)
    z = np.ma.filled(z, 1)

    print(z.shape)

    rgba = np.array((r, g, b, 0) * row * col)
    print(rgba.shape)
    rgba.shape = row, col, 4
    print(type(rgba[0, 0, 0]), rgb)

    rgba[:, :, 3] = z

    subplot(111)
    imshow(rgba, interpolation='none')


def drawRects(ax):
    """
    色标
    """
    divider = make_axes_locatable(ax)
    fig = ax.get_figure()
    ax2 = divider.append_axes("bottom", "2%", pad="0.1%")
    fig.add_axes(ax2)
    patches = []
    # add a rectangle
    x0 = 0.
    y0 = 0.1
    rectl = 0.05
    recth = 0.7
    step = 0.005
    for eachkey in colorkeys:
        rect = mpatches.Rectangle(
            (x0, y0), rectl, recth, ec='k', fc=color[eachkey], fill=True, lw=0.3)
        patches.append(rect)
        ax2.add_patch(rect)
        str = eachkey
        x1 = x0
        y1 = y0 - 0.6
        if eachkey.count(' ') == 2:  # 3个单词加回车
            i = eachkey.rfind(' ')
            l = list(eachkey)
            l[i] = '\n'
            str = ''.join(l)
            y1 = y1 - 0.5
        plt.text(x1, y1, str, ha="left", size=3)
        x0 = x0 + rectl + step
    axis('off')


class cloudMaskHdf:
    """
    对气溶胶产品5分钟块hdf的某一通道进行等经纬度投影，并输出投影后的5分钟块hdf
    """

    def __init__(self, Hdf5):
        '''
        Hdf4: HDF4文件路径全名
        '''
        self.fullpath = Hdf5
        self.fileName = os.path.basename(Hdf5)  # 文件名
        self.cloudMask = None
        self.ymd = None
        pat = '(FY3[A-Z])_MERSI_ORBT_L2_CLM_MLT_NUL_(\d{8})_(\d{4})_1000M_MS'
        # hdf文件名的正则表达式
        #         pat = u'(FY3[A-Z])_MERSI_GBAL_L2_(\d{8})_(\d{4})_CLMXX_MS'
        # FY3D_MERSI_GBAL_L2_20050101_0735_CLMXX_MS.HDF
        g = re.match(pat, self.fileName)

        if g:
            self.ymd = g.group(2)  # 年月日
            self.load()  # load dataset

    def load(self):
        try:
            h5file = h5py.File(self.fullpath, 'r')
        except:
            print('Error HDF5:', self.fullpath)
        else:
            # 数据集
            self.cloudMask = h5file.get('Cloud_Mask')[0, :, :]
            #        # lat
            #        self.Latitude = h5file.get('Latitude')[:,:]
            #        # lon
            #        self.Longitude = h5file.get('Longitude')[:,:]
            h5file.close()


def cloud_map(in_file, out_file):
    mpl.rcParams['figure.figsize'] = (5, 5.3)

    hdf = cloudMaskHdf(in_file)
    if not hdf.ymd:
        return

    z = hdf.cloudMask
    print(type(z[0, 0]))

    outpath = os.path.dirname(out_file)
    try:
        if not os.path.isdir(outpath):
            os.makedirs(outpath)
    except Exception as e:
        print(e)
        print('**[%s]**mkdir error because of MPI' % outpath)
        pass

    fig = figure()
    ax = subplot(111)
    subplots_adjust(left=0., right=1., bottom=0.01, top=0.98)

    z0 = z & 0b1
    z12 = (z >> 1) & 0b11
    z4 = (z >> 4) & 0b1
    z67 = (z >> 6) & 0b11

    mask = (z == 0)
    drawLayer(mask, color['Invalid'])

    mask = (z67 == 0b01)
    drawLayer(mask, color['Coastlines'])

    mask = (z12 == 0b01)
    drawLayer(mask, color['Uncertain'])

    # Cloud
    mask = (z12 == 0b00) & (z0 == 0b1)
    drawLayer(mask, color['Cloudy'])

    mask = ((z67 == 0b11) | (z67 == 0b10)) & (z12 == 0b10)  # & (z0 == 0b1)
    drawLayer(mask, color['Poss Land Clear'])

    mask = ((z67 == 0b11) | (z67 == 0b10)) & (z12 == 0b11)  # & (z0 == 0b1)
    drawLayer(mask, color['Land Clear'])

    mask = (z67 == 0b00) & (z12 == 0b11) & (z4 == 0b1)
    drawLayer(mask, color['Sea Clear'])

    mask = (z67 == 0b00) & (z12 == 0b11) & (z4 == 0b0)
    drawLayer(mask, color['Sun Glint'])

    mask = (z67 == 0b00) & (z12 == 0b10)
    drawLayer(mask, color['Poss Sea Clear'])

    axis('off')
    #    ax.set_title(nm, fontsize = 7)
    nm = os.path.basename(out_file)
    plt.title(nm, fontsize=7)

    drawRects(ax)

    plt.savefig(out_file, dpi=300)
    plt.close()
    fig.clear()


class ReadInYaml:

    def __init__(self, in_file):
        """
        读取yaml格式配置文件
        """
        if not os.path.isfile(in_file):
            sys.exit(-1)

        with open(in_file, 'r') as stream:
            cfg = yaml.load(stream)

        self.jobname = cfg['INFO']['job_name']
        self.ymd = cfg['INFO']['ymd']
        self.rewrite = cfg['INFO']['rewrite']

        self.ipath = cfg['PATH']['ipath']
        self.opath = cfg['PATH']['opath']


def main(in_file):
    # 01 ICFG = 输入配置文件类 ##########
    in_cfg = ReadInYaml(in_file)

    in_file = in_cfg.ipath
    outpath = in_cfg.opath

    rewrite = in_cfg.rewrite

    filename = os.path.basename(in_file)
    out_file = os.path.join(outpath, filename + '.PNG')
    if rewrite or (not os.path.isfile(out_file)):
        cloud_map(in_file, out_file)


if __name__ == '__main__':

    # 获取python输入参数，进行处理
    args = sys.argv[1:]
    if len(args) == 1:  # 跟参数，则处理输入的时段数据
        IN_FILE = args[0]
    else:
        print('input args error exit')
        sys.exit(-1)
    main(IN_FILE)
