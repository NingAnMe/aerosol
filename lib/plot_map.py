#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-09-28 14:57
# @Author  : NingAnMe <ninganme@qq.com>
from DV.dv_map import dv_map
from pylab import *


def plot_map_project(
        latitude,
        longitude,
        value,
        out_file,
        box=None,
        title=None,
        ptype=None,
        vmin=None,
        vmax=None,
        marker=None,
        markersize=None):
    if box is not None:
        box = box  # nlat, slat, wlon, elon:北（小），南（大），东（大），西（小）

    if title is not None:
        title = title
    else:
        title = "Map"

    if vmin is not None:
        vmin = vmin

    if vmax is not None:
        vmax = vmax

    if marker is not None:
        marker = marker
    else:
        marker = 's'

    if markersize is not None:
        markersize = markersize
    else:
        markersize = 5

    p = dv_map()

    p.easyplot(latitude, longitude, value, vmin=vmin, vmax=vmax, box=box,
               ptype=ptype, markersize=markersize, marker=marker)

    p.title = title
    p.savefig(out_file)
    print('>>> {}'.format(out_file))


def plot_shanghai(latitude,
                  longitude,
                  value,
                  out_file,
                  box=None,
                  title=None,
                  ptype=None,
                  vmin=None,
                  vmax=None,
                  marker=None,
                  markersize=None,
                  AREA=None):
    if box is not None:
        box = box  # nlat, slat, wlon, elon:北（小），南（大），东（大），西（小）
    if title is not None:
        title = title
    else:
        title = "Map"

    if vmin is not None:
        vmin = vmin
    if vmax is not None:
        vmax = vmax

    if marker is not None:
        marker = marker
    else:
        marker = 's'

    if markersize is not None:
        markersize = markersize
    else:
        markersize = 5

    # 开始画图-----------------------

    fig = plt.figure(figsize=(9., 8.))  # 图像大小

    p = dv_map(fig=fig)

    subplots_adjust(left=0.07, right=0.98, top=0.90, bottom=0.15)
    p.show_colorbar = False
    p.show_countries = False
    p.show_coastlines = False
    # p.show_line_of_latlon = False
    # p.show_china = True
    p.show_china_province = True
    p.show_inside_china = True
    p.show_inside_china_mini = False

    p.delat = 2  # 纬度刻度线分辨率
    p.delon = 2  # 经度刻度线分辨率
    p.color_coast = "#3a3a3a"  # 海岸线颜色
    p.color_contry = "#3a3a3a"  # 国家颜色
    p.fontsize_tick = 15

    # set color map
    p.valmin = vmin
    p.valmax = vmax
    p.colormap = plt.get_cmap('jet')  # mpl.cm.rainbow, summer
    # p.colorbar_extend = "max"

    # plot
    p.easyplot(latitude, longitude, value, box=box, markersize=markersize, ptype="pcolormesh")

    if AREA:
        print('设置地区 ：{}'.format(AREA))
        if AREA == 'YRD':
            citys = ["江苏省", "安徽省", "浙江省", "上海市"]
        elif AREA == 'BTH':
            citys = ["北京市", "天津市", "河北省"]
        elif AREA == 'FWP':
            citys = ["陕西省", "山西省", "河南省"]
        elif AREA == 'FWP':
            citys = ["广东省"]
        else:
            citys = []
        for city in citys:
            p.city_boundary(city, linewidth=1.2, shape_name='中国省级行政区')

    # 色标 ---------------------------
    cb_loc = [0.12, 0.07, 0.76, 0.03]
    # unit = r"$\mathregular{(10^{15}\/\/molec/cm^2)}$"
    fontsize = 16
    # p.add_custom_colorbar(cb_loc, p.valmin, p.valmax,
    #                       fmt="%d",
    #                       unit="(1E16 molec/cm^2)",
    #                       fontsize=fontsize)
    c_ax = fig.add_axes(cb_loc)
    cbar = fig.colorbar(p.cs, cax=c_ax, orientation='horizontal')
    for l in c_ax.xaxis.get_ticklabels():
        l.set_fontproperties(p.font_mid)
        l.set_fontsize(fontsize)
        l.set_color(p.color_ticker)
    # cbar写单位
    # cbar.ax.set_title(unit, x=1.0382, y=0, color=p.color_ticker,
    #                   ha='left', va='center',
    #                   fontproperties=p.font_mid, fontsize=fontsize)

    # 标题 ---------------------------
    p.w_title = p.suptitle(title, fontsize=14, y=0.97)

    # save
    p.savefig(out_file, dpi=300)
    print(">>> {}".format(out_file))
    p.clean()
