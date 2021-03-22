#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-07-24 12:55
# @Author  : NingAnMe <ninganme@qq.com>
import os
from datetime import datetime
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np

from lib.plot_core import get_ds_font, PlotAx, get_month_avg_std

ORG_NAME = ''
FONT0 = get_ds_font("OpenSans-Regular.ttf")
FONT_MONO = get_ds_font("DroidSansMono.ttf")

RED = '#f63240'
BLUE = '#1c56fb'
GRAY = '#c0c0c0'
EDGE_GRAY = '#303030'

LINE_WIDTH = 0.5

TICKER_FONT = FONT0.copy()
TICKER_FONT.set_size(11)

TITLE_FONT = FONT0.copy()
TITLE_FONT.set_size(14)

LABEL_FONT = FONT0.copy()
LABEL_FONT.set_size(12)

BOTTOM_FONT = FONT0.copy()
BOTTOM_FONT.set_size(13)

REGRESSION_ANNOTATE_SIZE = 13
REGRESSION_ANNOTATE_COLOR = 'red'


def make_sure_path_exists(path):
    if not os.path.isdir(path):
        os.makedirs(path)
        print('创建文件夹：{}'.format(path))


def plot_regression(
        x,
        y,
        w=None,
        out_file=None,
        title=None,
        x_label=None,
        y_label=None,
        x_range=None,
        y_range=None,
        x_interval=None,
        y_interval=None,
        annotate=None,
        ymd_start=None,
        ymd_end=None,
        ymd=None,
        density=False,
        bias_line=False):
    # style_file = os.path.join('plot_regression.mplstyle')
    # plt.style.use(style_file)
    figsize = (5, 5)
    dpi = 100
    fig = plt.figure(figsize=figsize, dpi=dpi)
    ax1 = plt.subplot2grid((1, 1), (0, 0))
    plot_ax = PlotAx()

    # ##### 画散点
    marker_size = 5
    if density:
        marker = 'o'
        alpha = 0.8
        zorder = 80
        result = plot_ax.plot_density_scatter(ax1, x, y, marker=marker, alpha=alpha, marker_size=marker_size,
                                              zorder=zorder)
        fig.colorbar(result, ax=ax1)
        # result = plot_ax.plot_hexbin(ax1, x, y, alpha=alpha, zorder=zorder)
        # cb = fig.colorbar(result, ax=ax1)
        # cb.set_label('Count')
    else:
        alpha = 0.8  # 透明度
        marker = "o"  # 形状
        color = "b"  # 颜色
        zorder = 80
        ax1.scatter(x, y, s=marker_size, marker=marker, c=color, lw=0, alpha=alpha, zorder=zorder)

    # ##### 画回归线
    color = 'r'
    linewidth = 1.2
    zorder = 90
    plot_ax.plot_regression_line(ax1, x, y, w, x_range, color=color, linewidth=linewidth,
                                 zorder=zorder)

    # ##### 画对角线
    color = '#808080'
    linewidth = 1.2
    zorder = 90
    plot_ax.plot_diagonal_line(ax1, x, y, x_range, y_range, color, linewidth, zorder=zorder)

    # 画误差线
    if bias_line:
        color = '#FF33E0'
        linewidth = 1.2
        zorder = 90
        plot_ax.plot_bias_line(ax1, x, x_range, color, linewidth, zorder)

    # ##### 格式化图片
    format_kwargs = {}

    if x_range is not None:
        format_kwargs['x_axis_min'] = x_range[0]
        format_kwargs['x_axis_max'] = x_range[1]
    if y_range is not None:
        format_kwargs['y_axis_min'] = y_range[0]
        format_kwargs['y_axis_max'] = y_range[1]
    if x_label is not None:
        format_kwargs['x_label'] = x_label
    if y_label is not None:
        format_kwargs['y_label'] = y_label
    if x_interval is not None:
        format_kwargs['x_interval'] = x_interval
    if y_interval is not None:
        format_kwargs['y_interval'] = y_interval

    if annotate is not None:
        format_kwargs['annotate'] = annotate
        format_kwargs['annotate_color'] = REGRESSION_ANNOTATE_COLOR
        format_kwargs['annotate_font_size'] = REGRESSION_ANNOTATE_SIZE

    plot_ax.format_ax(ax1, **format_kwargs)

    # ##### 标题 底部文字 LOGO
    plt.tight_layout()
    fig.subplots_adjust(bottom=0.15, top=0.85)

    if '\n' in title:
        title_y = 0.96
    else:
        title_y = 0.94
    fig.suptitle(title, y=title_y, ha='center', fontproperties=TITLE_FONT, fontsize=10)

    bottom_text = ''
    bottom_text_l = 0.7
    bottom_text_b = 0.02
    if ymd_start and ymd_end:
        bottom_text = bottom_text + '%s-%s' % (ymd_start, ymd_end)
    elif ymd:
        bottom_text = bottom_text + '%s' % ymd
    if ORG_NAME is not None:
        bottom_text = bottom_text + '   ' + ORG_NAME
    if bottom_text:
        fig.text(bottom_text_l, bottom_text_b, bottom_text, fontproperties=BOTTOM_FONT)

    # ##### 输出图片
    make_sure_path_exists(os.path.dirname(out_file))
    fig.savefig(out_file, dpi=dpi)
    fig.clear()
    plt.close()
    print('>>> {}'.format(out_file))


def plot_histogram(
        data,
        out_file=None,
        bins_count='auto',
        title=None,
        x_label=None,
        y_label=None,
        x_range=None,
        y_range=None,
        hist_label=None,
        annotate=None,
        ymd_start=None,
        ymd_end=None,
        ymd=None, ):
    import seaborn as sns
    # style_path = STYLE_PATH
    # style_file = os.path.join(style_path, 'plot_histogram.mplstyle')
    # plt.style.use(style_file)
    figsize = (6, 4)
    dpi = 100
    fig = plt.figure(figsize=figsize, dpi=dpi)
    ax1 = plt.subplot2grid((1, 1), (0, 0))

    sns.histplot(data, ax=ax1, color='b', bins=bins_count)

    plot_ax = PlotAx()

    format_kwargs = {
        'x_major_count': 11,
        'x_minor_count': 1,
    }

    if x_range is not None:
        format_kwargs['x_axis_min'] = x_range[0]
        format_kwargs['x_axis_max'] = x_range[1]
    if y_range is not None:
        format_kwargs['y_axis_min'] = y_range[0]
        format_kwargs['y_axis_max'] = y_range[1]
    if x_label is not None:
        format_kwargs['x_label'] = x_label
    if y_label is not None:
        format_kwargs['y_label'] = y_label
    if annotate is not None:
        format_kwargs['annotate'] = annotate

    plot_ax.format_ax(ax1, **format_kwargs)

    # --------------------
    plt.tight_layout()
    fig.suptitle(title, y=0.94, ha='center', fontproperties=TITLE_FONT)
    fig.subplots_adjust(bottom=0.15, top=0.85)

    if ymd_start and ymd_end:
        fig.text(0.50, 0.02, '%s-%s' % (ymd_start, ymd_end), fontproperties=BOTTOM_FONT)
    elif ymd:
        fig.text(0.50, 0.02, '%s' % ymd, fontproperties=BOTTOM_FONT)

    fig.text(0.8, 0.02, ORG_NAME, fontproperties=BOTTOM_FONT)
    # ---------------
    make_sure_path_exists(os.path.dirname(out_file))
    fig.savefig(out_file, dpi=100)
    fig.clear()
    plt.close()
    print('>>> {}'.format(out_file))


def plot_timeseries(
        data_x=None,  # datetime.datetime
        data_y=None,
        out_file=None,
        title=None,
        y_label=None,
        x_range=None,
        y_range=None,
        y_interval=None,
        ymd=None,
        ymd_start=None,
        ymd_end=None,
        annotate=None,
        plot_month=False,
        plot_zeroline=True):
    # style_path = STYLE_PATH
    # style_file = os.path.join(style_path, 'plot_timeseries.mplstyle')
    # plt.style.use(style_file)
    figsize = (6, 4)
    dpi = 100
    fig = plt.figure(figsize=figsize, dpi=dpi)
    ax1 = plt.subplot2grid((1, 1), (0, 0))

    plot_ax = PlotAx()

    # 配置属性
    if x_range is not None:
        x_range = x_range
    elif ymd_start is not None and ymd_end is not None:
        x_min = datetime.strptime(ymd_start, '%Y%m%d')
        x_max = datetime.strptime(ymd_end, '%Y%m%d')
        x_range = [x_min, x_max]
    else:
        x_min = np.nanmin(data_x)
        x_max = np.nanmax(data_x)
        x_range = [x_min, x_max]
    # 绘制日数据长时间序列
    plot_ax.plot_time_series(ax1, data_x, data_y, marker='x', color=BLUE, line_width=None,
                             marker_size=6,
                             marker_edgecolor=BLUE, marker_edgewidth=0.3, alpha=0.8, zorder=80,
                             label="Daily")

    if plot_month:
        month_data_x, month_data_y, month_data_std = get_month_avg_std(data_x, data_y)
        finite_idx = np.isfinite(month_data_y)
        month_data_x = month_data_x[finite_idx]
        month_data_y = month_data_y[finite_idx]
        month_data_std = month_data_std[finite_idx]

        # 绘制月数据长时间序列
        if month_data_x is not None and month_data_y is not None:
            plot_ax.plot_time_series(ax1, month_data_x, month_data_y, marker='o-', color=RED,
                                     line_width=0.6, marker_size=5,
                                     marker_edgecolor=RED, marker_edgewidth=0, alpha=0.8, zorder=90,
                                     label="Monthly")

            # 绘制背景填充
            if month_data_std is not None:
                plot_ax.plot_background_fill(ax1, x=month_data_x,
                                             y1=month_data_y - month_data_std,
                                             y2=month_data_y + month_data_std,
                                             color=RED,
                                             alpha=0.2,
                                             )
    ax1.legend(loc='upper left')
    # 绘制 y=0 线配置，在绘制之前设置x轴范围
    if plot_zeroline:
        plot_ax.plot_zero_line(ax1, data_x, x_range)

    format_kwargs = {
        'timeseries': True,
        'tick_font_size': 11,
        'y_minor_count': 0
    }
    if x_range is not None:
        format_kwargs['x_axis_min'] = x_range[0]
        format_kwargs['x_axis_max'] = x_range[1]
    if y_range is not None:
        format_kwargs['y_axis_min'] = y_range[0]
        format_kwargs['y_axis_max'] = y_range[1]
    if y_interval is not None:
        format_kwargs['y_interval'] = y_interval
    if y_label is not None:
        format_kwargs['y_label'] = y_label
    if annotate is not None:
        format_kwargs['annotate'] = annotate

    plot_ax.format_ax(ax1, **format_kwargs)
    # --------------------
    plt.tight_layout()
    fig.suptitle(title, y=0.96, ha='center', fontproperties=TITLE_FONT)
    if '\n' in title:
        fig.subplots_adjust(bottom=0.2, top=0.82)
    else:
        fig.subplots_adjust(bottom=0.2, top=0.85)

    # if ymd_start and ymd_end:
    #     circle1 = mpatches.Circle((74, 15), 6, color=BLUE, ec=GRAY, lw=0)
    #     circle2 = mpatches.Circle((164, 15), 6, color=RED, ec=GRAY, lw=0)
    #     fig.patches.extend([circle1, circle2])
    #
    #     fig.text(0.15, 0.02, 'Daily', color=BLUE, fontproperties=BOTTOM_FONT)
    #     fig.text(0.3, 0.02, 'Monthly', color=RED, fontproperties=BOTTOM_FONT)
    #     fig.text(0.50, 0.02, '%s-%s' % (ymd_start, ymd_end), fontproperties=BOTTOM_FONT)
    # elif ymd:
    #     fig.text(0.50, 0.02, '%s' % ymd, fontproperties=BOTTOM_FONT)
    #
    # fig.text(0.8, 0.02, ORG_NAME, fontproperties=BOTTOM_FONT)
    # ---------------
    make_sure_path_exists(os.path.dirname(out_file))
    fig.savefig(out_file, dpi=100)
    fig.clear()
    plt.close()
    print('>>> {}'.format(out_file))
