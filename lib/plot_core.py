# -*- coding: utf-8 -*-
"""
@Time    : 2018/6/29 14:33
@Author  : AnNing
"""
import os
from dateutil.relativedelta import relativedelta

import numpy as np
from scipy import stats
import matplotlib as mpl
import matplotlib.image as img

# mpl.use("Agg")  # 必须加这个字段，否则引用 pyplot 服务器会报错，服务器上面没有 TK

import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from matplotlib.ticker import LinearLocator
from matplotlib.font_manager import FontProperties
from matplotlib import colors
from matplotlib import colorbar


def get_ds_font(font_name="OpenSans-Regular.ttf"):
    """
    载入字体
    "OpenSans-Regular.ttf"
    "simhei.ttf"
    "微软雅黑.ttf"
    """
    self_path = os.path.split(os.path.realpath(__file__))[0]
    font0 = FontProperties()
    font_path = os.path.join(self_path, "FNT", font_name)
    if os.path.isfile(font_path):
        font0.set_file(font_path)
        return font0
    return None


FONT0 = get_ds_font("OpenSans-Regular.ttf")
FONT_MONO = get_ds_font("DroidSansMono.ttf")


class PlotAx(object):
    """
    格式化 matplotlib.axis 常用方法
    """

    def __init__(self):
        self.font = FONT_MONO  # 字体

        self.label_font = FONT_MONO
        self.x_label_font_size = 12
        self.y_label_font_size = 12

        self.tick_font = FONT0
        self.tick_font_size = 11
        self.tick_font_color = '#000000'

        self.annotate_font = FONT_MONO
        self.annotate_font_size = 12
        self.annotate_font_color = 'red'

    def format_ax(self, ax, **kwargs):
        """
        ax = ax

        font = FONT0  # 字体

        x_label = None  # X 轴标签
        y_label = None  # Y 轴标签
        x_label_font_size = 11  # X 轴标签字体
        y_label_font_size = 11  # Y 轴标签字体

        x_major_count = None  # X 轴主刻度数量
        x_minor_count = None  # Y 轴主刻度数量
        y_major_count = None  # X 轴次刻度数量
        y_minor_count = None  # Y 轴次刻度数量

        x_axis_min = None  # X 轴最小值
        x_axis_max = None  # X 轴最大值
        y_axis_min = None  # Y 轴最小值
        y_axis_max = None  # Y 轴最大值

        tick_font = FONT0  # 刻度字体
        tick_font_size = 11  # 刻度文字字体大小
        tick_font_color = '#000000'

        annotate = None  # 注释
        annotate_font_size = 11  # 注释字体大小
        annotate_font_color = '#000000'

        timeseries = None
        :param ax:
        :param kwargs: (dict)
        :return:
        """
        # 设置字体
        if 'font' in kwargs:
            self.font = kwargs.get('font')
        if 'annotate_font' in kwargs:
            self.annotate_font = kwargs.get('annotate_font')
        if 'label_font' in kwargs:
            self.label_font = kwargs.get('label_font')

        # 设置 label
        if 'x_label' in kwargs:
            x_label = kwargs.get('x_label')
            if 'x_label_font_size' in kwargs:
                x_label_font_size = kwargs.get('x_label_font_size')
            else:
                x_label_font_size = self.x_label_font_size
            ax.set_xlabel(x_label, fontsize=x_label_font_size, fontproperties=self.label_font)
        if 'y_label' in kwargs:
            y_label = kwargs.get('y_label')
            if 'y_label_font_size' in kwargs:
                y_label_font_size = kwargs.get('y_label_font_size')
            else:
                y_label_font_size = self.y_label_font_size
            ax.set_ylabel(y_label, fontsize=y_label_font_size, fontproperties=self.label_font)

        # 设置 x 轴的范围和Tick
        if 'x_axis_min' in kwargs and 'x_axis_max' in kwargs:
            x_axis_min = kwargs.get('x_axis_min')
            x_axis_max = kwargs.get('x_axis_max')
            ax.set_xlim(x_axis_min, x_axis_max)

            # 如果是长时间序列图，设置长时间序列x轴日期坐标
            if kwargs.get('timeseries'):
                self.set_timeseries_x_locator(ax, x_axis_min, x_axis_max)

            # 设置大小tick数量
            if kwargs.get('x_interval'):
                x_interval = kwargs.get('x_interval')
                x_major_count = int((x_axis_max - x_axis_min) / x_interval + 1)
                if kwargs.get('x_minor_count') is not None:
                    x_minor_count = kwargs.get('x_minor_count')
                else:
                    if x_major_count <= 11:
                        x_minor_count = 4
                    else:
                        x_minor_count = 1
                # 设置大刻度的数量
                ax.xaxis.set_major_locator(LinearLocator(x_major_count))
                # 设置小刻度的数量
                ax.xaxis.set_minor_locator(LinearLocator((x_major_count - 1) * (x_minor_count + 1) + 1))

        if 'y_axis_min' in kwargs and 'y_axis_max' in kwargs:
            y_axis_min = kwargs.get('y_axis_min')
            y_axis_max = kwargs.get('y_axis_max')
            ax.set_ylim(y_axis_min, y_axis_max)

            # 设置大小tick数量
            if kwargs.get('y_interval'):
                y_interval = kwargs.get('y_interval')
                y_major_count = int((y_axis_max - y_axis_min) / y_interval + 1)
                if kwargs.get('y_minor_count') is not None:
                    y_minor_count = kwargs.get('y_minor_count')
                else:
                    if y_major_count <= 11:
                        y_minor_count = 4
                    else:
                        y_minor_count = 1
                # 设置大刻度的数量
                ax.yaxis.set_major_locator(LinearLocator(y_major_count))
                # 设置小刻度的数量
                ax.yaxis.set_minor_locator(LinearLocator((y_major_count - 1) * (y_minor_count + 1) + 1))

        if 'tick_font' in kwargs:
            self.tick_font = kwargs['tick_font']
        if 'tick_font_color' in kwargs:
            self.tick_font_color = kwargs['tick_font_color']
        if 'tick_font_size' in kwargs:
            self.tick_font_size = kwargs['tick_font_size']
        if self.tick_font is not None or self.tick_font_color is not None or self.tick_font_size is not None:
            set_tick_font(ax, font=self.tick_font, color=self.tick_font_color, font_size=self.tick_font_size)

        # 设置图片注释文字
        if 'annotate' in kwargs:
            annotate = kwargs.get('annotate')
            self.annotate_font = kwargs.get('annotate_font', self.annotate_font)
            self.annotate_font_color = kwargs.get('annotate_font_color', self.annotate_font_color)
            self.annotate_font_size = kwargs.get('annotate_font_size', self.annotate_font_size)
            if 'font_size' in annotate:
                font_size = annotate.get('font_size')
            else:
                font_size = self.annotate_font_size
            if 'font_color' in annotate:
                font_color = annotate.get('font_color')
            else:
                font_color = self.annotate_font_color
            for k in annotate:
                add_annotate(ax, annotate[k], k, fontsize=font_size,
                             color=font_color, font=self.annotate_font)

    @classmethod
    def plot_density_scatter(cls, ax, x, y, marker='o', alpha=1, marker_size=5, zorder=100):
        pos = np.vstack([x, y])
        kernel = stats.gaussian_kde(pos)
        color = kernel.evaluate(pos)
        # color = color / 100  # 除100，变为小数
        # norm = plt.Normalize()
        # norm.autoscale(color)
        cmap = plt.get_cmap('jet')
        r = ax.scatter(x, y, c=color, s=marker_size, marker=marker,
                       cmap=cmap, lw=0,
                       alpha=alpha, zorder=zorder)
        return r

    @classmethod
    def plot_hexbin(cls, ax, x, y, alpha=1, zorder=100):
        cmap = plt.get_cmap('jet')
        r = ax.hexbin(x, y, cmap=cmap, alpha=alpha, zorder=zorder, mincnt=1)
        return r

    @classmethod
    def plot_regression_line(cls, ax, x, y, w, x_range=None, color='r', linewidth=1.2, zorder=100):
        ab = np.polyfit(x, y, 1, w=w)
        a = ab[0]
        b = ab[1]
        if x_range is not None:
            x_min, x_max = x_range
        else:
            x_min = np.nanmin(x)
            x_max = np.nanmax(x)
        ax.plot([x_min, x_max], [x_min * a + b, x_max * a + b],
                color=color, linewidth=linewidth, zorder=zorder)
        return ab

    @classmethod
    def plot_diagonal_line(cls, ax, x=None, y=None, x_range=None, y_range=None,
                           color='#808080', linewidth=1.2, zorder=100):
        if x_range is not None and y_range is not None:
            x_min, x_max = x_range
            y_min, y_max = y_range
        elif x is not None and y is not None:
            x_min, x_max = np.min(x), np.max(x)
            y_min, y_max = np.min(y), np.max(y)
        else:
            return
        ax.plot([x_min, x_max], [y_min, y_max], color=color,
                linewidth=linewidth, zorder=zorder)

    @classmethod
    def plot_bias_line(cls, ax, x=None, x_range=None, color='#FF33E0', linewidth=1.2, zorder=100):
        if x_range is not None:
            x_min, x_max = x_range
        elif x is not None:
            x_min, x_max = np.min(x), np.max(x)
        else:
            return
        x_ = np.array((x_min, x_max))
        y_ = x_ * 1.2 + 0.05
        ax.plot(x_, y_, color=color, linewidth=linewidth, zorder=zorder)
        y_ = x_ * 0.8 - 0.05
        ax.plot(x_, y_, color=color, linewidth=linewidth, zorder=zorder)

    @classmethod
    def plot_time_series(cls, ax, data_x, data_y, marker=None, marker_size=None,
                         marker_facecolor=None, marker_edgecolor=None, marker_edgewidth=None,
                         color=None, alpha=None, line_width=None, label=None, zorder=None):
        """
        :param ax:
        :param data_x:
        :param data_y:
        :param marker:
        :param marker_size:
        :param marker_facecolor:
        :param marker_edgecolor:
        :param marker_edgewidth:
        :param color:
        :param alpha:
        :param line_width:
        :param label:
        :param zorder:
        :return:
        """
        if marker is None:
            marker = 'o'
        if marker_size is None:
            marker_size = 6
        if marker_edgecolor is None:
            marker_edgecolor = 'b'
        if marker_edgewidth is None:
            marker_edgewidth = 0.3
        if color is None:
            color = 'b'
        if alpha is None:
            alpha = 1.0
        if zorder is None:
            zorder = 100

        ax.plot(data_x, data_y, marker, ms=marker_size, lw=line_width, c=color,
                markerfacecolor=marker_facecolor, markeredgecolor=marker_edgecolor,
                mew=marker_edgewidth, alpha=alpha, label=label, zorder=zorder)

    @classmethod
    def plot_time_series_omb(
            cls, ax, data_x, data_a, data_b, date_start, date_end, y_range, y_res=0.2,
            vmin=-4.0, vmax=4.0):

        y_min, y_max = y_range
        yy = np.arange(y_min, y_max, y_res) + y_res / 2.  # 一列的y值
        grid = np.ones(len(data_x)) * yy.reshape(-1, 1)

        aa = data_a * np.ones((len(grid), 1))
        bb = data_b * np.ones((len(grid), 1))

        grid = grid - np.divide((grid - bb), aa)

        # zz 要画的值
        x_size = (date_end - date_start).days
        if x_size <= 0:
            raise ValueError(u'时间间隔小于1')
        zz = np.full((len(yy), x_size), -65535)  # 将值填充为 - ，以前填充0
        zz = np.ma.masked_where(zz == -65535, zz)

        j = 0
        xx = []  # 一行的x值
        for i in range(x_size):  # 补充缺失数据的天
            date_i = date_start + relativedelta(days=i)
            xx.append(date_i)
            if j < len(data_x) and data_x[j] == date_i:
                zz[:, i] = grid[:, j]
                j = j + 1
        norm = mpl.colors.Normalize(vmin=vmin, vmax=vmax)
        ax.pcolormesh(xx, yy, zz, cmap='jet', norm=norm, shading='flat', zorder=0)
        return norm

    @classmethod
    def plot_bar(cls, ax, x, y, annotate=None, width=None, color=None, label=None):
        ax.bar(x, y, width=width, align="center",
               color=color, linewidth=0, label=label)
        if annotate is not None:
            for _x, _y, _annotate in zip(x, y, annotate):
                if _y > 0:
                    ax.text(_x, _y + 0.2,
                            "{}".format(_annotate), ha="center",
                            fontsize=6, fontproperties=FONT_MONO)

    @classmethod
    def plot_zero_line(cls, ax, data=None, x_range=None, line_color=None, line_width=None):
        """
        绘制 y = 0 线
        :param x_range:
        :param data:
        :param ax:
        :param line_color:
        :param line_width:
        :return:
        """
        if line_color is None:
            line_color = '#808080'
        if line_width is None:
            line_width = 1.0

        if x_range is not None:
            x_axis_min, x_axis_max = x_range
        elif data is not None:
            x_axis_min = np.nanmin(data)
            x_axis_max = np.nanmax(data)
        else:
            return

        ax.plot([x_axis_min, x_axis_max], [0, 0], color=line_color, linewidth=line_width)

    @classmethod
    def plot_background_fill(cls, ax, x=None, y1=None, y2=None, color=None, alpha=None,
                             zorder=None):
        """
        :param ax:
        :param x:
        :param y1:
        :param y2:
        :param color:
        :param alpha:
        :param zorder:
        :return:
        """
        if color is None:
            color = 'r'
        if alpha is None:
            alpha = 1.0
        if zorder is None:
            zorder = 100
        ax.fill_between(x, y1, y2, facecolor=color, edgecolor=color, alpha=alpha, zorder=zorder,
                        interpolate=True)

    @classmethod
    def set_timeseries_x_locator(cls, ax, xlim_min, xlim_max):
        """
        :param ax:
        :param xlim_min: datetime 数据
        :param xlim_max: datetime 数据
        :return:
        """
        day_range = (xlim_max - xlim_min).days
        if day_range <= 6:
            return
        if day_range <= 60:
            days = mdates.DayLocator(interval=(day_range / 6))
            ax.xaxis.set_major_locator(days)
            ax.xaxis.set_major_formatter(mdates.DateFormatter("%m/%d"))
        else:
            month_range = day_range / 30
            if month_range <= 12.:
                months = mdates.MonthLocator()  # every month
                ax.xaxis.set_major_locator(months)
                ax.xaxis.set_major_formatter(mdates.DateFormatter("%b"))
            elif month_range <= 24.:
                months = mdates.MonthLocator(interval=2)
                ax.xaxis.set_major_locator(months)
                ax.xaxis.set_major_formatter(mdates.DateFormatter("%b"))
            elif month_range <= 48.:
                months = mdates.MonthLocator(interval=4)
                ax.xaxis.set_major_locator(months)
                ax.xaxis.set_major_formatter(mdates.DateFormatter("%b"))
            else:
                years = mdates.YearLocator()
                ax.xaxis.set_major_locator(years)
                ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))

            if month_range <= 48:
                PlotAx.add_year_xaxis(ax, xlim_min, xlim_max)

    @classmethod
    def add_year_xaxis(cls, ax, xlim_min, xlim_max):
        """
        add year xaxis
        :param ax:
        :param xlim_min:
        :param xlim_max:
        :return:
        """
        if xlim_min.year == xlim_max.year:
            ax.set_xlabel(xlim_min.year, fontsize=12, fontproperties=FONT_MONO)
            return
        newax = ax.twiny()
        newax.set_frame_on(True)
        newax.grid(False)
        newax.patch.set_visible(False)
        newax.xaxis.set_ticks_position("bottom")
        newax.xaxis.set_label_position("bottom")
        newax.set_xlim(xlim_min, xlim_max)
        newax.xaxis.set_major_locator(mdates.YearLocator())
        newax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
        newax.spines["bottom"].set_position(("outward", 20))
        newax.spines["bottom"].set_linewidth(0.6)

        newax.tick_params(which="both", direction="in")
        set_tick_font(newax, font_size=12, color="#000000", font=FONT0)
        newax.xaxis.set_tick_params(length=5)

    @classmethod
    def add_colorbar_horizontal(cls, ax, valmin, valmax,
                                cmap='jet',
                                fmt="%d", extend="neither", bounds=None, unit=None,
                                font=FONT0, font_size=8, edge_lw=0.6):

        """
        在fig上添加水平colorbar
        """
        norm = mpl.colors.Normalize(vmin=valmin, vmax=valmax)
        cb = mpl.colorbar.ColorbarBase(ax, cmap=cmap,
                                       norm=norm, extend=extend,
                                       boundaries=bounds,
                                       ticks=bounds,
                                       orientation='horizontal', format=fmt)
        # font of colorbar
        for l in ax.yaxis.get_ticklabels():
            l.set_fontproperties(font)
            l.set_fontsize(font_size)
        if unit:
            cb.ax.set_title(unit, y=1.01,
                            fontproperties=font, fontsize=font_size)
        cb.outline.set_linewidth(edge_lw)


class PlotFigure(object):
    def __init__(self):
        pass

    @classmethod
    def add_ax(cls, fig, pos_x, pos_y, width, height):
        """
        添加子图ax
        :param fig: plt.figure
        :param pos_x: 左下角位置 百分比形式
        :param pos_y: 左下角位置 百分比形式
        :param width: 子图宽 百分比形式
        :param height: 子图高 百分比形式
        :return:
        """
        return fig.add_axes([pos_x, pos_y, width, height])

    @classmethod
    def add_image(cls, fig, imgsize, image_path, position='LB'):
        """
        :param fig: matplotlib.figure
        :param imgsize: (width, high)
        :param image_path:
        :param position: 'LB' or 'RB'
        :return:
        """
        img_width, img_high = imgsize
        if position == 'LB':
            rect = [0, 0, img_width, img_high]
        elif position == 'RB':
            rect = [1. - img_width, 0, img_width, img_high]
        else:
            raise KeyError
        image = img.imread(image_path)
        ax = fig.add_axes(rect, anchor='C')
        ax.axis('off')
        ax.imshow(image)
        return fig, ax


def colormap_blue2red():
    """
    自定义colormap 蓝到红
    """
    clst = [(0, '#0000ff'),
            (0.333, '#00ffff'),
            (0.667, '#ffff00'),
            (1, '#ff0000')]
    return colors.LinearSegmentedColormap.from_list('b2r', clst)


def get_colormap():
    """
    自定义colormap
    :return:
    """
    color_list = ['#000081', '#0000C8', '#1414FF', '#A3A3FF', '#FFA3A3', '#FF1414',
                  '#C70000', '#810000']
    cmap = colors.ListedColormap(color_list, 'indexed')
    return cmap


def set_tick_font(ax, font_size=None, color=None, font=None):
    """
    设定刻度的字体
    """
    for tick in ax.xaxis.get_major_ticks():
        if font is not None:
            tick.label1.set_fontproperties(font)
        if color is not None:
            tick.label1.set_color(color)
        if font_size is not None:
            tick.label1.set_fontsize(font_size)
    for tick in ax.yaxis.get_major_ticks():
        if font is not None:
            tick.label1.set_fontproperties(font)
        if color is not None:
            tick.label1.set_color(color)
        if font_size is not None:
            tick.label1.set_fontsize(font_size)


def add_label(ax, label, local, fontsize=11, fontproperties=FONT_MONO):
    """
    添加子图的标签
    :param fontproperties:
    :param fontsize:
    :param ax:
    :param label:
    :param local:
    :return:
    """
    if label is None:
        return
    if local == "xlabel":
        ax.set_xlabel(label, fontsize=fontsize, fontproperties=fontproperties)
    elif local == "ylabel":
        ax.set_ylabel(label, fontsize=fontsize, fontproperties=fontproperties)


def add_annotate(ax, strlist, local, color="#000000", fontsize=11, font=FONT_MONO):
    """
    添加上方注释文字
    loc must be "left_top" or "right_top"
    or "left_bottom" or "right_bottom"
    格式 ["annotate1", "annotate2"]
    """
    if strlist is None:
        return
    xticklocs = ax.xaxis.get_ticklocs()
    yticklocs = ax.yaxis.get_ticklocs()

    x_step = (xticklocs[1] - xticklocs[0])
    x_toedge = x_step / 6.
    y_toedge = (yticklocs[1] - yticklocs[0]) / 6.

    xlim = ax.get_xlim()
    ylim = ax.get_ylim()
    if local == "left_top":
        ax.text(xlim[0] + x_toedge, ylim[1] - y_toedge,
                "\n".join(strlist), ha="left", va="top", color=color,
                fontsize=fontsize, fontproperties=font, zorder=100)

    elif local == "right_top":
        ax.text(xlim[1] - x_toedge, ylim[1] - y_toedge,
                "\n".join(strlist), ha="right", va="top", color=color,
                fontsize=fontsize, fontproperties=font, zorder=100)

    elif local == "left_bottom":
        ax.text(xlim[0] + x_toedge, ylim[0] + y_toedge,
                "\n".join(strlist), ha="left", va="bottom", color=color,
                fontsize=fontsize, fontproperties=font, zorder=100)
    elif local == "right_bottom":
        ax.text(xlim[1] - x_toedge, ylim[0] + y_toedge,
                "\n".join(strlist), ha="right", va="bottom", color=color,
                fontsize=fontsize, fontproperties=font, zorder=100)
    else:
        return


def get_month_avg_std(date_day, value_day):
    """
    由日数据生成月平均数据
    :param date_day: (list) [datetime 实例]
    :param value_day: (list)
    :return: (date_month, avg_month, std_month)
    """
    date_month = []
    avg_month = []
    std_month = []

    date_day = np.array(date_day)
    value_day = np.array(value_day)

    ymd_start = np.nanmin(date_day)  # 第一天日期
    ymd_end = np.nanmax(date_day)  # 最后一天日期
    month_date_start = ymd_start - relativedelta(days=(ymd_start.day - 1))  # 第一个月第一天日期

    while month_date_start <= ymd_end:
        # 当月最后一天日期
        month_date_end = month_date_start + relativedelta(months=1) - relativedelta(days=1)

        # 查找当月所有数据
        month_idx = np.logical_and(date_day >= month_date_start, date_day <= month_date_end)
        value_month = value_day[month_idx]

        avg = np.nanmean(value_month)
        std = np.nanstd(value_month)
        # date_month = np.append(date_month, month_date_start + relativedelta(days=14))
        date_month = np.append(date_month, month_date_start)  # 2020-11-10用户改为对齐
        avg_month = np.append(avg_month, avg)
        std_month = np.append(std_month, std)

        month_date_start = month_date_start + relativedelta(months=1)
    return date_month, avg_month, std_month


def get_bar_data(x, y, x_range, step):
    step_seg = []
    mean_seg = []
    std_seg = []
    sample_numbers = []
    x_min, x_max = x_range
    for i in np.arange(x_min, x_max, step):
        idx = np.where(np.logical_and(x >= i, x < (i + step)))[0]

        if idx.size > 0:
            block = y[idx]
            mean_seg.append(np.mean(block))
            std_seg.append(np.std(block))
            sample_numbers.append(len(block))
            step_seg.append(i + step / 2.)

    return np.array(step_seg), np.array(mean_seg), np.array(std_seg), np.array(
        sample_numbers)

# if __name__ == "__main__":
#     t_base_map = Basemap()
#     t_m_patches = mpatches
#     t_m_colors = colors
#     t_colorbar = colorbar
