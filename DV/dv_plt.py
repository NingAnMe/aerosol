# coding: UTF-8
"""
Created on 2016年3月14日

@author: zhangtao
"""
from abc import ABCMeta, abstractmethod
import os
import errno
import matplotlib as mpl

mpl.use('Agg')
# mpl.rcParams['font.sans-serif'] = ['Source Han Sans', 'sans-serif']
import matplotlib.pyplot as plt
from matplotlib.font_manager import FontProperties
from matplotlib.ticker import MultipleLocator, FormatStrFormatter
import matplotlib.dates as mdates
from mpl_toolkits.axes_grid1 import make_axes_locatable
import numpy as np
from math import ceil, floor
from datetime import datetime
from matplotlib.colors import LinearSegmentedColormap
from scipy import stats
from importlib.resources import path
import DV

with path(DV, "") as selfPath:
    pass

RED = '#f63240'
GREEN = '#4cd964'
BLUE = '#1c56fb'
COLOR_Darkgray = '#191e1f'
EDGE_LW = 0.7


def make_sure_dirpath_exists(path):
    dirpath = os.path.dirname(path)
    if dirpath == '':
        return
    try:
        os.makedirs(dirpath)
    except OSError as exception:
        pass


#         if exception.errno != errno.EEXIST:
#             raise


def get_DV_Font(fontName=None):
    """
    载入字体
    'OpenSans-Regular.ttf'
    'simhei.ttf'
    'winhei.ttf' 微软雅黑
    """
    font0 = FontProperties()
    if fontName:
        font_path = selfPath / 'FNT' / fontName
        if font_path.is_file():  # os.path.isfile(font_path):
            font0.set_file(str(font_path))
            return font0
    # 默认字体
    # font_path = str(selfPath / 'FNT' / "SourceHanSansCN-Normal.otf")
    font_path = str(selfPath / 'FNT' / "sarasa-ui-sc-regular.ttf")
    font0.set_file(font_path)
    return font0


def colormap_blue2red():
    """
    自定义colormap 蓝到红
    """
    clst = [(0, '#0000ff'),
            (0.333, '#00ffff'),
            (0.667, '#ffff00'),
            (1, '#ff0000')]
    return LinearSegmentedColormap.from_list('b2r', clst)


def str_len(str_in):
    """
    返回字符串实际长度
    """
    try:
        row_l = len(str_in)
        utf8_l = len(str_in.encode('utf-8'))
        return (utf8_l - row_l) / 2 + row_l
    except:
        return None
    return None


def add_ax(fig, rect):
    """
    添加子图ax
    pos_x, pos_y 子图左下角位置
    width, height 子图宽高
    注意：pos_x, pos_y, width, height 都是百分比形式
    """
    return fig.add_axes(rect)


def add_sub_ax(fig, ax, rect):
    """
    在ax中添加子图
    """
    box = ax.get_position()
    width = box.width
    height = box.height
    inax_position = ax.transAxes.transform(rect[0:2])
    transFigure = fig.transFigure.inverted()
    infig_position = transFigure.transform(inax_position)
    x = infig_position[0]
    y = infig_position[1]
    width *= rect[2]
    height *= rect[3]  # <= Typo was here
    subax = fig.add_axes([x, y, width, height])
    return subax


def add_colorbar_horizontal(colorbar_ax, valmin, valmax,
                            cmap=colormap_blue2red(),
                            fmt="%d", extend="neither",
                            bounds=None, unit=None,
                            fontName="", fontSize=8):
    """
    在fig上添加水平colorbar
    """
    add_colorbar(colorbar_ax, valmin, valmax, "horizontal",
                 cmap=cmap, fmt=fmt, extend=extend,
                 bounds=bounds, unit=unit,
                 fontName=fontName, fontSize=fontSize)


def add_colorbar_vertical(colorbar_ax, valmin, valmax,
                          cmap=colormap_blue2red(),
                          fmt="%d", extend="neither",
                          bounds=None, unit=None,
                          fontName="", fontSize=8):
    """
    在fig上添加垂直colorbar
    """
    add_colorbar(colorbar_ax, valmin, valmax, "vertical",
                 cmap=cmap, fmt=fmt, extend=extend,
                 bounds=bounds, unit=unit,
                 fontName=fontName, fontSize=fontSize)


def add_colorbar(colorbar_ax, valmin, valmax, direction,
                 cmap=colormap_blue2red(),
                 fmt="%d", extend="neither",
                 bounds=None, unit=None,
                 fontName="", fontSize=8):
    """
    在fig上添加 colorbar
    """
    norm = mpl.colors.Normalize(vmin=valmin, vmax=valmax)
    # font of colorbar
    font = get_DV_Font(fontName)
    font.set_size(fontSize)

    if direction == "horizontal":
        cb = mpl.colorbar.ColorbarBase(colorbar_ax, cmap=cmap,
                                       norm=norm, extend=extend,
                                       boundaries=bounds,
                                       ticks=bounds,
                                       orientation='horizontal', format=fmt)
        for l in colorbar_ax.xaxis.get_ticklabels():
            l.set_fontproperties(font)
            l.set_fontsize(fontSize)
        if unit:
            cb.ax.set_title(unit, x=1.04, y=-1.9,
                            fontproperties=font, fontsize=fontSize)
    else:  # vertical
        cb = mpl.colorbar.ColorbarBase(colorbar_ax, cmap=cmap,
                                       norm=norm, extend=extend,
                                       boundaries=bounds,
                                       ticks=bounds,
                                       orientation='vertical', format=fmt)
        for l in colorbar_ax.yaxis.get_ticklabels():
            l.set_fontproperties(font)
            l.set_fontsize(fontSize)
        if unit:
            cb.ax.set_title(unit, y=1.01,
                            fontproperties=font, fontsize=fontSize)
    #     colorbar_ax.xaxis.label.set_font_properties(font) # colorbar's label
    cb.outline.set_linewidth(EDGE_LW)


class dv_base(object):
    """
    dv基类
    """
    __metaclass__ = ABCMeta

    def __init__(self, fig=None, **kwargs):
        """
        Constructor
        """
        # limit
        # 用户限制最大最小
        self.xlim_min = None
        self.xlim_max = None
        self.ylim_min = None
        self.ylim_max = None
        # 实际数据最大最小
        self.xmin = None
        self.xmax = None
        self.ymin = None
        self.ymax = None
        # text
        self.title = ''
        self.xlabel = ''
        self.ylabel = ''
        # font size
        self.fontsize_tick = 10  # 刻度字体大小
        self.fontsize_label = 10  # xy轴名字字体大小
        self.fontsize_title = 10.5  # 标题字体大小
        # font
        if "font" in kwargs:
            self.fontName = kwargs["font"]
        else:
            # self.fontName = "SourceHanSansCN-Normal.otf"
            self.fontName = "sarasa-ui-sc-regular.ttf"

        self.font = get_DV_Font(self.fontName)

        # color
        if "theme" in kwargs and kwargs["theme"] == "dark":
            self.theme = "dark"
            plt.style.use(str(selfPath / 'dv_dark.mplstyle'))
        else:
            self.theme = "white"
            plt.style.use(str(selfPath / 'dv_white.mplstyle'))

        #         self.edge_color = '#333333'
        #         self.text_color = '#191e1f'
        # line
        self.line_width = 0.6
        # fig
        if type(fig).__name__ == 'Figure':
            self.fig = fig
        elif type(fig).__name__ == 'tuple' and len(fig) == 2:
            self.fig = plt.figure(figsize=fig)
        elif fig is None and "figsize" in kwargs:
            self.fig = plt.figure(figsize=kwargs["figsize"])
        else:
            self.fig = plt.figure(figsize=(5, 4))  # default size

        if "ax" in kwargs:
            self.ax = kwargs["ax"]
        else:
            ax_kwargs = {}
            kw_keys = ["projection"]
            for eachkey in kw_keys:
                if eachkey in kwargs:
                    ax_kwargs[eachkey] = kwargs[eachkey]
            if "subplot" in kwargs:
                self.ax = self.fig.add_subplot(kwargs["subplot"], **ax_kwargs)
            else:
                self.ax = self.fig.add_subplot(111, **ax_kwargs)

        # legend
        self.leg_len = 0
        self.leg_pad = 0  # legend和图表的距离
        self.leg_loc = -1  # 默认右上框外
        self.show_leg = True
        # colorbar
        self.show_colorbar = True
        self.colorbar_fmt = "%d"
        self.colormap = colormap_blue2red()
        # tick
        self.x_fmt = None
        self.y_fmt = None
        #
        self.title_pos = 1.01
        # 是否已draw
        self.__bl_draw = False
        self.__bl_auto_adjust = False

    def set_title(self, title, fontsize=None):
        self.title = title
        if fontsize:
            self.fontsize_title = fontsize

    def set_xylabel(self, xlabel, ylabel, fontsize=None):
        self.xlabel, self.ylabel = xlabel, ylabel
        if fontsize:
            self.fontsize_label = fontsize

    def set_xminmax(self, xlist):
        if not isinstance(xlist, (list, np.ndarray)):
            raise ValueError("xlist must be list or ndarray, not %s" % type(xlist))
        if len(xlist) == 0: return

        if self.xmin is None:
            self.xmin = np.min(xlist)
        else:
            self.xmin = np.min([np.min(xlist), self.xmin])

        if self.xmax is None:
            self.xmax = np.max(xlist)
        else:
            self.xmax = np.max([np.max(xlist), self.xmax])

    #         if self.xmin == self.xmax:
    #             raise ValueError("xmin and xmax can't be the same, please set different value")

    def set_yminmax(self, ylist):
        if not isinstance(ylist, (list, np.ndarray)):
            raise ValueError("ylist must be list or ndarray, not %s" % type(ylist))
        if len(ylist) == 0: return

        if self.ymin is None:
            self.ymin = np.min(ylist)
        else:
            self.ymin = min(np.min(ylist), self.ymin)

        if self.ymax is None:
            self.ymax = np.max(ylist)
        else:
            self.ymax = max(np.max(ylist), self.ymax)
        if self.ymin == self.ymax:
            raise ValueError("ymin and ymax can't be the same, please set different value")

    def set_xlocator(self, major, minor):
        ax = self.get_main_ax()
        ax.xaxis.set_major_locator(MultipleLocator(major))
        ax.xaxis.set_minor_locator(MultipleLocator(minor))

    def set_ylocator(self, major, minor):
        ax = self.get_main_ax()
        ax.yaxis.set_major_locator(MultipleLocator(major))
        ax.yaxis.set_minor_locator(MultipleLocator(minor))

    def get_main_ax(self):
        return self.ax

    @abstractmethod
    def easyplot(self, *args, **kwargs):
        pass

    def plot(self, *args, **kwargs):
        self.ax.plot(*args, **kwargs)

    #     @staticmethod
    def singleline(self, x1, x2, y1, y2, color, linewidth, zorder=0):
        """
        画一条直线
        """
        self.ax.plot([x1, x2], [y1, y2], color=color, linewidth=linewidth, zorder=zorder)

    def grid(self, bl, which='major', axis='both', **kwargs):
        """
        画坐标网格
        """
        self.ax.grid(bl, which=which, axis=axis, **kwargs)

    def simple_axis(self, mode=1):
        """
        show left bottom of the frame only
        """
        ax = self.get_main_ax()
        if mode <= 1:
            ax.spines['top'].set_visible(False)
            ax.spines['right'].set_visible(False)
        if mode < 1:
            ax.axis('off')

    def subplots_adjust(self, **kwargs):
        self.fig.subplots_adjust(**kwargs)
        self.__bl_auto_adjust = False

    def legend(self, loc=-1):
        """
        图例
        """
        width = self.fig.get_size_inches()[0]
        marker_len = 8
        leg2inch = self.fontsize_tick / 144.
        right_inch = (self.leg_len + self.leg_pad + marker_len) * leg2inch

        if self.__bl_auto_adjust:
            self.fig.subplots_adjust(right=1. - right_inch / width)

        ax = self.get_main_ax()

        font_leg = get_DV_Font(self.fontName)
        font_leg.set_size(self.fontsize_label)

        kwargs = {"prop": font_leg}
        if loc == -1:
            kwargs["bbox_to_anchor"] = (1.01, 1)  # (1. + self.leg_pad * leg2inch / (width - right_inch), 1)
            kwargs["frameon"] = False
            kwargs["borderaxespad"] = 0
            ax.legend(loc="upper left", **kwargs)
        else:
            kwargs["frameon"] = True
            kwargs["borderaxespad"] = 0.5
            ax.legend(loc=loc)

    def annotate(self, strlist, loc='left', color='#303030', fontsize=10):
        """
        添加上方注释文字
        strlist  2d list
        loc must be 'left' or 'right'
        """
        if strlist is None: return

        #         self.show_tight = False  # or else can't get correct position

        ax = self.get_main_ax()
        point_bl = ax.get_position().get_points()[0]  # 左下
        point_tr = ax.get_position().get_points()[1]  # 右上

        width, height = self.fig.get_size_inches()

        x_toedge = fontsize / 120. / width
        y_toedge = x_toedge

        font_annotation = get_DV_Font('DroidSansMono.ttf')  # 注释用等宽字体
        font_annotation.set_size(fontsize)

        if loc == 'left':
            x = point_bl[0]
            y = point_tr[1] - y_toedge
            # print(x, y)
            for i, eachCol in enumerate(strlist):

                col_width = 0
                for eachStr in eachCol:
                    if str_len(eachStr) > col_width:
                        col_width = str_len(eachStr)
                x_step = fontsize / 120. * col_width / width
                x = x + x_toedge
                self.fig.text(x, y,
                              '\n'.join(eachCol), ha=loc, va='top', color=color,
                              fontsize=fontsize, fontproperties=font_annotation)
                x = x + x_step + x_toedge

        elif loc == 'right':
            x = point_tr[0]
            y = point_tr[1] - y_toedge

            for i, eachCol in enumerate(strlist):
                col_width = 0
                for eachStr in eachCol:
                    if str_len(eachStr) > col_width:
                        col_width = str_len(eachStr)
                x_step = fontsize / 120. * col_width / width
                x = x - x_toedge
                self.fig.text(x, y,
                              '\n'.join(eachCol), ha=loc, va='top', color=color,
                              fontsize=fontsize, fontproperties=font_annotation)
                x = x - x_step - x_toedge

    def set_tick_font(self, ax=None, fontsize=None):
        """
        设定刻度的字体
        """
        if ax is None:
            ax = self.get_main_ax()
        if fontsize:
            self.fontsize_tick = fontsize

        for tick in ax.xaxis.get_major_ticks():
            tick.label1.set_fontproperties(self.font)
            tick.label1.set_fontsize(self.fontsize_tick)
        for tick in ax.yaxis.get_major_ticks():
            tick.label1.set_fontproperties(self.font)
            tick.label1.set_fontsize(self.fontsize_tick)

    def custom_style(self):
        pass

    def draw(self):
        """
        画图
        """
        ax = self.get_main_ax()
        # xy limit
        ax.set_xlim(self.xlim_min, self.xlim_max)
        ax.set_ylim(self.ylim_min, self.ylim_max)

        # tick
        # if self.x_fmt:
        #     ax.xaxis.set_major_formatter(FormatStrFormatter(self.x_fmt))
        # if self.y_fmt:
        #     ax.yaxis.set_major_formatter(FormatStrFormatter(self.y_fmt))
        if self.x_fmt:
            ax.xaxis.set_major_formatter(self.x_fmt)
        if self.y_fmt:
            ax.yaxis.set_major_formatter(self.y_fmt)
        # text
        if self.title != '':
            # font = get_DV_Font("SourceHanSansCN-Medium.otf")
            font = get_DV_Font("sarasa-ui-sc-semibold.ttf")
            tt = ax.set_title(self.title, fontproperties=font, fontsize=self.fontsize_title)
            tt.set_y(self.title_pos)  # set gap space below title and subplot
        if self.xlabel != '':
            self.ax.set_xlabel(self.xlabel, fontproperties=self.font, fontsize=self.fontsize_label)
        if self.ylabel != '':
            self.ax.set_ylabel(self.ylabel, fontproperties=self.font, fontsize=self.fontsize_label)
        #
        self.custom_style()
        #
        self.set_tick_font(ax)

        if int(mpl.__version__.split(".")[0]) < 2:
            spines = ax.spines
            for eachspine in spines:
                spines[eachspine].set_linewidth(EDGE_LW)
        else:
            ax.tick_params(which='both', width=EDGE_LW)

        if self.show_leg:
            self.legend(loc=self.leg_loc)

        self.__bl_draw = True

    def suptitle(self, title, fontName="sarasa-ui-sc-bold.ttf", fontsize=15, **kwargs):
        font = get_DV_Font(fontName)
        font.set_size(fontsize)
        return self.fig.suptitle(title, fontproperties=font, fontsize=fontsize, **kwargs)

    def title_center(self, title):
        ax = self.get_main_ax()
        return ax.set_title(title, fontproperties=self.font, fontsize=self.fontsize_title)

    def title_left(self, title, fontName="sarasa-ui-sc-semibold.ttf"):
        font = get_DV_Font(fontName)
        ax = self.get_main_ax()
        return ax.set_title(title, fontproperties=font, fontsize=self.fontsize_title, loc='left')

    def title_right(self, title, fontName="sarasa-ui-sc-semibold.ttf"):
        font = get_DV_Font(fontName)
        ax = self.get_main_ax()
        return ax.set_title(title, fontproperties=font, fontsize=self.fontsize_title, loc='right')

    def add_logo(self, ax_loc, logo_path):
        ax1 = self.fig.add_axes(ax_loc, frameon=False)
        im = plt.imread(logo_path)
        ax1.imshow(im)
        ax1.set_axis_off()

    def savefig(self, figPath, dpi=300, compress=False, **kwargs):
        """
        保存图像
        """
        if not self.__bl_draw:
            self.draw()  # 已调用过，就不再调用self.draw()

        # save pic
        if type(figPath) == str:
            make_sure_dirpath_exists(figPath)
        if compress:
            from subprocess import Popen, PIPE
            import cStringIO
            sio = cStringIO.StringIO()
            self.fig.savefig(sio, format="png", dpi=dpi)

            cmd = ["/g4/zhuzhi/kts/gun_lib/bin/pngquant", "-f", "-o", figPath, "-"]
            p1 = Popen(cmd, stdin=PIPE)
            output = p1.communicate(sio.getvalue())[0]
        else:
            self.fig.savefig(figPath, dpi=dpi, **kwargs)

    def clean(self):
        self.fig.clf()
        plt.cla()
        plt.clf()
        plt.close()

    def errorbar(self, x, y, **kwargs):
        self.ax.errorbar(x, y, **kwargs)

    def bar(self, x, y, width, **kwargs):
        self.ax.bar(x, y, width, **kwargs)


class dv_line_chart(dv_base):
    """
    折线图
    """

    def __init__(self, *args, **kwargs):
        """
        Constructor
        """
        super(dv_line_chart, self).__init__(*args, **kwargs)
        self.show_leg = True
        self.y2lim_min = None
        self.y2lim_max = None

    def easyplot(self, x, y, color, name, marker='o-', markersize=4.2, mec=None, mew=None, alpha=1):
        """
        画折线
        marker = 'o-' or '-'
        """
        mfc = color
        if mec is None:
            mec = color
        if mew is None:
            mew = self.line_width
        self.leg_len = max(str_len(name), self.leg_len)
        self.set_xminmax(x)
        self.set_yminmax(y)
        self.ax.plot(x, y, marker, mfc=mfc, mec=mec, color=color,
                     ms=markersize, mew=mew, lw=self.line_width,
                     label=name, alpha=alpha)

    def twinx(self, x, y, color, name, ylabel='', marker='o-', markersize=5, mec=None, mew=None, alpha=1):
        mfc = color
        if mec is None:
            mec = color
        if mew is None:
            mew = self.line_width
        ax = self.get_main_ax()
        ax2 = ax.twinx()
        ax2.plot(x, y, marker, mfc=mfc, mec=mec,
                 ms=markersize, mew=mew, lw=self.line_width,
                 label=name, alpha=alpha)
        # 为了在ax的 legend里添加ax2的legend
        ax.plot(np.nan, np.nan, marker, mfc=mfc, mec=mec,
                ms=markersize, mew=mew, lw=self.line_width,
                label=name, alpha=alpha)
        ax2.set_ylim(self.y2lim_min, self.y2lim_max)

        len_y2 = max(len(str(max(y))), len(str(min(y))))
        if (self.y2lim_max is not None) and (len(str(self.y2lim_max)) > len_y2):
            len_y2 = len(str(self.y2lim_max))
        if (self.y2lim_min is not None) and (len(str(self.y2lim_min)) > len_y2):
            len_y2 = len(str(self.y2lim_min))

        y2label_width = 4
        self.leg_pad = max(len_y2 + y2label_width, self.leg_pad)
        if ylabel != '':
            ax2.set_ylabel(ylabel, fontproperties=self.font, fontsize=self.fontsize_label)

        for tick in ax2.yaxis.get_major_ticks():
            tick.label2.set_fontproperties(self.font)
            tick.label2.set_fontsize(self.fontsize_tick)

        ax.grid(False)
        ax2.grid(False)


class dv_time_series(dv_line_chart):
    """
    时序图
    """

    def __init__(self, *args, **kwargs):
        """
        Constructor
        """
        super(dv_time_series, self).__init__(*args, **kwargs)
        self.hour_interval = 4
        self.hour_showfmt = '%Hh'
        self.hour_showYMD = True
        self.auto_settime = True

    def custom_style(self):

        self.ax.set_xlabel('')

        ax = self.get_main_ax()
        if self.xlim_min is None or self.xlim_max is None:
            self.xlim_min, self.xlim_max = self.xmin, self.xmax
        if self.ylim_min is None or self.ylim_max is None:
            self.ylim_min, self.ylim_max = self.ymin, self.ymax

        if self.xlim_max == self.xlim_min:
            # TODO:
            pass

        if not self.auto_settime:
            return

        day_range = (self.xlim_max - self.xlim_min).days

        if day_range <= 2:
            hloc = mdates.HourLocator(interval=self.hour_interval)
            ax.xaxis.set_major_locator(hloc)
            ax.xaxis.set_major_formatter(mdates.DateFormatter(self.hour_showfmt))

            if self.hour_showYMD:
                if self.xmin.date() == self.xmax.date():
                    xlabel = self.xmin.strftime('%Y%m%d')
                else:
                    xlabel = '%s - %s' % (self.xmin.strftime('%Y%m%d'),
                                          self.xmax.strftime('%Y%m%d'))
                ax.set_xlabel(xlabel, fontproperties=self.font, fontsize=self.fontsize_label)
        else:
            ticker_step = int(ceil(day_range / 10.))  # 分10个格

            if ticker_step > 140:
                years = mdates.YearLocator(int(ceil(ticker_step / 365.)))
                ax.xaxis.set_major_locator(years)
                ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))

            else:
                if ticker_step > 6:
                    months = mdates.MonthLocator(interval=int(ceil(ticker_step / 30.)))
                    ax.xaxis.set_major_locator(months)
                    # ax.xaxis.set_major_formatter(mdates.DateFormatter('%b'))

                else:
                    days = mdates.DayLocator(interval=ticker_step)
                    ax.xaxis.set_major_locator(days)
                    ax.xaxis.set_major_formatter(mdates.DateFormatter('%m/%d'))
                # self.add_year_xaxis(ax, self.xmin, self.xmax)

    def add_year_xaxis(self, ax, xlim_min, xlim_max):
        """
        add year xaxis
        """
        if xlim_min.year == xlim_max.year:
            ax.set_xlabel(xlim_min.year, fontproperties=self.font, fontsize=self.fontsize_label)
            return
        ax_twiny = ax.twiny()
        ax_twiny.set_frame_on(True)
        ax_twiny.grid(False)
        ax_twiny.patch.set_visible(False)
        ax_twiny.xaxis.set_ticks_position('bottom')
        ax_twiny.xaxis.set_label_position('bottom')
        ax_twiny.set_xlim(xlim_min, xlim_max)
        ax_twiny.xaxis.set_major_locator(mdates.YearLocator())
        ax_twiny.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
        ax_twiny.spines['bottom'].set_position(('outward', 20))
        ax_twiny.spines['bottom'].set_linewidth(self.line_width)

        ax_twiny.tick_params(which='both', direction='in')
        self.set_tick_font(ax_twiny)
        ax_twiny.xaxis.set_tick_params(length=5)


class dv_scatter(dv_base):
    """
    散点图
    """

    def __init__(self, *args, **kwargs):
        """
        Constructor
        """
        super(dv_scatter, self).__init__(*args, **kwargs)
        self.show_leg = False

    def regression(self, slope, intercept, color):
        """
        画回归线 和 对角线
        TODO: 添加对角线正负5度线
        """
        if None in [self.xlim_max, self.ylim_max, self.xlim_min, self.ylim_min]:
            raise ValueError('Must set value of xlim_min, xlim_max, ylim_min, max first')

        #         self.xlim_max = self.ylim_max = max(self.xlim_max, self.ylim_max)
        #         self.xlim_min = self.ylim_min = min(self.xlim_min, self.ylim_min)

        # 对角线
        self.singleline(self.xlim_min, self.xlim_max, self.ylim_min, self.ylim_max, '#cccccc', self.line_width)
        # 回归线
        self.singleline(self.xlim_min, self.xlim_max,
                        slope * self.xlim_min + intercept, slope * self.xlim_max + intercept,
                        color, self.line_width, zorder=100)

    def regression_k(self, k0, k1, k2, color):
        """
        画回归线 和 对角线
        TODO: 添加对角线正负5度线
        """
        if None in [self.xlim_max, self.ylim_max, self.xlim_min, self.ylim_min]:
            raise ValueError('Must set value of xlim_min, xlim_max, ylim_min, max first')

        #         self.xlim_max = self.ylim_max = max(self.xlim_max, self.ylim_max)
        #         self.xlim_min = self.ylim_min = min(self.xlim_min, self.ylim_min)

        # 对角线
        self.singleline(self.xlim_min, self.xlim_max, self.ylim_min, self.ylim_max, '#cccccc', self.line_width)
        # 回归线
        x = np.linspace(self.xlim_min, self.xlim_max, 100)
        y = x * x * k0 + x * k1 + k2
        self.ax.plot(x, y, color=color, linewidth=self.line_width, zorder=100)

    def easyplot(self, x, y, color, name, marker='o', markersize=10, alpha=1, norm=None, **kwargs):
        """
        画散点
        color = "r"， "g"， "b" 或 "#191e1f" 等时，散点都是一个颜色
        color = "density" 时，散点按照密度着色
        """

        self.leg_len = max(str_len(name), self.leg_len)
        self.set_xminmax(x)
        self.set_yminmax(y)
        show_density_colorbar = False
        if type(color).__name__ == "str" and color == "density":
            color = self.get_density(x, y)
            print(np.min(color), np.max(color))
            norm = plt.Normalize()
            norm.autoscale(color)
            show_density_colorbar = True

        c = self.ax.scatter(x, y, marker=marker, c=color, norm=norm, cmap=self.colormap,
                            s=markersize, lw=0, label=name,
                            alpha=alpha, **kwargs)
        # fixme:
        if show_density_colorbar and self.show_colorbar:
            colorbar = self.fig.colorbar(c, ax=self.ax)
            colorbar.ax.set_title("normalized frequency\nof occurrence", y=1.01,
                                  fontproperties=self.font, fontsize=self.fontsize_label)
            for l in colorbar.ax.yaxis.get_ticklabels():
                l.set_fontproperties(self.font)

    def get_density(self, x, y, **kwargs):
        """
        取得密度
        """

        # try:
        #     from sklearn.neighbors import KernelDensity
        #     print("sklearn")
        #     kde_skl = KernelDensity(bandwidth=0.1, kernel="gaussian", rtol=1e-4, **kwargs)
        #     xy_train = pos.T
        #     kde_skl.fit(xy_train)
        #
        #     # score_samples() returns the log-likelihood of the samples
        #     log_pdf = kde_skl.score_samples(xy_train)
        #     return np.exp(log_pdf)
        # except:
        #     pass
        pos = np.vstack([x, y])
        kernel = stats.gaussian_kde(pos)
        return kernel(pos)


class dv_bar(dv_base):
    """
    柱状图
    """

    def __init__(self, *args, **kwargs):
        """
        Constructor
        """
        super(dv_bar, self).__init__(*args, **kwargs)
        self.show_leg = False

    def easyplot(self, x, y, color, width=0.3, err=None, bottom=None,
                 label='', align='center', pos_offset=0, alpha=1):
        """
        画柱状图
        """

        if x is None:
            x_pos = np.arange(len(y))
            self.set_yminmax(y)
            self.ax.bar(x_pos + pos_offset, y, width,
                        color=color,
                        align=align,
                        linewidth=0.1,
                        bottom=bottom,
                        label=label,
                        yerr=err, alpha=alpha)

        elif isinstance(x[0], str):
            x_pos = np.arange(len(y))
            self.set_yminmax(y)
            self.ax.bar(x_pos + pos_offset, y, width,
                        color=color,
                        align=align,
                        linewidth=0.1,
                        bottom=bottom,
                        label=label,
                        yerr=err, alpha=alpha)
            self.ax.set_xticks(x_pos)
            self.ax.set_xticklabels(x)

        elif y is None:
            y_pos = np.arange(len(x))
            self.set_xminmax(x)
            self.ax.barh(y_pos + pos_offset, x, width,
                         color=color,
                         align=align, linewidth=0.1,
                         left=bottom,
                         label=label,
                         yerr=err, alpha=alpha)

        elif isinstance(y[0], str):
            y_pos = np.arange(len(x))
            self.set_xminmax(x)
            self.ax.barh(y_pos + pos_offset, x, width,
                         color=color,
                         align=align, linewidth=0.1,
                         left=bottom,
                         label=label,
                         yerr=err, alpha=alpha)
            self.ax.set_yticks(y_pos, y)
            self.ax.invert_yaxis()


class dv_hist(dv_base):
    """
    直方图
    """

    def __init__(self, *args, **kwargs):
        """
        Constructor
        """
        super(dv_hist, self).__init__(*args, **kwargs)
        self.show_leg = False

    def easyplot(self, data, bins=256, color=None, cmap=None, range=None, alpha=1, **kwargs):
        """
        画直方图
        data 必须是1维
        cmap 传值时 color 不起作用
        **kwargs  density ...
        """
        #         if len(x) == 0 or len(y) == 0:
        #             return

        cm = plt.cm.get_cmap(cmap)

        n, bins, patches = self.ax.hist(data, bins, color=color, range=range, alpha=alpha, **kwargs)

        self.set_xminmax(data)
        if cmap is not None:
            bin_centers = 0.5 * (bins[:-1] + bins[1:])

            # scale values to interval [0,1]
            col = bin_centers - min(bin_centers)
            col /= max(col)

            for c, p in zip(col, patches):
                plt.setp(p, 'facecolor', cm(c))


class dv_imshow(dv_base):
    """
    矩阵图像
    """

    def __init__(self, *args, **kwargs):
        """
        Constructor
        """
        super(dv_imshow, self).__init__(*args, **kwargs)
        self.show_leg = False
        #         self.show_tight = False
        self.ax_btm = self.ax

    def easyplot(self, array2D, color_values, color_list):
        """
        画2维矩阵图像
        """
        if len(color_values) == len(color_list):
            pass

        else:
            print("color_values, color_list should have same size")
            return

        for i, color in enumerate(color_list):
            if type(color).__name__ == "str" and color.startswith("#"):
                color_list[i] = mpl.colors.hex2color(color)

        row, col = array2D.shape
        array_rgb = np.ones((row, col, 3))

        for i, eachValue in enumerate(color_values):
            index = array2D == eachValue
            array_rgb[index] = color_list[i]
        self.ax.imshow(array_rgb)

        self.ax.axis('off')

    def addRects(self, color_list, color_names, pad="2%", betweenRects=0.04, maxRect=8):
        """
        方块色标
        """
        import matplotlib.patches as mpatches

        divider = make_axes_locatable(self.ax)
        ax2 = divider.append_axes("bottom", "5%", pad=pad)
        self.fig.add_axes(ax2)

        patches = []
        # add a rectangle
        x0 = 0.02
        y0 = 0.1
        recth = 0.7
        rectl = (1. - x0 * 2 - betweenRects * (maxRect - 1)) / maxRect

        for i in range(len(color_list)):
            rect = mpatches.Rectangle((x0, y0), rectl, recth,
                                      ec=COLOR_Darkgray, fc=color_list[i],
                                      fill=True, lw=0.3)
            patches.append(rect)
            ax2.add_patch(rect)
            text = color_names[i]
            x1 = x0 + rectl / 2.
            y1 = y0 - 0.3

            #             self.ax.text(x1, y1, text, ha="center", va="top", size=8,
            #                      fontproperties=self.font, color=self.text_color)
            ax2.text(x1, y1, text, ha="center", va="top", size=8,
                     fontproperties=self.font)  # no: color=self.text_color, try use mplstyle for that
            x0 = x0 + rectl + betweenRects
        ax2.axis('off')


if __name__ == '__main__':
    # example
    x1 = [1, 2, 3, 4, 5, 6, 7]
    y1 = [6, 7, 6, 8, 4, 5, 9]
    x2 = [3, 4, 5, 6, 7, 8, 9]
    y2 = [2, 5, 3, None, 5, 8, 3]
    x3 = [datetime(2015, 3, 1, e) for e in x1]
    x4 = [datetime(2015, 3, 4, e) for e in x2]

    # #     # 散点 -------------------
    #     p = dv_scatter((5.5, 5))
    #     p.show_leg = False
    #     p.easyplot(x1, y1, 'r', u'甲')
    #     p.easyplot(x2, y2, 'b', u'乙')
    #
    #     p.title = u'标题'
    #     p.xlabel = u'X轴'
    #     p.ylabel = u'Y轴'
    # #     p.grid(True)
    # #     p.simple_axis()
    #     p.xlim_min = p.ylim_min = 0
    #     p.xlim_max = p.ylim_max = 10
    #     p.regression(0.9, 0, GREEN)  # 画回归线 a,b,color
    #
    #     strlist = [['2.0322x-0.6106', 'count:2168'], ['a = 0.9', 'b = 0', 'num = 10'], ['a = 0.9', 'b = 0', 'num = 10']]
    #
    #     # 若要调整图表位置 必须在 annotate之前，否则写了文字再改变图标位置，文字并不会跟着改变
    #     p.fig.subplots_adjust(bottom=0.1, top=0.92,  # 下， 上
    #                           left=0.1, right=0.95)  # 左， 右
    #     p.annotate(strlist, color='m', fontsize=9)
    #
    #     p.savefig('sd.png')

    # 折线 -------------------
    #     p1 = dv_line_chart()
    #     p2 = dv_line_chart()
    #     p1.easyplot(x1, y1, 'c', u'w', "x-", mec="c")
    #     p2.easyplot(x2, y2, 'b', u'a', mec="orange")
    #     p1.title = u'折线图1'
    #     p1.xlabel = u'X轴1'
    #     p1.ylabel = u'Y轴1'
    #     p2.title = u'折线图2'
    #     p2.xlabel = u'X轴2'
    #     p2.ylabel = u'Y轴2'
    #     p1.grid(False)
    # #     p.show_leg = False
    # #     p.y2lim_max = 8
    # #     p.y2lim_min = -8
    #     p1.twinx(x1, y2, 'r', "std", ylabel=u"Y轴2", marker='-')
    #     p1.draw()
    #     p2.draw()
    #     plt.show()
    #     p1.savefig('zx1.png')
    #     p2.savefig('zx2.png')

    # #     # 时序-----------------------
    #     import random
    #     fig = plt.figure(figsize=(7, 4))  # 图像大小
    #     p = dv_time_series(fig)
    # #     p.hour_interval = 1
    # #     p.hour_showYMD = False
    #     p.hour_showfmt = "%H:00"
    #     for i in xrange(6):
    #         random.shuffle(y2)
    #         p.easyplot(x4, y2, "None", 'abc' + str(i), mec="r")
    # #     p.title = u'标题'
    # #     p.xlabel = u'X轴'
    #     p.ylabel = u'Y轴'
    #     p.y_fmt = "%0.5f"
    # #     p.show_leg = False
    #     p.savefig('sx.png')

    #     # 柱状图-------------------------
    #     p = dv_bar()
    # #     p.xlim_max = 6
    #     xx = [u"华北地区\n(112°-120°E,\n35°-43°N)", u"东北地区\n(120°-130°E,\n40°-50°N)",
    #            u"长三角地区\n(116°-122°E,\n29°-34°N)", u"珠三角地区\n(111°-116°E,\n21°-24°N)"]
    #     yy = [15.503529052734375, 8.3990676100628932, 10.924575520833335, 5.1214023437499998]
    #
    #     p.easyplot(xx, yy , "#9999ff")
    # #     p.easyplot([5, 3, 2, 4], ["a\na", "bb", "cc", "dd"], "#9999ff",
    # #                bottom=[1, 1, 2, 1])
    #     p.title = u'标题'
    #     p.xlabel = u'x轴'
    #     p.ylabel = u'y轴'
    #     p.savefig('bar.png')

    #     # 直方图-------------------------
    #     data = 0.05 * np.random.randn(500) + 290
    #     p = dv_hist()
    #     p.easyplot(data, cmap="jet")
    # #     p.simple_axis(mode=0)
    #     p.savefig('hist.png')

    # # 2d矩阵色块图
    # array2D = np.random.random_integers(0., 10., (30, 30))
    # p = dv_imshow(figsize=(6, 8))
    #
    # val = [1, 2, 3, 4, 5, 6, 7, 8]
    # color_list = ["#ff0000", "#00ff00", "#0000ff", "#ffff00", "#00ffff", "#ff00ff", "#000000", "#ffffff"]
    # color_names = ['11', '22', '33', u'陆地', u'海洋', '66', '77', '88']
    # p.easyplot(array2D, val, color_list)
    # p.addRects(color_list, color_names)
    #
    # p.title = "ABC"
    # p.savefig('array2d.png')
