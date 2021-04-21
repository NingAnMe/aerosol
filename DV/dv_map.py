# coding: utf-8
'''
Created on 2016年3月16日

@author: zhangtao
'''
import os
import numpy as np
import matplotlib as mpl
import os
os.environ['PROJ_LIB'] = '/opt/KTS_LIB/miniconda3/share/proj'
mpl.use('Agg')
from DV.dv_plt import dv_base, str_len, EDGE_LW, colormap_blue2red, COLOR_Darkgray, \
    get_DV_Font, add_colorbar_horizontal, add_sub_ax
# from pylab import *
import matplotlib.pyplot as plt
from matplotlib.path import Path
from matplotlib.patches import PathPatch, Patch
from matplotlib import colors
from mpl_toolkits.axes_grid1.inset_locator import zoomed_inset_axes
import shapefile
from mpl_toolkits.axes_grid1.axes_divider import make_axes_locatable
from DV.dv_img import linearStretch, norm255
# from dp_prj import prj_gll, fill_points_2d
from importlib.resources import path

from mpl_toolkits.basemap import Basemap
import DV

with path(DV, "") as selfPath:
    pass


class dv_map(dv_base):

    def __init__(self, fig=None, **kwargs):
        '''
        Constructor
        '''
        # fig
        if fig is None and "figsize" not in kwargs:
            self.__fig_resize = True
        else:
            self.__fig_resize = False

        super(dv_map, self).__init__(fig, **kwargs)

        # color_开头： 颜色设定
        self.color_ocean = '#BFEBFD'  # show_ocean_color时启用
        self.color_land = 'w'
        if self.theme == "dark":
            COLOR_littleWhite = "#afafaf"
            COLOR_littleWhite = "#cfcfcf"
            self.color_coast = COLOR_littleWhite
            self.color_contry = COLOR_littleWhite
            self.color_ticker = COLOR_littleWhite
            self.color_china = COLOR_littleWhite
        else:
            self.color_coast = COLOR_Darkgray
            self.color_contry = COLOR_Darkgray
            self.color_ticker = COLOR_Darkgray
            self.color_china = COLOR_Darkgray
        self.color_latlon = "#999999"

        # fontsize_开头： 字体大小
        self.fontsize_tick = 9.5  # 刻度字体大小
        self.fontsize_cbtick = 9  # colorbar刻度字体大小
        self.fontsize_label = 9  # xy轴名字字体大小
        self.fontsize_title = 10  # 标题字体大小
        self.font_mid = get_DV_Font("sarasa-ui-sc-semibold.ttf")

        # show_开头：展示开关
        self.show_leg = False  # legend
        self.show_colorbar = True  # colorbar
        self.show_coastlines = True  # 海岸线
        self.show_countries = False  # 国境线
        self.show_china_boundary = True  # 中国国境
        self.show_china_province = False  # 中国省界
        self.show_china_county = False  # 中国县市界
        self.show_line_of_latlon = True  # 经纬度线
        self.show_china = False  # 中国区域图 左下九段线插曲图
        self.show_china_river = False  # 长江黄河
        self.show_inside_china = False  # 中国境内
        self.show_inside_china_mini = False  # 南海小图中国境内
        self.show_south_pole = False  # 南极极射赤面
        self.show_north_pole = False  # 北极极射赤面
        self.show_north = False  # 南极
        self.show_south = False  # 北极
        self.show_ocean_color = False  # 海陆背景颜色
        #         self.show_tight = False

        self.area = None
        self.areaNameLst = []
        self.clip_path = None
        self.clip_path_mini = None
        self.title_pos = 1.03
        self.box = [90., -90., -180., 180.]  # 经纬度范围 NSWE
        #
        self.m = None  # 主图对象
        self.m2 = None  # 南海小图对象
        self.ax2 = None  # 南海小图ax对象
        self.ax_cb = None  # colorbar ax对象
        self.nanhai_loc = [0.036, 0.068, 0.13, 0.226]
        self.valmin = None
        self.valmax = None
        self.delat = None
        self.delon = None
        self.projection = "cyl"  # 等经纬
        self.resolution = "l"  # resolution = 'h' for high-res coastlines
        self.colormap = colormap_blue2red()  # jet
        self.colorbar_bounds = None
        self.colorbar_unit = None
        self.colorbar_extend = 'neither'
        self.colorbar_label_list = None
        self.norm = None
        # lw: Line Width
        self.lw_china = 0.6
        self.lw_boundray = self.lw_china * 0.618
        self.lw_latlon = 0.2
        self.lw_marker = 0.2
        self.contour_level = 10

    def _map_init(self, showArea=None, ax=None):
        '''
        地图底图
        '''
        if showArea == "China":
            self.projection = "lcc"
            m = Basemap(
                projection=self.projection,
                lat_1=30., lat_2=60, lon_0=105, lat_0=45,
                resolution=self.resolution, ax=ax,
                llcrnrlon=79, llcrnrlat=13.,
                urcrnrlon=145, urcrnrlat=50.)
        elif showArea == "CSJ":  # 长三角
            self.projection = "lcc"
            m = Basemap(
                projection=self.projection,
                lat_1=30., lat_2=60, lon_0=105, lat_0=45,
                resolution=self.resolution, ax=ax,
                llcrnrlon=113.88, llcrnrlat=27.7,
                urcrnrlon=126.17, urcrnrlat=34.00)
        # elif showArea == "":  # 珠三角 华北  东北  京津冀
        #     self.projection = "lcc"
        #     m = Basemap(
        #         projection=self.projection,
        #         lat_1=30., lat_2=60, lon_0=105, lat_0=45,
        #         resolution=self.resolution, ax=ax,
        #         llcrnrlon=113.88, llcrnrlat=26.16,
        #         urcrnrlon=126.17, urcrnrlat=35.8)
        elif showArea == "SouthPole":
            self.projection = "spaeqd"
            m = Basemap(projection=self.projection, boundinglat=-58, lon_0=180,
                        resolution=self.resolution, ax=ax)

        elif showArea == "NorthPole":
            self.projection = "npaeqd"
            m = Basemap(projection=self.projection, boundinglat=58, lon_0=0,
                        resolution=self.resolution, ax=ax)

        elif showArea == "South":
            self.projection = "ortho"
            m = Basemap(projection=self.projection, lon_0=-40, lat_0=-65,
                        resolution=self.resolution, ax=ax)

        elif showArea == "North":
            self.projection = "ortho"
            m = Basemap(projection=self.projection, lon_0=140, lat_0=65,
                        resolution=self.resolution, ax=ax)

        elif len(self.box) == 4:
            # 矩形
            nlat, slat, wlon, elon = self.box
            m = Basemap(llcrnrlon=wlon, llcrnrlat=slat,
                        urcrnrlon=elon, urcrnrlat=nlat,
                        projection=self.projection,
                        lat_1=25., lat_2=47, lon_0=105, lat_0=35,
                        resolution=self.resolution, ax=ax)

        elif len(self.box) == 2:
            lat0, lon0 = self.box
            self.projection = "ortho"
            m = Basemap(projection=self.projection, lon_0=lon0, lat_0=lat0, resolution=self.resolution, ax=ax)

        # draw parallels
        if self.projection in ["ortho"]:
            labels = [0, 0, 0, 0]
        else:
            labels = [1, 0, 0, 1]

        fnt = get_DV_Font(self.fontName)
        fnt.set_size(self.fontsize_tick)
        if self.delat is not None:
            parallels = np.arange(0., 91., self.delat).tolist() + \
                        np.arange(-self.delat, -91., -self.delat).tolist()
            m.drawparallels(parallels, linewidth=self.lw_latlon, labels=labels, color=self.color_latlon,
                            dashes=[100, .0001], fontproperties=fnt, textcolor=self.color_ticker,
                            latmax=90, zorder=10)
            m.drawparallels(parallels, linewidth=self.lw_latlon, labels=labels, color=self.color_latlon,
                            dashes=[100, .0001], fontproperties=fnt, textcolor=self.color_ticker,
                            latmax=90, zorder=10)

        # draw meridians
        if self.delon is not None:
            meridians = np.arange(0., 181., self.delon).tolist() + \
                        np.arange(-self.delon, -180., -self.delon).tolist()
            m.drawmeridians(meridians, linewidth=self.lw_latlon, labels=labels, color=self.color_latlon,
                            dashes=[100, .0001], fontproperties=fnt, textcolor=self.color_ticker,
                            latmax=90, zorder=10)
            m.drawmeridians(meridians, linewidth=self.lw_latlon, labels=labels, color=self.color_latlon,
                            dashes=[100, .0001], fontproperties=fnt, textcolor=self.color_ticker,
                            latmax=90, zorder=10)

        # 边框粗细
        if self.projection == "ortho":
            # set earth no edge line
            for eachpatch in ax.patches:
                eachpatch.set_linewidth(EDGE_LW)
                eachpatch.set_edgecolor(self.color_ticker)
        else:
            spines = ax.spines
            for eachspine in spines:
                spines[eachspine].set_linewidth(EDGE_LW)

        return m

    def set_discrete_colormap(self, color_list, bounds, label_list):
        '''
        must set before easyplot or drawOnMap
        
        color_list be like:
           ["#74FFE9", "#CBFDDD", "#FFFFB5"]
        '''
        self.colormap = colors.ListedColormap(color_list)
        self.colorbar_bounds = bounds
        self.norm = colors.BoundaryNorm(bounds, self.colormap.N)
        self.colorbar_label_list = label_list

    def set_linear_colormap(self, color_list, vmin, vmax, step=0):
        '''
        must set before easyplot or drawOnMap
        
        color_list be like:
           [(0, '#0000ff'), (0.333, '#00ffff'), (0.667, '#ffff00'), (1, '#ff0000')]
        '''
        self.colormap = colors.LinearSegmentedColormap.from_list('dummy', color_list)
        self.valmin = vmin
        self.valmax = vmax
        self.norm = mpl.colors.Normalize(vmin=self.valmin, vmax=self.valmax)
        if step > 0:
            self.colorbar_bounds = np.arange(vmin, vmax + step * 0.1, step)

    def easyplot(self, lats, lons, values, box=None,
                 vmin=None, vmax=None, colormap=None,  # color=None,
                 marker='o', markersize=6,  # markeredgecolor=None,
                 ptype=None, alpha=1):
        '''
        画地图
        ptype  None 散点
               pcolormesh 方格  数据必须是2维
               contour 等高线  数据必须是2维
               contourf 填充等高线  数据必须是2维
        '''
        if lats is None or lons is None or values is None:
            pass
        elif type(values).__name__ == "str":
            ptype = None
        elif ptype == "imshow":
            pass
        elif lats.shape == lons.shape == values.shape:
            if values.ndim == 1:
                ptype = None
        elif values.ndim >= 3 and lats.shape == lons.shape == values.shape[:2]:
            pass
        else:
            raise ValueError("values' shape not match lots, lats")

        #         self.lons, self.lats = lons, lats
        #         self.values = values
        if lons is not None:
            idx = np.logical_and(lons > 180, lons <= 360)
            lons[idx] = lons[idx] - 360.

        if colormap:
            self.colormap = colormap
        else:
            colormap = self.colormap
        self.marker = marker
        self.markersize = markersize

        # default show global
        if box:
            self.box = box

        if self.colorbar_extend not in ["neither", "both", "max", "min"]:
            self.colorbar_extend = "neither"

        # set Area
        area = self.area
        if self.show_north_pole or self.show_south_pole:
            self.show_china = False
            self.show_countries = True

        if self.show_china:
            self.show_china_province = True
            area = "China"

        elif self.show_north_pole:
            area = "NorthPole"
        elif self.show_south_pole:
            area = "SouthPole"
        elif self.show_north:
            area = "North"
        elif self.show_south:
            area = "South"
        elif len(self.box) == 4:
            nlat, slat, wlon, elon = self.box
            minRange = min((nlat - slat, elon - wlon))

        if self.__fig_resize:
            fig_resize(self.fig, self.box, area)

        if self.show_line_of_latlon == True:
            if self.delat is None:
                if self.show_china:
                    self.delat = 10
                elif self.show_north_pole or self.show_south_pole:
                    self.delat = 10
                else:
                    self.delat = 30
            if self.delon is None:
                if self.show_china:
                    self.delon = 20
                elif self.show_north or self.show_south:
                    self.delon = 30
                elif self.show_north_pole or self.show_south_pole:
                    self.delon = 30
                else:
                    self.delon = 60
        else:
            self.lw_latlon = 0

        fnt = get_DV_Font(self.fontName)
        fnt.set_size(self.fontsize_tick)
        self.m = self._map_init(area, self.ax)
        # 边界
        self.draw_boundary()

        # 南海小图
        if self.show_china:
            self.nanhai_minimap()

        #         # test
        #         import pickle
        #         pickle.dump(self.m, open('china.pkl', 'wb'))

        if values is not None:
            if self.valmin is None:
                self.valmin = np.nanmin(values)
            if self.valmax is None:
                self.valmax = np.nanmax(values)
        # value min max
        if vmin is None:
            vmin = self.valmin
        else:
            self.valmin = vmin
        if vmax is None:
            vmax = self.valmax
        else:
            self.valmax = vmax

        # ## Drawing
        self.drawOnMap(lats, lons, values, vmin, vmax, colormap, ptype, alpha)

    def drawOnMap(self, lats, lons, values, vmin, vmax, cmap, ptype='scatter', alpha=1):
        '''
        在地图上的值
        '''
        use_old_clip = True

        if ptype != "imshow" and (lats is None or lons is None or values is None):
            return

        var = values
        zorder = 0  # 2

        if vmin is None and type(values).__name__ != "str":
            vmin = np.min(values)
        if vmax is None and type(values).__name__ != "str":
            vmax = np.max(values)

        norm = mpl.colors.Normalize(vmin=vmin, vmax=vmax)
        # self.norm = norm

        m = self.m
        x1, y1 = m(lons, lats)
        if ptype == 'pcolormesh':
            cs = m.pcolormesh(x1, y1, var, norm=norm, rasterized=True, cmap=cmap, zorder=zorder,
                              alpha=alpha, shading='auto')
        elif ptype == 'pcolor':
            cs = m.pcolor(x1, y1, var, norm=norm, cmap=cmap, zorder=zorder, alpha=alpha)
        elif ptype == 'imshow':
            cs = m.imshow(var, interpolation="spline16", norm=norm, cmap=cmap, zorder=zorder, alpha=alpha)
        elif ptype == 'contour':
            cs = m.contour(x1, y1, var, self.contour_level, linewidths=0.5, norm=norm, cmap=cmap, zorder=zorder,
                           alpha=alpha)
        elif ptype == 'contourf':
            if self.colorbar_bounds is None:
                step = (self.valmax - self.valmin) / self.contour_level
                clevs = np.arange(self.valmin, self.valmax + step * 0.1, step)
            else:
                clevs = self.colorbar_bounds

            cs = m.contourf(x1, y1, var, clevs, norm=norm, cmap=cmap, zorder=zorder, alpha=alpha,
                            extend=self.colorbar_extend)
        else:
            lw = self.lw_marker
            # if markeredgecolor is not None:
            #     lw = self.lw_latlon

            cs = m.scatter(x1, y1, marker=self.marker, s=self.markersize, c=var,  # edgecolors=markeredgecolor,
                           cmap=cmap, norm=norm, lw=lw, zorder=zorder + 1, alpha=alpha)

        #             cs = m.plot(x1, y1, c=var,
        #                         lw=0, zorder=zorder, alpha=alpha)

        self.cs = cs  # test ??

        if use_old_clip and self.clip_path is not None:
            pass
        else:
            vertices = []
            codes = []
            if self.show_inside_china:
                sf = shapefile.Reader(str(selfPath / u'SHP/中国省级行政区'), encoding="gbk")
                for shape_rec in sf.shapeRecords():
                    pts = shape_rec.shape.points
                    prt = list(shape_rec.shape.parts) + [len(pts)]
                    for i in range(len(prt) - 1):
                        for j in range(prt[i], prt[i + 1]):
                            vertices.append(m(pts[j][0], pts[j][1]))
                        codes += [Path.MOVETO]
                        codes += [Path.LINETO] * (prt[i + 1] - prt[i] - 2)
                        codes += [Path.CLOSEPOLY]

            if len(self.areaNameLst) > 0:
                sf = shapefile.Reader(str(selfPath / u'SHP/中国省级行政区'), encoding="gbk")
                for shape_rec in sf.shapeRecords():
                    strName = shape_rec.record[0]
                    for eachArea in self.areaNameLst:
                        if eachArea in strName:
                            # print(eachArea)
                            pts = shape_rec.shape.points
                            prt = list(shape_rec.shape.parts) + [len(pts)]
                            for i in range(len(prt) - 1):
                                for j in range(prt[i], prt[i + 1]):
                                    vertices.append(m(pts[j][0], pts[j][1]))
                                codes += [Path.MOVETO]
                                codes += [Path.LINETO] * (prt[i + 1] - prt[i] - 2)
                                codes += [Path.CLOSEPOLY]

            if len(codes) > 0:
                clip = Path(vertices, codes)
                self.clip_path = PathPatch(clip, transform=m.ax.transData)

        if self.clip_path:
            if hasattr(cs, "collections"):
                # for eachClip in clipLst:
                for contour in cs.collections:
                    contour.set_clip_path(self.clip_path)
            else:
                cs.set_clip_path(self.clip_path)

        if self.m2 and self.show_inside_china_mini:
            self.drawOnMiniMap(lats, lons, values, ptype, alpha)

    def drawOnMiniMap(self, lats, lons, values, ptype, alpha=1):
        '''
        在小地图上的值
        '''
        if not self.m2: return

        use_old_clip = True

        var = values
        zorder = 2

        m = self.m2
        x1, y1 = m(lons, lats)
        if ptype == 'pcolormesh':
            cs = m.pcolormesh(x1, y1, var, norm=self.norm, rasterized=True, cmap=self.colormap, zorder=zorder,
                              alpha=alpha)
        elif ptype == 'pcolor':
            cs = m.pcolor(x1, y1, var, norm=self.norm, cmap=self.colormap, zorder=zorder, alpha=alpha)
        elif ptype == 'imshow':
            cs = m.imshow(var, interpolation="spline16", norm=self.norm, cmap=self.colormap, zorder=zorder, alpha=alpha)
        elif ptype == 'contour':
            cs = m.contour(x1, y1, var, 10, linewidths=0.5, norm=self.norm, cmap=self.colormap, zorder=zorder,
                           alpha=alpha)
        elif ptype == 'contourf':
            if self.colorbar_bounds is None:
                step = (self.valmax - self.valmin) / 512.
                clevs = np.arange(self.valmin, self.valmax + step * 0.1, step)
            else:
                clevs = self.colorbar_bounds

            cs = m.contourf(x1, y1, var, clevs, norm=self.norm, cmap=self.colormap, zorder=zorder, alpha=alpha)
        else:
            cs = m.scatter(x1, y1, marker=self.marker, s=self.markersize, c=var,
                           cmap=self.colormap, norm=self.norm, lw=0, zorder=zorder, alpha=alpha)

        self.cs2 = cs

        if use_old_clip and self.clip_path_mini is not None:
            pass
        else:
            vertices = []
            codes = []
            if self.show_inside_china:
                sf = shapefile.Reader(str(selfPath / u'SHP/中国省级行政区'), encoding="gbk")
                for shape_rec in sf.shapeRecords():
                    pts = shape_rec.shape.points
                    prt = list(shape_rec.shape.parts) + [len(pts)]
                    for i in range(len(prt) - 1):
                        for j in range(prt[i], prt[i + 1]):
                            vertices.append(m(pts[j][0], pts[j][1]))
                        codes += [Path.MOVETO]
                        codes += [Path.LINETO] * (prt[i + 1] - prt[i] - 2)
                        codes += [Path.CLOSEPOLY]

            if len(self.areaNameLst) > 0:
                sf = shapefile.Reader(str(selfPath / u'SHP/中国省级行政区'), encoding="gbk")
                for shape_rec in sf.shapeRecords():
                    strName = shape_rec.record[0].decode('gbk')
                    for eachArea in self.areaNameLst:
                        if eachArea in strName:
                            pts = shape_rec.shape.points
                            prt = list(shape_rec.shape.parts) + [len(pts)]
                            for i in range(len(prt) - 1):
                                for j in range(prt[i], prt[i + 1]):
                                    vertices.append(m(pts[j][0], pts[j][1]))
                                codes += [Path.MOVETO]
                                codes += [Path.LINETO] * (prt[i + 1] - prt[i] - 2)
                                codes += [Path.CLOSEPOLY]

            if len(codes) > 0:
                clip = Path(vertices, codes)
                self.clip_path_mini = PathPatch(clip, transform=m.ax.transData)

        if self.clip_path_mini:
            if hasattr(cs, "collections"):
                # for eachClip in clipLst:
                for contour in cs.collections:
                    contour.set_clip_path(self.clip_path_mini)
            else:
                cs.set_clip_path(self.clip_path_mini)

    def remove_dataonmap(self):
        if hasattr(self, "cs"):
            if hasattr(self.cs, "collections"):
                # for eachClip in clipLst:
                for contour in self.cs.collections:
                    contour.remove()
            else:
                self.cs.remove()

        if hasattr(self, "cs2"):
            if hasattr(self.cs2, "collections"):
                # for eachClip in clipLst:
                for contour in self.cs2.collections:
                    contour.remove()
            else:
                self.cs2.remove()

    def remove_colorbar(self):
        if self.ax_cb is not None:
            self.ax_cb.remove()
            self.norm = None
            self.colorbar_bounds = None
            self.colorbar_unit = None
            self.colorbar_extend = 'neither'
            self.colorbar_label_list = None

    def draw_boundary(self):
        '''
        边界线
        '''
        zorder = 50

        # 背景颜色
        if self.show_ocean_color:
            self.m.fillcontinents(color=self.color_land)
            self.m.drawmapboundary(fill_color=self.color_ocean)

        if self.show_china_boundary:
            self.m.readshapefile(str(selfPath / u'SHP/国界'), 'china',
                                 linewidth=self.lw_china,
                                 color=self.color_china, zorder=zorder, default_encoding="gbk")

        if self.show_coastlines:
            # 画 海岸线
            if self.show_china:
                self.m.readshapefile(str(selfPath / u'SHP/周边海岸线'), '',
                                     linewidth=self.lw_boundray,
                                     color=self.color_contry, zorder=zorder, default_encoding="gbk")
            else:
                self.m.drawcoastlines(linewidth=self.lw_boundray, color=self.color_coast)  # basemap自带的海岸线包含一些湖的边界，不好看

        if self.show_countries:
            # 画 国境线
            self.m.drawcountries(linewidth=self.lw_boundray, color=self.color_contry, zorder=zorder)

        if self.show_china_province:
            # 画省线
            self.m.readshapefile(str(selfPath / u'SHP/省界'), 'province',
                                 linewidth=self.lw_boundray,
                                 color=self.color_contry, zorder=zorder, default_encoding="gbk")

        if self.show_china_county:
            # 画县市线
            self.m.readshapefile(str(selfPath / u'SHP/中国县市'), 'province',
                                 linewidth=self.lw_boundray * 0.8,
                                 color=self.color_contry, zorder=zorder, default_encoding="gbk")

        if self.show_china_river:
            # 画长江黄河
            self.m.readshapefile(str(selfPath / u'SHP/长江黄河'), '',
                                 linewidth=self.lw_boundray * 1.3,
                                 color="xkcd:vivid blue", zorder=zorder, default_encoding="gbk")

        if self.show_north_pole or self.show_south_pole:
            self._setLatsLabelWithinFig()

    def nanhai_minimap(self):
        # 画南海 十段线

        # zoomed_inset_axes 不能 pickle, 所以弃用，换用fig.add_axes 自己设定位置---
        #         ax = plt.gca()
        #         ax2 = zoomed_inset_axes(ax, 0.42, loc=3, borderpad=0.9)
        #         ax2.set_xlim(105, 125)
        #         ax2.set_ylim(5, 25)
        #         plt.xticks(visible=False)
        #         plt.yticks(visible=False)
        # ---------------------------------------------------------------------
        ax2 = self.fig.add_axes(self.nanhai_loc)
        plt.setp(ax2.get_yticklabels(), visible=False)
        plt.setp(ax2.get_yticklabels(), visible=False)

        map2 = Basemap(projection='aea',
                       lat_1=25., lat_2=47, lon_0=105, lat_0=35,
                       resolution=self.resolution, ax=ax2,
                       llcrnrlon=105.5, llcrnrlat=2,
                       urcrnrlon=124, urcrnrlat=24)
        zorder = 5
        if self.show_coastlines:
            map2.drawcoastlines(linewidth=self.lw_boundray, color=self.color_coast, zorder=zorder)

        if self.show_ocean_color:
            map2.fillcontinents(color=self.color_land)
            map2.drawmapboundary(fill_color=self.color_ocean)

        map2.readshapefile(str(selfPath / u'SHP/国界'), 'china',
                           linewidth=self.lw_china * 0.9,
                           color=self.color_china, zorder=zorder, default_encoding="gbk")
        self.m2 = map2
        self.ax2 = ax2

        # 边框粗细
        spines = ax2.spines
        for eachspine in ax2.spines:
            spines[eachspine].set_linewidth(EDGE_LW * 0.9)

    def city_boundary(self, name,
                      drawbounds=True, zorder=None,
                      linewidth=0.5, color='k',
                      default_encoding='utf-8',
                      shape_name='中国省级行政区'):
        """
        Read in shape file, optionally draw boundaries on map.

        .. note::
          - Assumes shapes are 2D
          - only works for Point, MultiPoint, Polyline and Polygon shapes.
          - vertices/points must be in geographic (lat/lon) coordinates.

        Mandatory Arguments:

        .. tabularcolumns:: |l|L|

        ==============   ====================================================
        Argument         Description
        ==============   ====================================================
        shapefile        path to shapefile components.  Example:
                         shapefile='/home/jeff/esri/world_borders' assumes
                         that world_borders.shp, world_borders.shx and
                         world_borders.dbf live in /home/jeff/esri.
        name             name for Basemap attribute to hold the shapefile
                         vertices or points in map projection
                         coordinates. Class attribute name+'_info' is a list
                         of dictionaries, one for each shape, containing
                         attributes of each shape from dbf file, For
                         example, if name='counties', self.counties
                         will be a list of x,y vertices for each shape in
                         map projection  coordinates and self.counties_info
                         will be a list of dictionaries with shape
                         attributes.  Rings in individual Polygon
                         shapes are split out into separate polygons, and
                         additional keys 'RINGNUM' and 'SHAPENUM' are added
                         to the shape attribute dictionary.
        ==============   ====================================================

        The following optional keyword arguments are only relevant for Polyline
        and Polygon shape types, for Point and MultiPoint shapes they are
        ignored.

        .. tabularcolumns:: |l|L|

        ==============   ====================================================
        Keyword          Description
        ==============   ====================================================
        drawbounds       draw boundaries of shapes (default True).
        zorder           shape boundary zorder (if not specified,
                         default for mathplotlib.lines.LineCollection
                         is used).
        linewidth        shape boundary line width (default 0.5)
        color            shape boundary line color (default black)
        antialiased      antialiasing switch for shape boundaries
                         (default True).
        ax               axes instance (overrides default axes instance)
        ==============   ====================================================

        A tuple (num_shapes, type, min, max) containing shape file info
        is returned.
        num_shapes is the number of shapes, type is the type code (one of
        the SHPT* constants defined in the shapelib module, see
        http://shapelib.maptools.org/shp_api.html) and min and
        max are 4-element lists with the minimum and maximum values of the
        vertices. If ``drawbounds=True`` a
        matplotlib.patches.LineCollection object is appended to the tuple.
        """
        import shapefile as shp
        from shapefile import Reader
        from matplotlib.collections import LineCollection
        #         shp.default_encoding = default_encoding

        shapefile = str(selfPath / u'SHP/{}'.format(shape_name))
        if not os.path.exists('%s.shp' % shapefile):
            raise IOError('cannot locate %s.shp' % shapefile)
        if not os.path.exists('%s.shx' % shapefile):
            raise IOError('cannot locate %s.shx' % shapefile)
        if not os.path.exists('%s.dbf' % shapefile):
            raise IOError('cannot locate %s.dbf' % shapefile)
        # open shapefile, read vertices for each object, convert
        # to map projection coordinates (only works for 2D shape types).
        try:
            shf = Reader(shapefile, encoding="gbk")
        except:
            raise IOError('error reading shapefile %s.shp' % shapefile)
        fields = shf.fields
        coords = []
        attributes = []
        msg = "dummy"
        shptype = shf.shapes()[0].shapeType
        bbox = shf.bbox.tolist()
        info = (shf.numRecords, shptype, bbox[0:2] + [0., 0.], bbox[2:] + [0., 0.])
        npoly = 0
        for shprec in shf.shapeRecords():
            if shape_name == '中国县市':
                if (name != shprec.record[4] and name != shprec.record[2] and name != shprec.record[5]):
                    continue
            elif shape_name == '中国省级行政区':
                if name != shprec.record[0]:
                    continue

            shp = shprec.shape
            rec = shprec.record
            npoly = npoly + 1
            if shptype != shp.shapeType:
                raise ValueError('readshapefile can only handle a single shape type per file')
            if shptype not in [1, 3, 5, 8]:
                raise ValueError('readshapefile can only handle 2D shape types')
            verts = shp.points
            if shptype in [1, 8]:  # a Point or MultiPoint shape.
                lons, lats = list(zip(*verts))
                if max(lons) > 721. or min(lons) < -721. or max(lats) > 90.01 or min(lats) < -90.01:
                    raise ValueError(msg)
                # if latitude is slightly greater than 90, truncate to 90
                lats = [max(min(lat, 90.0), -90.0) for lat in lats]
                if len(verts) > 1:  # MultiPoint
                    x, y = self.m(lons, lats)
                    coords.append(list(zip(x, y)))
                else:  # single Point
                    x, y = self.m(lons[0], lats[0])
                    coords.append((x, y))
                attdict = {}
                for r, key in zip(rec, fields[1:]):
                    attdict[key[0]] = r
                attributes.append(attdict)
            else:  # a Polyline or Polygon shape.
                parts = shp.parts.tolist()
                ringnum = 0
                for indx1, indx2 in zip(parts, parts[1:] + [len(verts)]):
                    ringnum = ringnum + 1
                    lons, lats = list(zip(*verts[indx1:indx2]))
                    if max(lons) > 721. or min(lons) < -721. or max(lats) > 90.01 or min(lats) < -90.01:
                        raise ValueError(msg)
                    # if latitude is slightly greater than 90, truncate to 90
                    lats = [max(min(lat, 90.0), -90.0) for lat in lats]
                    x, y = self.m(lons, lats)
                    coords.append(list(zip(x, y)))
                    attdict = {}
                    for r, key in zip(rec, fields[1:]):
                        attdict[key[0]] = r
                    # add information about ring number to dictionary.
                    attdict['RINGNUM'] = ringnum
                    attdict['SHAPENUM'] = npoly
                    attributes.append(attdict)
        # draw shape boundaries for polylines, polygons  using LineCollection.
        if shptype not in [1, 8] and drawbounds:
            # get current axes instance (if none specified).
            ax = self.ax
            # make LineCollections for each polygon.
            lines = LineCollection(coords, antialiaseds=(1,))
            lines.set_color(color)
            lines.set_linewidth(linewidth)
            lines.set_label('_nolabel_')
            if zorder is not None:
                lines.set_zorder(zorder)
            ax.add_collection(lines)
            # set axes limits to fit map region.
            self.m.set_axes_limits(ax=ax)
            # clip boundaries to map limbs
            lines, c = self.m._cliplimb(ax, lines)
            info = info + (lines,)

    def custom_style(self):

        plt.xlabel('')
        plt.ylabel('')
        self.set_tick_font()

        if self.show_colorbar:
            # 画 colorbar
            self.add_colorbar_right()

    def add_background_rgb(self, lats, lons, aryR, aryG, aryB, facecolor="k", **kwargs):
        '''
        为地图添加RGB合成背景, 
        aryR, aryG, aryB 中的无效值需要提前设好，比如0
        '''
        self._plot_rgb(self.ax, self.m, self.box, lats, lons, aryR, aryG, aryB, facecolor, **kwargs)
        self._plot_rgb(self.ax2, self.m2, [60, 10, 70, 150], lats, lons, aryR, aryG, aryB, facecolor, **kwargs)

    def _plot_rgb(self, ax, m, box, lats, lons, aryR, aryG, aryB, facecolor, **kwargs):
        '''
       画RGB
        '''
        if ax is None or m is None:
            return

        ax.set_facecolor(facecolor)  # facecolor只能是 黑"k" 或 白"w"

        if "linear" in kwargs.keys():  # 线性拉伸
            linear = kwargs["linear"]
        else:
            linear = 0.02

        alpha = 1.0  # 透明度
        if "alpha" in kwargs.keys():
            alpha = kwargs["alpha"]
        if alpha > 1: alpha = 1.0
        if alpha < 0: alpha = 0.0

        # 投影成等经纬
        if "res" in kwargs.keys():  # 分辨率
            res = kwargs["res"]
        else:
            res = 0.05
        if len(box) != 4:
            box = [90., -90., -180., 180.]
        LatN, LatS, LonW, LonE = box
        gll = prj_gll(nlat=LatN, slat=LatS, wlon=LonW, elon=LonE, resLat=res, resLon=res)

        projLats, projLons = gll.generateLatsLons()
        proj_i, proj_j = gll.lonslats2ij(lons, lats)
        raw_i, raw_j = np.mgrid[0:lats.shape[0]:1, 0:lats.shape[1]:1]

        # inside box
        index = np.logical_and.reduce((proj_i >= 0, proj_i < projLats.shape[0],
                                       proj_j >= 0, proj_j < projLats.shape[1]))
        proj_i = proj_i[index]
        proj_j = proj_j[index]
        raw_i = raw_i[index]
        raw_j = raw_j[index]

        RR = np.zeros_like(projLats)
        GG = np.zeros_like(projLats)
        BB = np.zeros_like(projLats)

        RR[proj_i, proj_j] = aryR[raw_i, raw_j]
        GG[proj_i, proj_j] = aryG[raw_i, raw_j]
        BB[proj_i, proj_j] = aryB[raw_i, raw_j]

        for _ in range(3):  # 补点3次
            fill_points_2d(RR, 0.)
            fill_points_2d(GG, 0.)
            fill_points_2d(BB, 0.)

        lats = projLats
        lons = projLons
        aryR = RR
        aryG = GG
        aryB = BB

        # RGB color must be in range 0. - 1.
        aryR = linearStretch(norm255(aryR), linear) / 255.
        aryG = linearStretch(norm255(aryG), linear) / 255.
        aryB = linearStretch(norm255(aryB), linear) / 255.

        if facecolor == "w":  # set bg color white
            condition = np.logical_and.reduce((aryR == 0, aryG == 0, aryB == 0))
            aryR[condition] = 1
            aryG[condition] = 1
            aryB[condition] = 1

        # get corner lats lons
        cornerLats = getCorners(lats)
        cornerLons = getCorners(lons)
        xCorners, yCorners = m(cornerLons, cornerLats)

        # combine to RGB tuple
        colorTuple = tuple(np.array((aryR.ravel(),
                                     aryG.ravel(),
                                     aryB.ravel())).transpose().tolist())

        # add alpha, RGB -> RGBA
        colorTuple = np.insert(colorTuple, 3, alpha, axis=1)
        # draw background
        quad = m.pcolormesh(xCorners, yCorners, aryR, color=colorTuple, clip_on=True, linewidth=0)
        quad.set_array(None)

    def add_landmark(self, lon, lat, label, marker="star", color="r", edgecolors="#999999", markersize=70,
                     fontsize=7.5, fontcolor=None, xplus=None, yplus=None):

        if marker == "star":
            marker = (5, 1, 0)

        if fontcolor == None:
            fontcolor = COLOR_Darkgray

        if xplus == None:
            xplus = 0

        if yplus == None:
            if self.projection == "aea":
                yplus = 75000
            elif self.projection == "ortho":
                yplus = 160000
            elif self.projection == "cyl":
                yplus = 0.05
            else:
                yplus = 3

        xpt, ypt = self.m(lon, lat)
        if marker not in ["", None]:
            self.m.scatter(xpt, ypt, marker=marker, s=markersize, color=color, edgecolors=edgecolors, lw=0.1, zorder=10)

        font = self.font_mid
        if self.projection == "cyl":
            self.ax.annotate(label, xy=(lon + xplus, lat + yplus), va="center", ha="center",
                             fontproperties=font, size=fontsize,
                             color=fontcolor)
        else:
            self.ax.annotate(label, xy=(xpt + xplus, ypt + yplus), va="center", ha="center",
                             fontproperties=font, size=fontsize,
                             color=fontcolor)

    def add_shenghui(self, marker=".", color=COLOR_Darkgray, edgecolors="#999999", markersize=50,
                     fontsize=7, fontcolor=COLOR_Darkgray, multi=1.):
        xyplus = {u"北京": [60000, 40000],
                  u"天津": [90000, -20000],
                  u"太原": [60000, -60000],
                  u"杭州": [80000, -50000],
                  u"合肥": [-80000, 0],
                  u"南京": [60000, 40000],
                  u"广州": [0, 40000],
                  u"澳门": [0, -60000],
                  u"福州": [100000, 0],
                  u"台北": [60000, 40000]}
        sf = shapefile.Reader(str(selfPath / u"SHP/省会城市"), encoding="gbk")
        for shape_rec in sf.shapeRecords():
            shenghui = shape_rec.record[5]
            # print(type(shenghui), shenghui)
            lon, lat = shape_rec.shape.points[0]

            if shenghui in xyplus.keys():
                xplus, yplus = xyplus[shenghui]
            else:
                xplus, yplus = (str_len(shenghui) / 2. + 1) * 30000, 0

            self.add_landmark(lon, lat, shenghui, marker=marker, color=color, edgecolors=edgecolors,
                              markersize=markersize,
                              fontsize=fontsize, fontcolor=fontcolor,
                              xplus=xplus * multi, yplus=yplus * multi)

    #     def add_colorbar_right1(self):
    #         '''
    #         add colorbar at right of pic
    #         '''
    #         labellen = max(str_len(self.colorbar_fmt % self.valmin), str_len(self.colorbar_fmt % self.valmax))
    #         width = self.fig.get_size_inches()[0]
    #         lspace = (labellen + 8.) / width * 0.07
    #
    #         self.fig.subplots_adjust(left=lspace * 0.5, right=1. - lspace)
    #         self.fig.canvas.draw()
    #
    #         ax = self.get_main_ax()
    #         point_bl = ax.get_position().get_points()[0]  # 左下
    #         point_tr = ax.get_position().get_points()[1]  # 右上
    #
    #         cbar_width = (0.18 / width)
    #         space = cbar_width * 0.618
    #
    #         colorbar_ax = self.fig.add_axes([point_tr[0] + space,
    #                                     point_bl[1],
    #                                     cbar_width,
    #                                     point_tr[1] - point_bl[1]])
    #
    #         norm = mpl.colors.Normalize(vmin=self.valmin, vmax=self.valmax)
    #         cb = mpl.colorbar.ColorbarBase(colorbar_ax, cmap=self.colormap,
    #                                   norm=norm, extend=self.colorbar_extend,
    #                                   boundaries=self.colorbar_bounds,
    #                                   ticks=self.colorbar_bounds,
    #                                   orientation='vertical', format=self.colorbar_fmt)
    #         # font of colorbar
    #         for l in colorbar_ax.yaxis.get_ticklabels():
    #             l.set_fontproperties(self.font)
    #             l.set_fontsize(self.fontsize_cbtick)
    #         if self.colorbar_unit:
    #             cb.ax.set_title(self.colorbar_unit, y=1.01,
    #                             fontproperties=self.font_leg, fontsize=self.fontsize_label)
    #         cb.outline.set_linewidth(EDGE_LW)

    def add_colorbar_right(self):
        labellen = max(str_len(self.colorbar_fmt % self.valmin), str_len(self.colorbar_fmt % self.valmax))
        width = self.fig.get_size_inches()[0]

        colorbar_len = 8
        leg2inch = self.fontsize_cbtick / 144.
        right_inch = (labellen + colorbar_len) * leg2inch
        lspace = right_inch / width
        if (lspace * 0.8) > (1. - lspace):
            raise ValueError("Plase set arg 'figsize=(width, height)' when using dv_map, and increase width a little.")

        self.fig.subplots_adjust(left=lspace * 0.8, right=1. - lspace)

        ax = self.get_main_ax()
        divider = make_axes_locatable(ax)

        cbar_width = 0.15
        space = cbar_width * 0.8

        colorbar_ax = divider.append_axes("right", size=cbar_width, pad=space)

        norm = mpl.colors.Normalize(vmin=self.valmin, vmax=self.valmax)
        cb = mpl.colorbar.ColorbarBase(colorbar_ax, cmap=self.colormap,
                                       norm=norm, extend=self.colorbar_extend,
                                       boundaries=self.colorbar_bounds,
                                       ticks=self.colorbar_bounds,
                                       orientation='vertical', format=self.colorbar_fmt)
        # font of colorbar
        for l in colorbar_ax.yaxis.get_ticklabels():
            l.set_fontproperties(self.font)
            l.set_fontsize(self.fontsize_cbtick)
        if self.colorbar_unit:
            cb.ax.set_title(self.colorbar_unit, y=1.01,
                            fontproperties=self.font_leg, fontsize=self.fontsize_label, loc="left")
        cb.outline.set_linewidth(EDGE_LW)

    def set_area(self, shengName):
        '''
        设定显示的区域
        shpName   shape文件名
        areaNameLst  地区名，shape文件中包含的地区
        '''
        if type(shengName).__name__ == "str":
            self.areaNameLst.append(shengName)
        elif type(shengName).__name__ == "list":
            self.areaNameLst = self.areaNameLst + shengName

    def _setLatsLabelWithinFig(self):
        if self.show_south_pole:
            lat_labels = [(0.0, -90.0), (0.0, -80.0), (0.0, -70.0), (0.0, -60.0)]
            for lon, lat in (lat_labels):
                xpt, ypt = self.m(lon, lat)
                self.ax.text(xpt, ypt, str(lat)[1:3] + u'°S', fontproperties=self.font_leg)
        elif self.show_north_pole:
            lat_labels = [(0.0, 90.0), (0.0, 80.0), (0.0, 70.0), (0.0, 60.0)]
            for lon, lat in (lat_labels):
                xpt, ypt = self.m(lon, lat)
                self.ax.text(xpt, ypt, str(lat)[0:2] + u'°N', va="top",
                             fontproperties=self.font_leg)

    def add_custom_colorbar(self, ax_loc, vmin, vmax, cmap=None, fmt="%.1f",
                            invert_axis=False, bar_lable_space=1.,
                            extend="neither",
                            fontsize=8, unit=None):

        if cmap is None:
            cmap = self.colormap

        c_ax = self.fig.add_axes(ax_loc)
        self.ax_cb = c_ax

        if self.norm is None:
            # default颜色标尺
            norm = mpl.colors.Normalize(vmin=vmin, vmax=vmax)
        #         cbar = colorbar(self.c, ax_cb=c_ax, cmap=self.colormap, norm=self.norm,
        #                         boundaries=self.colorbar_bounds)
        else:
            norm = self.norm

        font = self.font_mid
        if ax_loc[2] < ax_loc[3]:
            cbar = mpl.colorbar.ColorbarBase(c_ax,
                                             cmap=cmap,
                                             norm=norm,
                                             extend=extend,
                                             boundaries=self.colorbar_bounds,
                                             orientation='vertical',
                                             format=fmt)
            if invert_axis:
                cbar.ax.invert_yaxis()

            if self.colorbar_label_list:
                cbar.ax.get_yaxis().set_ticks([])
                for j, lab in enumerate(self.colorbar_label_list):
                    cbar.ax.text(1. + bar_lable_space, (j + 0.5) / len(self.colorbar_label_list),
                                 lab, ha='center', va='center', fontproperties=font, size=fontsize,
                                 color=self.color_ticker)
            else:
                for l in c_ax.yaxis.get_ticklabels():
                    l.set_fontproperties(font)
                    l.set_fontsize(fontsize)
                    l.set_color(self.color_ticker)

            # if self.colorbar_unit:
            #     c_ax.set_title(self.colorbar_unit, y=1.01, color=self.color_ticker,
            #                    fontproperties=font, fontsize=fontsize, loc="left")

            if unit:
                cbar.ax.set_title(unit, y=1.0191, color=self.color_ticker,
                                  fontproperties=font, fontsize=fontsize)
        else:
            cbar = mpl.colorbar.ColorbarBase(c_ax,
                                             cmap=cmap,
                                             norm=norm,
                                             extend=extend,
                                             boundaries=self.colorbar_bounds,
                                             orientation='horizontal',
                                             format=fmt)
            if invert_axis:
                cbar.ax.invert_xaxis()
            if self.colorbar_label_list:
                cbar.ax.get_xaxis().set_ticks([])
                for j, lab in enumerate(self.colorbar_label_list):
                    cbar.ax.text((j + 0.5) / len(self.colorbar_label_list), -bar_lable_space,
                                 lab, ha='center', va='top', fontproperties=font, size=fontsize,
                                 color=self.color_ticker)
            else:
                for l in c_ax.xaxis.get_ticklabels():
                    l.set_fontproperties(font)
                    l.set_fontsize(fontsize)
                    l.set_color(self.color_ticker)
            if unit:
                cbar.ax.set_title(unit, x=1.0382, y=0, color=self.color_ticker,
                                  ha='left', va='center',
                                  fontproperties=font, fontsize=fontsize)
        cbar.outline.set_linewidth(self.line_width)


def getCorners(centers):
    one = centers[:-1, :]
    two = centers[1:, :]
    d1 = (two - one) / 2.
    one = one - d1
    two = two + d1
    stepOne = np.zeros((centers.shape[0] + 1, centers.shape[1]))
    stepOne[:-2, :] = one
    stepOne[-2:, :] = two[-2:, :]
    one = stepOne[:, :-1]
    two = stepOne[:, 1:]
    d2 = (two - one) / 2.
    one = one - d2
    two = two + d2
    stepTwo = np.zeros((centers.shape[0] + 1, centers.shape[1] + 1))
    stepTwo[:, :-2] = one
    stepTwo[:, -2:] = two[:, -2:]
    return stepTwo


def fig_resize(fig, box, showArea=None):
    '''
    地图底图
    '''
    fig_height = 5.5
    if showArea == "China":
        fig_width = fig_height * 1.4

    elif showArea in ["SouthPole", "NorthPole", "South", "North"]:
        fig_width = fig_height  # 背景是正方形

    elif len(box) == 4:
        # 矩形
        nlat, slat, wlon, elon = box
        fig_width = np.floor(fig_height * (elon - wlon) / (nlat - slat) * 1.0)

    elif len(box) == 2:
        fig_width = fig_height * 1.2

    fig.set_size_inches(fig_width, fig_height)


if __name__ == '__main__':
    # example
    from matplotlib.colors import LinearSegmentedColormap


    def colormap_blue2red():
        """
        自定义colormap 蓝到红
        """
        clst = [(0, '#07037e'),
                (0.1, '#0000ff'),
                (0.3, '#00ffff'),
                (0.4, '#00ff00'),
                (0.5, "#b9ff00"),
                (0.6, '#ffff00'),
                (0.9, '#ff0000'),
                (1, '#6a0000')]
        return LinearSegmentedColormap.from_list('b2r', clst)


    def colormap_no2():
        """
        自定义colormap 蓝到红
        """
        clst = [(0, '#8785ab'),
                (0.1, '#827eb7'),
                (0.3, '#7fbbb4'),
                (0.4, '#7cbe8e'),
                (0.5, "#b1bc78"),
                (0.6, '#c0bb83'),
                (0.9, '#a88084'),
                (1, '#978987')]
        return LinearSegmentedColormap.from_list('b2r', clst)


    # 全球-----------------------

    lats, lons = np.mgrid[70:0:-2, 50: 150: 2]
    values = np.random.randint(0., 10000. + 1, lats.shape)
    #     values = np.ma.masked_all_like(lats)
    #     box = [30, -30, 60, 120]
    #     box = [90, -90, -180, 180]
    #     box = [60, 0, 60, 140]
    #     box = [55, 15, 70, 140]
    #     lats = np.arange(-90, 90)
    #     lons = np.arange(-90, 90)
    #     values = np.arange(-90, 90)

    #     lats = lats.reshape((18, 10))
    #     lons = lons.reshape((18, 10))
    #     values = values.reshape((18, 10))
    #     print lats.shape
    #     values = np.ma.masked_where(values > 60000, values)

    #     plt.style.use('dark_background')

    # 全球 ---------------------
    from DV.dv_plt import add_ax

    # fig = plt.figure(figsize=(11, 6))
    # #     pos1 = [0.05, 0.06, 0.9, 0.85]
    # #     ax1 = add_ax(fig, *pos1)
    # #     p = dv_map(fig, ax=ax1)
    # p = dv_map(fig)
    # #     p.show_colorbar = False
    # p.colormap = colormap_no2()
    # p.colorbar_fmt = "%d"
    # p.show_line_of_latlon = False
    # p.show_countries = False
    # # p.colorbar_bounds = range(0, 10001, 1000)
    # p.valmax = np.max(values)
    # p.valmin = np.min(values)
    # p.easyplot(lats, lons, values, ptype='contourf', vmin=0, vmax=10000)
    # p.title = u'全球分布图'
    # # 在北京的位置画颗星星
    # lon, lat, location = np.array([116.30]), np.array([40.03]), np.array([u"北京"])
    # p.add_landmark(lon, lat, location)
    #
    # lats = [-30, 30, 30, -30]
    # lons = [-50, -50, 50, 50]
    # x, y = p.m(lons, lats)
    # xy = list(zip(x, y))
    # xy = [(113.7, 26.1), (113.7, 35.9), (126.2, 35.9), (126.2, 26.1)]
    # from matplotlib.patches import Polygon
    #
    # poly = Polygon(xy, linestyle="--", edgecolor='k', facecolor='None', linewidth=0.5, alpha=0.7)
    # p.ax.add_patch(poly)
    #
    # p.draw()
    # strlist = [['2.0322x-0.6106', 'count:2168'], ['a = 0.9', 'b = 0', 'num = 10'], ['a = 0.9', 'b = 0', 'num = 10']]
    # p.annotate(strlist, color='m', fontsize=12)
    # p.savefig('glb.png')

    #     # 东半球中国 --------------------------
    #     p = dv_map(figsize=(5.4, 5.8), theme="dark")
    #     p.show_countries = False
    #     p.easyplot(None, None, values, box=[33, 108], ptype='pcolormesh', vmin=0, vmax=1)
    #     p.show_colorbar = False
    #     add_colorbar_below(p.fig, 0.26, 0.74, 0.05, 0, 100)
    #     p.fig.subplots_adjust(left=0.03, right=1. - 0.03, top=0.93, bottom=0.1)
    #     p.suptitle(u'Average Aerosol Optical Depth from 15 Nov. 2016 to 31 Jan. 2017', fontsize=12)
    #     p.savefig('easthalf.png')

    #     # 中国 -----------------------
    #     p = dv_map(theme="dark")
    #     p.colorbar_fmt = "%d"
    #     p.show_countries = False
    #     p.show_china = True
    #     p.show_inside_china = True
    #     p.easyplot(lats, lons, values, ptype='contourf', vmin=0, vmax=10000)
    #     p.colorbar_unit = u"（单位：ppm）"
    #
    #     p.suptitle(u"美国OCO卫星监测大气二氧化碳年平均浓度分布图")
    #     p.title_left(u'中国陆地区域')
    #     p.title_right(u'2017年')
    #
    #     # 标出地理位置
    #     lonlatlocation = [(117.117, 40.65, u"上甸子站"), (100.9, 36.283, u"瓦里关站"),
    #                       (127.6, 44.733, u"龙凤山站"), (119.733, 30.3, u"临安站"),
    #                       (99.733, 28.017, u"香格里拉站"), (114.2, 29.633, u"金沙站"),
    #                       (87.967, 47.1, u"阿克达拉站")]
    #     p.add_landmark(lonlatlocation, marker=".")
    #
    #     p.savefig('china.png')

    #     # 北半球 -------------------------------
    #     p = dv_map(theme="dark")
    #     p.colorbar_fmt = "%d"
    #     p.show_north = True
    #     p.colorbar_unit = "unit:(m)"
    #     p.colorbar_extend = "both"
    #     p.easyplot(lats, lons, values, ptype='contourf', vmin=0, vmax=10000)
    #     p.title = u'北极地区分布图'
    #     p.savefig('north.png')

    #     # 北极 ----------------------------------
    #     p = dv_map(theme="dark")
    #     p.colorbar_fmt = "%d"
    #     p.show_north_pole = True
    #     p.colorbar_unit = "unit:(m)"
    #     p.colorbar_extend = "both"
    #     p.easyplot(lats, lons, values, ptype='contourf', vmin=0, vmax=10000)
    #     p.title = u'北极地区分布图'
    #     p.savefig('npole.png')

    #     # 南极 --------------------------------
    #     p = dv_map(theme="dark")
    #     p.colorbar_fmt = "%d"
    #     p.show_south_pole = True
    #     p.colorbar_unit = "unit:(m)"
    #     p.colorbar_extend = "both"
    #     p.easyplot(lats, lons, values, ptype='contourf', vmin=0, vmax=10000)
    #     p.title = u'南极地区分布图'
    #     p.savefig('spole.png')

    # 北京
    p = dv_map(figsize=(6.6, 6.), theme="dark")
    p.colorbar_fmt = "%d"
    p.show_coastlines = False
    p.show_countries = False
    p.show_china_province = True
    p.show_line_of_latlon = True
    p.set_area(["北京市", "天津市"])
    p.projection = "aea"
    p.delat = 1
    p.delon = 2
    p.lw_boundray = 0.54
    p.colorbar_bounds = range(0, 10001, 1000)
    p.easyplot(lats, lons, values, box=[41.5, 38.5, 113.9, 118.9], ptype='contourf', vmin=0, vmax=10000)
    p.city_boundary("天津市")
    p.title = u'北京区域分布图'
    p.savefig('beijing.png')

    # # 赣州
    # p = dv_map(figsize=(6.6, 6.))
    # box = [28, 24, 113, 117]
    # p.colorbar_fmt = "%d"
    # p.show_coastlines = False
    # p.show_countries = False
    # p.show_line_of_latlon = True
    # p.delat = 1
    # p.delon = 2
    # p.valmin = 0
    # p.valmax = 10000
    # p.lw_boundray = 0.54
    # p.colorbar_bounds = range(0, 10001, 1000)
    # p.easyplot(lats, lons, values, box=box, ptype='pcolormesh', vmin=0, vmax=10000)
    # p.city_boundary("赣县")
    # p.city_boundary("宁都县")
    # p.add_landmark(115.27, 25.81, u"赣州市", marker="")
    #
    # p.title = u'区域分布图'
    # p.savefig('ganzhou.png')
