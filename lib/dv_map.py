# coding: utf-8
'''
Created on 2016年3月16日

@author: zhangtao
'''
import os

from matplotlib.patches import PathPatch
from matplotlib.path import Path
import shapefile

# from DP.dp_prj import prj_gll, fill_points_2d
# from lib.dv_img import linearStretch, norm255
from lib.dv_plt import dv_base, str_len, EDGE_LW, colormap_blue2red, COLOR_Darkgray, get_DV_Font
from mpl_toolkits.axes_grid1.axes_divider import make_axes_locatable
from mpl_toolkits.axes_grid1.inset_locator import zoomed_inset_axes
from mpl_toolkits.basemap import Basemap
import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np

mpl.use('Agg')

selfPath = os.path.split(os.path.realpath(__file__))[0]


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
        self.color_ocean = '#b3d1ff'  # show_bg_color时启用
        self.color_land = '#f4f3f0'  # show_bg_color时启用
        if self.theme == "dark":
            COLOR_littleWhite = "#afafaf"
            self.color_coast = COLOR_littleWhite
            self.color_contry = COLOR_littleWhite
            self.color_ticker = COLOR_littleWhite
        else:
            self.color_coast = COLOR_Darkgray
            self.color_contry = COLOR_Darkgray
            self.color_ticker = COLOR_Darkgray
        self.color_latlon = "#999999"

        # fontsize_开头： 字体大小
        self.fontsize_tick = 9.5  # 刻度字体大小
        self.fontsize_cbtick = 9  # colorbar刻度字体大小
        self.fontsize_label = 9  # xy轴名字字体大小
        self.fontsize_title = 10  # 标题字体大小

        # show_开头：展示开关
        self.show_leg = False  # legend
        self.show_colorbar = True  # colorbar
        self.show_coastlines = True  # 海岸线
        self.show_countries = False  # 国境线
        self.show_china_boundary = True  # 中国国境
        self.show_china_province = False  # 中国省界
        self.show_china_county = False  # 中国县市界
        self.show_nanhai_minimap = False  # 南海小图
        self.show_line_of_latlon = True  # 经纬度线
        self.show_china = False  # 中国区域图
        self.show_inside_china = False  # 中国境内
        self.show_south_pole = False  # 南极极射赤面
        self.show_north_pole = False  # 北极极射赤面
        self.show_north = False  # 南极
        self.show_south = False  # 北极
        self.show_bg_color = False  # 海陆背景颜色
        #         self.show_tight = False

        self.areaNameLst = []
        self.title_pos = 1.03
        self.box = [90., -90., -180., 180.]  # 经纬度范围 NSWE
        #
        self.m = None  # 主图对象
        self.m2 = None  # 南海小图对象
        self.ax2 = None  # 南海小图ax对象
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
        self.norm = None
        # lw: Line Width
        self.lw_boundray = 0.27
        self.lw_latlon = 0.2

    def _map_init(self, showArea=None, ax=None):
        '''
        地图底图
        '''
        if showArea == "China":
            self.projection = "aea"
            m = Basemap(  # width=5500000, height=4900000,
                projection=self.projection, \
                lat_1=25., lat_2=47, lon_0=105, lat_0=35,
                resolution=self.resolution, ax=ax,
                llcrnrlon=78, llcrnrlat=13,
                urcrnrlon=146, urcrnrlat=51)

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
            m = Basemap(projection=self.projection, lon_0=lon0,
                        lat_0=lat0, resolution=self.resolution, ax=ax)

        # 背景颜色
        if self.show_bg_color:
            m.fillcontinents(
                color=self.color_land, lake_color=self.color_ocean)
            m.drawmapboundary(fill_color=self.color_ocean)

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
        ax_self = plt.gca()
        if self.projection == "ortho":
            # set earth no edge line
            for eachpatch in ax_self.patches:
                eachpatch.set_linewidth(EDGE_LW)
                eachpatch.set_edgecolor(self.color_ticker)
        else:
            spines = ax_self.spines
            for eachspine in spines:
                spines[eachspine].set_linewidth(EDGE_LW)

        return m

    def easyplot(self, lats, lons, values, box=None,
                 vmin=None, vmax=None, colormap=None,  # color=None,
                 marker='o', markersize=6, ptype=None, alpha=1):
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
        elif lats.shape == lons.shape == values.shape:
            if values.ndim == 1:
                ptype = None
        elif values.ndim >= 3 and lats.shape == lons.shape == values.shape[:2]:
            pass
        else:
            raise ValueError("values' shape not match lots, lons")

        self.lons, self.lats = lons, lats
        self.values = values

        if colormap:
            self.colormap = colormap
        self.marker = marker
        self.markersize = markersize

        # default show global
        if box:
            self.box = box

        if self.colorbar_extend not in ["neither", "both"]:
            self.colorbar_extend = "neither"

        # set Area
        area = None
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

        self.ptype = ptype

        # value min max
        if type(values).__name__ != "str":
            if vmin is None:
                self.valmin = np.min(values)
            else:
                self.valmin = vmin
            if vmax is None:
                self.valmax = np.max(values)
            else:
                self.valmax = vmax
            # 颜色标尺
            self.norm = mpl.colors.Normalize(
                vmin=self.valmin, vmax=self.valmax)

        # ## Drawing
        self.drawOnMap(self.m, alpha)

        self.draw_boundary()

        if self.m2:
            self.drawOnMap(self.m2, alpha)

    def drawOnMap(self, m, alpha):
        '''
        在地图上的值
        '''
        if self.lats is None or self.lons is None or self.values is None:
            self.show_colorbar = False
            return
        ptype = self.ptype
        var = self.values
        zorder = 2

        x1, y1 = m(self.lons, self.lats)
        if ptype == 'pcolormesh':
            cs = m.pcolormesh(x1, y1, var, norm=self.norm, rasterized=True,
                              cmap=self.colormap, zorder=zorder, alpha=alpha)
        elif ptype == 'pcolor':
            cs = m.pcolor(
                x1, y1, var, norm=self.norm, cmap=self.colormap, zorder=zorder, alpha=alpha)
        elif ptype == 'imshow':
            cs = m.imshow(
                var, interpolation="spline16", cmap=self.colormap, zorder=zorder, alpha=alpha)
        elif ptype == 'contour':
            cs = m.contour(x1, y1, var, 10, linewidths=0.5,
                           cmap=self.colormap, zorder=zorder, alpha=alpha)
        elif ptype == 'contourf':
            if self.colorbar_bounds is None:
                step = (self.valmax - self.valmin) / 512.
                clevs = np.arange(self.valmin, self.valmax + step * 0.1, step)
            else:
                clevs = self.colorbar_bounds

            cs = m.contourf(
                x1, y1, var, clevs, cmap=self.colormap, zorder=zorder, alpha=alpha)
        else:
            cs = m.scatter(x1, y1, marker=self.marker, s=self.markersize, c=var,
                           cmap=self.colormap, norm=self.norm, lw=0, zorder=zorder, alpha=alpha)

        vertices = []
        codes = []
        if self.show_inside_china:
            sf = shapefile.Reader(os.path.join(selfPath, u'SHP/中国省级行政区'))
            for shape_rec in sf.shapeRecords():
                pts = shape_rec.shape.points
                prt = list(shape_rec.shape.parts) + [len(pts)]
                for i in range(len(prt) - 1):
                    for j in range(prt[i], prt[i + 1]):
                        vertices.append(m(pts[j][0], pts[j][1]))
                    codes += [Path.MOVETO]
                    codes += [Path.LINETO] * (prt[i + 1] - prt[i] - 2)
                    codes += [Path.CLOSEPOLY]

        # TODO ## 报错，暂时去除
        # sf = shapefile.Reader(os.path.join(selfPath, u'SHP/中国省级行政区'))
        # for shape_rec in sf.shapeRecords():
        #     strName = shape_rec.record[0].decode('gbk')
        #     for eachArea in self.areaNameLst:
        #         if eachArea in strName:
        #             pts = shape_rec.shape.points
        #             prt = list(shape_rec.shape.parts) + [len(pts)]
        #             for i in range(len(prt) - 1):
        #                 for j in range(prt[i], prt[i + 1]):
        #                     vertices.append(m(pts[j][0], pts[j][1]))
        #                 codes += [Path.MOVETO]
        #                 codes += [Path.LINETO] * (prt[i + 1] - prt[i] - 2)
        #                 codes += [Path.CLOSEPOLY]

        ax = plt.gca()  # current ax is not main ax
        if len(codes) > 0:
            clip = Path(vertices, codes)
            clip = PathPatch(clip, transform=ax.transData)

            if hasattr(cs, "collections"):
                # for eachClip in clipLst:
                for contour in cs.collections:
                    contour.set_clip_path(clip)
            else:
                cs.set_clip_path(clip)

    def draw_boundary(self):
        '''
        边界线
        '''
        if self.show_china_boundary:
            self.m.readshapefile(os.path.join(selfPath, u'SHP/国界'), 'china',
                                 linewidth=self.lw_boundray,
                                 color=self.color_contry)

        if self.show_coastlines:
            # 画 海岸线
            self.m.drawcoastlines(
                linewidth=self.lw_boundray, color=self.color_coast)

        if self.show_countries:
            # 画 国境线
            self.m.drawcountries(
                linewidth=self.lw_boundray, color=self.color_contry)

        if self.show_china_province:
            # 画省线
            #             self.m.readshapefile(os.path.join(selfPath, 'SHP/CHN_adm1'), 'province',
            # linewidth=0.2, color=color_contry)
            self.m.readshapefile(os.path.join(selfPath, u'SHP/省界'), 'province',
                                 linewidth=self.lw_boundray,
                                 color=self.color_contry)

        if self.show_china_county:
            # 画县市线
            #             self.m.readshapefile(os.path.join(selfPath, 'SHP/CHN_adm1'), 'province',
            # linewidth=0.2, color=color_contry)
            self.m.readshapefile(os.path.join(selfPath, u'SHP/中国县市'), 'province',
                                 linewidth=self.lw_boundray * 0.8,
                                 color=self.color_contry)

        if self.show_north_pole or self.show_south_pole:
            self._setLatsLabelWithinFig()

        if self.show_china:
            # 画南海 十段线
            ax = plt.gca()
            axins = zoomed_inset_axes(ax, 0.55, loc=4, borderpad=0.4)
            axins.set_xlim(105, 125)
            axins.set_ylim(5, 25)

            plt.xticks(visible=False)
            plt.yticks(visible=False)

            map2 = Basemap(projection='aea',
                           lat_1=25., lat_2=47, lon_0=105, lat_0=35,
                           resolution=self.resolution, ax=axins,
                           llcrnrlon=106, llcrnrlat=2,
                           urcrnrlon=124, urcrnrlat=25)

            if self.show_coastlines:
                map2.drawcoastlines(
                    linewidth=self.lw_boundray, color=self.color_coast)

            if self.show_bg_color:
                map2.fillcontinents(
                    color=self.color_land, lake_color=self.color_ocean)
                map2.drawmapboundary(fill_color=self.color_ocean)

            map2.readshapefile(os.path.join(selfPath, u'SHP/国界'), 'china',
                               linewidth=self.lw_boundray, color=self.color_contry)

            self.m2 = map2
            self.ax2 = axins

            # box line width
            spines = axins.spines
            for eachspine in spines:
                spines[eachspine].set_linewidth(EDGE_LW * 0.9)

    def counties_boundary(self, name,
                          drawbounds=True, zorder=None,
                          linewidth=0.5, color='k',
                          default_encoding='utf-8'):
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
        # shp.default_encoding = default_encoding

        shapefile = os.path.join(selfPath, u'SHP/中国县市')
        if not os.path.exists('%s.shp' % shapefile):
            raise IOError('cannot locate %s.shp' % shapefile)
        if not os.path.exists('%s.shx' % shapefile):
            raise IOError('cannot locate %s.shx' % shapefile)
        if not os.path.exists('%s.dbf' % shapefile):
            raise IOError('cannot locate %s.dbf' % shapefile)
        # open shapefile, read vertices for each object, convert
        # to map projection coordinates (only works for 2D shape types).
        try:
            shf = Reader(shapefile)
        except:
            raise IOError('error reading shapefile %s.shp' % shapefile)
        fields = shf.fields
        coords = []
        attributes = []
        msg = "dummy"
        shptype = shf.shapes()[0].shapeType
        bbox = shf.bbox.tolist()
        info = (shf.numRecords, shptype, bbox[
                                         0:2] + [0., 0.], bbox[2:] + [0., 0.])
        npoly = 0
        for shprec in shf.shapeRecords():
            #             print type(shprec.record[4])
            if name != str(shprec.record[4]):
                continue

            shp = shprec.shape
            rec = shprec.record
            npoly = npoly + 1
            if shptype != shp.shapeType:
                raise ValueError(
                    'readshapefile can only handle a single shape type per file')
            if shptype not in [1, 3, 5, 8]:
                raise ValueError(
                    'readshapefile can only handle 2D shape types')
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

    # def add_background_rgb(self, lats, lons, aryR, aryG, aryB, facecolor = "k", **kwargs):
    #     '''
    #     为地图添加RGB合成背景,
    #     aryR, aryG, aryB 中的无效值需要提前设好，比如0
    #     '''
    #     self._plot_rgb(
    #         self.ax, self.m, self.box, lats, lons, aryR, aryG, aryB, facecolor, **kwargs)
    #     self._plot_rgb(self.ax2, self.m2, [
    #                    60, 10, 70, 150], lats, lons, aryR, aryG, aryB, facecolor, **kwargs)
    #
    # def _plot_rgb(self, ax, m, box, lats, lons, aryR, aryG, aryB, facecolor, **kwargs):
    #     '''
    #    画RGB
    #     '''
    #     if ax is None or m is None:
    #         return
    #
    #     ax.set_facecolor(facecolor)  # facecolor只能是 黑"k" 或 白"w"
    #
    #     if "linear" in kwargs.keys():  # 线性拉伸
    #         linear = kwargs["linear"]
    #     else:
    #         linear = 0.02
    #
    #     alpha = 1.0  # 透明度
    #     if "alpha" in kwargs.keys():
    #         alpha = kwargs["alpha"]
    #     if alpha > 1:
    #         alpha = 1.0
    #     if alpha < 0:
    #         alpha = 0.0
    #
    #     # 投影成等经纬
    #     if "res" in kwargs.keys():  # 分辨率
    #         res = kwargs["res"]
    #     else:
    #         res = 0.05
    #     if len(box) != 4:
    #         box = [90., -90., -180., 180.]
    #     LatN, LatS, LonW, LonE = box
    #     gll = prj_gll(
    #         nlat = LatN, slat = LatS, wlon = LonW, elon = LonE, resLat = res, resLon = res)
    #
    #     projLats, projLons = gll.generateLatsLons()
    #     proj_i, proj_j = gll.lonslats2ij(lons, lats)
    #     raw_i, raw_j = np.mgrid[0:lats.shape[0]:1, 0:lats.shape[1]:1]
    #
    #     # inside box
    #     index = np.logical_and.reduce((proj_i >= 0, proj_i < projLats.shape[0],
    #                                    proj_j >= 0, proj_j < projLats.shape[1]))
    #     proj_i = proj_i[index]
    #     proj_j = proj_j[index]
    #     raw_i = raw_i[index]
    #     raw_j = raw_j[index]
    #
    #     RR = np.zeros_like(projLats)
    #     GG = np.zeros_like(projLats)
    #     BB = np.zeros_like(projLats)
    #
    #     RR[proj_i, proj_j] = aryR[raw_i, raw_j]
    #     GG[proj_i, proj_j] = aryG[raw_i, raw_j]
    #     BB[proj_i, proj_j] = aryB[raw_i, raw_j]
    #
    #     for _ in range(3):  # 补点3次
    #         fill_points_2d(RR, 0.)
    #         fill_points_2d(GG, 0.)
    #         fill_points_2d(BB, 0.)
    #
    #     lats = projLats
    #     lons = projLons
    #     aryR = RR
    #     aryG = GG
    #     aryB = BB
    #
    #     # RGB color must be in range 0. - 1.
    #     aryR = linearStretch(norm255(aryR), linear) / 255.
    #     aryG = linearStretch(norm255(aryG), linear) / 255.
    #     aryB = linearStretch(norm255(aryB), linear) / 255.
    #
    #     if facecolor == "w":  # set bg color white
    #         condition = np.logical_and.reduce(
    #             (aryR == 0, aryG == 0, aryB == 0))
    #         aryR[condition] = 1
    #         aryG[condition] = 1
    #         aryB[condition] = 1
    #
    #     # get corner lats lons
    #     cornerLats = getCorners(lats)
    #     cornerLons = getCorners(lons)
    #     xCorners, yCorners = m(cornerLons, cornerLats)
    #
    #     # combine to RGB tuple
    #     colorTuple = tuple(np.array([aryR.ravel(),
    #                                  aryG.ravel(),
    #                                  aryB.ravel()]).transpose().tolist())
    #
    #     # add alpha, RGB -> RGBA
    #     colorTuple = np.insert(colorTuple, 3, alpha, axis = 1)
    #     # draw background
    #     quad = m.pcolormesh(
    #         xCorners, yCorners, aryR, color = colorTuple, clip_on = True, linewidth = 0)
    #     quad.set_array(None)

    def add_landmark(self, lonlatlocation, color="r", marker="star", markersize=70, fontsize=7.5, yplus=None):

        if marker == "star":
            marker = (5, 1, 0)
        if yplus == None:
            if self.projection == "aea":
                yplus = 75000
            elif self.projection == "ortho":
                yplus = 160000
            elif self.projection == "cyl":
                yplus = 0.05
            else:
                yplus = 3
        for lon, lat, location in lonlatlocation:
            xpt, ypt = self.m(lon, lat)
            #         circle1 = mpl.patches.Star.Circle((xpt, ypt), 6, color=RED, ec=EDGE_GRAY, lw=0.3)
            self.m.scatter(xpt, ypt, marker=marker, s=markersize,
                           color=color, edgecolors="#999999", lw=0.1, zorder=10)
            fnt = get_DV_Font("SourceHanSansCN-Medium.otf")

            if self.projection == "cyl":
                self.ax.annotate(location, xy=(
                    lon, lat + yplus), ha="center", fontproperties=fnt, size=fontsize, color=COLOR_Darkgray)
            else:
                self.ax.annotate(location, xy=(
                    xpt, ypt + yplus), ha="center", fontproperties=fnt, size=fontsize, color=COLOR_Darkgray)

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
        labellen = max(str_len(self.colorbar_fmt % self.valmin), str_len(
            self.colorbar_fmt % self.valmax))
        width = self.fig.get_size_inches()[0]

        colorbar_len = 8
        leg2inch = self.fontsize_cbtick / 144.
        right_inch = (labellen + colorbar_len) * leg2inch
        lspace = right_inch / width
        if (lspace * 0.8) > (1. - lspace):
            raise ValueError(
                "Plase set arg 'figsize=(width, height)' when using dv_map, and increase width a little.")

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

    def setArea(self, shengName):
        '''
        设定显示的区域
        shpName   shape文件名
        areaNameLst  地区名，shape文件中包含的地区
        '''
        if type(shengName).__name__ == "unicode":
            self.areaNameLst.append(shengName)
        elif type(shengName).__name__ == "list":
            self.areaNameLst = self.areaNameLst + shengName

    def _setLatsLabelWithinFig(self):
        if self.show_south_pole:
            lat_labels = [
                (0.0, -90.0), (0.0, -80.0), (0.0, -70.0), (0.0, -60.0)]
            for lon, lat in (lat_labels):
                xpt, ypt = self.m(lon, lat)
                self.ax.text(
                    xpt, ypt, str(lat)[1:3] + u'°S', fontproperties=self.font_leg)
        elif self.show_north_pole:
            lat_labels = [(0.0, 90.0), (0.0, 80.0), (0.0, 70.0), (0.0, 60.0)]
            for lon, lat in (lat_labels):
                xpt, ypt = self.m(lon, lat)
                self.ax.text(xpt, ypt, str(lat)[0:2] + u'°N', va="top",
                             fontproperties=self.font_leg)


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
    # 全球-----------------------

    lats, lons = np.mgrid[70:0:-6, 50: 150: 6]
    values = np.random.random_integers(0., 10000., lats.shape)
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

    #     # 全球 ---------------------
    #     p = dv_map(theme="dark")
    #     p.colorbar_fmt = "%d"
    #     p.show_line_of_latlon = False
    #     p.show_countries = False
    #     p.colorbar_bounds = range(0, 10001, 1000)
    #     p.easyplot(lats, lons, values, ptype='contourf', vmin=0, vmax=10000)
    #     p.title = u'全球分布图'
    #     # 在北京的位置画颗星星
    #     lonlatlocation = [(116.30, 40.03, u"北京")]
    #     p.add_landmark(lonlatlocation)
    #     p.savefig('glb.png')

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

    #     # 北京
    #     p = dv_map(figsize=(6.6, 6.), theme="dark")
    #     p.colorbar_fmt = "%d"
    #     p.show_coastlines = False
    #     p.show_countries = False
    #     p.show_china_province = True
    #     p.show_line_of_latlon = True
    #     p.setArea(u"北京市")
    #     p.projection = "aea"
    #     p.delat = 1
    #     p.delon = 2
    #     p.lw_boundray = 0.54
    #     p.colorbar_bounds = range(0, 10001, 1000)
    #     p.easyplot(lats, lons, values, box=[41.5, 38.5, 113.9, 118.9], ptype='contourf', vmin=0, vmax=10000)
    #     p.title = u'北京区域分布图'
    #     p.savefig('beijing.png')

    # 赣州
    p = dv_map(figsize=(6.6, 6.))
    box = [28, 24, 113, 117]
    p.colorbar_fmt = "%d"
    p.show_coastlines = False
    p.show_countries = False
    #     p.show_china_province = True
    p.show_line_of_latlon = True
    #     p.setArea(u"北京市")
    #     p.projection = "aea"
    p.delat = 1
    p.delon = 2
    p.lw_boundray = 0.54
    p.colorbar_bounds = range(0, 10001, 1000)
    p.easyplot(
        lats, lons, values, box=box, ptype='contourf', vmin=0, vmax=10000)
    p.counties_boundary(u"赣州市")
    lonlatlocation = [(115.27, 25.81, u"赣州市")]
    p.add_landmark(lonlatlocation, marker="", yplus=0)

    p.title = u'区域分布图'
    p.savefig('ganzhou.png')
