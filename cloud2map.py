# coding:utf-8
"""
Created on 2011-12-14

@author: zhangtao
"""
from pylab import *
import h5py

from mpl_toolkits.axes_grid1 import make_axes_locatable
import matplotlib.patches as mpatches

localpath = os.path.split(os.path.realpath(__file__))[0]  # 取得本程序所在目录

Overwrite = True


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
        pat = u'(FY3[A-Z])_MERSI_ORBT_L2_CLM_MLT_NUL_(\d{8})_(\d{4})_1000M_MS'
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
            print 'Error HDF5:', self.fullpath
        else:
            # 数据集
            self.cloudMask = h5file.get('Cloud_Mask')[0, :, :]
    #        # lat
    #        self.Latitude = h5file.get('Latitude')[:,:]
    #        # lon
    #        self.Longitude = h5file.get('Longitude')[:,:]
            h5file.close()


color = {}
color['Cloudy'] = (1., 1., 1.)  # 生成白色，这个地方是云
color['Uncertain'] = (169 / 255., 169 / 255., 169 / 255.)  # 生成灰色，这个地方不确定
color['Land Clear'] = (0, 128 / 255., 0)  # 生成绿色，这个地方陆地晴空
color['Poss Land Clear'] = (0., 1., 0.)  # 生成亮绿，这个地方陆地也许晴空
color['Sea Clear'] = (25 / 255., 25 / 255., 112 / 255.)  # 生成深蓝，这个地方水面晴空
color['Poss Sea Clear'] = (65 / 255., 105 / 255., 225 / 255.)  # 生成淡蓝，这个地方水面也许晴空
color['Invalid'] = (0., 0., 0.)  # 生成黑色，这个地方数据无效
color['Sun Glint'] = (255 / 255., 0., 0.)  # 生成红色，这个地方水面太阳耀斑
color['Coastlines'] = (255 / 255., 215 / 255., 0)  # 生成黄色，这个是海岸线


def drawLayer(mask, rgb):
    '''
    mask: array of mask
    rgb: color of True in mask
    '''
    row, col = mask.shape
    r, g, b = rgb
    z = np.zeros((row, col))
    z = ma.masked_where(mask, z)
    z = ma.filled(z, 1)

    print z.shape

    rgba = np.array((r, g, b, 0) * row * col)
    print rgba.shape
    rgba.shape = row, col, 4
    print type(rgba[0, 0, 0]), rgb

    rgba[:, :, 3] = z

    subplot(111)
    imshow(rgba, interpolation='none')


def run(hdf5path):
    if not hdf5path[-3:] == 'HDF':
        return

    mpl.rcParams['figure.figsize'] = (5, 5.3)

#    config = getParam()
    outpath = os.path.join(os.path.split(hdf5path)[0], 'pic')

    hdf = cloudMaskHdf(hdf5path)
    if not hdf.ymd:
        return

    z = hdf.cloudMask
    print type(z[0, 0])

    try:
        if not os.path.isdir(outpath):
            os.makedirs(outpath)
    except:
        print '**[%s]**mkdir error because of MPI' % outpath
        pass

    nm = hdf.fileName[:-4]  # Name without .hdf
    figfp = os.path.join(outpath, '%s.png' % nm)

    if (Overwrite is False) and (os.path.isfile(figfp)):
        return  # 不覆盖

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
    plt.title(nm, fontsize=7)

    drawRects(ax)

    plt.savefig(figfp, dpi=300)
    plt.close()
    fig.clear()

colorkeys = ['Cloudy', 'Uncertain', 'Land Clear', 'Poss Land Clear',
             'Sea Clear', 'Poss Sea Clear', 'Invalid', 'Sun Glint', 'Coastlines']


def drawRects(ax):
    '''
    色标
    '''
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

# 画一个文件夹下的所有txt


def hdfinfolder(folderpath):
    '''
    folderpath: 文件夹路径
    '''
    # 文件夹不存在，则返回
    if not os.path.isdir(folderpath):
        return

    for eachfile in os.listdir(folderpath):
        # 画每个文件
        fp = os.path.join(folderpath, eachfile)
        if os.path.isdir(fp):
            hdfinfolder(fp)
        else:
            run(fp)

# def getParam():
#    '''
#    读取配置参数
#    '''
#    config = ConfigParser.SafeConfigParser()
#    file_name = os.path.join(localpath, 'param.ini')
#    config.read(file_name) # 读取地域范围配置文件
#    return config


def main():
    '''
    控制台输入参数：文件全路径
    '''
    xArg = sys.argv[1:]  # print xArg
    if len(xArg) == 1:
        if os.path.isdir(xArg[0]):
            hdfinfolder(xArg[0])
        if os.path.isfile(xArg[0]):
            run(xArg[0])

if __name__ == '__main__':
    main()
