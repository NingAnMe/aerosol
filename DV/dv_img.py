# coding: utf-8
'''
Created on 2015-11-6

@author: zhangtao
'''
import numpy as np
from PIL import Image
# import colorsys
# import cv2
# from math import ceil

# rgb_to_hsv = np.vectorize(colorsys.rgb_to_hsv)
# hsv_to_rgb = np.vectorize(colorsys.hsv_to_rgb)


def dv_rgb(aryR, aryG, aryB, picFile, linear=0, shrink=1):
    '''
    根据输入的数据，画快试图
    aryR：2维数据矩阵，红色通道
    aryG：2维数据矩阵，绿色通道
    aryB：2维数据矩阵，蓝色通道
    picFile：生成png文件的路径和名称
    linear：直方图线性拉伸百分比
    shrink: 矩阵精度缩减倍数
    '''
    if not aryR.shape == aryG.shape == aryB.shape:
        print("R/G/B must have same shape!")
        return
    arrR = norm255(aryR[::shrink, ::shrink])  # 红色数据集
    arrG = norm255(aryG[::shrink, ::shrink])  # 绿色数据集
    arrB = norm255(aryB[::shrink, ::shrink])  # 蓝色数据集

    if linear > 0:
        # 2% 线性拉伸
        arrR = linearStretch(arrR, linear / 100.)
        arrG = linearStretch(arrG, linear / 100.)
        arrB = linearStretch(arrB, linear / 100.)
#     # 非线性拉伸
#     arrR = customStretch(arrR)
#     arrG = customStretch(arrG)
#     arrB = customStretch(arrB)

    # 3通道合成
    imr = Image.fromarray(arrR.astype('uint8'))
    img = Image.fromarray(arrG.astype('uint8'))
    imb = Image.fromarray(arrB.astype('uint8'))
    im = Image.merge('RGB', (imr, img, imb))  # color image

    # baocun
    try:
        im.save(picFile)

    except Exception:
        print('Error when saving Pic: %s' % picFile)
    return


def invertAry(ary):
    """ Add color of the given hue to an RGB image.
 
    By default, set the saturation to 1 so that the colors pop!
    """
    ary = 255 - ary
    return ary


def alpha_composite(src, dst):
    '''
    Return the alpha composite of src and dst.

    Parameters:
    src -- PIL RGBA Image object
    dst -- PIL RGBA Image object

    The algorithm comes from http://en.wikipedia.org/wiki/Alpha_compositing
    '''
    # http://stackoverflow.com/a/3375291/190597
    # http://stackoverflow.com/a/9166671/190597
    src = np.asarray(src)
    dst = np.asarray(dst)
    out = np.empty(src.shape, dtype='float')
    alpha = np.index_exp[:, :, 3:]
    rgb = np.index_exp[:, :, :3]
    src_a = src[alpha] / 255.0
    dst_a = dst[alpha] / 255.0
    out[alpha] = src_a + dst_a * (1 - src_a)
    old_setting = np.seterr(invalid='ignore')
    out[rgb] = (src[rgb] * src_a + dst[rgb] * dst_a * (1 - src_a)) / out[alpha]
    np.seterr(**old_setting)
    out[alpha] *= 255
    np.clip(out, 0, 255)
    # astype('uint8') maps np.nan (and np.inf) to 0
    out = out.astype('uint8')
    out = Image.fromarray(out, 'RGBA')
    return out


def linearStretch(argArry, percent=0.02):
    '''
    线性拉伸, 去除最大最小的percent的数据，然后拉伸
    argArry：通道数据集
    percent：最大最小部分不参与拉伸的百分比
    return：RGB数据集
    '''
#     cv2.equalizeHist(argArry, argArry)
#     return argArry
    if percent == 0:
        return argArry

    hist, bins = np.histogram(argArry, 256)

    pixelnum = sum(hist)
    if float(hist[0]) / pixelnum > 0.5:
        return argArry
    if float(hist[-1]) / pixelnum > 0.5:
        return argArry

    cdf = hist.cumsum()  # 计算累积直方图

    i1 = 0
    i2 = 255
    for i in range(len(cdf)):
        if cdf[i] > pixelnum * percent and i1 == 0:
            i1 = i
        if cdf[i] > pixelnum * (1. - percent) and i2 == 255:
            i2 = i

    index1 = np.where(argArry < i1)
    index2 = np.where(argArry >= i2)
    mask = np.logical_or(argArry < i1, argArry >= i2)
    retArry = np.ma.masked_where(mask, argArry)
    maxValue = np.max(retArry)
    minValue = np.min(retArry)

    if maxValue == minValue: return argArry

    retArry = (retArry - minValue) * 255. / (maxValue - minValue)  # 数值拉伸到 0 ~ 255
    retArry = np.ma.filled(retArry, 0).astype('uint8')
    retArry[index1] = 0
    retArry[index2] = 255

    return retArry


def customStretch(argArry):
    '''   
    Table 3: Non-­‐‑linear Brightness Enhancement Table (non cloud)
    Input Brightness Output Brightness
    0 0
    30 110
    60 160
    120 210
    190 240
    255 255
    '''
#     fromList = [0, 30, 60, 120, 190, 255]
#     toList = [0, 110, 160, 210, 240, 255]

    fromList = [0, 30, 255]
    toList = [0, 120, 255]

    fromList = [0, 30, 60, 120, 190, 255]
    toList = [0, 110, 160, 210, 240, 255]

    retArry = np.zeros_like(argArry, dtype='float')

    for i in range(len(fromList) - 1):
        from_min = fromList[i]
        from_max = fromList[i + 1]
        to_min = toList[i]
        to_max = toList[i + 1]
        index = np.where(np.logical_and(argArry > from_min, argArry <= from_max))
        retArry[index] = (argArry[index] - from_min) * float(to_max - to_min) / float(from_max - from_min) + to_min  # 数值拉伸到

    return retArry.astype('uint8')


def norm255(argArry):
    '''
    把原通道数据集转换成RGB数据集
    argArry：通道数据集
    return：RGB数据集
    '''
    argArry[np.isnan(argArry)] = 0
    argArry[np.isinf(argArry)] = 0
    mask = (argArry <= 0)
    retArry = np.ma.masked_where(mask, argArry)
    maxValue = np.ma.max(retArry)
    minValue = np.ma.min(retArry)
#     print(maxValue, minValue)
    if maxValue == minValue: return argArry
    retArry = (retArry - minValue) * 255. / (maxValue - minValue)  # 数值拉伸到 0 ~ 255
    retArry = np.ma.filled(retArry, 0)
    return retArry.astype('uint8')


if __name__ == '__main__':
    pass

