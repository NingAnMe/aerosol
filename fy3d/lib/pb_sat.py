# coding: utf-8
from __future__ import division

from datetime import datetime

import numpy as np


def solar_zen(yy, mm, dd, hh, xlon, xlat):
    """
    calculate solar zenith
    """
    stime = datetime.strptime('%s%s%s' % (yy, mm, dd), '%Y%m%d')
    xj = int(stime.strftime('%j'))
    # xj = getJulianDay(yy, mm, dd)

    tsm = hh + xlon / 15.  # GMT(Hour)-->local time(Hour)
    fac = np.pi / 180.

#     xlo=xlon.*fac #degree to rad
    xla = xlat * fac

    a1 = (1.00554 * xj - 6.28306) * fac
    a2 = (1.93946 * xj + 23.35089) * fac
    et = -7.67825 * np.sin(a1) - 10.09176 * np.sin(a2)
    tsv = tsm + et / 60. - 12.

    ah = tsv * 15. * fac

    a3 = (0.9683 * xj - 78.00878) * fac
    delta = 23.4856 * np.sin(a3) * fac

    amuzero = np.sin(xla) * np.sin(delta) + np.cos(xla) * \
        np.cos(delta) * np.cos(ah)

    elev = np.arcsin(amuzero) * 180. / np.pi

    asol = 90. - elev
    return asol


def getasol6s(ymd, hms, lons, lats):
    """
    Function:    getasol6s
    Description: 计算太阳天顶角
    author:      陈林提供C源码( wangpeng转)
    date:        2017-03-22
    Input:       ymd hms : 20180101 030400
    Output:
    Return:      太阳天顶角弧度类型(修改为度类型 2018年4月28日)
    Others:
    """
    # jays(儒略日,当年的第几天), GMT(世界时 小时浮点计数方式 )
    dtime = datetime.strptime('%s %s' % (ymd, hms), '%Y%m%d %H%M%S')
    jday = int(dtime.strftime('%j'))
#     print jday
    GMT = float(hms[0:2]) + float(hms[2:4]) / 60.
#     print GMT
    lats = lats * np.pi / 180.
#     lons = lons * np.pi / 180.
    b1 = 0.006918
    b2 = 0.399912
    b3 = 0.070257
    b4 = 0.006758
    b5 = 0.000907
    b6 = 0.002697
    b7 = 0.001480

    a1 = 0.000075
    a2 = 0.001868
    a3 = 0.032077
    a4 = 0.014615
    a5 = 0.040849
    A = 2 * np.pi * jday / 365.0
    delta = b1 - b2 * np.cos(A) + b3 * np.sin(A) - b4 * np.cos(2 * A) + \
        b5 * np.sin(2 * A) - b6 * np.cos(3 * A) + b7 * np.sin(3 * A)
    ET = 12 * (a1 + a2 * np.cos(A) - a3 * np.sin(A) - a4 *
               np.cos(2 * A) - a5 * np.sin(2 * A)) / np.pi
    MST = GMT + lons / 15.0
    TST = MST + ET
    t = 15.0 * np.pi / 180.0 * (TST - 12.0)

    asol = np.arccos(
        np.cos(delta) * np.cos(lats) * np.cos(t) + np.sin(delta) * np.sin(lats))
    return np.rad2deg(asol)


def sun_earth_dis_correction(ymd):
    """
    Instantaneous distance between earth and sun correction factor ==(d0/d)^2
    ymd: yyyymmdd
    """

    stime = datetime.strptime(ymd, '%Y%m%d')
    jjj = int(stime.strftime('%j'))
    OM = (0.9856 * (jjj - 4)) * np.pi / 180.
    dsol = 1. / ((1. - 0.01673 * np.cos(OM)) ** 2)
    return dsol


def sun_glint_cal_old(obs_a, obs_z, sun_a, sun_z):
    """
    计算太阳耀斑角
    """
#      https://svn.ssec.wisc.edu/repos/cloud_team_cr/trunk/viewing_geometry_module.f90
#      glint_angle = cos ( sol_zen * DTOR ) * cos ( sen_zen * DTOR )
#                +   sin ( sol_zen * DTOR ) * sin ( sen_zen * DTOR )
#                * cos ( rel_az * DTOR )
#
#      glint_angle = max(-1.0 , min( glint_angle ,1.0 ) )
#
#      glint_angle = acos(glint_angle) / DTOR

    ti = np.deg2rad(sun_z)
    tv = np.deg2rad(obs_z)
    phi = np.deg2rad(sun_a - obs_a)
    cos_phi = np.cos(phi)

    cos_tv = np.cos(tv)
    cos_ti = np.cos(ti)
    sin_tv = np.sin(tv)
    sin_ti = np.sin(ti)
    cos_res = cos_ti * cos_tv + sin_ti * sin_tv * cos_phi
#     cos_res = cos_ti * cos_tv - sin_ti * sin_tv * cos_phi  # 徐寒冽修正
    v_arrayMin = np.vectorize(arrayMin)
    Min = v_arrayMin(-cos_res, 1.0)
#     Min = v_arrayMin(cos_res, 1.0)  # 徐寒冽修正

    v_arrayMax = np.vectorize(arrayMax)
    Max = v_arrayMax(Min, -1.)
    res = np.arccos(Max)
    glint = np.rad2deg(res)

    return glint


def sun_glint_cal(obs_a, obs_z, sun_a, sun_z):
    """
    计算太阳耀斑角
    """
    ti = np.deg2rad(sun_z)
    tv = np.deg2rad(obs_z)
    phi = np.deg2rad(sun_a - obs_a)
    cos_phi = np.cos(phi)

    cos_tv = np.cos(tv)
    cos_ti = np.cos(ti)
    sin_tv = np.sin(tv)
    sin_ti = np.sin(ti)
#     cos_res = cos_ti * cos_tv + sin_ti * sin_tv * cos_phi
    cos_res = cos_ti * cos_tv - sin_ti * sin_tv * cos_phi  # 徐寒冽修正
    v_arrayMin = np.vectorize(arrayMin)
#     Min = v_arrayMin(-cos_res, 1.0)
    Min = v_arrayMin(cos_res, 1.0)  # 徐寒冽修正

    v_arrayMax = np.vectorize(arrayMax)
    Max = v_arrayMax(Min, -1)
    res = np.arccos(Max)
    glint = np.rad2deg(res)

    return glint


def arrayMax(array_a, b):
    return max(array_a, b)


def arrayMin(array_a, b):
    return min(array_a, b)


def spec_interp(WaveNum1, WaveRad1, WaveNum2):
    """
    IN, WaveNum1:原光谱的波数(cm-1)
    IN, WaveRad1:原光谱的响应值（0-1）
    IN, WaveNum2:目标光谱的波数
    Rturn, Wave_rad2：目标谱的响应值（0-1）
    """

    # 插值插值，过滤掉最大值的千分之9的数据
    maxRad = np.max(WaveRad1)
    idx = np.where(WaveRad1 < maxRad * 0.009)
    WaveRad1[idx] = 0.
    WaveRad2 = np.interp(WaveNum2, WaveNum1, WaveRad1, 0, 0)
    return WaveRad2


def spec_convolution(WaveNum, WaveRad, RealRad):
    """
    IN, WaveNum: 光谱的波数(cm-1)
    IN, WaveRad: 光谱的响应值（0-1）
    IN, RealRad: 光谱的真实响应值
    return , S 卷积后的响应值
    """
    # RealRad的光谱信息必须和WaveNum,WaveRad的长度一致
    if WaveNum.shape[-1] != WaveRad.shape[-1] != RealRad.shape[-1]:
        print('The spectral response length must be the same')
        return -1

    # 默认对最后一维进行卷积 numpy.trapz(y, x=None, dx=1.0, axis=-1)
    s1 = np.trapz(RealRad * WaveRad, WaveNum)
    s2 = np.trapz(WaveRad, WaveNum)
    S = (s1 / s2)
    return S


def planck_r2t_test(r, w):
    """
    function radiance2tbb: convert radiance data into brightness temperature (i.e., equivalent blackbody temperature)
    r: spectral radiance data in w/m2/sr/um  单位(mW/(m2.cm-1.sr))
    w: wavelength in micro  Equiv Mid_wn (cm-1)  等效中心波数
    a: TbbCorr_Coeff A  (典型温度黑体辐亮度以及通道亮温修正系数)
    b: TbbCorr_Coeff B
    return: reture value, brightness temperature in K (absolute temperature)
    """
    c1 = 1.1910439e-16  # 1.19104*10-5 mW/m2.sr.cm-1
    c2 = 1.438769e-2  # 1.43877 K/cm-1
    vs = 1.0E+2 * w
    tbb = c2 * vs / np.log(c1 * vs ** 3 / (1.0E-5 * r) + 1.0)

    return tbb

# def plank_iras_rad2tb(ch_num, Rad_array):
#     '''
#     plank for IRAS rad2tb
#     ch_num starts from 1
#     '''
#     c1 = 0.000011910659
#     c2 = 1.438833
#     ich = ch_num - 1
#     a1 = c2 * wn[ich]
#     a2 = np.log(1 + (c1 * wn[ich] ** 3) / Rad_array)
#     # a2 = np.log(1 + (c1 * wn[ich] ** 3) / np.array(Rad_array))
#     Tbb = a1 / a2
#     return Tbb


# def plank_iras_tb2rad(T, W, a=None, b=None):
#     '''
#     plank for IRAS tb2rad
#     T : TBB
#     W : center wavenums
#
#     '''
#     c1 = 0.000011910659
#     c2 = 1.438833
#     a1 = c1 * W ** 3
#     a2 = (np.exp(c2 * W / T) - 1.0)
#     Rad = a1 / a2
#     return Rad


def planck_r2t(R, W):
    """
    plank for IRAS rad2tb
    R: radiance
    W: center wavenums
    """
    c1 = 0.000011910659
    c2 = 1.438833
    a1 = c2 * W
    a2 = np.log(1 + (c1 * W ** 3) / R)
    # a2 = np.log(1 + (c1 * wn[ich] ** 3) / np.array(Rad_array))
    Tbb = a1 / a2
    return Tbb


def planck_t2r(T, W):
    """
    plank for IRAS tb2rad
    T : TBB
    W : center wavenums

    """
    c1 = 0.000011910659
    c2 = 1.438833
    a1 = c1 * W ** 3
    a2 = (np.exp(c2 * W / T) - 1.0)
    Rad = a1 / a2
    return Rad


def radiance2tbb(r, cnw):
    """
    function radiance2tbb: convert radiance data into brightness temperature (i.e., equivalent blackbody temperature)
    r: spectral radiance data in w/m2/sr/um
    w: wavelength in micro
    return: reture value, brightness temperature in K (absolute temperature)
    """

#     cwn = [2.647409E+03, 2.511760E+03, 2.517908E+03, 2.462442E+03,
#            2.248296E+03, 2.209547E+03, 1.474262E+03, 1.361626E+03,
#            1.169626E+03, 1.028740E+03, 9.076813E+02, 8.308411E+02,
#            7.482978E+02, 7.307766E+02, 7.182094E+02, 7.035007E+02]
#
#     tcs = [9.993363E-01, 9.998626E-01, 9.998627E-01, 9.998707E-01,
#            9.998737E-01, 9.998770E-01, 9.995694E-01, 9.994867E-01,
#            9.995270E-01, 9.997382E-01, 9.995270E-01, 9.997271E-01,
#            9.999173E-01, 9.999070E-01, 9.999198E-01, 9.999233E-01]
#
#     tci = [4.818401E-01, 9.426663E-02, 9.458604E-02, 8.736613E-02,
#            7.873285E-02, 7.550804E-02, 1.848769E-01, 2.064384E-01,
#            1.674982E-01, 8.304364E-02, 1.343433E-01, 7.135051E-02,
#            1.948513E-02, 2.131043E-02, 1.804156E-02, 1.683156E-02]

    h = 6.62606876e-34  # Planck constant (Joule second)
    c = 2.99792458e+8
    k = 1.3806503e-23

    c1 = 2.0 * h * c * c
    c2 = (h * c) / k
    w = 1.0e+4 / cnw
    ws = 1.0e-6 * w

    tbb = c2 / (ws * np.log(c1 / (1.0e+6 * r * ws ** 5) + 1.0))
#     r = (r - i) / s

    return tbb


def is_ad_orbit(lats):
    """
    lats: input latitude  range: -90 ~ 90.

    """
    diff_list = []
    if not isinstance(lats, np.ndarray):
        print('lats must be ndarray')
    if lats.ndim != 2:
        print('lats shape must be 2')

    # change 0-180
    lats = lats + 90.
    row, col = lats.shape
#     print row, col
    mcol = col // 2
    for i in range(row - 1):
        diff = lats[i + 1, mcol] - lats[i, mcol]
        diff_list.append(diff)
    diff_array = np.array(diff_list)
    d_idx = np.where(diff_array < 0.)
    a_idx = np.where(diff_array > 0.)
    d_100 = len(d_idx[0]) / row * 100
    a_100 = len(a_idx[0]) / row * 100
    if a_100 >= d_100:
        return 'A'
    else:
        return 'D'
