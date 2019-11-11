# coding:utf-8
'''
Created on 2013-11-23

'''
from math import sin, cos, asin, acos, pi, sqrt
import numpy as np
# from numba import jit

# 角度 -> 弧度
DEGREES_TO_RADIANS = np.pi / 180.
# 弧度 -> 角度
RADIANS_TO_DEGREES = 180. / np.pi
# 地球平均半径
EARTH_MEAN_RADIUS_KM = 6371.009
# 地球极半径
EARTH_POLAR_RADIUS_KM = 6356.752
# 地球赤道半径
EARTH_EQUATOR_RADIUS_KM = 6378.137

WGS84_A = 6378137.0
WGS84_F = 1.0 / 298.257223563
WGS84_B = WGS84_A * (1.0 - WGS84_F)
WGS84_E2 = 2 * WGS84_F - WGS84_F ** 2


# Rotational angular velocity of Earth in radians/sec from IERS
#   Conventions (2003).
ANGVEL = 7.2921150e-5


def getasol6s(jday, GMT, lats, lons):
    '''
    Function:    getasol6s
    Description: 计算太阳天顶角
    author:      陈林提供C源码( wangpeng转)
    date:        2017-03-22
    Input:       jays(儒略日,当年的第几天), GMT(世界时 小时浮点计数方式 ), lats(纬度信息 矩阵类型), lons(经度信息 矩阵类型)
    Output:      
    Return:      太阳天顶角弧度类型
    Others: 
    '''
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
    delta = b1 - b2 * cos(A) + b3 * sin(A) - b4 * cos(2 * A) + \
        b5 * sin(2 * A) - b6 * cos(3 * A) + b7 * sin(3 * A)
    ET = 12 * (a1 + a2 * cos(A) - a3 * sin(A) - a4 *
               cos(2 * A) - a5 * sin(2 * A)) / np.pi
    MST = GMT + lons / 15.0
    TST = MST + ET
    t = 15.0 * np.pi / 180.0 * (TST - 12.0)

    asol = np.arccos(
        np.cos(delta) * np.cos(lats) * np.cos(t) + np.sin(delta) * np.sin(lats))

    return asol


def cal_G(lon, lat):
    # sqrt((earth_b/1000)^2*(cos(lat*!dtor))^2+(earth_a/1000)^2*(sin(lat*!dtor))^2))
    # ;曲率半径
    a = WGS84_A / 1000.
    b = WGS84_B / 1000.
    coslat = np.cos(np.deg2rad(lat))
    sinlat = np.sin(np.deg2rad(lat))
    # wangpeng  modify the old code bug 20180401
#     rad_earth2 = (a * b) / 1000. / np.sqrt(b ** 2 * (coslat ** 2 + a ** 2 + sinlat ** 2))
    rad_earth = (a * b) * 1000. / \
        np.sqrt(b ** 2 * coslat ** 2 + a ** 2 * sinlat ** 2)
    N_lat = rad_earth

    G = np.full((3,), 0.)
    H = 0
    G[0] = 1. * (N_lat + H) * np.cos(np.deg2rad(lat)) * np.cos(np.deg2rad(lon))
    G[1] = (N_lat + H) * np.cos(np.deg2rad(lat)) * np.sin(np.deg2rad(lon))
    G[2] = (N_lat + H) * np.sin(np.deg2rad(lat))

    return G[0], G[1], G[2]


def LLA2ECEF(lonIn, latIn, altIn):
    """
    Transform lon,lat,alt (WGS84 degrees, meters) to  ECEF
    x,y,z (meters)
    """
    lonRad = np.deg2rad(np.asarray(lonIn, dtype=np.float64))
    latRad = np.deg2rad(np.asarray(latIn, dtype=np.float64))
    alt = np.asarray(altIn, dtype=np.float64)
#     print 'alt', alt
    a, b, e2 = WGS84_A, WGS84_B, WGS84_E2

    # # N = Radius of Curvature (meters), defined as:
    N = a / np.sqrt(1.0 - e2 * (np.sin(latRad) ** 2.0))

    # #$ calcute X, Y, Z
    x = (N + alt) * np.cos(latRad) * np.cos(lonRad)
    y = (N + alt) * np.cos(latRad) * np.sin(lonRad)
    z = (b ** 2.0 / a ** 2.0 * N + altIn) * np.sin(latRad)

    return x, y, z


def RAE2ENU(azimuthIn, zenithIn, rangeIn):
    """
    Transform azimuth, zenith, range to ENU x,y,z (meters)
    """
    azimuth = np.deg2rad(np.asarray(azimuthIn, dtype=np.float64))
    zenith = np.deg2rad(np.asarray(zenithIn, dtype=np.float64))
    r = np.asarray(rangeIn, dtype=np.float64)
    print azimuth.shape, r.shape, zenith.shape
    # up
    up = r * np.cos(zenith)

    # projection on the x-y plane
    p = r * np.sin(zenith)

    # north
    north = p * np.cos(azimuth)

    # east
    east = p * np.sin(azimuth)

    return east, north, up


def ENU2ECEF(east, north, up, lon, lat):
    """
    Convert local East, North, Up (ENU) coordinates to the (x,y,z) Earth Centred Earth Fixed (ECEF) coordinates
    Reference is here:  
    http://www.navipedia.net/index.php/Transformations_between_ECEF_and_ENU_coordinates
    Note that laitutde should be geocentric latitude instead of geodetic latitude 
    Note: 

    On June 16 2015
    This note from https://en.wikipedia.org/wiki/Geodetic_datum 
    Note: \ \phi is the geodetic latitude. A prior version of this page showed use of the geocentric latitude (\ \phi^\prime).
    The geocentric latitude is not the appropriate up direction for the local tangent plane. If the
    original geodetic latitude is available it should be used, otherwise, the relationship between geodetic and geocentric
    latitude has an altitude dependency, and is captured by ...
    """

    x0 = np.asarray(east, dtype=np.float64)
    y0 = np.asarray(north, dtype=np.float64)
    z0 = np.asarray(up, dtype=np.float64)

    lm = np.deg2rad(np.asarray(lon, dtype=np.float64))
    ph = np.deg2rad(np.asarray(lat, dtype=np.float64))

    x = -1.0 * x0 * np.sin(lm) - y0 * np.cos(lm) * \
        np.sin(ph) + z0 * np.cos(lm) * np.cos(ph)
    y = x0 * np.cos(lm) - y0 * np.sin(lm) * np.sin(ph) + \
        z0 * np.sin(lm) * np.cos(ph)
    z = x0 * 0 + y0 * np.cos(ph) + z0 * np.sin(ph)

    return x, y, z


def deg2meter(deg):
    return deg * np.pi * EARTH_EQUATOR_RADIUS_KM * 1000. / 180.


def distance_Flat(lat1, lon1, lat2, lon2):
    '''
    Spherical Earth projected to a plane
    http://en.wikipedia.org/wiki/Geographical_distance
    '''
    delta_lat = lat1 - lat2
    delta_lon = lon1 - lon2
    mean_lat = (lat1 + lat2) / 2.

    distance = EARTH_MEAN_RADIUS_KM * sqrt((delta_lat * DEGREES_TO_RADIANS) ** 2 +
                                           (cos(mean_lat * DEGREES_TO_RADIANS) * delta_lon * DEGREES_TO_RADIANS) ** 2)

    return distance


def distance_GreatCircle(lat1, lon1, lat2, lon2):
    '''
    http://en.wikipedia.org/wiki/Great-circle_distance
    '''
    delta_lon = lon1 - lon2

    distance = EARTH_MEAN_RADIUS_KM * acos(sin(lat1 * DEGREES_TO_RADIANS) * sin(lat2 * DEGREES_TO_RADIANS) +
                                           cos(lat1 * DEGREES_TO_RADIANS) * cos(lat2 * DEGREES_TO_RADIANS) * cos(delta_lon * DEGREES_TO_RADIANS))
    return distance


def distance_GreatCircle_np(lat1, lon1, lat2, lon2):
    '''
    http://en.wikipedia.org/wiki/Great-circle_distance
    lat1, lon1, lat2, lon2 all are np.array
    '''
    delta_lon = lon1 - lon2

    distance = EARTH_MEAN_RADIUS_KM * np.arccos(np.sin(lat1 * DEGREES_TO_RADIANS) * np.sin(lat2 * DEGREES_TO_RADIANS) +
                                                np.cos(lat1 * DEGREES_TO_RADIANS) * np.cos(lat2 * DEGREES_TO_RADIANS) * np.cos(delta_lon * DEGREES_TO_RADIANS))
    return distance


def myfunc(Lats1, Lons1, Lats2, Lons2):

    dis = distance_GreatCircle_np(Lats1, Lons1, Lats2, Lons2)
    return dis


def sub_rodrigues_rotate(V, satZ, k):
    '''
    V: 旋转前的矢量 x y z
    satZ:  旋转角度 ,卫星天顶角
    k:   轴矢量
    return V_ROT: 旋转后的矢量 x y z
    '''

    v_unit_rot = np.full((3, 3), 0.)
    # 度转弧度
    th_d = np.deg2rad(satZ)

    # 原矢量单位
    v_unit = V / np.sqrt(V[0] ** 2 + V[1] ** 2 + V[2] ** 2)

    # 根据罗格里德旋转公式，计算旋转矢量
    v_unit_rot[0, 0] = np.cos(th_d) + k[0] ** 2 * (1 - np.cos(th_d))
    v_unit_rot[1, 0] = k[0] * k[1] * (1 - np.cos(th_d)) + k[2] * np.sin(th_d)
    v_unit_rot[2, 0] = -1. * k[1] * \
        np.sin(th_d) + k[0] * k[2] * (1 - np.cos(th_d))

    v_unit_rot[0, 1] = k[0] * k[1] * (1 - np.cos(th_d)) - k[2] * np.sin(th_d)
    v_unit_rot[1, 1] = np.cos(th_d) + k[1] ** 2 * (1 - np.cos(th_d))
    v_unit_rot[2, 1] = k[0] * np.sin(th_d) + k[1] * k[2] * (1 - np.cos(th_d))

    v_unit_rot[0, 2] = k[1] * np.sin(th_d) + k[0] * k[2] * (1 - np.cos(th_d))
    v_unit_rot[1, 2] = -1. * k[0] * \
        np.sin(th_d) + k[1] * k[2] * (1 - np.cos(th_d))
    v_unit_rot[2, 2] = np.cos(th_d) + k[2] ** 2 * (1 - np.cos(th_d))
    #     print v_unit_rot

    V_ROT = np.dot(v_unit_rot, v_unit)
    V_ROT = V_ROT / np.sqrt(V_ROT[0] ** 2 + V_ROT[1] ** 2 + V_ROT[2] ** 2)
    #     print V_ROT

    return V_ROT


def computing_L_R(SAT, FOVS):
    '''
        计算L和R，计算位置
    '''

    earth_a = 6378.137e3
    earth_b = 6356.752e3

    l_root = np.full((2,), 0.)
    a = FOVS[0] ** 2 * earth_b ** 2 + FOVS[1] ** 2 * \
        earth_b ** 2 + FOVS[2] ** 2 * earth_a ** 2
    b = 2 * FOVS[0] * SAT[0] * earth_b ** 2 + 2 * FOVS[1] * \
        SAT[1] * earth_b ** 2 + 2 * FOVS[2] * SAT[2] * earth_a ** 2
    c = SAT[0] ** 2 * earth_b ** 2 + SAT[1] ** 2 * earth_b ** 2 + \
        SAT[2] ** 2 * earth_a ** 2 - earth_a ** 2 * earth_b ** 2

    l_root[0] = (-1. * b + np.sqrt(b ** 2 - 4 * a * c)) / (2. * a)
    l_root[1] = (-1. * b - np.sqrt(b ** 2 - 4 * a * c)) / (2. * a)

    L = np.min(l_root)
    R = np.full((3,), 0.)
    R[0] = SAT[0] + L * FOVS[0]
    R[1] = SAT[1] + L * FOVS[1]
    R[2] = SAT[2] + L * FOVS[2]

    return R


def ggp_footpoint(LOS_ECEF, LOS_SAT_ECEF, th, fai, lat, lon, dfai):

    if lon < 0:
        lon = 360. + lon
    fov_foot_lon = np.full((37,), 0.)
    fov_foot_lat = np.full((37,), 0.)
    earth_a = 6378.137e3
    earth_b = 6356.752e3
    a = LOS_ECEF[0]
    b = LOS_ECEF[1]
    c = LOS_ECEF[2]
    k = np.full((3,), 0.)
    k[0] = 0
    k[2] = np.sqrt(b ** 2 / (c ** 2 + b ** 2))
    k[1] = -1. * c * k[2] / b
    dfai = 0.5 * dfai
    fov_unit = sub_rodrigues_rotate(LOS_ECEF, dfai, k)
#     print 'fov_unit', fov_unit
    los_unit = LOS_ECEF / \
        (np.sqrt(LOS_ECEF[0] ** 2 + LOS_ECEF[1] ** 2 + LOS_ECEF[2] ** 2))

    for i in xrange(37):
        #         print i
        th_i = i * 10
        fovs_unit = sub_rodrigues_rotate(fov_unit, th_i, los_unit)

        R = computing_L_R(LOS_SAT_ECEF, fovs_unit)

#         if i == 0:
#         print 'times %d' % i, 'fovs_unit and R=', fovs_unit, R

        r_lon = np.arctan(R[1] / R[0])
        if R[0] < 0 and R[1] > 0:
            r_lon = r_lon + np.pi
        elif R[0] < 0 and R[1] < 0:
            r_lon = r_lon - np.pi
#         r_lat = np.arctan(R[2] * np.sin(r_lon) / R[1])
#         r_lat = np.arctan(R[2] * np.sin(r_lon) / R[1])
        r_lat = np.arctan(np.sqrt(
            (earth_a ** 2 / (earth_b ** 2) ** 2) / (1.0 / R[2] ** 2 - 1.0 / earth_b ** 2)))

        fov_foot_lon[i] = np.rad2deg(r_lon)
        fov_foot_lat[i] = np.rad2deg(r_lat)

        if lat < 0:
            fov_foot_lat = fov_foot_lat * -1

    return fov_foot_lon, fov_foot_lat

if __name__ == '__main__':

    #     print distance_Flat(-73.78, 172.40, -73.84, 172.38)
    #     print distance_GreatCircle(9.684, -42.625, 9.684, -42.653999)
    # 和C程序经过验证结果一致100%
    #     print getasol6s(165, 23.75, 6.55584, 105.447)
    #     print getasol6s(156, 0.602368324944, 29.9398, 80.0413)
    print getasol6s(156, 0.618181666667, 29.9452, 80.0361)
#     print solar_zen(2014, 6, 5, 23.5, 105.447, 6.55584)
#     print sun_glint_cal(-44, 33, 44, 55)
#     print sun_glint_cal(316, 33, 44, 55)
#     aa = sun_earth_dis_correction('20180501')
    print deg2meter(0.05)
    pass
