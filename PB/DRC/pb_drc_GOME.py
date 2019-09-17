# -*- coding: utf-8 -*-

from datetime import datetime
import os
import re
import time

from PB.pb_sat import planck_r2t, spec_interp, spec_convolution
from PB.pb_time import time_block
from pb_drc_VISSR import ReadVissrL1
from pb_drc_base import ReadL1
import beatl2
import coda
import numpy as np

__description__ = u'IASI传感器读取'
__author__ = 'wangpeng'
__date__ = '2018-08-28'
__version__ = '1.0.0_beat'

g_main_path, g_main_file = os.path.split(os.path.realpath(__file__))


class ReadGomeL1(ReadL1):
    """
    读取 GOME 传感器的 L1 数据
    分辨率：Main channels: 40 x 40 km2 associated to 960 km swath or
         40 x 80 km2 associated to 1920 km swath. Polarisation
         channels: respectively 40 x 5 km2 or 40 x 10 km2 .
    卫星： [Metop-A Metop-B]
    通道数量：
    红外通道：8461
    """

    def __init__(self, in_file):
        sensor = 'GOME'
        super(ReadGomeL1, self).__init__(in_file, sensor)

    def set_resolution(self):
        """
        use filename set self.resolution
        """
        file_name = os.path.basename(self.in_file)
        if 'GOME_xxx_1B' in file_name:
            self.resolution = 40000
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def set_satellite(self):
        """
        use filename set self.satellite
        """
        file_name = os.path.basename(self.in_file)

        if 'GOME_xxx_1B_M02' in file_name:
            self.satellite = 'METOP-A'
        elif 'GOME_xxx_1B_M01' in file_name:
            self.satellite = 'METOP-B'
        else:
            raise ValueError('Cant get the satellite name from file name.')

    def set_ymd_hms(self):
        """
        use filename  set self.ymd self.hms
        """
        file_name = os.path.basename(self.in_file)
        if self.resolution == 40000:
            pat = u'\w+_(\d{14})Z_(\d{14})Z_N_O_\d{14}Z__\d{14}$'
        g = re.match(pat, file_name)
        if g:
            self.ymd = g.group(1)[:8]
            self.hms = g.group(1)[8:]
        else:
            raise ValueError('Cant get the ymdhms from file name.')

    def set_file_attr(self):
        """
        seft self.file_att is dict
        """
        data = dict()
        if self.resolution == 40000:
            satellite_type1 = ['METOP-A', 'METOP-B']
            if self.satellite in satellite_type1:

                try:
                    fp = coda.open(self.in_file)
                    product_class = coda.get_product_class(fp)
                    product_type = coda.get_product_type(fp)
                    product_version = coda.get_product_version(fp)
                    product_format = coda.get_product_format(fp)
                    product_size = coda.get_product_file_size(fp)
                    coda.close(fp)
                except Exception as e:
                    print str(e)
                    return

                data['class'] = product_class
                data['size'] = product_size
                data['type'] = product_type
                data['version'] = product_version
                data['format'] = product_format
                self.file_attr = data
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

    def set_channels(self):
        """
        return sensor channels
        """
        if self.resolution == 40000:
            satellite_type1 = ['METOP-A', 'METOP-B']
            if self.satellite in satellite_type1:
                self.channels = 8461
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))

    def set_data_shape(self):
        """
        set self.data_shape
        """
        if self.resolution == 40000:
            satellite_type1 = ['METOP-A', 'METOP-B']
            if self.satellite in satellite_type1:
                try:
                    self.record = beatl2.ingest(self.in_file)
                    # iasi 的观测点会变化 2640 2760等，所以根据数据长度获取，另外统一转成2维
                    self.data_shape = (self.record.time.shape[0], 1)
                    print self.data_shape
                except Exception as e:
                    print 'Open file error {}'.format(e)
                    return
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

    def get_spectral_response(self):
        """
        return 光谱波数和响应值，1维，2维
        """
        k = 1.98644746103858e-9
        if self.resolution == 40000:
            satellite_type1 = ['METOP-A', 'METOP-B']
            if self.satellite in satellite_type1:
                try:
                    fp = coda.open(self.in_file)
                    wave3 = coda.fetch(
                        fp, 'MDR', -1, 'Earthshine', 'WAVELENGTH_3')
                    wave4 = coda.fetch(
                        fp, 'MDR', -1, 'Earthshine', 'WAVELENGTH_4')
                    lambda_smr = coda.fetch(fp, 'VIADR_SMR', -1, 'LAMBDA_SMR')
                    smr = coda.fetch(fp, 'VIADR_SMR', -1, 'SMR')

                    sunz = coda.fetch(
                        fp, 'MDR', -1, 'Earthshine', 'GEO_EARTH', 'SOLAR_ZENITH')
                    coda.close(fp)

                    #  gome 959x4096 取后2048个点的辐射值
                    wavelens = self.record.wavelength[0, 2048:]
                    response = self.record.spectral_radiance[:, 2048:]

                    data_size = self.data_shape[0]
                    data_row = sunz.shape[0]
                    data_col = sunz[0].shape[1]
                    # gome的辐亮度计算 wave3 wave4  是30*1024

                    for m in xrange(data_size):
                        row, col = np.unravel_index(m, (data_row, data_col))
                        for i in xrange(2048):
                            if i < 1024:
                                response[m, i] = response[
                                    m, i] * k / wave3[row][i]
                            else:
                                response[m, i] = response[
                                    m, i] * k / wave4[row][i - 1024]

                    # 计算太阳辐亮度
                    sol_wavelens = np.zeros((2048,))  # 太阳辐亮度
                    sol_response = np.zeros((2048,))  # 太阳辐亮度对应的波长
                    for i in xrange(2048):
                        if i < 1024:
                            sol_response[i] = (
                                smr[0][2][i] * k) / lambda_smr[0][2][i]
                            sol_wavelens[i] = lambda_smr[0][2][i]
                        else:
                            sol_response[i] = (
                                smr[0][3][i - 1024] * k) / lambda_smr[0][3][i - 1024]
                            sol_wavelens[i] = lambda_smr[0][3][i - 1024]

                    idx = np.where(response < 0)
                    print len(idx[0])
                    if len(idx[0] > 0):
                        response[idx] = 0.
                    gome_wavelens, gome_response, solar_response = self.combine_gome_band34(
                        wavelens, response, sol_wavelens, sol_response)
                    s0, s1 = gome_response.shape
                    gome_response = gome_response.reshape(s0, 1, s1)
                    print gome_response.shape, solar_response.shape
                except Exception as e:
                    print 'Open file error {}'.format(e)
                    return
            self.solar_response = solar_response
        return gome_wavelens, gome_response

    def get_ref(self, wave_nums, wave_spec, band_list, central_wave_nums, a, b, lut):
        """
        wave_nums: 波数cm-1（字典形式，key是通道信息，针对FY数据）
        wave_spec: 响应0-1（字典形式，key是通道信息，针对FY数据）
        band_list： CH_20-CH_25
        center_wave_nums: 目前未用，只是为了和get_tbb统一
        a: 目前未用，只是为了和get_tbb统一
        b: 目前未用，只是为了和get_tbb统一
        return channel rad
        """
        data = dict()
        if self.resolution == 40000:
            satellite_type1 = ['METOP-A', 'METOP-B']

            if self.satellite in satellite_type1:
                # gome 不需要用波数，使用波长即可
                gome_wavelens, gome_response = self.get_spectral_response()

                for band in sorted(band_list):
                    wave_nums1 = wave_nums[band]
                    wave_spec1 = wave_spec[band]
                    # 插值 把FY光谱插值到GOME光谱上
                    wave_spec2 = spec_interp(
                        wave_nums1, wave_spec1, gome_wavelens)
                    # 对应FY的响应值，大小一致就可以和FY比较了，响应是IASI的
                    rads = spec_convolution(
                        gome_wavelens, wave_spec2, gome_response)
                    print band, np.allclose(rads, np.nan, equal_nan = True)
                    # 过滤<=0的结果

                    # 把数据1光谱插值到gome的太阳光谱
                    wave_spec2_sol = spec_interp(
                        wave_nums1, wave_spec1, gome_wavelens)
        #             WaveRad2_solar = D1.spec_interp(WaveNum1, WaveRad1, self.vec_Solar_WL)
                    rads_sol = spec_convolution(
                        gome_wavelens, wave_spec2_sol, self.solar_response)
                    solar_zen = self.get_solar_zenith()
                    data[band] = rads / rads_sol * \
                        np.pi  # / np.cos(np.deg2rad(solar_zen)) wrh 建议注释掉
#                     print data

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def combine_gome_band34(self, wavelens, response, sol_wavelens, sol_response):

        # 把gome的通道做成390-800波段间隔1纳米的响应
        gome_wavelens = np.arange(390, 801, 1)
        gome_shape = (response.shape[0], len(gome_wavelens))
        gome_response3 = np.full(gome_shape, 0)
        gome_response4 = np.full(gome_shape, 0)

        for i in xrange(response.shape[0]):
            gome_response3[i, :] = spec_interp(
                wavelens[0:1024], response[i, 0:1024], gome_wavelens)
            gome_response4[i, :] = spec_interp(
                wavelens[1024:], response[i, 1024:], gome_wavelens)

            gome_response = gome_response3 + gome_response4

        solar_response3 = spec_interp(
            sol_wavelens[0:1024], sol_response[0:1024], gome_wavelens)
        solar_response4 = spec_interp(
            sol_wavelens[1024:], sol_response[1024:], gome_wavelens)
        solar_response = solar_response3 + solar_response4

        return gome_wavelens, gome_response, solar_response

    def get_ref2(self, wave_nums, wave_spec, band_list, central_wave_nums, a, b, lut):
        """
        wave_nums: 波数cm-1（字典形式，key是通道信息，针对FY数据）
        wave_spec: 响应0-1（字典形式，key是通道信息，针对FY数据）
        band_list： CH_20-CH_25
        center_wave_nums: 目前未用，只是为了和get_tbb统一
        a: 目前未用，只是为了和get_tbb统一
        b: 目前未用，只是为了和get_tbb统一
        return channel rad
        """
        data = dict()
        if self.resolution == 40000:
            satellite_type1 = ['METOP-A', 'METOP-B']

            if self.satellite in satellite_type1:
                # gome 不需要用波数，使用波长即可
                wavelens, response, sol_response = self.get_spectral_response()
                for band in sorted(band_list):
                    wave_nums1 = wave_nums[band]
                    wave_spec1 = wave_spec[band]
                    # 插值 把FY光谱插值到GOME光谱上
                    wave_spec2 = spec_interp(
                        wave_nums1, wave_spec1, wavelens)
                    # 对应FY的响应值，大小一致就可以和FY比较了，响应是IASI的
                    rads = spec_convolution(wavelens, wave_spec2, response)
                    print band, np.allclose(rads, np.nan, equal_nan = True)

                    rads_sol = spec_convolution(
                        wavelens, wave_spec2, sol_response)
                    solar_zen = self.get_solar_zenith()
                    data[band] = rads / rads_sol * \
                        np.pi / np.cos(np.deg2rad(solar_zen))
                    print data

            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_longitude(self):
        """
        return longitude
        """
        if self.resolution == 40000:
            satellite_type1 = ['METOP-A', 'METOP-B']
            if self.satellite in satellite_type1:

                try:
                    # record = beatl2.ingest(self.in_file)
                    data_pre = self.record.longitude
                    invalid_index = np.logical_or(
                        data_pre < -180, data_pre > 180)
                    data_pre = data_pre.astype(np.float32)
                    data_pre[invalid_index] = np.nan
                    data = data_pre.reshape(data_pre.size, 1)

                except Exception as e:
                    print 'Open file error {}'.format(e)
                    return
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_longitude_corner(self):
        """
        return longitude
        """
        if self.resolution == 40000:
            satellite_type1 = ['METOP-A', 'METOP-B']
            if self.satellite in satellite_type1:

                try:
                    # record = beatl2.ingest(self.in_file)
                    data_pre = self.record.corner_longitude
                    invalid_index = np.logical_or(
                        data_pre < -180, data_pre > 180)
                    data_pre = data_pre.astype(np.float32)
                    data_pre[invalid_index] = np.nan
                    data = data_pre
#                     data = data_pre.reshape(data_pre.size, 1)

                except Exception as e:
                    print 'Open file error {}'.format(e)
                    return
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_latitude(self):
        """
        return latitude
        """
        if self.resolution == 40000:
            satellite_type1 = ['METOP-A', 'METOP-B']
            if self.satellite in satellite_type1:

                try:
                    # record = beatl2.ingest(self.in_file)
                    data_pre = self.record.latitude
                    invalid_index = np.logical_or(
                        data_pre < -90, data_pre > 90)
                    data_pre = data_pre.astype(np.float32)
                    data_pre[invalid_index] = np.nan
                    data = data_pre.reshape(data_pre.size, 1)

                except Exception as e:
                    print 'Open file error {}'.format(e)
                    return
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_latitude_corner(self):
        """
        return latitude
        """
        if self.resolution == 40000:
            satellite_type1 = ['METOP-A', 'METOP-B']
            if self.satellite in satellite_type1:

                try:
                    # record = beatl2.ingest(self.in_file)
                    data_pre = self.record.corner_latitude
                    invalid_index = np.logical_or(
                        data_pre < -90, data_pre > 90)
                    data_pre = data_pre.astype(np.float32)
                    data_pre[invalid_index] = np.nan
                    data = data_pre

                except Exception as e:
                    print 'Open file error {}'.format(e)
                    return
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_sensor_azimuth(self):
        """
        return sensor_azimuth
        """
        if self.resolution == 40000:
            satellite_type1 = ['METOP-A', 'METOP-B']
            if self.satellite in satellite_type1:
                try:
                    fp = coda.open(self.in_file)
                    angle = coda.fetch(
                        fp, 'MDR', -1, 'Earthshine', 'GEO_EARTH', 'SAT_AZIMUTH')
                    coda.close(fp)
                    # angle = 30*3*32 = 1440, 取30*1*32 = 960, 取前959个
                    data_size = self.data_shape[0]
                    data_row = angle.shape[0]
                    data_col = angle[0].shape[1]
                    data_pre = np.full(self.data_shape, -999.)

                    for i in xrange(data_size):
                        row, col = np.unravel_index(i, (data_row, data_col))
#                         print row, col, angle[row][1][col]
                        data_pre[i] = angle[row][1][col]

                    # 过滤无效值
                    invalid_index = np.logical_or(
                        data_pre < 0, data_pre > 360)
                    data_pre = data_pre.astype(np.float32)
                    data_pre[invalid_index] = np.nan

                    data = data_pre

                except Exception as e:
                    print 'Open file error {}'.format(e)
                    return
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_sensor_zenith(self):
        """
        return sensor_zenith
        """
        if self.resolution == 40000:
            satellite_type1 = ['METOP-A', 'METOP-B']
            if self.satellite in satellite_type1:

                try:
                    fp = coda.open(self.in_file)
                    angle = coda.fetch(
                        fp, 'MDR', -1, 'Earthshine', 'GEO_EARTH', 'SAT_ZENITH')
                    coda.close(fp)
                    # angle = 30*3*32 = 1440, 取30*1*32 = 960, 取前959个
                    data_size = self.data_shape[0]
                    data_row = angle.shape[0]
                    data_col = angle[0].shape[1]
                    data_pre = np.full(self.data_shape, -999.)

                    for i in xrange(data_size):
                        row, col = np.unravel_index(i, (data_row, data_col))
                        data_pre[i] = angle[row][1][col]

                    # 过滤无效值
                    invalid_index = np.logical_or(
                        data_pre < 0, data_pre > 180)
                    data_pre = data_pre.astype(np.float32)
                    data_pre[invalid_index] = np.nan

                    data = data_pre

                except Exception as e:
                    print 'Open file error {}'.format(e)
                    return
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_solar_azimuth(self):
        """
        return solar_azimuth
        """
        if self.resolution == 40000:
            satellite_type1 = ['METOP-A', 'METOP-B']
            if self.satellite in satellite_type1:

                try:
                    fp = coda.open(self.in_file)
                    angle = coda.fetch(
                        fp, 'MDR', -1, 'Earthshine', 'GEO_EARTH', 'SOLAR_AZIMUTH')
                    coda.close(fp)
                    # angle = 30*3*32 = 1440, 取30*1*32 = 960, 取前959个
                    data_size = self.data_shape[0]
                    data_row = angle.shape[0]
                    data_col = angle[0].shape[1]
                    data_pre = np.full(self.data_shape, -999.)

                    for i in xrange(data_size):
                        row, col = np.unravel_index(i, (data_row, data_col))
                        data_pre[i] = angle[row][1][col]

                    # 过滤无效值
                    invalid_index = np.logical_or(
                        data_pre < 0, data_pre > 360)
                    data_pre = data_pre.astype(np.float32)
                    data_pre[invalid_index] = np.nan

                    data = data_pre

                except Exception as e:
                    print 'Open file error {}'.format(e)
                    return
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_solar_zenith(self):
        """
        return solar_zenith
        """
        if self.resolution == 40000:
            satellite_type1 = ['METOP-A', 'METOP-B']
            if self.satellite in satellite_type1:

                try:
                    fp = coda.open(self.in_file)
                    angle = coda.fetch(
                        fp, 'MDR', -1, 'Earthshine', 'GEO_EARTH', 'SOLAR_ZENITH')
                    coda.close(fp)
                    # angle = 30*3*32 = 1440, 取30*1*32 = 960, 取前959个
                    data_size = self.data_shape[0]
                    data_row = angle.shape[0]
                    data_col = angle[0].shape[1]
                    data_pre = np.full(self.data_shape, -999.)

                    for i in xrange(data_size):
                        row, col = np.unravel_index(i, (data_row, data_col))
                        data_pre[i] = angle[row][1][col]

                    # 过滤无效值
                    invalid_index = np.logical_or(
                        data_pre < 0, data_pre > 180)
                    data_pre = data_pre.astype(np.float32)
                    data_pre[invalid_index] = np.nan

                    data = data_pre

                except Exception as e:
                    print 'Open file error {}'.format(e)
                    return
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))
        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data

    def get_timestamp(self):
        """
        return from 1970-01-01 00:00:00 seconds
        """
        if self.resolution == 40000:
            satellite_type1 = ['METOP-A', 'METOP-B']
            if self.satellite in satellite_type1:
                s = self.data_shape
                try:
                    ary_time = self.record.time
                    # GOME 数据的时间单位是距离 2000年1月1日 UTC时间的秒
                    secs = int(
                        (datetime(2000, 1, 1, 0, 0, 0) - datetime(1970, 1, 1, 0, 0, 0)).total_seconds())

                    ary_time = ary_time + secs
                    data = ary_time.reshape(s)
                    data = data.astype(np.int32)

                except Exception as e:
                    print 'Open file error {}'.format(e)
                    return
            else:
                raise ValueError(
                    'Cant read this satellite`s data.: {}'.format(self.satellite))

        else:
            raise ValueError(
                'Cant read this data, please check its resolution: {}'.format(self.in_file))
        return data


def ymd2ymd(ymd, data):

    hour_point, hour = np.modf(data / 3600.)
    mins_point, mins = np.modf(hour_point * 60.)
    _, secs = np.modf(mins_point * 60.)

    times = datetime.strptime(
        "%s%02d%02d%02d" % (ymd, hour, mins, secs), '%Y%m%d%H%M%S')
    ymdhms = (times - datetime(1970, 1, 1, 0, 0, 0)).total_seconds()
    return ymdhms


if __name__ == '__main__':
    T1 = datetime.now()
    BandLst = ['CH_20', 'CH_21', 'CH_22', 'CH_23', 'CH_24', 'CH_25']
    L1File = 'D:/data/GOME/GOME_xxx_1B_M02_20160109122059Z_20160109122359Z_N_O_20160109134314Z__20160109135951'
    L1File = 'GOME_xxx_1B_M02_20190701004155Z_20190701004455Z_N_O_20190701020511Z__20190701020612'
    L1File2 = 'D:/data/VISSR/FY2H_FDI_ALL_NOM_20181001_0800.hdf'
    gome = ReadGomeL1(L1File)
#     fy2h = ReadVissrL1(L1File2)
    with time_block('>>>>>>>>>>>>read iasi', True):
        print gome.satellite  # 卫星名
        print gome.sensor  # 传感器名
        print gome.ymd  # L1 文件年月日 YYYYMMDD
        print gome.hms  # L1 文件时分秒 HHMMSS
        print gome.resolution  # 分辨率
        print gome.channels  # 通道数量
        print gome.data_shape
        print gome.file_attr

        def print_data_status(datas, name = None):
            data_shape = datas.shape
            data_min = np.nanmin(datas)
            data_max = np.nanmax(datas)
            data_mean = np.nanmean(datas)
            data_median = np.nanmedian(datas)
            print "{}: shape: {}, min: {}, max: {}, mean: {}, median: {}".format(
                name, data_shape, data_min, data_max, data_mean, data_median)

#         band_list = ['CH_05']
#         wave_nums, wave_spec = fy2h.get_spectral_response()
#         central_wave_numbers = fy2h.get_central_wave_number()
#         a = fy2h.get_tbb_k1()
#         b = fy2h.get_tbb_k0()
#         lut_bt = fy2h.get_lut_bt()
#         rad = gome.get_ref(
#             wave_nums, wave_spec, band_list, central_wave_numbers, a, b, lut_bt)

        print gome.record
        lat = gome.get_latitude()
        lat_corner = gome.get_latitude_corner()
        print lat.shape
        print lat_corner.shape
        print lat[0, 0]
        print lat_corner[0, :]

#         print 'get_spectral_response:'
#         wavelens, response, s_wavelens, s_response = gome.get_spectral_response()
#         print '222', response.shape
#         print_data_status(wavelens)
#         print_data_status(response)

#         print 'longitude:'
#         t_data = gome.get_longitude()
#         print_data_status(t_data)
#
#         print 'longitude:'
#         t_data = gome.get_latitude()
#         print_data_status(t_data)

#         print 'sensor_azimuth:'
#         t_data = gome.get_sensor_azimuth()
#         print_data_status(t_data)
#         print 'sensor_zenith:'
#         t_data = gome.get_sensor_zenith()
#         print_data_status(t_data)
#         print 'solar_azimuth:'
#         t_data = gome.get_solar_azimuth()
#         print_data_status(t_data)
#         print 'solar_zenith:'
#         t_data = gome.get_solar_zenith()
#         print_data_status(t_data)
#
#         print 'timestamp:'
#         t_data = gome.get_timestamp()
#         print t_data.shape
#         print time.gmtime(t_data[0, 0])
#         print time.gmtime(t_data[-1, -1])
