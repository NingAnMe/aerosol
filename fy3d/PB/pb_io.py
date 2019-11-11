# -*- coding: utf-8 -*-
from contextlib import contextmanager
from datetime import datetime
import errno
import os
import random
import re
import time

from configobj import ConfigObj
from dateutil.relativedelta import relativedelta
import h5py
import yaml
import numpy as np
from PB.pb_sat import getasol6s

__description__ = 'IO处理的函数'
__author__ = 'wangpeng'
__date__ = '2018-01-25'
__version__ = '1.0.0_beat'


def write_yaml_file(yaml_dict, file_yaml):
    path_yaml = os.path.dirname(file_yaml)
    if not os.path.isdir(path_yaml):
        os.makedirs(path_yaml)
    with open(file_yaml, 'w') as stream:
        yaml.dump(yaml_dict, stream, default_flow_style=False,
                  encoding='utf-8', allow_unicode=True)


def make_sure_path_exists(path):
    try:
        os.makedirs(path)
    except OSError as exception:
        if exception.errno != errno.EEXIST:
            raise


def find_file(path, reg):
    '''
    path: 要遍历的目录
    reg: 符合条件的文件
    '''
    FileLst = []
    try:
        lst = os.walk(path)
        for root, dirs, files in lst:
            for name in files:
                try:
                    m = re.match(reg, name)
                except Exception as e:
                    continue
                if m:
                    FileLst.append(os.path.join(root, name))
    except Exception as e:
        print(str(e))

    return sorted(FileLst)


def path_replace_ymd(path, ymd):
    '''
    path:替换路径中的日期 ,path中%YYYY%MM%DD%JJJ 等关键字会被ymd日期实例
    ymd: yyyymmdd  (20180101)
    '''
    # 转成datetime类型
    ymd = datetime.strptime(ymd, '%Y%m%d')
    yy = ymd.strftime('%Y')
    mm = ymd.strftime('%m')
    dd = ymd.strftime('%d')
    jj = ymd.strftime('%j')
    path = path.replace('%YYYY', yy)
    path = path.replace('%MM', mm)
    path = path.replace('%DD', dd)
    path = path.replace('%JJJ', jj)

    return path


def is_none(*args):
    """
    判断传入的变量中是否有 None
    :param args:
    :return:
    """
    has_none = False
    for arg in args:
        if arg is None:
            has_none = True
    return has_none


def copy_attrs_h5py(pre_object, out_object):
    """
    复制 file、dataset 或者 group 的属性
    :param pre_object: 被复制属性的 dataset 或者 group
    :param out_object: 复制属性的 dataset 或者 group
    :return:
    """
    for akey in list(pre_object.attrs.keys()):
        out_object.attrs[akey] = pre_object.attrs[akey]


def read_dataset_hdf5(file_path, set_name):
    """
    读取 hdf5 文件，返回一个 numpy 多维数组
    :param file_path: (unicode)文件路径
    :param set_name: (str or list)表的名字
    :return: 如果传入的表名字是一个字符串，返回 numpy.ndarray
             如果传入的表名字是一个列表，返回一个字典，key 是表名字，
             value 是 numpy.ndarry
    """
    if isinstance(set_name, str):
        if os.path.isfile(file_path):
            file_h5py = h5py.File(file_path, 'r')
            data = file_h5py.get(set_name)[:]
            dataset = np.array(data)
            file_h5py.close()
            return dataset
        else:
            raise ValueError('value error: file_path')
    elif isinstance(set_name, list):
        datasets = {}
        if os.path.isfile(file_path):
            file_h5py = h5py.File(file_path, 'r')
            for name in set_name:
                data = file_h5py.get(name)[:]
                dataset = np.array(data)
                datasets[name] = dataset
            file_h5py.close()
            return datasets
        else:
            raise ValueError('value error: file_path')
    else:
        raise ValueError('value error: set_name')


def attrs2dict(attrs):
    """
    将一个 HDF5 attr 类转为 Dict 类
    :return:
    """
    d = {}
    for k, v in list(attrs.items()):
        d[k] = v
    return d


class Config(object):
    """
    加载配置文件
    """

    def __init__(self, config_file):
        """
        初始化
        """
        self.error = False

        self.config_file = config_file  # config 文件路径
        self.config_data = None  # 加载到的配置数据

        # load config file
        self.load_config_file()
        # load config data
        self.load_config_data()

    def load_config_file(self):
        """
        尝试加载配置文件
        :return:
        """
        try:
            self.load_yaml_file()
        except Exception:
            self.load_cfg_file()
        finally:
            if len(self.config_data) == 0:
                self.error = True
                print("Load config file error: {}".format(self.config_file))

    def load_yaml_file(self):
        """
        加载 yaml 文件内容
        :return:
        """
        with open(self.config_file, 'r') as stream:
            self.config_data = yaml.load(stream)

    def load_cfg_file(self):
        """
        加载 config 文件内容
        :return:
        """
        self.config_data = ConfigObj(self.config_file)

    def load_config_data(self):
        """
        读取配置数据
        :return:
        """
        self._load_config_data(self.config_data)

    def _load_config_data(self, config_data, prefix=""):
        """
        读取配置数据，动态创建属性值
        :return:
        """
        if self.error:
            return
        for k in config_data:
            data = config_data[k]
            attr = prefix + k.lower()
            self.__dict__[attr] = data
            if isinstance(data, dict):
                next_prefix = attr + "_"
                self._load_config_data(data, prefix=next_prefix)


@contextmanager
def progress_lock(max_wait_time=5):
    try:
        sleep_time = 0
        lock = "progress.lock"
        while True:
            if os.path.isfile(lock):
                if sleep_time > max_wait_time:
                    try:
                        os.remove(lock)
                        break
                    except:
                        continue
                else:
                    random_number = random.random() * 0.1
                    sleep_time += random_number

                time.sleep(random_number)
            else:
                break
        with open(lock, "w"):
            pass
        yield
    finally:
        try:
            os.remove(lock)
        except:
            pass


def write_txt(in_file, head, bodys, keylens=8):
    """
    description: wangpeng add 20180615 (写入或更新txt)
    :in_file 写入文件位置
    :head  文件头信息
    :bodys 文件体
    :keylens 更新文件使用的第一列关键字长度
    """
    allLines = []
    DICT_D = {}
    FilePath = os.path.dirname(in_file)
    if not os.path.exists(FilePath):
        os.makedirs(FilePath)

    if os.path.isfile(in_file) and os.path.getsize(in_file) != 0:
        fp = open(in_file, 'r')
        fp.readline()
        Lines = fp.readlines()
        fp.close()
        # 使用字典特性，保证时间唯一，读取数据
        for Line in Lines:
            DICT_D[Line[:keylens]] = Line[keylens:]
        # 添加或更改数据
        for Line in bodys:
            DICT_D[Line[:keylens]] = Line[keylens:]
        # 按照时间排序
        newLines = sorted(
            iter(DICT_D.items()), key=lambda d: d[0], reverse=False)

        for i in range(len(newLines)):
            allLines.append(str(newLines[i][0]) + str(newLines[i][1]))
        fp = open(in_file, 'w')
        fp.write(head)
        fp.writelines(allLines)
        fp.close()
    else:
        fp = open(in_file, 'w')
        fp.write(head)
        fp.writelines(bodys)
        fp.close()


def str_format(string, values):
    """
    格式化字符串
    :param string:(str) "DCC: %sat_sensor_Projection_%ymd（分辨率 %resolution 度）"
    :param values:(dict) {"sat_sensor": sat_sensor, "resolution": str(resolution), "ymd": ymd}
    :return: DCC: FY3D+MERSI_Projection_201712（分辨率 1 度）
    """
    if not isinstance(string, str):
        return

    for k, v in values.items():
        string = string.replace("%" + str(k), str(v))
    return string


def get_files_by_ymd(dir_path, time_start, time_end, ext=None, pattern_ymd=None):
    """
    :param dir_path: 文件夹
    :param time_start: 开始时间
    :param time_end: 结束时间
    :param ext: 后缀名, '.hdf5'
    :param pattern_ymd: 匹配时间的模式, 可以是 r".*(\d{8})_(\d{4})_"
    :return: list
    """
    files_found = []
    if pattern_ymd is not None:
        pattern = pattern_ymd
    else:
        pattern = r".*(\d{8})"

    for root, dirs, files in os.walk(dir_path):
        for file_name in files:
            if ext is not None:
                if '.' not in ext:
                    ext = '.' + ext
                if os.path.splitext(file_name)[1].lower() != ext.lower():
                    continue
            re_result = re.match(pattern, file_name)
            if re_result is not None:
                time_file = ''.join(re_result.groups())
            else:
                continue
            if int(time_start) <= int(time_file) <= int(time_end):
                files_found.append(os.path.join(root, file_name))
    files_found.sort()
    return files_found


# def ymdhms2date(ymd, hms):
#     """
#     ymd = 20180101
#     hms = 04:04:04
#     """
#     ymdhms = ymd + hms
#     return datetime.strptime(ymdhms, '%Y%m%d%H:%M:%S')


class ReadCrossFile(object):

    def __init__(self, file_type):

        # 原始cross文件数据内容
        self.data = {
            'ymdhms1': None,
            'ymdhms2': None,
            'lon1': None,
            'lat1': None,
            'lon2': None,
            'lat2': None,
            'name1': None,
            'name2': None}
        self.file_type = file_type
        # 过滤cross文件数据内容,只记录时间
        self.lines = []

#     @staticmethod
    def load(self, in_file):
        """
        :param in_file:
        :param file_type:
        :return:
        """
        if not os.path.isfile(in_file):
            print('***WARNING***File is not exist: {}'.format(in_file))
            return self.data

        if self.file_type == 'leo_area':
            dtype = {'names': ('d1', 'd2', 'd3', 'd4', 'd5', 'd6', 'd7'),
                     'formats': ('S8', 'S8', 'S8', 'f4', 'f4', 'f4', 'f4')}
            data_raw = np.loadtxt(in_file,  dtype, skiprows=10, ndmin=1)

            name1, name2 = get_cross_file_line2(in_file)
            if data_raw.size != 0:
                ymd = data_raw['d1']
                hms1 = data_raw['d2']
                hms2 = data_raw['d3']
                ymdhms1 = list(map(ymd_hms, ymd, hms1))
                ymdhms2 = list(map(ymd_hms, ymd, hms2))

                ymdhms1 = np.array(ymdhms1)
                ymdhms2 = np.array(ymdhms2)

                self.data['ymdhms1'] = ymdhms1
                self.data['ymdhms2'] = ymdhms2
                self.data['lat1'] = data_raw['d4']
                self.data['lon1'] = data_raw['d5']
                self.data['lat2'] = data_raw['d6']
                self.data['lon2'] = data_raw['d7']
                lens = self.data['lat1'].size
                self.data['name1'] = np.array([name1] * lens)
                self.data['name2'] = np.array([name2] * lens)

        elif self.file_type == 'leo_leo' or self.file_type == 'leo_snox':

            dtype = {'names': ('d1', 'd2', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8', 'd9'),
                     'formats': ('S8', 'S8', 'f4', 'f4', 'S8', 'f4', 'f4', 'f4', 'f4')}
            data_raw = np.loadtxt(in_file,  dtype, skiprows=10, ndmin=1)
            name1, name2 = get_cross_file_line2(in_file)

            if data_raw.size != 0:
                ymd = data_raw['d1']
                hms1 = data_raw['d2']
                hms2 = data_raw['d5']

                ymdhms1 = list(map(ymd_hms, ymd, hms1))
                ymdhms2 = list(map(ymd_hms, ymd, hms2))
                ymdhms1 = np.array(ymdhms1)
                ymdhms2 = np.array(ymdhms2)
                self.data['ymdhms1'] = ymdhms1
                self.data['ymdhms2'] = ymdhms2
                self.data['lat1'] = data_raw['d3']
                self.data['lon1'] = data_raw['d4']
                self.data['lat2'] = data_raw['d6']
                self.data['lon2'] = data_raw['d7']
                lens = self.data['lat1'].size
                self.data['name1'] = np.array([name1] * lens)
                self.data['name2'] = np.array([name2] * lens)

        elif self.file_type == 'leo_fix':
            # 数据
            dtype = {'names': ('d1', 'd2', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8',),
                     'formats': ('S8', 'S8', 'S16', 'f4', 'f4', 'f4', 'f4', 'f4')}

            data_raw = np.loadtxt(in_file,  dtype, skiprows=10, ndmin=1)
            name1, _ = get_cross_file_line2(in_file)

            if data_raw.size != 0:
                ymd = data_raw['d1']
                hms1 = data_raw['d2']
                hms2 = data_raw['d2']
                ymdhms1 = list(map(ymd_hms, ymd, hms1))
                ymdhms2 = list(map(ymd_hms, ymd, hms2))

                ymdhms1 = np.array(ymdhms1)
                ymdhms2 = np.array(ymdhms2)

                self.data['ymdhms1'] = ymdhms1
                self.data['ymdhms2'] = ymdhms2
                self.data['lat1'] = data_raw['d6']
                self.data['lon1'] = data_raw['d7']
                self.data['lat2'] = data_raw['d4']
                self.data['lon2'] = data_raw['d5']
                lens = self.data['lat1'].size
                self.data['name1'] = np.array([name1] * lens)
                self.data['name2'] = data_raw['d3']

        elif self.file_type == 'geo_leo':
            # 信息

            dtype = {'names': ('d1', 'd2', 'd3', 'd4', 'd5', 'd6', 'd7'),
                     'formats': ('S8', 'S8', 'S8', 'f4', 'f4', 'f4', 'f4')}
            data_raw = np.loadtxt(in_file,  dtype, skiprows=10, ndmin=1)
            name1, name2 = get_cross_file_line2(in_file)
            if data_raw.size != 0:
                ymd = data_raw['d1']
                hms1 = data_raw['d2']
                hms2 = data_raw['d3']
                ymdhms1 = list(map(ymd_hms, ymd, hms1))
                ymdhms2 = list(map(ymd_hms, ymd, hms2))
                ymdhms1 = np.array(ymdhms1)
                ymdhms2 = np.array(ymdhms2)

                self.data['ymdhms1'] = ymdhms1
                self.data['ymdhms2'] = ymdhms2
                self.data['lat1'] = data_raw['d4']
                self.data['lon1'] = data_raw['d5']
                self.data['lat2'] = data_raw['d6']
                self.data['lon2'] = data_raw['d7']
                lens = self.data['lat1'].size
                self.data['name1'] = np.array([name1] * lens)
                self.data['name2'] = np.array([name2] * lens)

        else:
            raise KeyError(
                'Cant handle this file type： {}'.format(self.file_type))

    def is_day_neight(self, time_type, sun_z):

        if time_type == 'all':
            return True
        elif time_type == 'night' and sun_z > 100:
            return True
        elif time_type == 'day' and sun_z <= 100:
            return True

    def filter_time_type(self, time_type):
        """
        time
        """
        # 数据长度
        lens = self.data['lat1'].size
        idx_list = []
        for i in range(lens):
            ymd = self.data['ymdhms1'][i][0:8]
            hms = self.data['ymdhms1'][i][8:]
            lat = self.data['lat1'][i]
            lon = self.data['lon1'][i]
            sun_z = getasol6s(ymd, hms, lon, lat)
            # 所属时间类型 白天 夜间 全天 符合给定条件则进入
            if self.is_day_neight(time_type, sun_z):
                idx_list.append(i)

        if len(idx_list) > 0:
            idx = (np.array(idx_list))
            for key in list(self.data.keys()):
                self.data[key] = self.data[key][idx]

    def filter_time_secs(self, sat1, secs):

        # 数据长度
        lens = self.data['lat1'].size
        for i in range(lens):
            if self.file_type in ['geo_leo', 'leo_area']:
                ymdhms1 = ymdhms2date(self.data['ymdhms1'][i])
                ymdhms2 = ymdhms2date(self.data['ymdhms2'][i])
                ss = ymdhms1 - relativedelta(seconds=secs)
                ee = ymdhms2 + relativedelta(seconds=secs)
                self.lines.append([ss, ee])
            elif self.file_type in ['leo_leo', 'leo_fix', 'leo_snox']:
                name1 = self.data['name1'][i]
                name2 = self.data['name2'][i]
                if sat1 == name1:
                    ymdhms = ymdhms2date(self.data['ymdhms1'][i])
                elif sat1 == name2:
                    ymdhms = ymdhms2date(self.data['ymdhms2'][i])
                ss = ymdhms - relativedelta(seconds=secs)
                ee = ymdhms + relativedelta(seconds=secs)
                self.lines.append([ss, ee])

        # 这类型要保留前4后4
        if 'leo_leo' == self.file_type:
            save_cross_num = 8
            half_num = int(save_cross_num / 2)
            if len(self.lines) > save_cross_num:
                self.lines = self.lines[:half_num] + self.lines[-half_num:]


def get_cross_file_line2(in_file):

    name1 = None
    name2 = None
    if os.path.isfile(in_file):
        fp = open(in_file, 'r')
        line1 = fp.readline()
        line2 = fp.readline()
        fp.close()

        name1 = line1.split(':')[1]
        name2 = line2.split(':')[1]
        name1 = name1.split()[0]
        name2 = name2.split()[0]
    return name1.strip(), name2.strip()


def ymd_hms(ymd, hms):
    """
    ymd = 20180101
    hms = 040404
    """
    ymdhms = ymd + hms.replace(':', '')
    return np.str(ymdhms)


def ymdhms2date(ymdhms):
    return datetime.strptime(ymdhms, '%Y%m%d%H%M%S')

if __name__ == '__main__':
    pass
    path_out_map = str_format('/%ID', {
        'ID': '201900157023181149038',
    })
    print(path_out_map)
#     path1 = "E:/projects/ocrs/cfg/global.cfg"
#     path2 = "E:/projects/ocrs/cfg/FY3B+MERSI.yaml"
    # c = Config(path1)
    # c = Config(path2)
    # print c.error
    # l = c.__dict__.keys()
    # l = sorted(l)
    # for k in l:
    # print k, ":", c.__dict__[k]
    # print k

    # ################# test ReadOrbitCrossFile ################
    # LEO_AREA
#     leo_area_name = r'C:\Users\wangpeng\Desktop\tmp\cross\AQUA_australia_LEO_AREA_20171221.txt'
#     read_data = ReadOrbitCrossFile.read_cross_file(leo_area_name, 'leo_area')

    # LEO_LEO
#     leo_leo_name = r'C:\Users\wangpeng\Desktop\tmp\cross\FENGYUN-3D_NPP_LEO_LEO_20180901.txt'
#     read_data = ReadOrbitCrossFile.read_cross_file(leo_leo_name, 'leo_leo')

    # LEO_FIX
#     leo_fix_name = r'C:\Users\wangpeng\Desktop\tmp\cross\AQUA_FIX_LEO_FIX_20181101.txt'
#     read_data = ReadOrbitCrossFile.read_cross_file(leo_fix_name, 'leo_fix')

    # GEO_LEO
#     geo_leo_name = r'C:\Users\wangpeng\Desktop\tmp\cross\FENGYUN-2F_METOP-A_GEO_LEO20181101.txt'
#     read_data = ReadOrbitCrossFile.read_cross_file(geo_leo_name, 'geo_leo')

#     keys = read_data.keys()
#     keys.sort()
#     for data_name in keys:
#         print data_name, type(read_data[data_name]), read_data[data_name]
