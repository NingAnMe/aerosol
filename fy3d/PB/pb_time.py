# coding: utf-8
from contextlib import contextmanager
from datetime import datetime
# import datetime
import calendar
import math
import re
import time

from dateutil.relativedelta import relativedelta
__author__ = 'wangpeng'

'''
FileName:     pb_time.py
Description:  时间处理的公共函数,逐步完善关于时间处理的函数
Author:       wangpeng
Date:         2015-08-21
version:      1.0.0.050821_beat
'''


def run(date_s, date_e):
    while date_s <= date_e:
        print('Test:', date_s)
        date_s = date_s + relativedelta(days = 1)


def ymdCheck(ymd, hms, fmt = "%Y%m%d %H%M%S"):
    '''
    check string time in a specified format, and return a float point secs.
    '''
    try:
        d = time.strptime('%s %s' % (ymd, hms), fmt)

        ts = time.mktime(d)
        return ts
    except:
        print("ymdCheck %s %s is wrong date。\n" % (ymd, hms))
        return None


def ymd2date(ymd):
    return datetime.strptime('%s' % (ymd), "%Y%m%d")


def ymd2y_m_d(ymd, split = '.'):
    if len(ymd) != 8:
        return None
    return split.join([ymd[:4], ymd[4:6], ymd[6:]])


def ymd_plus(ymd, days):
    date1 = ymd2date(ymd)
    return (date1 + relativedelta(days = days)).strftime('%Y%m%d')


def get_local_time():
    '''
    获得当日日期, datetime格式
    '''
    return datetime.now()


def get_utc_time():
    '''
    获得当日UTC日期, datetime格式
    '''
    return datetime.utcnow()


def arg_str2date(str_time = ""):
    '''
    把字符串格式的日期转化为datetime类型
    :param str_time: YYYYMMDD-YYYYMMDD YYYYMM-YYYYMM
    :return:
    '''
    if str_time == "":
        date_s = datetime.utcnow()
        date_e = date_s
        return date_s, date_e

    # 正确的日期格式正则
    patRange = '(\d{8})-(\d{8})'
    patRange_M = '(\d{6})-(\d{6})'
    if len(str_time) == 17:

        m = re.match(patRange, str_time)  # 检查时间的输入格式是否正确
        if m is None:
            print('input Format: YYYYMMDD-YYYYMMDD !')
            exit(1)
        ymd1 = m.group(1)
        ymd2 = m.group(2)

        try:  # 检查时间是否是有效时间
            datetime.strptime(ymd1, '%Y%m%d')
            datetime.strptime(ymd2, '%Y%m%d')
        except:
            print("%s or %s is invalid" % (ymd1, ymd2))
            exit(1)

        # 把字符串时间转化为datetime
        date_s = datetime.strptime(ymd1, '%Y%m%d')
        date_e = datetime.strptime(ymd2, '%Y%m%d')
        return date_s, date_e

    elif len(str_time) == 13:
        m = re.match(patRange_M, str_time)
        if m is None:
            print('input Format: YYYYMM-YYYYMM !')
            exit(1)
        ymd1 = m.group(1) + '01'
        ymd2 = m.group(2) + '01'

        try:  # 检查时间是否是有效时间
            datetime.strptime(ymd1, '%Y%m%d')
            datetime.strptime(ymd2, '%Y%m%d')
        except:
            print("%s or %s is invalid" % (ymd1, ymd2))
            exit(1)
        # 把字符串时间转化为datetime
        date_s = datetime.strptime(ymd1, '%Y%m%d')
        date_e = datetime.strptime(ymd2, '%Y%m%d')
        return date_s, date_e

    else:
        print('time length error')
        exit(1)


def lonIsday(ymd, hms, lon):
    zone = int(lon / 15.)
    stime = datetime.strptime('%s %s' % (ymd, hms), '%Y%m%d %H:%M:%S')
    HH = (stime + relativedelta(hours = zone)).strftime('%H')
    if 6 <= int(HH) <= 18:
        return True
    else:
        return False


def lon2timezone(lon):
    '''
    经度转时区
    '''
    return int(lon / 15.)


def getJulianDay(yy, mm, dd, startwith = 1):
    '''
    get the day of the year
    '''
    date1 = datetime.date(yy, 1, 1)
    date2 = datetime.date(yy, mm, dd)
    return (date2 - date1).days + startwith


def JDay2Datetime(stryear, strJdays, strhms):
    '''
    day of year 2 datetime
    '''
    strJdays = strJdays.zfill(3)
    if len(strhms) == 4:
        strhms = strhms + '00'

    return datetime.strptime('%s%s %s' % (stryear, strJdays, strhms), '%Y%j %H%M%S')


def ymd2ymd(num, interval, namerule, ymd):
    '''
    把输入的任意日期转换到对应产品频次的日期
    '''
    date_s = datetime.strptime(ymd, '%Y%m%d')

    if interval == 'MONTHLY':
        # 如果订购月产品，要获取上个月时间信息
        t_ymd = (date_s - relativedelta(months = 1)).strftime('%Y%m%d')
        if 'YYYYMMDF' in namerule:
            newYmd = '%s%02d' % (t_ymd[:6], 0o1)
        elif 'YYYYMMDL' in namerule:
            lastday = calendar.monthrange(int(t_ymd[:4]), int(t_ymd[4:6]))[1]
            newYmd = '%s%02d' % (t_ymd[:6], lastday)

    elif interval == '10DAY':
        # 如果订购月产品，要获取上个月时间信息
        if 10 < int(ymd[6:8]) <= 20:
            if 'YYYYMMDF' in namerule:
                newYmd = '%s%02d' % (ymd[:6], 0o1)
            elif 'YYYYMMDL' in namerule:
                newYmd = '%s%02d' % (ymd[:6], 10)

        elif int(ymd[6:8]) > 20:
            if 'YYYYMMDF' in namerule:
                newYmd = '%s%02d' % (ymd[:6], 11)
            elif 'YYYYMMDL' in namerule:
                newYmd = '%s%02d' % (ymd[:6], 20)

        else:
            t_ymd = (date_s - relativedelta(months = 1)).strftime('%Y%m%d')
            if 'YYYYMMDF' in namerule:
                newYmd = '%s%02d' % (t_ymd[:6], 21)
            elif 'YYYYMMDL' in namerule:
                lastday = calendar.monthrange(
                    int(t_ymd[:4]), int(t_ymd[4:6]))[1]
                newYmd = '%s%02d' % (t_ymd[:6], lastday)
    else:
        newYmd = ymd

    return newYmd


def days2hms(days):
    '''
    Function:    days2ymd
    Description: 将天的计数信息转换为时分秒微秒信息
    author:      wangpeng
    date:        2017-03-21
    Input:       days 天的计数数值(0-1)的浮点数
    Output:
    Return:      类型：字符串   格式:HH:MM:SS:MS  实例: 09:00:00:999  (异常返回 None)
    Others:
    '''
    if days >= 1:
        return None
    # 天单位转换成小时
    hour_point, hour = math.modf(days * 24.)
    # 把小时的小数部分转换成分钟
    minute_point, minute = math.modf(hour_point * 60.)
    # 把分钟的小数部分转化成秒
    cecond_point, second = math.modf(minute_point * 60.)
    # 把秒的小数部分转成毫秒
    millisecond_point, millisecond = math.modf(cecond_point * 60.)
    return '%02d:%02d:%02d.%03d' % (hour, minute, second, millisecond)


def npp_ymd2seconds(ymdhms):
    '''
    npp VIIRS和 CRIS 数据的时间单位是距离 1958年1月1日 UTC时间的microseconds 微秒
    ymdhms/1000000 = 秒  （距离1958年1月1日 UTC时间）

    '''
    T1 = ymdhms / 1000000
    secs = int(
        (datetime(1970, 1, 1, 0, 0, 0) - datetime(1958, 1, 1, 0, 0, 0)).total_seconds())
    # 返回距离1970-01-01的秒
    return (T1 - secs)


def metop_ymd2seconds(ymdhms):
    '''
    METOP-A IASI 数据的时间单位是距离 2000年1月1日 UTC时间的秒
    return 使用此函数转换为距离1970年01月01日的秒
    '''
    T1 = ymdhms
    secs = int(
        (datetime(2000, 1, 1, 0, 0, 0) - datetime(1970, 1, 1, 0, 0, 0)).total_seconds())
    # 返回距离1970-01-01的秒
    return (T1 + secs)


def fy3_ymd2seconds(ary_day, ary_time):
    '''
    FY3D 卫星的时间计算方式
    ary_day, 天计数  从2000, 1, 1, 12, 0, 0
    ary_time 毫秒
    reurn   返回 1970年01月01日的秒
    '''

#   源矩阵类型 uint32  和  int32 不同 此函数relativedelta不支持 需要转成浮点，或是在外部统一int32,奇怪的问题，未来得及细细解决。
    newTime = datetime(2000, 1, 1, 12, 0, 0) + \
        relativedelta(days = ary_day, seconds = ary_time / 1000.)
#     newTime = datetime(2000, 1, 1, 12, 0, 0) + relativedelta(days=ary_day, microseconds=ary_time)
    secs = int((newTime - datetime(1970, 1, 1, 0, 0, 0)).total_seconds())
    # 返回距离1970-01-01的秒
    return secs


def is_day_timestamp_and_lon(timestamp, lon):
    """
    根据距离 1970-01-01 年的时间戳和经度计算是否为白天
    :param timestamp: 距离 1970-01-01 年的时间戳
    :param lon: 经度
    :return:
    """
    zone = int(lon / 15.)
    stime = datetime.utcfromtimestamp(timestamp)
    HH = (stime + relativedelta(hours = zone)).strftime('%H')
    if 6 <= int(HH) <= 18:
        return True
    else:
        return False


@contextmanager
def time_block(flag, switch = True):
    """
    计算一个代码块的运行时间
    :param flag: 标签
    :param on: 是否开启
    :return:
    """
    time_start = datetime.now()
    try:
        yield
    finally:
        if switch is True:
            time_end = datetime.now()
            all_time = time_end - time_start
            print("{} time: {}".format(flag, all_time))


def get_ymd(in_file):
    """
    从输入文件中获取 ymd
    :param in_file:
    :return:
    """
    if not isinstance(in_file, str):
        return
    m = re.match(r".*_(\d{8})_", in_file)

    if m is None:
        return
    else:
        return m.groups()[0]


def get_hm(in_file):
    """
    从输入文件中获取 hm
    :param in_file:
    :return:
    """
    if not isinstance(in_file, str):
        return
    m = re.match(r".*_(\d{4})_", in_file)

    if m is None:
        return
    else:
        return m.groups()[0]


def get_dsl(ymd, launch_date):
    """
    根据文件名和发射时间获取相差的天数
    :param ymd: (str)
    :param launch_date: (str)卫星发射时间 YYYYMMDD
    :return: (int)
    """
    date1 = ymd2date(ymd)
    date2 = ymd2date(launch_date)
    delta = date1 - date2
    dsl = delta.days
    return dsl


def is_cross_time(t1_s, t1_e, t2_s, t2_e):
    '''
    判断俩个时间段是否有交叉
    '''
    if t1_s <= t2_s < t1_e:
        return True
    elif t1_s < t2_e <= t1_e:
        return True
    elif t1_s > t2_s and t1_e < t2_e:
        return True
    else:
        return False


def combine_timelist1(time_list):
    '''
    : 把时间段有重复部分进行融合
    :time_list: [[datetime1, datetime2], ...]
    :return : [[datetime1, datetime2], ...]
    '''
    # 将时间段list中有重叠的时间段进行融合为新的时间段
    com_time_list = []
    # 默认排序,升序
    time_list.sort()
    # 标记有时间融合的时间
    stime = time_list[0][0]
    etime = time_list[0][1]
    for i in range(1, len(time_list), 1):
        if time_list[i][1] <= etime:
            continue
        elif time_list[i][0] <= etime <= time_list[i][1]:
            etime = time_list[i][1]
        elif time_list[i][0] > etime:
            com_time_list.append([stime, etime])
            stime = time_list[i][0]
            etime = time_list[i][1]

    com_time_list.append([stime, etime])

    return com_time_list


def combine_timelist(time_list):
    '''
    : 把时间段有重复部分进行融合
    :time_list: [[datetime1, datetime2], ...]
    :return : [[datetime1, datetime2], ...]
    '''
    # 将时间段list中有重叠的时间段进行融合为新的时间段
    com_time_list = []
    # 默认排序,升序
    time_list.sort()
    # 标记有时间融合的时间
    stime = time_list[0][0]
    etime = time_list[0][1]
    for t1 in time_list:
        if t1[1] <= etime:
            continue
        elif t1[0] <= etime <= t1[1]:
            etime = t1[1]
        elif t1[0] > etime:
            com_time_list.append([stime, etime])
            stime = t1[0]
            etime = t1[1]

    com_time_list.append([stime, etime])

    return com_time_list


def repeat_time_list(time_list1, time_list2):
    time_list = []
    for t1 in time_list1:
        flag = 0
        t1_start = t1[0]
        t1_end = t1[1]
        for t2 in time_list2:
            t2_start = t2[0]
            t2_end = t2[1]
            if is_cross_time(t2_start, t2_end, t1_start, t1_end):
                flag = 1
        if flag == 0:
            time_list.append([t1_start, t1_end])
    return time_list


def delete_rep_time_list(time_list1, time_list2):

    time_list = []

#     len1 = len(time_list1)
    for t1 in time_list1:
        flag = 0
        t1_start = t1[0]
        t1_end = t1[1]
        for t2 in time_list2:
            t2_start = t2[0]
            t2_end = t2[1]

            # 数据在迁移记录时间范围内
            if  t1_start >= t2_start and t1_end <= t2_end:
                flag = 1
                pass
            elif t2_start <= t1_start < t2_end:
                flag = 1
                time_list.append([t2_end, t1_end])
                break
            elif  t2_start < t1_end <= t2_end:
                flag = 1
                time_list.append([t1_start, t2_start])
                break
            elif t1_start <= t2_start and t1_end >= t2_end:
                flag = 1
                time_list.append([t1_start, t2_start])
                time_list.append([t2_end, t1_end])
                break
        if flag == 0:
            print('ssss', t1_start, t1_end)
            time_list.append([t1_start, t1_end])
#     len2 = len(time_list)
#     print len2, len1
    print('flag', flag)
    if flag == 1:
        return delete_rep_time_list(time_list, time_list2)
#         print 're', len(time_list)
    return time_list


if __name__ == '__main__':

    pass
