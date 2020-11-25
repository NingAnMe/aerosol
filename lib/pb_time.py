#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-06-03 14:34
# @Author  : NingAnMe <ninganme@qq.com>
from datetime import datetime


def JDay2Datetime(stryear, strJdays, strhms):
    '''
    day of year 2 datetime
    '''
    strJdays = strJdays.zfill(3)
    if len(strhms) == 4:
        strhms = strhms + '00'

    return datetime.strptime('%s%s %s' % (stryear, strJdays, strhms), '%Y%j %H%M%S')
