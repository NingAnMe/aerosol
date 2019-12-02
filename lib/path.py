#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2019/11/11 16:01
# @Author  : NingAnMe <ninganme@qq.com>
import os


LIB_PATH = os.path.dirname(os.path.abspath(__name__))


def get_root_path():
    return os.path.dirname(LIB_PATH)


def get_aid_path():
    return os.path.join(get_root_path(), 'aid')
