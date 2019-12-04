#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2019/11/11 16:01
# @Author  : NingAnMe <ninganme@qq.com>
import os


LIB_PATH = os.path.dirname(os.path.realpath(__file__))
ROOT_PATH = os.path.dirname(LIB_PATH)


def get_root_path():
    return ROOT_PATH


def get_aid_path():
    return os.path.join(get_root_path(), 'aid')

print(get_aid_path())