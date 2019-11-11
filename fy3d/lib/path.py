#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2019/11/11 16:01
# @Author  : NingAnMe <ninganme@qq.com>
import os

lib_path = os.path.dirname(os.path.abspath(__name__))


def get_aid_path():
    return os.path.join(os.path.dirname(lib_path), 'aid')
