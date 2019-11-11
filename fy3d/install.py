#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2019/11/11 16:16
# @Author  : NingAnMe <ninganme@qq.com>


from .a01_get_modis_nevi_metadata import get_metadata
file_ = get_metadata()
print('Install: {}'.format(file_))
