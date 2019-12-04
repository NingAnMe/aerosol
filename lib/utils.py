#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2019/12/3 10:47
# @Author  : NingAnMe <ninganme@qq.com>
import inspect


def get_function_name():
    """获取正在运行函数(或方法)名称"""
    return inspect.stack()[1][3]
