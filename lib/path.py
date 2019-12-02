#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2019/11/11 16:01
# @Author  : NingAnMe <ninganme@qq.com>
import os
import sys
sys.path.append('..')
from config import ROOT_PATH


def get_aid_path():
    return os.path.join(ROOT_PATH, 'aid')
