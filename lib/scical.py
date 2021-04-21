#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-09-22 10:13
# @Author  : NingAnMe <ninganme@qq.com>
import numpy as np


def rmse(predictions, targets):
    return np.sqrt(((predictions - targets) ** 2).mean())
