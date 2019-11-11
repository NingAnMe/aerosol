#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2019/11/11 15:46
# @Author  : NingAnMe <ninganme@qq.com>
"""
获取将FY3D转换为MODIS格式的flat文件需要的metadata
储存为metadatas.pickle文件
"""

import os
import pickle

from spectral import envi

from .lib.path import get_aid_path

aid_dir = get_aid_path()

in_dir = aid_dir
out_dir = aid_dir

filenames = ['a1.17299.1910.1000m.hdr',
             'a1.17299.1910.geo.hdr',
             'a1.17299.1910.mod35.hdr',
             'a1.17299.1910.mod35qa.hdr',
             'a1.17299.1910.met.hdr'
             ]

metadatas = {}

for filename in filenames:
    in_file = os.path.join(in_dir, filename)
    print('hdr file: {}'.format(in_file))

    metadata = envi.read_envi_header(in_file)
    interleave = metadata.pop('interleave')
    metadata.pop('description')
    file_type = filename.split('.')[-2]
    metadatas[file_type] = {'metadata': metadata, 'interleave': interleave}

out_file = os.path.join(out_dir, 'metadatas.pickle')

with open(out_file, 'wb') as fp:
    pickle.dump(metadatas, fp)
