#!/usr/bin/env python 
# -*- coding: utf-8 -*-
# @Time    : 2020-04-30 10:23
# @Author  : NingAnMe <ninganme@qq.com>
from datetime import datetime
from dateutil.relativedelta import relativedelta
import os

cmd = r"cp -rf /RED1BDATA/cma/AEROSOL_1.0/SupportData/FY3D_MERSI/Granule/20191115/20191115/{}/*mod35* /RED1BDATA/cma/AEROSOL_1.0/SupportData/FY3B_MERSI/Granule/20191115/20191115/{}/"
# cmd1 = r"cp -rf /RED1BDATA/cma/AEROSOL_1.0/SupportData/FY3D_MERSI/Granule/20191115/20191115/{}/*geo* /RED1BDATA/cma/AEROSOL_1.0/SupportData/FY3B_MERSI/Granule/20191115/20191115/{}/"
# cmd2 = r"cp -rf /RED1BDATA/cma/AEROSOL_1.0/SupportData/FY3D_MERSI/Granule/20191115/20191115/{}/*met* /RED1BDATA/cma/AEROSOL_1.0/SupportData/FY3B_MERSI/Granule/20191115/20191115/{}/"
# cmd3 = r"cp -rf /RED1BDATA/cma/AEROSOL_1.0/SupportData/FY3D_MERSI/Granule/20191115/20191115/{}/*1000m* /RED1BDATA/cma/AEROSOL_1.0/SupportData/FY3B_MERSI/Granule/20191115/20191115/{}/"

dt = datetime.strptime("20191115", "%Y%m%d")

cmds = list()
for i in range(288):
    dt_n = dt + relativedelta(minutes=i * 5)
    hm = "{:02}{:02}".format(dt_n.hour, dt_n.minute)
    cmds.append(cmd.format(hm, hm))
    # cmds.append(cmd1.format(hm, hm))
    # cmds.append(cmd2.format(hm, hm))
    # cmds.append(cmd3.format(hm, hm))

for c in cmds:
    os.system(c)
