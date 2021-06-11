#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
@Time    : 2018/6/7 11:46
@Author  : AnNing
"""
import os

NCEP_TABLE = [
    "PRES:surface:anl",
    "PWAT",
    "UGRD:10 m above",
    "VGRD:10 m above",
    "TOZNE",
    "TMP:1000 mb",
    "TMP:925 mb",
    "TMP:850 mb",
    "TMP:700 mb",
    "TMP:500 mb",
    "TMP:400 mb",
    "TMP:300 mb",
    "TMP:250 mb",
    "TMP:200 mb",
    "TMP:150 mb",
    "TMP:100 mb",
    "TMP:70 mb",
    "TMP:50 mb",
    "TMP:30 mb",
    "TMP:20 mb",
    "TMP:10 mb",
    "RH:1000 mb",
    "RH:925 mb",
    "RH:850 mb",
    "RH:700 mb",
    "RH:500 mb",
    "RH:400 mb",
    "RH:300 mb",
    "LAND:",
    "TMP:surface:anl",
    "ICEC"]


class Ncep2Byte(object):
    """
    将 NCEP 文件转为二进制文件
    201501221200，时间大于等于这个时间用新版命令，默认使用新版命令
    """

    def __init__(self, in_file, out_file, new=True, ncep_table=None):
        """
        :param in_file: (str) 输入的文件
        :param out_file: (str) 输出的文件
        :param new: (bool)  # 时间是否大于等于 201501221200
        :param ncep_table: (list) 需要处理的 ncep 类型
        """

        self.error = False

        self.in_file = in_file
        self.out_file = out_file
        self.new = new

        if ncep_table is None:
            self.ncep_table = NCEP_TABLE
        else:
            self.ncep_table = ncep_table

        self.wgrib1 = "wgrib -s %s | grep -E \"%s\" | wgrib -i %s -nh -append -o %s"  # wgrib1 命令
        self.wgrib2 = "wgrib2 -set local_table 1 -s %s | grep -E \"%s\" | wgrib2 -i %s -no_header " \
                      "-append -bin %s"  # wgrib2 命令

        self.cmd1 = None
        self.cmd2 = None

    def _get_cmd(self, ncep_type):
        """
        获取两个命令版本
        :param ncep_type:
        :return:
        """
        if self.error:
            return
        try:
            self.cmd1 = self.wgrib1 % (self.in_file, ncep_type, self.in_file, self.out_file)
            self.cmd2 = self.wgrib2 % (self.in_file, ncep_type, self.in_file, self.out_file)
        except Exception as why:
            print(why)
            self.error = True
            return

    def _remove_file(self):
        """
        如果文件存在，删除原来的文件
        :return:
        """
        if os.path.isfile(self.out_file):
            os.remove(self.out_file)

    def ncep2byte(self):
        if self.error:
            return
        self._remove_file()  # 如果已经存在文件，删除文件后重新处理
        for ncep_type in self.ncep_table:
            self._get_cmd(ncep_type)
            # 201501221200，时间大于等于这个时间用新版命令
            if self.new:
                # print(self.cmd2)
                os.system(self.cmd2)
            else:
                # print(self.cmd1)
                os.system(self.cmd1)
