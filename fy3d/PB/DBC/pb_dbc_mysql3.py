# coding: utf-8
'''
Created on 2012-2-21

@author: Administrator
'''

import datetime
import pymysql
import numpy as np


class DBUtil:

    def __init__(self):
        '''
        默认连接
        '''
        self.connectDB = self.connector_DB_hz

    def setDBConnector(self, dbConnectorFunc):
        '''
        设定连接DB所用函数
        '''
        self.connectDB = dbConnectorFunc

    def connector_DB_hz(self):
        '''
        建立和数据库系统的连接
        '''
        db = pymysql.connect(host = '183.230.93.188', user = 'hzqx',
                             passwd = 'PassWord@1234', db = 'hangZhou', use_unicode = True, charset = "utf8")
        return db

    def executeSearch(self, sql):
        '''
        检索DB
        '''
        db = self.connectDB()
        cursor = db.cursor()
        count = 0
        results = ()
        try:
            count = cursor.execute(sql)
            if count > 0:
                results = cursor.fetchall()
        except Exception, e:
            print 'sql:%s, error:%s' % (sql, e)
        finally:
            cursor.close()
            db.close()
        return results

    def executeInsert(self, sql, params = []):
        '''
        插入DB，一行or多行
        '''
        db = self.connectDB()
        cursor = db.cursor()
        try:
            if len(params) == 0:
                cursor.execute(sql)
            elif len(np.array(params).shape) == 2:
                cursor.executemany(sql, params)
            else:
                return
            db.commit()
        except Exception, e:
            db.rollback()
            print 'sql:%s, error:%s' % (sql, e)
        finally:
            cursor.close()
            db.close()

    def executeUpdate(self, sql):
        '''
        更新DB一条记录
        '''
        db = self.connectDB()
        cursor = db.cursor()
        try:
            cursor.execute(sql)
            db.commit()
        except Exception, e:
            db.rollback()
            print 'sql:%s, error:%s' % (sql, e)
        finally:
            cursor.close()
            db.close()

    def executeDelete(self, sql):
        '''
        删除DB记录
        '''
        self.executeUpdate(sql)


if __name__ == '__main__':

    db = DBUtil()
    db.setDBConnector(db.connector_DB_hz)

    sql = u"""INSERT INTO task (plantime,status,operation,starttime,endtime) VALUES(%s)""" % (','.join(['%s'] * 5))
    datalst = [[datetime.datetime(2019, 6, 18, 7, 0), 1, 'PPS_H08_AHI_NVI_2KM', datetime.datetime(2019, 6, 18, 8, 47, 50, 354217), datetime.datetime(2019, 6, 18, 8, 47, 50, 354217)]]
    sql = u"""select * from task"""
    res = db.executeSearch(sql)
    print res
    db.executeInsert(sql, datalst)
