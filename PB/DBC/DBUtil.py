# coding: UTF-8
'''
Created on 2016-06-07
@author: huangyejian
'''
# import MySQLdb
import pymysql
import time
from _mysql import NULL
from MySQLdb.constants.FLAG import AUTO_INCREMENT
from datetime import date, datetime
# from _sqlite3 import Cursor


class DBUtil:

    def __init__(self, host, user, passwd, dbname, port):
        '''
        Need Input Params : HOST , USER , PASSWD , DBNAME
        If can not connect database, will connect always .
        '''
        self.HOST = host
        self.USER = user
        self.PASSWD = passwd
        self.DBNAME = dbname
        self.PORT = port
        self.db = ''

        while(1):
            try:
                self.db = MySQLdb.connect(
                    host=host, user=user, passwd=passwd, db=dbname, port=port, use_unicode=True, charset="utf8")
                print 'CONNECT OK'
                break
            except Exception, e:
                print ('Sql:%s <%s> ,Connect Error,%s' % (host, dbname, e))
                time.sleep(5)

        return

    def disconnect(self):
        '''
        Close database connect .
        '''
        if self.db == NULL:
            return 'OK'
        try:
            self.db.close()

        except Exception, e:
            print ('Sql:%s <%s> ,Close SQL error %s' %
                   (self.HOST, self.DBNAME, e))
            return 'ERROE'
        finally:
            pass

        self.db = NULL
        return 'OK'

    def droptable(self, tablename):
        '''
        Drop table .
        If the table exist, drop it
        If the table is not exist ,return OK
        '''
        cursor = self.db.cursor()
        sql = 'DROP TABLE IF EXISTS %s' % (tablename)
        try:
            cursor.execute(sql)
        except Exception, e:
            print ('Sql:Sql:%s <%s> ,Drop table %s error %s' %
                   (self.HOST, self.DBNAME, tablename, e))
            cursor.close()
            return 'ERROR'

        cursor.close()
        self.db.commit()
        return 'OK'

    def createtable(self, sql):
        '''
        create table
        If the table is exists ,drop it and create a new table
        '''
        key = sql.split('(')
        k = key[0].split()
        tableName = k[2].strip()

        cursor = self.db.cursor()
        try:
            cursor.execute("DROP TABLE IF EXISTS %s" % tableName)
            try:
                cursor.execute(sql)
            except Exception, e:
                print ('Sql:%s <%s> ,Create table error %s' % (e))
                cursor.close()
                return 'ERROR'
            finally:
                pass

        except Exception, e:
            print ('Sql:%s <%s> ,Create table or drop table error %s' % (e))
            cursor.close()
            return 'ERROR'
        finally:
            cursor.close()

        return 'OK'

    def executeInsert(self, sql):
        '''
        Insert a sqldata 
        '''
        cursor = self.db.cursor()
        try:
            cursor.execute(sql)
            self.db.commit()
            cursor.close()

        except Exception, e:
            print ('Sql:Sql:%s <%s> ,Insert data error %s' %
                   (self.HOST, self.DBNAME, e))
            self.db.rollback()
            cursor.close()
            return 'ERROR'
        finally:
            pass

        return 'OK'

    def executeInsertMany(self, sql, params=[]):
        '''
        Insert many data
        '''
        if len(params) == 0:
            return self.executeInsert(sql)
        else:
            cursor = self.db.cursor()
            try:
                cursor.executemany(sql, params)
                self.db.commit()
            except Exception, e:
                print ('Sql:Sql:%s <%s> ,Insert many data error %s' %
                       (self.HOST, self.DBNAME, e))
                self.db.rollback()
                cursor.close()
                return 'ERROR'
            finally:
                cursor.close()

        return 'OK'

    def executeSelect(self, sql):
        '''
        select data
        '''
        cursor = self.db.cursor()
        count = 0
        results = ()

        try:
            count = cursor.execute(sql)
            if count <= 0:
                return 'NODATA'
            elif count > 0:
                results = cursor.fetchall()

        except Exception, e:
            cursor.close()
            print ('Sql:Sql:%s <%s> ,Insert many data error %s' %
                   (self.HOST, self.DBNAME, e))
            return 'ERROR'
        finally:
            pass

        cursor.close()
        return results

    def executeUpdate(self, sql):
        '''
        update data
        '''
        cursor = self.db.cursor()
        try:
            cursor.execute(sql)
            self.db.commit()

        except Exception, e:
            print ('Sql:%s <%s> ,Update data error %s' % (e))
            self.db.rollback()
            cursor.close()
            return 'ERROR'
        finally:
            cursor.close()

        return 'OK'

    def executeDelData(self, sql):
        '''
        delect data from table
        '''
        cursor = self.db.cursor()
        try:
            cursor.execute(sql)
            self.db.commit()

        except Exception, e:
            print ('Sql:%s <%s> ,delete data error %s' % (e))
            self.db.rollback()
            cursor.close()
            return 'ERROR'
        finally:
            cursor.close()

        return 'OK'
if __name__ == '__main__':
    a = DBUtil('127.0.0.1', 'root', 'huang123', 'kingtansin-db', '3306')
