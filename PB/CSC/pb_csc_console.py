# coding: utf-8

from datetime import datetime
from multiprocessing import Pool
from posixpath import join as urljoin
import email
import getopt
import imaplib
import logging
import os
import poplib
import socket
import subprocess
import sys
import time

from dateutil.relativedelta import relativedelta

from PB import pb_io


python = 'python -W ignore'
mpi_run = 'mpirun'
mpi_main = 'mpi.py'
cores = 56


__description__ = u'调度控制'
__author__ = 'wangpeng'
__date__ = '2019-03-19'
__version__ = '1.0'


def mail_date2date(mail_str_date):
    str_date = mail_str_date.split(',')[1].split('+')[0]
    fmt_datetime = datetime.strptime(
        str_date.strip(), '%d %b %Y %H:%M:%S')
    return fmt_datetime


def whoImportMe():
    return sys._getframe(2).f_code.co_filename  # .f_code.co_name


class LogServer:

    def __init__(self, logPath, loggerName=None):

        if logPath != '' and not os.path.isdir(logPath):
            os.makedirs(logPath)

        if loggerName is None:
            loggerName = whoImportMe()

        logFile = os.path.splitext(os.path.basename(loggerName))[0] + '.log'

        self.logger = logging.getLogger(loggerName)
        handler = logging.FileHandler(urljoin(logPath, logFile))
        formatter = logging.Formatter('%(asctime)s %(levelname)s %(message)s')
        handler.setFormatter(formatter)
        handler.setLevel(logging.INFO)

        self.logger.addHandler(handler)
        self.logger.setLevel(logging.DEBUG)

        ch = logging.StreamHandler(sys.stdout)
        ch.setLevel(logging.DEBUG)
        self.logger.addHandler(ch)

    def error(self, msg):
        self.logger.error(msg)
        self.logger.handlers[0].flush()

    def info(self, msg):
        self.logger.info(msg)
        self.logger.handlers[0].flush()


class SocketServer(object):
    '''
    socket服务器
    '''

    def __init__(self):
        self.ssdic = {}

    def __deinit__(self):
        for key in self.ssdic.keys():
            sserver = self.ssdic.get(key)
            if sserver:
                sserver.close()

    # 创建socket服务
    def createSocket(self, port):
        ret = False
        try:
            sserver = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            # print socket.gethostname()
            # sserver.bind((socket.gethostname(), port))
            sserver.bind(('localhost', port))
            sserver.listen(5)
            self.ssdic[port] = sserver
            ret = True
        except Exception as e:
            print str(e)

        return ret

    # 关闭socket服务
    def closeSocket(self, port):
        sserver = self.ssdic.get(port)
        if sserver:
            try:
                sserver.close()
                self.ssdic.pop(port)
                sserver = None
            except Exception, e:
                print e


class MailServer_pop3(poplib.POP3):

    def __init__(self, host, ordernumber):

        poplib.POP3.__init__(self, host)
        self.ordernumber = ordernumber  # 订单号
        self.lines = []  # 邮件内容 (list)
        self.title = ''  # 邮件题目
        self.mailnumber = -1  # 邮件在邮箱中的编号

    def connect(self, user, password):
        try:
            #             print("Attempting APOP authentication")
            self.apop(user, password)
        except poplib.error_proto:
            #             print("Attempting standard authentication")
            try:
                self.user(user)
                self.pass_(password)
            except poplib.error_proto, e:
                print "Login failed:", e

    def findmail(self):
        infos = self.list()
        # df2=self.stat()
        if infos[1] is not None:
            mailNum = len(infos[1])
            for num in xrange(mailNum, 0, -1):
                number, octets = infos[1][num - 1].split(' ')
                # 获得邮件内容 (list of lines)
                lines = []
                try:
                    res = self.retr(number)
                    lines = res[1]
                except Exception, e:
                    print(str(e))
                    continue
                # 取得邮件题目
                mail = email.message_from_string('\n'.join(lines))
                title = mail['Subject']
#                log.info('mail tile:%s'%title)
#                log.info('sys.stdin.encoding:%s'%sys.stdin.encoding)
                if title is None:
                    return False

                title = unicode(title, 'UTF-8')
#                 print title, self.ordernumber
                # 比较邮件题目 (CLASS Order xxxxxxxx Processing Complete)
                if (u'Your Langley ASDC FTP Order <%s>' % self.ordernumber) in title:
                    self.title = title
                    self.mailnumber = number
                    self.lines = lines
#                     return True
                elif (u'CLASS Order %s Processing Complete' % self.ordernumber) in title:
                    self.title = title
                    self.mailnumber = number
                    self.lines = lines
#                     return True

                elif (u'CLASS Order %s Verification' % self.ordernumber) in title:
                    #                     self.title = title
                    #                     self.mailnumber = number
                    #                     self.lines = lines
                    self.dele(number)
#                     return False
        if len(self.title) > 0:
            return True
        else:
            return False

    def savemail(self, MAIL_SAVE_PATH, delete_after_save=False):
        #         print ('save the mail NO. %s' % (self.ordernumber))
        msg = email.message_from_string('\n'.join(self.lines))
        if not os.path.dirname(MAIL_SAVE_PATH):
            os.makedirs(os.path.dirname(MAIL_SAVE_PATH))

        # 在保存路径建立新文件
#         mail = open(os.path.join(MAIL_SAVE_PATH, self.title + '.eml'), 'w')
        mail = open(MAIL_SAVE_PATH, 'w')
        # 写入
        mail.write(msg.as_string(unixfrom=1))
        # 结束
        mail.write('\n')
        # 关闭文件
        mail.close()
        # 删除该邮件
        if delete_after_save:
            self.dele(self.mailnumber)


class MailServer_imap(imaplib.IMAP4):

    def __init__(self, host, ordernumber):

        imaplib.IMAP4.__init__(self, host)
        self.ordernumber = ordernumber  # 订单号
        self.lines = []  # 邮件内容 (list)
        self.title = ''  # 邮件题目
        self.mailnumber = -1  # 邮件在邮箱中的编号

    def connect(self, user, password):
        try:
            self.login(user, password)
        except Exception, e:
            print e

    def demail(self):
        # 删除该邮件
        try:
            self.store(self.mailnumber, '+FLAGS', '\\Deleted')
            self.expunge()
            print 'delelte mail %s' % self.mailnumber
        except Exception, e:
            print e

    def findmail(self):
        self.select()
        typ, data = self.search(None, 'ALL')
        assert(typ) == 'OK'
        numlist = [int(e) for e in data[0].split()]
        for number in sorted(numlist, reverse=False):
            print number
            typ, data1 = self.fetch(number, "(UID BODY[HEADER])")
#             typ, data1 = self.fetch(number, "(UID RFC822)")
            rawmail = data1[0][1]
            email_message = email.message_from_string(rawmail)
            title = email_message['Subject']
            print title
            if (u'Your Langley ASDC FTP Order <%s>' % self.ordernumber) in title:
                self.title = title
                self.mailnumber = number
                typ, data2 = self.fetch(number, "(UID BODY[TEXT])")
                self.lines = data2[0][1]
                return True
            elif (u'CLASS Order %s Processing Complete' % self.ordernumber) in title:
                self.title = title
                self.mailnumber = number
                typ, data2 = self.fetch(number, "(UID BODY[TEXT])")
                self.lines = data2[0][1]
                return True
            # yushuai  20181018修改： 邮件内会出现包含多个订单号的返回结果的情况
            elif ('%s' % self.ordernumber) in title and 'Verification' not in title:
                self.title = title
                self.mailnumber = number
                typ, data2 = self.fetch(number, "(UID BODY[TEXT])")
                self.lines = data2[0][1]
                return True

    def findspam(self):
        self.select()
        typ, data = self.search(None, 'ALL')
        assert(typ) == 'OK'
        for number in data[0].split():
            typ, data1 = self.fetch(number, "(UID BODY[HEADER])")
            rawmail = data1[0][1]
            email_message = email.message_from_string(rawmail)
            title = email_message['Subject']
            if 'Verification' in title:
                self.mailnumber = number
                self.demail()
            elif 'Cleanup Alert'in title:
                self.mailnumber = number
                self.demail()
            elif 'Expired' in title:
                self.mailnumber = number
                self.demail()

            # wangpeng add 2018-12-05 添加了清理邮件功能，7天之前的邮件自动删除
            fmt_datetime = mail_date2date(email_message['Date'])
            fmt_date_time_now = datetime.now() - relativedelta(days=5)

            if fmt_datetime < fmt_date_time_now:
                print fmt_datetime, fmt_date_time_now
                self.mailnumber = number
                self.demail()
            else:
                break

        return False

    def savemail(self, savefile):
        if not os.path.dirname(savefile):
            os.makedirs(os.path.dirname(savefile))
        # 在保存路径建立新文件
        # 多个订单在同一封邮件就用这个关键字分割；
        order_lines = self.lines.split(
            'NOTE: You must pick up your data within 96 hours of this notice.')
        mail = open(savefile, 'w')
        # 最后一个是thank you 丢弃
        for line in order_lines[:-1]:
            if 'CLASS has processed your order number %s' % self.ordernumber in line:
                # 写入
                mail.write(line + '\n')
        mail.close()


def usage():
    print(u"""
    -h / --help :使用帮助
    -v / --verson: 显示版本号
    -j / --job : 作业步骤 -j 0110 or --job 0110
    -s / --sat : 卫星信息  -s FY3B+MERSI_AQUA+MODIS or --sat FY3B+MERSI_AQUA+MODIS
    -t / --time :日期   -t 20180101-20180101 or --time 20180101-20180101
    """)


def get_args():
    try:
        opts, _ = getopt.getopt(
            sys.argv[1:], "hv:j:s:t:p:P:", ["version", "help", "job=", "sat=", "time=", "port=", "thread="])
    except getopt.GetoptError as err:
        # print help information and exit:
        print str(err)  # will print something like "option -a not recognized"
        usage()
        sys.exit(1)

    for key, val in opts:
        if key in ('-v', '--version'):
            verbose = '1.0.1'
            print 'Version: %s' % verbose
            sys.exit()

        elif key in ("-h", "--help"):
            usage()
            sys.exit()
        elif key in ("-s", "--sat"):
            args_pair = val

        elif key in ("-j", "--job"):
            args_id = val

        elif key in ("-t", "--time"):
            args_date = val

        elif key in ("-p", "--port"):
            args_port = int(val)

        elif key in ("-T", "--thread"):
            args_thread = int(val)
        else:
            assert False, "unhandled option"

    return args_pair, args_id, args_date, args_port, args_thread


def get_pair_list(args_pair, g_var_cfg):
    """
    u 获取参数中的卫星对
    return : list
    """

    if args_pair.lower() == 'all':
        args_pair_list = g_var_cfg['PAIRS'].keys()
    else:
        args_pair_list = args_pair.split(',')
    return args_pair_list


def get_date_list(args_pair_list, args_date, g_var_cfg):
    """
    u 获取参数中的时间范围,args_data 支持all 和 时间范围
    return : dict
    """
    args_date_list = dict()
    cfg_launch_date = g_var_cfg['LAUNCH_DATE']
    cfg_rolldays = g_var_cfg['CROND']['rolldays']

    # 从卫星对清单中获取每个卫星对，进行时间获取
    for pair in args_pair_list:
        # 时间字典的key用卫星对来标记
        short_sat = pair.split('+')[0]
        if pair not in args_date_list:
            args_date_list[pair] = []

        if args_date.lower() == 'all':  # 发星以来的日期
            date_start = cfg_launch_date[short_sat]
            date_end = datetime.utcnow()
            date_start = datetime.strptime(date_start, '%Y%m%d')

            while date_start <= date_end:
                args_date_list[pair].append(date_start)
                date_start = date_start + relativedelta(days=1)

        elif args_date.lower() == 'auto':  # 日期滚动
            for rday in cfg_rolldays:
                rd = int(rday)
                date_start = (datetime.utcnow() - relativedelta(days=rd))
                args_date_list[pair].append(date_start)

        else:  # 手动输入的日期
            date_start, date_end = args_date.split('-')
            date_start = datetime.strptime(date_start, '%Y%m%d')
            date_end = datetime.strptime(date_end, '%Y%m%d')
            while date_start <= date_end:
                args_date_list[pair].append(date_start)
                date_start = date_start + relativedelta(days=1)

    return args_date_list


def get_job_id_list(args_pair_list, args_id, g_var_cfg):
    """
    u 获取参数中的作业编号,args_id支持all 和  自定义id(0310,0311)
    return: dict
    """
    args_id_list = dict()

    for pair in args_pair_list:
        if pair not in args_id_list:
            args_id_list[pair] = []
        if args_id.lower() == 'all':  # 自动作业流
            # 若果是all就根据卫星对获取对应的作业流
            job_flow = g_var_cfg['PAIRS'][pair]['job_flow']
            job_flow_def = g_var_cfg['JOB_FLOW_DEF'][job_flow]
            args_id_list[pair] = job_flow_def
        else:  # 手动作业流
            args_id_list[pair] = ['job_%s' % id_ for id_ in args_id.split(',')]

    return args_id_list


def run_command(cmd_list, threads):
    # 开启进程池

    if len(cmd_list) > 0:
        pool = Pool(processes=int(threads))
        for cmd in cmd_list:
            pool.apply_async(command, (cmd,))
        pool.close()
        pool.join()


def command(args_cmd):
    '''
    args_cmd: python a.py 20180101  (完整的执行参数)
    '''

    print args_cmd
    try:
        P1 = subprocess.Popen(args_cmd.split())
    except Exception, e:
        print (e)
        return

    timeout = 3600 * 5
    t_beginning = time.time()
    seconds_passed = 0

    while (P1.poll() is None):

        seconds_passed = time.time() - t_beginning

        if timeout and seconds_passed > timeout:
            print seconds_passed
            P1.kill()
        time.sleep(1)
    P1.wait()


def run_command_parallel(arg_list):

    arg_list = [each + '\n' for each in arg_list]
    fp = open('filelist.txt', 'w')
    fp.writelines(arg_list)
    fp.close()

    cmd = '%s -np %d -machinefile hostfile %s %s' % (
        mpi_run, cores, python, mpi_main)
    os.system(cmd)


def get_cmd_list(job_exe, sat_pair, job_id, date_list, g_path_interface):
    cmd_list = []
    for date_s in date_list:
        ymd = date_s.strftime('%Y%m%d')
        date_s = date_s + relativedelta(days=1)
        path_yaml = os.path.join(g_path_interface, sat_pair, job_id, ymd)
        if os.path.isdir(path_yaml):
            file_list_yaml = pb_io.find_file(path_yaml, '.*.yaml')
            for file_yaml in file_list_yaml:
                cmd = '%s %s %s' % (python, job_exe, file_yaml)
                cmd_list.append(cmd)

    return cmd_list

if __name__ == '__main__':
    #     Log = LogServer('D:/111.log')
    #     Log.info('testinfo')
    m = MailServer_imap('imap.exmail.qq.com', '3566799134')
#     m = MailServer_imap('imap.aliyun.cimap.aliyun.cnom', '123')
#     m = MailServer_imap('imap.kingweather.cn', '123')

    m.connect('FY3gsics@kingtansin.com', 'Kts123')
    # 清理7天之前的所有邮件
    m.findspam()
    if (m.findmail()):
        print '3'
        m.savemail('./3566799134.txt')

    m.close()
    m.logout()
