# -*- coding: utf-8 -*-

from configobj import ConfigObj
from qiae_lib.pb_csc_console import SocketServer
from qiae_lib.pb_io import write_yaml_file, str_format
from qiae_lib.pb_csc_crontrol import *
import glob, re
import warnings


def get_job_id_func(job_id):
    """
    u 返回jobid对应的函数名称 ，jobid唯一性
    :return:
    """
    job_id_func = {
        "job_0110": job_0110,
        "job_0210": job_0210,
    }
    return job_id_func.get(job_id)


def main():
    # 获取必要的三个参数(卫星对，作业编号，日期范围 , 端口, 线程, 数据补侠士每次订购天数)
    job_name, job_step, job_time, job_cfg, job_port, threads, histdays = get_args()
#     job_name = 'FY3B+VIRR'  # 测试使用的临时参数
#     job_step = '0110'
#     job_time = '20160104-20160104'
#     job_cfg = 'cfg/sst.cfg'
#     job_port = 0
#     threads = 1
    # 端口大于0 就开启
    if job_port > 0:
        sserver = SocketServer()
        if sserver.createSocket(job_port) == False:
            sserver.closeSocket(job_port)
            sys.exit(-1)

    # 读取模块配置文件内容
    job_cfg = ConfigObj(job_cfg)
    # 覆盖接口文件标记
    run_jobs = job_cfg['CROND']['run_jobs'].lower()
    run_mode = job_cfg['CROND']['run_mode']
    interface = job_cfg['PATH']['OUT']['interface']

    # 1 获取卫星对清单
    job_name_list = get_job_name_list(job_name, job_cfg)
    # 2 获取作业流清单
    job_step_list = get_job_step_list(job_name_list, job_step, job_cfg)
    # 3 获取日期的清单
    job_time_list = get_job_time_list(job_name_list, job_time, job_cfg)

    #  开始根据卫星对处理作业流
    for job_name in job_name_list:  # 卫星对
        for job_id in job_step_list[job_name]:  # 作业编号
            process_name = job_cfg['BAND_JOB_MODE'][job_id]  # 作业进程
            for date_s, date_e in job_time_list[job_name]:  # 处理时间
                get_job_id_func(job_id)(job_name, job_id, date_s, date_e, job_cfg, threads)

                # 开始获取执行指令信息
                if 'on' in run_jobs:
                    cmd_list = get_cmd_list(
                        process_name, job_name, job_id, date_s, date_e, interface)
                    if 'onenode' in run_mode:
                        print ("2222222222", date_s, date_e,job_name, job_id)
                        aaa=run_command(cmd_list, threads)
                        print ("2222222222end", aaa)
                    elif 'cluster' in run_mode:
                        run_command_parallel(cmd_list)
                    else:
                        print ('error: parallel_mode args input onenode or cluster')
                        sys.exit(-1)

# 以上部分完全可复用，在不同不摸直接复制即可


def job_0110(job_name, job_id, date_s, date_e, job_cfg, threads):
    date1 = datetime.strptime(date_s.strftime('%Y%m%d'), '%Y%m%d')
    date2 = datetime.strptime(date_e.strftime('%Y%m%d'), '%Y%m%d')
#     pool = Pool(processes = int(threads))
    while date1 <= date2:
        ymd = date1.strftime('%Y%m%d')
        #         pool.apply_async(create_job_0110, (job_name, job_id, ymd, job_cfg))
        create_job_0110(job_name, job_id, ymd, job_cfg)
        date1 = date1 + relativedelta(days = 1)
#     pool.close()
#     pool.join()


def job_0210(job_name, job_id, date_s, date_e, job_cfg, threads):
    date1 = datetime.strptime(date_s.strftime('%Y%m%d'), '%Y%m%d')
    date2 = datetime.strptime(date_e.strftime('%Y%m%d'), '%Y%m%d')
#     pool = Pool(processes = int(threads))
    while date1 <= date2:
        ymd = date1.strftime('%Y%m%d')
        #         pool.apply_async(create_job_0110, (job_name, job_id, ymd, job_cfg))
        create_job_0210(job_name, job_id, ymd, job_cfg)
        date1 = date1 + relativedelta(days = 1)
#     pool.close()
#     pool.join()


def create_job_0210(job_name, job_id, ymd, job_cfg):
    """
    气溶胶日合成代码
    """

    # 输入信息
    ipath_granule = job_cfg['PATH']['MID']['granule']

    # 输出信息
    opath_daily = job_cfg['PATH']['MID']['daily']
    opath_yaml = job_cfg['PATH']['OUT']['interface']

    # 实例
    yy = ymd[0:4]
    mm = ymd[4:6]
    dd = ymd[6:8]
    ipath_granule = str_format(ipath_granule, {'JOBNAME': job_name, 'YYYY': yy, 'MM': mm, 'DD': dd})
    opath_daily = str_format(opath_daily, {'JOBNAME': job_name, 'YYYY': yy, 'MM': mm, 'DD': dd})
    opath_yaml = str_format(opath_yaml, {'JOBNAME': job_name, 'YYYY': yy, 'MM': mm, 'DD': dd})
    opath_yaml = os.path.join(opath_yaml, job_id, ymd)
    # 接口文件输出目录创建
    if not os.path.isdir(opath_yaml):
        os.makedirs(opath_yaml)
    file_list = glob.glob('%s/*%s*.HDF5' % (ipath_granule, ymd))
    file_list.sort()
    if len(file_list) > 0:
        # 输出接口文件
        yaml_dict = {'INFO': {'job_name': job_name, 'ymd': ymd},
                     'PATH': {'ipath': file_list, 'opath': opath_daily}}
        full_opath_yaml = os.path.join(opath_yaml, '%s.yaml' % (ymd))
        write_yaml_file(yaml_dict, full_opath_yaml)
        print('%s %s create yaml interface success' % (job_name, ymd))


def create_job_0110(job_name, job_id, ymd, job_cfg):
    # 调度文件中的路径信息

    ipath_01 = job_cfg['PAIRS'][job_name]['ipath_01']
    ipath_02 = job_cfg['PAIRS'][job_name]['ipath_02']
    ipath_03 = job_cfg['PAIRS'][job_name]['ipath_03']

    opath_granule = job_cfg['PATH']['MID']['granule']
    opath_yaml = job_cfg['PATH']['OUT']['interface']
#     ipath_01 = str_format(ipath_01, {'JOBNAME': job_name, 'YYYYMMDD': ymd})

    yy = ymd[0:4]
    mm = ymd[4:6]
    dd = ymd[6:8]
    ipath_01 = str_format(ipath_01, {'YYYY': yy, 'MM': mm, 'DD': dd})
    ipath_02 = str_format(ipath_02, {'YYYY': yy, 'MM': mm, 'DD': dd})
    ipath_03 = str_format(ipath_03, {'YYYY': yy, 'MM': mm, 'DD': dd})

    opath_granule = str_format(opath_granule, {'JOBNAME': job_name, 'YYYY': yy, 'MM': mm, 'DD': dd})
    opath_yaml = str_format(opath_yaml, {'JOBNAME': job_name, 'YYYY': yy, 'MM': mm, 'DD': dd})
    opath_yaml = os.path.join(opath_yaml, job_id, ymd)

    # 接口文件输出目录创建
    if not os.path.isdir(opath_yaml):
        os.makedirs(opath_yaml)
    print (ipath_01)
    flist_l1b = glob.glob('%s/*%s*.HDF' % (ipath_01, ymd))
    flist_geo = glob.glob('%s/*%s*.HDF' % (ipath_02, ymd))
    flist_clm = glob.glob('%s/*%s*.HDF' % (ipath_03, ymd))
    print('---', len(flist_l1b))
    print('---', len(flist_geo))
    print('---', len(flist_clm), ipath_03)

    # 把L1数据放入字典
    reg = '.*(\d{8})_(\d{4}).*.HDF'
    flist_l1b_dict = dict()
    for each in flist_l1b:
        file_name = os.path.basename(each)
        mreg = re.match(reg, file_name)
        if mreg:
            key_wd = '{}_{}'.format(mreg.group(1), mreg.group(2))
            flist_l1b_dict[key_wd] = each

    # 把GEO数据放入字典
    reg = '.*(\d{8})_(\d{4}).*.HDF'
    flist_geo_dict = dict()
    for each in flist_geo:
        file_name = os.path.basename(each)
        mreg = re.match(reg, file_name)
        if mreg:
            key_wd = '{}_{}'.format(mreg.group(1), mreg.group(2))
            flist_geo_dict[key_wd] = each

    # 把云检测数据放入字典
    reg = '.*(\d{8})_(\d{4}).*.HDF'
    flist_clm_dict = dict()
    for each in flist_clm:
        file_name = os.path.basename(each)
        mreg = re.match(reg, file_name)
        if mreg:
            key_wd = '{}_{}'.format(mreg.group(1), mreg.group(2))
            flist_clm_dict[key_wd] = each
    # 交集

    if 'FY3D' in job_name:
        key_wd_cross = flist_l1b_dict.keys() & flist_geo_dict.keys() & flist_clm_dict.keys()
    else:
        key_wd_cross = flist_l1b_dict.keys() & flist_clm_dict.keys()
    for key in sorted(key_wd_cross):

        ifile_l1b = flist_l1b_dict.get(key)
        ifile_geo = flist_geo_dict.get(key)
        ifile_clm = flist_clm_dict.get(key)

        ymd = key.split('_')[0]
        hms = key.split('_')[1] + '00'
        # 输出接口文件
        yaml_dict = {'INFO': {'job_name': job_name, 'ymd': ymd, 'hms':hms},
                 'PATH': {'ipath_l1b': ifile_l1b, 'ipath_geo': ifile_geo,
                          'ipath_clm': ifile_clm, 'opath': opath_granule}
                 }
        full_opath_yaml = os.path.join(opath_yaml, '%s_%s.yaml' % (ymd, hms))
        write_yaml_file(yaml_dict, full_opath_yaml)
        print('%s %s %s create yaml interface success' % (job_name, ymd, hms))


if __name__ == '__main__':
    warnings.filterwarnings("ignore")
    # 运行命令如下
    main()
