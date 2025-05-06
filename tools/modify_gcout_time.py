#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import re
import argparse
import netCDF4 as nc
from datetime import datetime, timedelta

def modify_gcout_time(input_dir, source_start_date, source_end_date, start_date, end_date):
    """
    修改GEOS-Chem输出文件中的time.units属性
    
    参数:
    - input_dir (str): 包含NetCDF文件的目录
    - source_start_date (str): 源文件起始日期，格式为'YYYYMMDD'
    - source_end_date (str): 源文件结束日期，格式为'YYYYMMDD'
    - start_date (str): 目标起始日期，格式为'YYYYMMDD'
    - end_date (str): 目标结束日期，格式为'YYYYMMDD'
    """
    # 文件模式的正则表达式
    stateMet_pattern = re.compile(r'GEOSChem\.StateMet\.(\d{8})_0000z\.nc4')
    speciesConc_pattern = re.compile(r'GEOSChem\.SpeciesConc\.(\d{8})_0000z\.nc4')
    
    # 将日期字符串转换为datetime对象以便进行日期比较
    src_start = datetime.strptime(source_start_date, '%Y%m%d')
    src_end = datetime.strptime(source_end_date, '%Y%m%d')
    tgt_start = datetime.strptime(start_date, '%Y%m%d')
    tgt_end = datetime.strptime(end_date, '%Y%m%d')
    
    # 计算日期偏移量（天数）
    days_offset = (tgt_start - src_start).days
    
    print(f"源日期范围: {source_start_date} 到 {source_end_date}")
    print(f"目标日期范围: {start_date} 到 {end_date}")
    print(f"日期偏移量: {days_offset} 天")
    
    # 遍历输入目录中的所有文件
    modified_count = 0
    for filename in os.listdir(input_dir):
        file_path = os.path.join(input_dir, filename)
        
        # 检查文件是否为我们要处理的两种类型之一
        stateMet_match = stateMet_pattern.match(filename)
        speciesConc_match = speciesConc_pattern.match(filename)
        
        if stateMet_match or speciesConc_match:
            # 提取文件名中的日期
            if stateMet_match:
                file_date = stateMet_match.group(1)
            else:
                file_date = speciesConc_match.group(1)
            
            # 将文件名中的日期转换为datetime对象
            try:
                file_datetime = datetime.strptime(file_date, '%Y%m%d')
            except ValueError:
                print(f"  警告: 文件{filename}中的日期格式无效")
                continue
            
            # 检查文件是否在目标日期范围内（在filename中的日期就是目标日期）
            if file_datetime < tgt_start or file_datetime > tgt_end:
                print(f"  跳过文件 {filename}: 不在目标日期范围内")
                continue
            
            print(f"处理文件: {filename}")
            
            # 计算源日期（根据目标日期和偏移量）
            source_datetime = file_datetime - timedelta(days=days_offset)
            
            # 格式化要写入time.units的目标日期
            target_date_str = file_datetime.strftime('%Y-%m-%d')
            
            print(f"  文件日期: {file_date}")
            print(f"  对应源日期: {source_datetime.strftime('%Y%m%d')}")
            print(f"  目标日期: {target_date_str}")
            
            # 修改时间单位
            try:
                with nc.Dataset(file_path, 'r+') as ncfile:
                    if 'time' in ncfile.variables:
                        # 保存原始单位以便打印
                        original_units = ncfile.variables['time'].units
                        
                        # 构建新的时间单位字符串
                        new_units = f"minutes since {target_date_str} 00:00:00"
                        
                        # 更新时间单位
                        ncfile.variables['time'].units = new_units
                        
                        # 强制写入文件
                        ncfile.sync()
                        
                        print(f"  原时间单位: {original_units}")
                        print(f"  新时间单位: {new_units}")
                        modified_count += 1
                    else:
                        print(f"  警告: 文件{filename}中未找到time变量")
            except Exception as e:
                print(f"  处理文件{filename}时出错: {str(e)}")
    
    print(f"共处理了 {modified_count} 个文件")

def main():
    # 设置命令行参数解析
    parser = argparse.ArgumentParser(description='修改GEOS-Chem输出文件中的时间单位')
    parser.add_argument('--input_dir', '-i', required=True, help='包含NetCDF文件的输入目录')
    parser.add_argument('--source_start_date', '-ss', required=True, help='源文件起始日期，格式为YYYYMMDD')
    parser.add_argument('--source_end_date', '-se', required=True, help='源文件结束日期，格式为YYYYMMDD')
    parser.add_argument('--start_date', '-s', required=True, help='目标起始日期，格式为YYYYMMDD')
    parser.add_argument('--end_date', '-e', required=True, help='目标结束日期，格式为YYYYMMDD')
    
    args = parser.parse_args()
    
    # 调用主函数
    modify_gcout_time(
        args.input_dir,
        args.source_start_date,
        args.source_end_date,
        args.start_date,
        args.end_date
    )

if __name__ == "__main__":
    main()
