#!/bin/csh

#SBATCH -p cpu_part
#SBATCH -N 1
#SBATCH -n 32
#SBATCH -c 1
#SBATCH -t UNLIMITED
#SBATCH --job-name=cp_rename
#SBATCH --output= cp_rename%j.log

#===================================================================
# 所有变量定义
#===================================================================

# 公共目录变量
set gc_data_dir = /home/camx/ljr/geoschem/20xx_prep/code/global/base/merra2_2x25_tropchem/OutputDir
set output_dir = $gc_data_dir

# 任务1：将 20241227 的文件，复制重命名为 20241228-2024
set source_date = 20241227
set start_date = 20241228
set end_date = 20241231

# 任务2：将 20240101-20240430 的文件，复制重命名为 20250101-20250430
set source_start_date = 20240101
set source_end_date = 20240430
set target_start_date = 20250101
set target_end_date = 20250430

set source_dir = /home/camx/ljr/geoschem/20xx_prep/code/global/base/merra2_2x25_tropchem/OutputDir
set target_dir = $source_dir

# 添加Python脚本路径
set python_script = ./tools/modify_gcout_time.py

#===================================================================
# 逻辑实现部分
#===================================================================

echo "开始执行任务..."

# 任务1：将 20241227 的 文件，复制重命名为 20241228-2024
echo "执行任务1：将 ${source_date} 的文件复制重命名为 ${start_date}-${end_date}"

# 确保输出目录存在
mkdir -p $output_dir

# 设置源文件路径
set source_statemet = "${gc_data_dir}/GEOSChem.StateMet.${source_date}_0000z.nc4"
set source_species = "${gc_data_dir}/GEOSChem.SpeciesConc.${source_date}_0000z.nc4"

# 检查源文件是否存在
if (! -e $source_statemet) then
    echo "错误：源文件 $source_statemet 不存在"
    exit 1
endif

if (! -e $source_species) then
    echo "错误：源文件 $source_species 不存在"
    exit 1
endif

# 创建日期序列并复制文件
set current_date = $start_date
while ($current_date <= $end_date)
    # 设置目标文件路径
    set target_statemet = "${output_dir}/GEOSChem.StateMet.${current_date}_0000z.nc4"
    set target_species = "${output_dir}/GEOSChem.SpeciesConc.${current_date}_0000z.nc4"
    
    # 复制文件
    echo "复制 $source_statemet 到 $target_statemet"
    cp $source_statemet $target_statemet
    
    echo "复制 $source_species 到 $target_species"
    cp $source_species $target_species
    
    # 计算下一个日期（简单方式：按天递增）
    set year = `echo $current_date | cut -c1-4`
    set month = `echo $current_date | cut -c5-6`
    set day = `echo $current_date | cut -c7-8`
    
    # 使用 date 命令计算下一天
    set next_date = `date -d "$year-$month-$day + 1 day" +%Y%m%d`
    set current_date = $next_date
end

echo "任务 1 完成：将 ${source_date} 的文件复制重命名为 ${start_date}-${end_date}"

# 调用Python脚本修改任务1的NC文件日期
echo "开始修改任务1的NC文件日期..."
python $python_script --input_dir $output_dir \
    --source_start_date $source_date --source_end_date $source_date \
    --start_date $start_date --end_date $end_date

# 任务2：将 20240101-20240430 的 文件，复制重命名为 20250101-20250430
echo "执行任务2：将 ${source_start_date}-${source_end_date} 的文件复制重命名为 ${target_start_date}-${target_end_date}"

# 确保目标目录存在
mkdir -p $target_dir

# 创建日期映射并复制文件
set source_date = $source_start_date
set target_date = $target_start_date

while ($source_date <= $source_end_date && $target_date <= $target_end_date)
    # 设置源文件和目标文件路径
    set source_statemet = "${source_dir}/GEOSChem.StateMet.${source_date}_0000z.nc4"
    set source_species = "${source_dir}/GEOSChem.SpeciesConc.${source_date}_0000z.nc4"
    
    set target_statemet = "${target_dir}/GEOSChem.StateMet.${target_date}_0000z.nc4"
    set target_species = "${target_dir}/GEOSChem.SpeciesConc.${target_date}_0000z.nc4"
    
    # 检查源文件是否存在
    if (-e $source_statemet) then
        echo "复制 $source_statemet 到 $target_statemet"
        cp $source_statemet $target_statemet
    else
        echo "警告：源文件 $source_statemet 不存在"
    endif
    
    if (-e $source_species) then
        echo "复制 $source_species 到 $target_species"
        cp $source_species $target_species
    else
        echo "警告：源文件 $source_species 不存在"
    endif
    
    # 计算下一个日期
    set source_year = `echo $source_date | cut -c1-4`
    set source_month = `echo $source_date | cut -c5-6`
    set source_day = `echo $source_date | cut -c7-8`
    
    set target_year = `echo $target_date | cut -c1-4`
    set target_month = `echo $target_date | cut -c5-6`
    set target_day = `echo $target_date | cut -c7-8`
    
    # 使用 date 命令计算下一天
    set next_source_date = `date -d "$source_year-$source_month-$source_day + 1 day" +%Y%m%d`
    set next_target_date = `date -d "$target_year-$target_month-$target_day + 1 day" +%Y%m%d`
    
    set source_date = $next_source_date
    set target_date = $next_target_date
end

echo "任务 2 完成：将 ${source_start_date}-${source_end_date} 的文件复制重命名为 ${target_start_date}-${target_end_date}"

# 调用Python脚本修改任务2的NC文件日期
echo "开始修改任务2的NC文件日期..."
python $python_script --input_dir $target_dir \
    --source_start_date $source_start_date --source_end_date $source_end_date \
    --start_date $target_start_date --end_date $target_end_date

echo "所有任务执行完毕"