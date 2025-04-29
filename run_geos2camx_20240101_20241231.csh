#!/bin/csh

#SBATCH -p cpu_part
#SBATCH -N 1
#SBATCH -n 32
#SBATCH -c 1
#SBATCH -t UNLIMITED
#SBATCH --job-name=24_geos2camx
#SBATCH --output=geos2camx_24_%j.log


#==============================================================================
# 配置部分
# 需要修改 
# 1. 处理时间；
# 2. 基础目录；
# 3. GEOS-Chem 输出文件目录（需要包含 SpeciesConc 和 StateMet） 
#==============================================================================
# 1. 处理时间范围
setenv start_date 20240101
setenv end_date   20241231

# 2. GEOSChem 输出目录
setenv gc_data_dir  /home/camx/ljr/geoschem/20xx_prep/code/global/base/merra2_2x25_tropchem/OutputDir  # 替换为 GEOSChem 输出文件夹，需要和处理日期保持一致

# 不需要修改
# 3. 输出目录
setenv YYYY `echo $start_date | cut -c 1-4`   # 提取年份
setenv merge_dir output/gc_merge/$YYYY
setenv bc_dir    output/gc_bc/$YYYY

# 4. 工具目录
# setenv met_data_dir tools/20240412_orig_a/data
setenv met_data_dir tools/beijing_45_9_18/wrfcamx/data
setenv mapping_file tools/geos2camx/Species_Mapping/Mapping_geos2camx_CB6r4Ix.simpleSOA_wDMS.txt
source env.sh


#==============================================================================
# 运行
#==============================================================================
echo "=== 开始 GEOSChem 到 CAMx 边界条件处理 ==="
echo "开始时间: `date`"

# 创建必要的目录
mkdir -p $merge_dir $bc_dir $bc_dir/logs

# 设置可执行文件路径
set EXEC = tools/geos2camx/src/geos2camx
chmod +x $EXEC

# 设置气象文件
set metfile = ${met_data_dir}/camx.3d.d01.20240401.bin
set topofile = ${met_data_dir}/camx.lu.d01.bin

echo "处理日期范围: $start_date 到 $end_date"
echo "注意: 处理每一天需要当天和后一天的 GEOSChem 文件"

# 检查最后一天之后的数据是否存在
set after_end_date = `date -d "$end_date + 1 day" +%Y%m%d`
set end_statemet = "${gc_data_dir}/GEOSChem.StateMet.${after_end_date}_0000z.nc4"
set end_species = "${gc_data_dir}/GEOSChem.SpeciesConc.${after_end_date}_0000z.nc4"

set current_date = $start_date
while ($current_date <= $end_date)
    echo "====================================================="
    echo "处理日期: $current_date"
    echo "====================================================="
    
    set next_date = `date -d "$current_date +1 day" +%Y%m%d`
    set JDATE = `date -d "$current_date" '+%Y%j'`
    
    set input_statemet = "${gc_data_dir}/GEOSChem.StateMet.${current_date}_0000z.nc4"
    set input_species = "${gc_data_dir}/GEOSChem.SpeciesConc.${current_date}_0000z.nc4"
    set next_statemet = "${gc_data_dir}/GEOSChem.StateMet.${next_date}_0000z.nc4"
    set next_species = "${gc_data_dir}/GEOSChem.SpeciesConc.${next_date}_0000z.nc4"
    
    set merged_file = "${merge_dir}/GEOSChem.SpeciesConc.${current_date}.nc"
    set next_merged_file = "${merge_dir}/GEOSChem.SpeciesConc.${next_date}.nc"
    
    if (! -e "$input_statemet" || ! -e "$input_species") then
        echo "错误: 缺失 $current_date 的输入文件，跳过"
        set current_date = $next_date
        continue
    endif
    
    if (! -e "$next_statemet" || ! -e "$next_species") then
        echo "错误: 缺失 $next_date 的输入文件，跳过"
        set current_date = $next_date
        continue
    endif
    
    echo "步骤1a: 合并 ${current_date} 的 StateMet 和 SpeciesConc 文件"
    if (! -e "$merged_file") then
        cdo -sellevidx,1/20 -merge "$input_statemet" "$input_species" "$merged_file"
        if ($status != 0) then
            echo "错误: 合并 $current_date 失败，跳过"
            set current_date = $next_date
            continue
        endif
    else
        echo "$merged_file 已存在，跳过合并步骤..."
    endif
    
    echo "步骤1b: 合并 ${next_date} 的 StateMet 和 SpeciesConc 文件 (用于时区调整)"
    if (! -e "$next_merged_file") then
        cdo -sellevidx,1/20 -merge "$next_statemet" "$next_species" "$next_merged_file"
        if ($status != 0) then
            echo "错误: 合并 $next_date 失败，跳过"
            set current_date = $next_date
            continue
        endif
    else
        echo "$next_merged_file 已存在，跳过合并步骤..."
    endif
    
    echo "步骤2: 修改合并文件的时间单位"
    python tools/modify_time_units.py $merge_dir $merge_dir $current_date $next_date
    if ($status != 0) then
        echo "错误: 修改时间单位失败，跳过"
        set current_date = $next_date
        continue
    endif

    echo "步骤3: 运行 geos2camx 创建 CAMx 边界文件"
    set OUTFILEBC = ${bc_dir}/camx.bc.ljr.$current_date.bin
    set OUTFILEIC = ${bc_dir}/camx.ic.ljr.$current_date.bin
    set OUTFILETC = ${bc_dir}/camx.tc.ljr.$current_date.bin
    set OUTFILESFC = ${bc_dir}/camx.sfc.ljr.$current_date.bin
    
    $EXEC << ieof |& tee ${bc_dir}/logs/geos2camx.$current_date.log
NetCDF output files|.false.
1st GC file name   |$merged_file
2nd GC file name   |$next_merged_file
Spc mapping file   |$mapping_file
CAMx zp/3D file    |$metfile
CAMx topo file     |$topofile
Julian date,YYYYJJJ|$JDATE
OUTFILE for BC     |$OUTFILEBC
OUTFILE for IC     |$OUTFILEIC
OUTFILE for TC     |$OUTFILETC
OUTFILE for SFC    |$OUTFILESFC
ieof

    if ($status != 0) then
        echo "错误: geos2camx 运行失败，跳过"
        set current_date = $next_date
        continue
    endif

    echo "步骤4: 清理中间合并文件"
    if (-e "$OUTFILEBC") then
        echo "CAMx 边界文件创建成功"
        if ($current_date != $end_date) then
            echo "删除合并文件: $merged_file"
            rm -f $merged_file
        endif
    else
        echo "警告: 边界文件未创建，保留中间文件以调试"
    endif

    set current_date = $next_date
    echo "移至下一天: $current_date"
end

if (-e "${merge_dir}/GEOSChem.SpeciesConc.${end_date}.nc") then
    echo "删除最后一天的合并文件: ${merge_dir}/GEOSChem.SpeciesConc.${end_date}.nc"
    rm -f ${merge_dir}/GEOSChem.SpeciesConc.${end_date}.nc
endif

if (-e "${merge_dir}/GEOSChem.SpeciesConc.${after_end_date}.nc") then
    echo "删除结束日期后一天的合并文件: ${merge_dir}/GEOSChem.SpeciesConc.${after_end_date}.nc"
    rm -f ${merge_dir}/GEOSChem.SpeciesConc.${after_end_date}.nc
endif

echo "=== GEOSChem 到 CAMx 边界条件处理完成 ==="
echo "结束时间: `date`"