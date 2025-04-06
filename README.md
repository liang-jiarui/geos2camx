
# 配置环境
```bash
# 创建环境
conda create -n geos2camx python=3.11
conda activate geos2camx
# 安装 cdo 和 netcdf4
conda install -c conda-forge cdo  netcdf4

cd geos2camx
source env.sh
```

# 修改
```bash
# run_geos2camx_test.csh
# 1. 处理时间范围
setenv start_date 20230411
setenv end_date   20230411

# 2. 基础目录
setenv root_dir /public/home/liangjiarui/ljr/research/phd/test_geos2camx/geos2camx_ljr   # 替换为你的目录，比如 setenv root_dir /home/liujiemei/XXX/geos2camx_ljr

# 3. GEOSChem 输出目录
setenv gc_data_dir  /public/home/liangjiarui/ljr/research/phd/test_geos2camx/geos2camx_ljr/gcout  # 替换为 GEOSChem 输出文件夹，需要和处理日期保持一致
```


# 运行
```bash
# 进入运行目录
cd geos2camx_ljr

# 添加环境变量和依赖
source env.sh

# 1. 直接 csh 运行（需要进入计算节点）
ssh comput27
cd geos2camx_ljr
conda activate geos2camx
# csh 运行
csh run_geos2camx_2013.csh
csh run_geos2camx_2017.csh
csh run_geos2camx_2020.csh
csh run_geos2camx_2023.csh

# 2. sbatch 运行
sbatch run_geos2camx_2013.csh
sbatch run_geos2camx_2017.csh
sbatch run_geos2camx_2020.csh
sbatch run_geos2camx_2023.csh
```


# 测试
```bash
# 修改 run_geos2camx_test.csh 中处理日期
# 1. 处理时间范围
setenv start_date 20230101
setenv end_date   20230101


# 运行
csh run_geos2camx_test.csh
```