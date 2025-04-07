
# 配置环境
```bash
# 创建环境
conda create -n geos2camx python=3.11
conda activate geos2camx
# 安装 cdo 和 netcdf4
conda install -c conda-forge cdo  netcdf4

cd geos2camx
```

# 测试
```bash
# 进入运行目录
cd geos2camx

# 1. 直接 csh 运行（需要进入计算节点）
ssh comput27
cd geos2camx
conda activate geos2camx

# 1. 修改 run_geos2camx_test.csh 中处理日期
setenv start_date 20230411
setenv end_date   20230411

# 2. GEOSChem 输出目录
setenv gc_data_dir  /public/home/liangjiarui/ljr/research/phd/test_geos2camx/geos2camx/gcout  # 替换为 GEOSChem 输出文件夹，需要和处理日期保持一致

# 3. 运行
csh run_geos2camx_test.csh
```


# 运行
- 制作 1 年边界场大概需要 1 天的时间，最好 sbatch 分别提交 4 年
```bash
# 1. 直接 csh 运行（需要进入计算节点）
ssh comput27
cd geos2camx
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