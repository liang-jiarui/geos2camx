# 配置环境
```bash
# 创建环境
conda create -n geos2camx python=3.11
conda activate geos2camx
# 安装 cdo 和 netcdf4
conda install -c conda-forge cdo netcdf4

cd geos2camx
```

# 运行 2024-2025 边界场处理流程

## 第一步：修改 GEOSChem 输出路径

1. 打开 `step1_copy_rename.csh` 脚本，修改以下变量：
   ```bash
   set gc_data_dir = /home/camx/ljr/geoschem/20xx_prep/code/global/base/merra2_2x25_tropchem/OutputDir
   set output_dir 
   ```
   将路径改为您的 GEOSChem 输出目录路径。

2. 同样修改 `step2_geos2camx_2024.csh` 和 `step3_geos2camx_2025.csh` 中的相同路径：
   ```bash
   setenv gc_data_dir  /home/camx/ljr/geoschem/20xx_prep/code/global/base/merra2_2x25_tropchem/OutputDir
   set output_dir 
   ```

## 第二步：运行处理脚本

按照以下顺序依次运行三个脚本：

1. 首先运行文件复制和重命名脚本：
   ```bash
   ./step1_copy_rename.csh
   # 或者提交作业
   sbatch step1_copy_rename.csh
   ```
   这将把原始日期的文件复制并重命名为目标日期。

2. 然后运行2024年的GEOS到CAMx转换脚本：
   ```bash
   ./step2_geos2camx_2024.csh
   # 或者提交作业
   sbatch step2_geos2camx_2024.csh
   ```
   这将处理2024年的数据并生成CAMx边界条件文件。

3. 最后运行2025年的GEOS到CAMx转换脚本：
   ```bash
   ./step3_geos2camx_2025.csh
   # 或者提交作业
   sbatch step3_geos2camx_2025.csh
   ```
   这将处理2025年的数据并生成CAMx边界条件文件。

## 注意事项

- 确保所有脚本都有执行权限，可以使用 `chmod +x *.csh` 添加
- 确保 Python 脚本 `tools/modify_gcout_time.py` 存在且可执行
- 输出文件将保存在 `output/gc_bc/YYYY` 目录下

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


# 运行其他年份
- 制作 1 年边界场大概需要 1 天的时间，最好 sbatch 分别提交各年份
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