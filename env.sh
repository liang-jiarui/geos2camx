#!/bin/bash

module load apps/anaconda3/2021.05
module load compiler/intel-compiler/
source /public/software/profile.d/compiler_intel-compiler-2021.3.0.sh
source /public/software/profile.d/mpi_intelmpi-2021.3.0.sh
module load mpi/intelmpi/2021.3.0


export NCARG_ROOT=./tools/ncl_662
export NETCDF=./tools/netcdf_4.4.1-icc17
export libpng=./tools/libpng_1.6.37
export pnetcdf=./tools/pnetcdf_1.11.1
export hdf5=./tools/hdf5_1.8.13
export jasper=./tools/jasper_1.9
export szip=./tools/szip_2.1.1
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export WRF_EM_CORE=1
export PYTHONUNBUFFERED=1


export JASPERLIB=./tools/jasper_1.9/lib
export JASPERINC=./tools/jasper_1.9/include

export PATH=$NCARG_ROOT/bin:$NETCDF/bin:$libpng/bin:$pnetcdf/bin:$hdf5/bin:$jasper/bin:$PATH
export LD_LIBRARY_PATH=$NCARG_ROOT/lib:$NETCDF/lib:$libpng/lib:$pnetcdf/lib:$hdf5/lib:$jasper/lib:$szip/lib:$LD_LIBRARY_PATH


# echo $LD_LIBRARY_PATH | grep ljr
