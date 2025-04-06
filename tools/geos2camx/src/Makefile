.SUFFIXES: .c .f .o .f90 .F90
#
#-------------------------------------------------------------------
#     with CHUNK set the output files will be in compressed format
#-------------------------------------------------------------------
#
CHUNK = -DCHUNK
###CHUNK = 

MOD_DIR = .
NCF_INST = /public23/soft/netcdf/4.4.1-icc17


FC = /public23/home/sc61466/zjr/tools/PGI.Workstation.Complete.v10.6.LINUX.x64/linux86-64/10.6/bin/pgf90
FLGS =  -O2 -byteswapio -Mextend -Bstatic_pgi -I./Includes -I$(NCF_INST)/include
MODULES=-I${MOD_DIR} -module ${MOD_DIR}

###FC = ifort
###FLGS =  -O2 -convert big_endian -extend_source -Bstatic -I./Includes -I$(NCF_INST)/include
###MODULES=-I${MOD_DIR} -module ${MOD_DIR}

###FC = gfortran
###FLGS =  -O2 -fconvert=big-endian -frecord-marker=4 -ffixed-line-length-0 -I./Includes -I$(NCF_INST)/include
###MODULES=-I${MOD_DIR} -J${MOD_DIR}

LIBS =  -L$(NCF_INST)/lib -lnetcdff -lnetcdf
#
#-------------------------------------------------------------------
#     some builds of NetCDF will need these libararies instead
#-------------------------------------------------------------------
#
###LIBS =  -L$(NCF_INST)/lib -lnetcdff -lnetcdf \
###-L/usr/local/hdf5/lib/ -lhdf5_hl -lhdf5 -lhdf5hl_fortran -lhdf5_fortran \
###-L/usr/local/zlib/lib -lz

MOD_OBJ = \
 IO_NCF/ncf_iodat.o

OBJCTS = \
 addtime.o \
 addday.o \
 airqnt.o \
 geos2camx.o \
 get_grid_data.o \
 jstlft.o \
 juldate.o \
 caldate.o \
 lat_lon.o \
 lcpgeo.o \
 map_chem.o \
 pspgeo.o \
 rdopts.o \
 rd_outfnames.o \
 read_chem.o \
 utmgeo.o \
 wrt_conc_data.o \
 wrt_file_hdr.o \
 wrt_spec_table.o \
 IO_NCF/ncf_createfile.o \
 IO_NCF/ncf_get_tstep.o \
 IO_NCF/ncf_set_compress_flag.o \
 IO_NCF/ncf_set_vars_base.o \
 IO_NCF/ncf_set_tstep.o \
 IO_NCF/ncf_set_specatt.o \
 IO_NCF/ncf_set_global.o \
 IO_NCF/ncf_wrt_dim.o \
 IO_NCF/ncf_wrt_global.o \
 IO_NCF/ncf_wrt_vars_base.o \
 IO_NCF/ncf_wrt_vars_species.o \
 IO_NCF/ncf_wrt_data_grid.o \
 IO_NCF/ncf_wrt_data_tstep.o \
 IO_NCF/ncf_wrt_data_species_bc.o \
 IO_NCF/ncf_wrt_data_species_ic.o \
 IO_NCF/ncf_wrt_data_species_tc.o \
 IO_NCF/getime.o


TARGT = geos2camx

$(TARGT): $(MOD_OBJ) $(OBJCTS)
	$(FC) -o $(TARGT) $(FLGS) $(MOD_OBJ) $(OBJCTS) $(LIBS)
.f.o	:
	$(FC) -c -o $@ $(FLGS) $(MODULES) $<
.F90.o  :
	$(FC) -c -o $@ $(CHUNK) $(FLGS) $(MODULES) $<
.f90.o  :
	$(FC) -c -o $@ $(FLGS) $(MODULES) $<
.F.o  :
	$(FC) -c -o $@ $(FLGS) $(MODULES) $<

clean:
	rm -rvf $(MOD_OBJ) $(OBJCTS) $(MOD_DIR)/*.mod $(TARGT) 
