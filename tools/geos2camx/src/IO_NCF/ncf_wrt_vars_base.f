c**** NCF_WRT_VARS_BASE
c
      subroutine ncf_wrt_vars_base(ierr,action,iounit)
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine writes the variable definitions and descriptions to 
c    the NetCDF file
c
c      Argument description:
c       Inputs:
c           action  C  name of file to open
c           iounit  I  NetCDF file ID of file
c       Outputs:
c           ierr    I  error code
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'ncf_iodat.inc'
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer       ierr
      character*(*) action
      integer       iounit
      integer       nspcs
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer grid_dimid(2), z_dimid(4), time_dimid(3), this_varid
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      grid_dimid(1) = ncf_col_dimid
      grid_dimid(2) = ncf_row_dimid
c
      z_dimid(1) = ncf_col_dimid
      z_dimid(2) = ncf_row_dimid
      z_dimid(3) = ncf_lay_dimid
      z_dimid(4) = ncf_tstep_dimid
c
      time_dimid(1) = ncf_date_time_dimid
      time_dimid(2) = ncf_var_dimid
      time_dimid(3) = ncf_tstep_dimid
c
      ierr = nf_def_var(iounit, "X", NF_DOUBLE, 1, 
     &                                      ncf_col_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                         LEN_TRIM(ncf_x_units),TRIM(ncf_x_units) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &                 LEN_TRIM(ncf_x_long_name),TRIM(ncf_x_long_name) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                   LEN_TRIM(ncf_x_var_desc),TRIM(ncf_x_var_desc) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "Y", NF_DOUBLE, 1, 
     &                                      ncf_row_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                        LEN_TRIM(ncf_y_units),TRIM(ncf_y_units) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &                LEN_TRIM(ncf_y_long_name),TRIM(ncf_y_long_name) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                  LEN_TRIM(ncf_y_var_desc),TRIM(ncf_y_var_desc) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "layer", NF_DOUBLE, 1,
     &                                ncf_lay_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &               LEN_TRIM(ncf_layer_units),TRIM(ncf_layer_units) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &        LEN_TRIM(ncf_layer_long_name),TRIM(ncf_layer_long_name) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &          LEN_TRIM(ncf_layer_var_desc),TRIM(ncf_layer_var_desc) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "TFLAG", NF_INT, 3, 
     &                                  time_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &              LEN_TRIM(ncf_tflag_units),TRIM(ncf_tflag_units) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &      LEN_TRIM(ncf_tflag_long_name),TRIM(ncf_tflag_long_name) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &       LEN_TRIM(ncf_tflag_var_desc),TRIM(ncf_tflag_var_desc) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "ETFLAG", NF_INT, 3, 
     &                                 time_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &             LEN_TRIM(ncf_etflag_units),TRIM(ncf_etflag_units) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &     LEN_TRIM(ncf_etflag_long_name),TRIM(ncf_etflag_long_name) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &      LEN_TRIM(ncf_etflag_var_desc),TRIM(ncf_etflag_var_desc) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "longitude", NF_DOUBLE, 2, 
     &                              grid_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &      LEN_TRIM(ncf_longitude_units),TRIM(ncf_longitude_units) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &                    LEN_TRIM(ncf_longitude_long_name),
     &                                 TRIM(ncf_longitude_long_name) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                     LEN_TRIM(ncf_longitude_var_desc),
     &                                 TRIM(ncf_longitude_var_desc) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'coordinates',
     &                     LEN_TRIM(ncf_longitude_coordinates),
     &                              TRIM(ncf_longitude_coordinates) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "latitude", NF_DOUBLE, 2, 
     &                                grid_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &        LEN_TRIM(ncf_latitude_units),TRIM(ncf_latitude_units) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &                     LEN_TRIM(ncf_latitude_long_name),
     &                                TRIM(ncf_latitude_long_name) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                     LEN_TRIM(ncf_latitude_var_desc),
     &                                 TRIM(ncf_latitude_var_desc) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'coordinates',
     &                     LEN_TRIM(ncf_latitude_coordinates),
     &                             TRIM(ncf_latitude_coordinates) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "topo", NF_FLOAT, 2,
     &                                grid_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &             LEN_TRIM(ncf_topo_units),TRIM(ncf_topo_units) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &          LEN_TRIM(ncf_topo_long_name),TRIM(ncf_topo_long_name) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &             LEN_TRIM(ncf_topo_var_desc),TRIM(ncf_topo_var_desc) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'coordinates',
     &       LEN_TRIM(ncf_topo_coordinates),TRIM(ncf_topo_coordinates) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "z", NF_FLOAT, 4,
     &                                         z_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                      LEN_TRIM(ncf_z_units),TRIM(ncf_z_units) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &              LEN_TRIM(ncf_z_long_name),TRIM(ncf_z_long_name) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                LEN_TRIM(ncf_z_var_desc),TRIM(ncf_z_var_desc) )
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'coordinates',
     &          LEN_TRIM(ncf_z_coordinates),TRIM(ncf_z_coordinates) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = 1
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(//,a)') 'ERROR in NCF_WRT_VARS_BASE:'
      write(*,'(A)') TRIM(action)
      write(*,'(A)') 'Cannot create file variable for grid variables.'
      stop
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
 
