c**** NCF_WRT_DATA_GRID
c
      subroutine ncf_wrt_data_grid(ierr,action,iounit,
     &         grid_ncol,grid_nrow,grid_xorig,grid_yorig,grid_deltax,
     &                   grid_deltay,nlays,grid_lat,grid_lon,grid_topo)
      implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine writes the variable data for the grid definiton 
c    variables to the NetCDF file
c
c      Argument description:
c       Inputs:
c           action      C name of file to open
c           iounit      I NetCDF file ID of file
c           grid_ncol   I number of grid cells in X direction
c           grid_nrow   I number of grid cells in Y direction
c           grid_xorig  R X coordinate of grid origin      
c           grid_yorig  R Y coordinate of grid origin      
c           grid_dxcell R cell width in X direction
c           grid_dycell R cell width in Y direction
c           nlays       I number of layers in this file
c           grid_lat    R latitude value of cell center
c           grid_lon    R longitude value of cell center
c           grid_topo   R height above sea level of cell center
c       Outputs:
c           ierr        I error code
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer       ierr
      character*(*) action
      integer       iounit
      integer       grid_ncol
      integer       grid_nrow
      real          grid_xorig
      real          grid_yorig
      real          grid_deltax
      real          grid_deltay
      integer       nlays
      real          grid_lat(grid_ncol,grid_nrow)
      real          grid_lon(grid_ncol,grid_nrow)
      real          grid_topo(grid_ncol,grid_nrow)
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*200 this_var
      real*8,       allocatable, dimension(:)   :: darray_1d
      real*8,       allocatable, dimension(:,:) :: darray_2d
      integer,      allocatable, dimension(:)   :: iarray_1d
      integer       this_varid, icol, irow, ilay
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- variable for X coordinates ---
c
      this_var = "X"
      allocate( darray_1d(grid_ncol) )
      do icol=1,grid_ncol
        darray_1d(icol) = DBLE((grid_xorig + 
     &                           (REAL(icol)-0.5)*grid_deltax)/1000.)
      enddo
      ierr = nf_inq_varid(iounit,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_double(iounit,this_varid,darray_1d)
      if( ierr .NE. NF_NOERR ) goto 7001
      deallocate( darray_1d )
c
c  --- variable for Y coordinates ---
c
      this_var = "Y"
      allocate( darray_1d(grid_nrow) )
      do irow=1,grid_nrow
        darray_1d(irow) = DBLE((grid_yorig + 
     &                           (REAL(irow)-0.5)*grid_deltay)/1000.)
      enddo
      ierr = nf_inq_varid(iounit,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_double(iounit,this_varid,darray_1d)
      if( ierr .NE. NF_NOERR ) goto 7001
      deallocate( darray_1d )
c
c  --- variable for Layer heights ---
c
      this_var = "layer"
      allocate( iarray_1d(nlays) )
      do ilay=1,nlays
        iarray_1d(ilay) = ilay
      enddo
      ierr = nf_inq_varid(iounit,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_int(iounit,this_varid,iarray_1d)
      if( ierr .NE. NF_NOERR ) goto 7001
      deallocate( iarray_1d )
c
c  --- variable for longitude coordinates ---
c
      allocate( darray_2d(grid_ncol,grid_nrow) )
c
      this_var = "longitude"
      do icol=1,grid_ncol
        do irow=1,grid_nrow
           darray_2d(icol,irow) = DBLE(grid_lon(icol,irow))
        enddo
      enddo
      ierr = nf_inq_varid(iounit,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_double(iounit,this_varid,darray_2d)
      if( ierr .NE. NF_NOERR ) goto 7001
c
c  --- variable for latitude coordinates ---
c
      this_var = "latitude"
      do icol=1,grid_ncol
        do irow=1,grid_nrow
           darray_2d(icol,irow) = DBLE(grid_lat(icol,irow))
        enddo
      enddo
      ierr = nf_inq_varid(iounit,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_double(iounit,this_varid,darray_2d)
      if( ierr .NE. NF_NOERR ) goto 7001
c
c  --- variable for topo ---
c
      this_var = "topo"
      ierr = nf_inq_varid(iounit,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_real(iounit,this_varid,grid_topo)
      if( ierr .NE. NF_NOERR ) goto 7001
      deallocate( darray_2d )
c
      ierr = 1
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_GRID:'
      write(*,'(A)') TRIM(action)
      write(*,'(2A)') 'Cannot find variable id for: ',TRIM(this_var)
      stop
c
 7001 continue
      write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_GRID:'
      write(*,'(A)') TRIM(action)
      write(*,'(2A)') 'Cannot write data for the variable: ',TRIM(this_var)
      stop
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
 
