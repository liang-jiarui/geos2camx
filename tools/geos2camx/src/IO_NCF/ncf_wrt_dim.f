c**** NCF_WRT_DIM
c
      subroutine ncf_wrt_dim(ierr,action,iounit,numcols,numrows,numlays,nspcs)
      use ncf_iodat
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine writes the dimensions to the NetCDF file
c
c      Argument description:
c       Inputs:
c           ierr    I  error message
c           action  C  name of file to open
c           iounit  I  NetCDF file ID of file
c           numcols I number of cols in this file
c           numrows I number of cols in this file
c           numlays I number of layers in this file
c           nspcs   I  number of species in the file
c       Outputs:
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
      integer       numcols
      integer       numrows
      integer       numlays
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
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ncf_date_time = 2
      ncf_lay = numlays
      ncf_col = numcols
      ncf_row = numrows
      ncf_var = nspcs + 1
c
      ierr = nf_def_dim(iounit, "TSTEP", NF_UNLIMITED, ncf_tstep_dimid )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_dim(iounit, "DATE-TIME", ncf_date_time, ncf_date_time_dimid )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_dim(iounit, "LAY", ncf_lay, ncf_lay_dimid )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_dim(iounit, "VAR", ncf_var, ncf_var_dimid )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_dim(iounit, "ROW", ncf_row, ncf_row_dimid )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_dim(iounit, "COL", ncf_col, ncf_col_dimid )
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
      write(*,'(//,a)') 'ERROR in NCF_WRT_DIM:'
      write(*,'(A)') TRIM(action)
      write(*,'(A)') 'Cannot write dimensions to file.'
      stop
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
 
