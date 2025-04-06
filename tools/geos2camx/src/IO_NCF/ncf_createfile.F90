!**** NCF_CREATEFILE
!
      subroutine ncf_createfile(ierr,fname,action,iounit)
      use ncf_iodat
!
!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!   This routine opens a NetCDF file.
!
!      Argument description:
!       Inputs:
!           ierr   I  error code
!           fname  C  name of file to open
!           action C  description of exactly what this call is doing
!       Outputs:
!           iounit I  NetCDF file ID of opened file
!
!-----------------------------------------------------------------------
!    LOG:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Include files:
!-----------------------------------------------------------------------
!
      include 'netcdf.inc'
!
!-----------------------------------------------------------------------
!    Argument declarations:
!-----------------------------------------------------------------------
!
      integer       ierr
      character*(*) fname
      character*(*) action
      integer       iounit
!
!-----------------------------------------------------------------------
!    External functions:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Local variables:
!-----------------------------------------------------------------------
!
      integer ncf_format
!
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------
!
      ncf_format = NF_FORMAT_CLASSIC
      if( ncf_compress ) ncf_format = OR(NF_NETCDF4,NF_FORMAT_CLASSIC)
      ierr = nf_create(fname, ncf_format, iounit)      
      if( ierr .NE. nf_noerr ) then
         write(*,'(//,a)') 'ERROR in NCF_CREATEFILE:'
         write(*,'(A)') TRIM(action)
         write(*,'(2A)') 'Could not open file: ', TRIM(fname)
         stop
      endif
!
#ifdef CHUNK
      if( ncf_compress ) then
         ierr = nf_set_chunk_cache(NCF_CACHESIZE, NCF_NELEMS, NCF_PREEMPTION)
         if( ierr .NE. nf_noerr ) then
            write(*,'(//,a)') 'ERROR in NCF_CREATEFILE:'
            write(*,'(A)') TRIM(action)
            write(*,'(2A)') 'Could not set chunk parameters for: ', &
                                          TRIM(fname)
            stop
         endif
      endif
#endif
!
      ierr = 1
      goto 9999
!
!-----------------------------------------------------------------------
!    Return point:
!-----------------------------------------------------------------------
!
 9999 continue
      return
      end
