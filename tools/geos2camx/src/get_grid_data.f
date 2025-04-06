C**** GET_GRID_DATA
c
      subroutine get_grid_data(lnetcdf,iounit,fname,var_name_bin,
     &                         var_name_ncf,action,ncols,nrows,nlays,
     &                         nhours,nmetvars,grid_array)
      implicit none
c
c-----------------------------------------------------------------------
c
c     This routine reads the options and filenames for the GES2CANx 
c     program from the job script:
c
c   Arguments:
c     Inputs:
c        lnetcdf        L  .TRUE. if writing NetCDF output files
c        iounit         I  file unit of file to write
c        fname          C  name of file to write
c        var_name_bin   C  name of variable to read in FORTRAN binary file
c        var_name_ncf   C  name of variable to read in NetCDF file
c        action         C  decription of what is happening now
c        ncols          I  number of columns in the grid
c        nrows          I  number of rows in the grid
c        nlays          I  number of layers in the grid
c        nhours         I  number of hours to read
c        nmetvars       I  number of variables in file
c        grid_array     R  array to store data
c     Outputs:
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      logical       lnetcdf
      integer       iounit
      character*256 fname
      character*10  var_name_bin
      character*10  var_name_ncf
      character*256 action
      integer       ncols
      integer       nrows
      integer       nlays
      integer       nhours
      integer       nmetvars
      real          grid_array(ncols,nrows,nlays,nhours)
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      character*10  var_name, this_var
      character*4   iname(10)
      integer       this_varid, data_start(4), data_count(4)
      integer       this_hour, ivar, idum, i, j, k
      integer       ierr
      logical       lfound
      real          rdum
c
      real, allocatable :: tmp_grid(:,:,:)
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c  ---- allocate local array ---
c
      allocate( tmp_grid(ncols,nrows,nlays) ) 
c
c  --- loop over hours to read ---
c
      do this_hour=1,nhours
         if( lnetcdf ) then
c
            data_start(1) = 1
            data_count(1) = ncols
            data_start(2) = 1
            data_count(2) = nrows
            data_start(3) = 1
            data_count(3) = nlays
            data_start(4) = this_hour
            data_count(4) = 1
            var_name = var_name_ncf
            ierr = nf_inq_varid(iounit,trim(var_name),this_varid)
            if( ierr .NE. NF_NOERR) goto 7000
            ierr = nf_get_vara_real(iounit,this_varid,data_start,
     &                                            data_count,tmp_grid)
            if( ierr .NE. NF_NOERR) goto 7001
         else
            var_name = var_name_bin
            lfound = .FALSE.
            read(iounit,ERR=7001) 
            do ivar=1,nmetvars
               if( .NOT. lfound ) then
                  do k=1,nlays
                     read(iounit,ERR=7001) idum,(iname(i),i=1,10),
     &                     ((tmp_grid(i,j,k),i=1,ncols),j=1,nrows)
                  enddo
                  write(this_var,'(10A1)') (iname(i),i=1,10)
                  if( TRIM(this_var) .EQ. TRIM(var_name) ) lfound = .TRUE.
               else
                  do k=1,nlays
                     read(iounit,ERR=7001)
                  enddo
               endif
            enddo
            if( .NOT. lfound ) goto 7000
         endif
c
c  --- load data into output array for this hour ----
c
        do i=1,ncols
           do j=1,nrows
              do k=1,nlays
                 grid_array(i,j,k,this_hour) = tmp_grid(i,j,k)
              enddo
           enddo
        enddo
c
c  --- next hour ---
c
      enddo
c
c  ---- free up space ---
c
      deallocate( tmp_grid )
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(/,A)') 'ERROR in GET_GRID_DATA.'
      write(*,'(A)') TRIM(action)
      write(*,'(2A)') 'Cannot find variable in file: ',TRIM(var_name)
      write(*,'(2A,//)') 'File: ',TRIM(fname)
      call flush(6)
      stop
c
 7001 continue
      write(*,'(/,A)') 'ERROR in GET_GRID_DATA.'
      write(*,'(A)') TRIM(action)
      write(*,'(2A)') 'Cannot read gridded data for variable: ',
     &                                               TRIM(var_name)
      write(*,'(2A,//)') 'File: ',TRIM(fname)
      call flush(6)
      stop
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
