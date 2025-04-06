c**** NCF_WRT_DATA_SPECIES_IC
c
      subroutine ncf_wrt_data_species_ic(ierr,action,iounit,this_hour,
     &             num_cols,num_rows,num_lays_in,num_lays_out,nspcs,
     &                                       spec_name,height,concic)
      implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine writes the data for the timestep variables to the
c    NetCDF file
c
c      Argument description:
c       Inputs:
c           action       C name of file to open
c           iounit       I NetCDF file ID of file
c           this_hour    I index of current hour
c           num_cols     I number of columns in this grid
c           num_rows     I number of rows in this grid
c           num_lays_in  I number of layers
c           num_lays_out I number of layers
c           nspcs        I number of species in the file
c           height       R gridded array of layer heights
c           concic       R grdded array of concentrations
c       Outputs:
c           ierr         I error code
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
      character*(*) action
      integer       iounit
      integer       this_hour
      integer       num_cols
      integer       num_rows
      integer       num_lays_in
      integer       num_lays_out
      integer       nspcs
      character*(*) spec_name(nspcs)
      real          height(num_cols,num_rows,num_lays_in)
      real          concic(num_cols,num_rows,num_lays_in,nspcs)
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*14 this_var
      integer      data_start(4), data_count(4), ispc, i, j, k
      integer      ierr, this_varid
      real,        allocatable, dimension(:,:,:) :: array_3d
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- set the position in the NetCDF variable to write ---
c
      data_start(1) = 1
      data_count(1) = num_cols
      data_start(2) = 1
      data_count(2) = num_rows
      data_start(3) = 1
      data_count(3) = num_lays_out
      data_start(4) = this_hour
      data_count(4) = 1
c
c  --- get the id for the z variable and write the data ---
c
      this_var = "z"
      ierr = nf_inq_varid(iounit,"z",this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_vara_real(iounit,this_varid,data_start,data_count,height)
      if( ierr .NE. NF_NOERR ) goto 7001
c
c  --- allocate the array that will be used to write the data ---
c
      allocate( array_3d(num_cols,num_rows,num_lays_out) )
c
c  --- loop over all variables in this file ---
c
      do ispc=1,nspcs
c
c  --- get name for this variable, and get it's variable ID
c      in this file ---
c
         this_var = spec_name(ispc)
         call jstlft(this_var)
         ierr = nf_inq_varid(iounit,TRIM(this_var),this_varid)
         if( ierr .NE. NF_NOERR ) goto 7000
c
c  --- load the data into the local array to write ---
c
         do k=1,num_lays_out
            do j=1,num_rows
              do i=1,num_cols
                array_3d(i,j,k) = concic(i,j,k,ispc)
              enddo
            enddo
         enddo
c
         ierr = nf_put_vara_real(iounit,this_varid,data_start,data_count,array_3d)
         if( ierr .NE. NF_NOERR ) goto 7001
c
      enddo
c
c  ---- deallocate the array ---
c
      deallocate( array_3d )
c
      ierr = 1
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_SPECIES:'
      write(*,'(A)') TRIM(action)
      write(*,'(2A)') 'Cannot find variable id for: ',TRIM(this_var)
      stop
c
 7001 continue
      write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_SPECIES:'
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
 
