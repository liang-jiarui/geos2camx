C**** RD_OUTFNAMES
c
      subroutine rd_outfnames(lnetcdf_out,file_type,luse_this_file,
     &                                fname_this_file,inunit_this_file)
      implicit none
c
c-----------------------------------------------------------------------
c
c     This routine reads the names of the output file and opens th files.
c
c   Arguments:
c     Inputs:
c        lnetcdf_out       L   .TRUE. if writing NetCDF outputs
c        file_type         C   string that describes what type of file is read
c     Outputs:
c        luse_this_file    L   .TRUE. if this file will be created
c        fname_this_file   C   name of the file to be created
c        inunit_this_file  I   unit number of the file to be created
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      logical       lnetcdf_out
      character*256 file_type
      logical       luse_this_file
      character*256 fname_this_file
      integer       inunit_this_file
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
      character*200 action, infilec
      integer       ierr
      logical       lcheck
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c ---- get the filename to read ---
c
      action = 'Opening output '//TRIM(file_type)//' file.'
      luse_this_file = .FALSE.
      read(*,'(20X,A)',ERR=7000) infilec
      if( LEN_TRIM(infilec) .GT. 0 ) then
        fname_this_file = infilec
        luse_this_file = .TRUE.
        if( lnetcdf_out ) then
          inquire(file=infilec,exist=lcheck)
          if( lcheck ) goto 7001
          call ncf_createfile(ierr,infilec,action,inunit_this_file)
          if( ierr .LE. 0 ) stop
        else
          open(inunit_this_file,file=infilec,status='UNKNOWN',
     &                            form='UNFORMATTED',IOSTAT=ierr)
          if( ierr .NE. 0 ) goto 7002
        endif
        if( lnetcdf_out ) then
           write(*,'(3A,/,A,/)') 'Opened output ',TRIM(file_type),
     &                              ' file in NetCDF format: ',TRIM(infilec)
        else
           write(*,'(3A,/,A,/)') 'Opened output ',TRIM(file_type),
     &                              ' file in FORTRAN binary format: ',TRIM(infilec)
        endif
      else
        write(*,'(3A,/)') 'No ',TRIM(file_type),' file will be created.'
      endif
      call flush(6)
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(/,A)') 'ERROR in RD_OUTFNAMES.'
      write(*,'(A)') TRIM(action)
      write(*,'(A,//)') 'Cannot read data from job script.'
      call flush(6)
      stop
c
 7001 continue
      write(*,'(/,A)') 'ERROR in RD_OUTFNAMES.'
      write(*,'(A)') TRIM(action)
      write(*,'(A)') 'Output file already exists.'
      write(*,'(2A,//)') 'File: ',TRIM(infilec)
      call flush(6)
      stop
c
 7002 continue
      write(*,'(/,A)') 'ERROR in RD_OUTFNAMES.'
      write(*,'(A)') TRIM(action)
      write(*,'(A)') 'Cannot open file.'
      write(*,'(2A,//)') 'File: ',TRIM(infilec)
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
