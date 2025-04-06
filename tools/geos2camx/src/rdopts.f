C**** RDOPTS
c
      subroutine rdopts(lnetcdf_out,lgeos_file,geos_infile,geos_unit,
     &                  fname_spcmap,iinspcmap,lnetcdf_in,fname_3dmet,
     &                  iinmet,ltopo,fname_topo,iintopo,judate)
      implicit none
c
c-----------------------------------------------------------------------
c
c     This routine reads the options and filenames for the GEOS2CANx 
c     program from the job script:
c
c   Arguments:
c     Inputs:
c     Outputs:
c        lnetcdf_out   L   .TRUE. if writing NetCDF outputs
c        lgeos_file    L   .TRUE. if the GeosChem file is provided
c        geos_infile   C   name of GeosChem input files
c        geos_unit     I   unit number of the GeosChem files
c        fname_spcmap  C   name of Species Mapping input file
c        iinspcmap     I   unit number of Species Mapping input file
c        fname_3dmet   C   name of input CAMx 3D met file
c        iinmet        I   unit number of input 3D met file
c        ltopo         L   .TRUE. if CAMx landuse file is provided
c        fname_topo    C   name of input CAMx landuse file
c        iintopo       I   unit number of input landuse file
c        judate        I   Julian day of day to processs
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      logical       lnetcdf_out
      logical       lgeos_file(2)
      character*256 geos_infile(2)
      integer       geos_unit(2)
      character*256 fname_spcmap
      integer       iinspcmap
      logical       lnetcdf_in
      character*256 fname_3dmet
      integer       iinmet
      logical       ltopo
      character*256 fname_topo
      integer       iintopo
      integer       judate
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
      character*10  cname
      character*4   name(10)
      integer       ifile, ierr
      logical       lcheck
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c ---- get flag for NetCDF format ---
c
      action = 'Reading flag for NetCDF output.' 
      read(*,'(20X,L10)',ERR=7000) lnetcdf_out
      if( lnetcdf_out ) then
         write(*,'(A,/)') 'Generating outputs in NetCDF format.'
      else
         write(*,'(A,/)') 'Generating outputs in FORTRAN binary format.'
      endif
      call flush(6)
c
c  --- GeosChem output files to be procoessed ---
c
      action = 'Opening first NetCDF file with GeosChem output.'
      do ifile=1,2
        lgeos_file(ifile) = .FALSE.
        read(*,'(20X,A)',ERR=7000) infilec
        if( LEN_TRIM(infilec) .LE. 0 ) then
            if( ifile .EQ. 1 ) goto 7001
            if( ifile .EQ. 2 ) cycle
        endif
        inquire(file=infilec,exist=lcheck)
        if( .NOT. lcheck ) goto 7002
        ierr = nf_open(infilec,NF_NOWRITE,geos_unit(ifile))
        if( ierr .NE. NF_NOERR ) goto 7003
        geos_infile(ifile) = infilec
        write(*,'(A,/,A,/)') 'Opened input Geos-Chem file: ',TRIM(infilec)
        call flush(6)
        action = 'Opening second NetCDF file with GeosChem output.'
        lgeos_file(ifile) = .TRUE.
      enddo
c
c ---- Species mapping file ----
c
      action = 'Opening input Species Mapping file.'
      read (*,'(20X,A)',ERR=7000) infilec
      inquire(file=infilec,exist=lcheck)
      if( .NOT. lcheck ) goto 7002
      open(iinspcmap,file=infilec,status='OLD',IOSTAT=ierr)
      if( ierr .NE. 0 ) goto 7003
      write(*,'(A,/,A,/)') 'Opened input species mapping file: ',TRIM(infilec)
      fname_spcmap = infilec
      call flush(6)
c
c ---- Open 3d met file ---
c
      action = 'Opening input CAMx 3D met file.'
      lnetcdf_in = .FALSE.
      read(*,'(20X,A)',ERR=7000) infilec
      inquire(file=infilec,exist=lcheck)
      if( .NOT. lcheck ) goto 7002
      open(iinmet,file=infilec,status='OLD',form='unformatted',IOSTAT=ierr)
      if( ierr .NE. 0 ) goto 7003
      fname_3dmet = infilec
      read(iinmet) name
      write(cname,'(10A1)') name
      rewind(iinmet)
      if( cname .NE. 'AVERAGE   ' ) then
         lnetcdf_in = .TRUE.
         action = 'Opening input CAMx NetCDF 3D met file.'
         close(iinmet)
         ierr = nf_open(infilec,NF_NOWRITE,iinmet)
         if( ierr .NE. NF_NOERR ) goto 7003
      endif
      if( lnetcdf_in ) then
         write(*,'(A,/,A,/)') 'Opened input 3D Met file in NetCDF format: ',TRIM(infilec)
      else
         write(*,'(A,/,A,/)') 'Opened input 3D Met file in FORTRAN binary format: ',TRIM(infilec)
      endif
      call flush(6)
c
c  --- Open topo file ---
c
      action = 'Opening input CAMx landuse file.'
      read(*,'(20X,A)',ERR=7000) infilec
      ltopo = .FALSE.
      if( LEN_TRIM(infilec) .GT. 0 ) then
         fname_topo = infilec
         ltopo = .TRUE.
         if( lnetcdf_in ) then
            action = 'Opening input CAMx NetCDF landuse file.'
            ierr = nf_open(infilec,NF_NOWRITE,iintopo)
            if( ierr .NE. NF_NOERR ) goto 7003
         else
            action = 'Opening input CAMx landuse file.'
            open(iintopo, file=infilec, status='OLD',form='unformatted',IOSTAT=ierr)
            if( ierr .NE. NF_NOERR ) goto 7003
         endif
         if( lnetcdf_in ) then
            write(*,'(A,/,A,/)') 'Opened input landuse file in NetCDF format: ',TRIM(infilec)
         else
            write(*,'(A,/,A,/)') 'Opened input landuse file in FORTRAN binary format: ',TRIM(infilec)
         endif
      else
         write(*,'(A,/)') 'No landuse file used. No topo data will be used.'
      endif
      call flush(6)
c
      action = 'Reading Julian day of day to processs.'
      read(*,'(20X,I10)',ERR=7000) judate
      write(*,'(A,I10,/)') 'Date of Output file: ',judate
      call flush(6)
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(/,A)') 'ERROR in RDOPTS.'
      write(*,'(A)') TRIM(action)
      write(*,'(A,//)') 'Cannot read data from job script.'
      call flush(6)
      stop
c
 7001 continue
      write(*,'(/,A)') 'ERROR in RDOPTS.'
      write(*,'(A)') TRIM(action)
      write(*,'(A,//)') 'Blank filename where name is needed.'
      call flush(6)
      stop
c
 7002 continue
      write(*,'(/,A)') 'ERROR in RDOPTS.'
      write(*,'(A)') TRIM(action)
      write(*,'(A)') 'Input file not found. '
      write(*,'(2A,//)') 'File: ',TRIM(infilec)
      call flush(6)
      stop
c
 7003 continue
      write(*,'(/,A)') 'ERROR in RDOPTS.'
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
