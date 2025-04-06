C**** WRT_CONC_DATA
c
      subroutine wrt_conc_data(lnetcdf_out,iounit,fname,filetype,
     &                        cname,nspecs,ncols,nrows,nlays_grid,
     &                        nlays_out,xorig,yorig,xcell,ycell,
     &                        tstep,this_tstep,beg_date,beg_time,
     &                        spcnames,lat_array,lon_array,topo_array,
     &                                             z_array,conc_array)
      implicit none
c
c-----------------------------------------------------------------------
c
c     This routine reads the options and filenames for the GES2CANx 
c     program from the job script:
c
c   Arguments:
c     Inputs:
c        lnetcdf_out    L  .TRUE. if writing NetCDF output files
c        iounit         I  file unit of file to write
c        fname          C  name of file to write
c        filetype       C  type of file to write
c        cname          C  filename for CAMx_NAME variable
c        nspecs         I  number of spsecies to write
c        ncols          I  number of columns in grid
c        nrows          I  number of rows in grid
c        nlays_grid     I  number of layers in the gridded array
c        nlays_out      I  number of layers to write to file
c        xorig          R  cell origin in X-direction
c        yorig          R  cell origin in Y-direction
c        xcell          R  cell width in X-direction
c        ycell          R  cell width in Y-direction
c        tstep          I  iteration of data time step
c        this_tstep     I  number of the current time step
c        beg_date       I  beginning day in Julian
c        beg_time       R  beginning time
c        spcnames       C  array of species names
c        lat_array      R  array of lattitude for each cell
c        lon_array      R  array of longitude for each cell
c        topo_array     R  array of ground elevation
c        z_array        R  arrays of layers heights
c        conc_array     R  array of gridded concentrations to write
c     Outputs:
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      logical       lnetcdf_out
      integer       iounit
      character*256 fname
      character*256 filetype
      character*10  cname
      integer       nspecs
      integer       ncols
      integer       nrows
      integer       nlays_grid
      integer       nlays_out
      real          xorig
      real          yorig
      real          xcell
      real          ycell
      integer       tstep
      integer       this_tstep
      integer       beg_date
      real          beg_time
      character*12  spcnames(*)
      real          lat_array(ncols,nrows)
      real          lon_array(ncols,nrows)
      real          topo_array(ncols,nrows)
      real          z_array(ncols,nrows,nlays_grid)
      real          conc_array(ncols,nrows,nlays_grid,nspecs)
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
      integer IONE
      parameter( IONE = 1 )
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      character*256 action
      integer       ierr, jdate5b, jdate5e, ispc, ilay, i, j, k, m
      real          timeb, timee 
      character*4   spec(10)
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c  ---- write all of the data ---
c
      if( lnetcdf_out ) then
         if( cname .EQ. 'AIRQUALITY') cname = 'INITIAL   '
         action = 'Writing grid data to output NetCDF '
     &                                       //TRIM(filetype)//' file.'
c
c  --- if this is the first timestep write the grid description ---
c
         if( this_tstep .EQ. 1 ) call ncf_wrt_data_grid(ierr,action,
     &           iounit,ncols,nrows,xorig,yorig,xcell,ycell,nlays_out,
     &                                    lat_array,lon_array,topo_array)
         if( ierr .LT. 0 ) stop
c
         action = 'Writing timestamp to output NetCDF '
     &                                       //TRIM(filetype)//' file.'
         call ncf_wrt_data_tstep(ierr,action,iounit,nspecs)
         if( ierr .LT. 0 ) stop
c
         action = 'Writing gridded data to output NetCDF '
     &                                       //TRIM(filetype)//' file.'
         if( cname(1:8) .EQ. 'BOUNDARY' ) then
             call ncf_wrt_data_species_bc(ierr,action,iounit,this_tstep,
     &                      ncols,nrows,nlays_grid,nlays_out,nspecs,
     &                                    spcnames,z_array,conc_array)
         else
             call ncf_wrt_data_species_ic(ierr,action,iounit,this_tstep,
     &                      ncols,nrows,nlays_grid,nlays_out,nspecs,
     &                                    spcnames,z_array,conc_array)
         endif
         if( ierr .LT. 0 ) stop
      else
         jdate5b = MOD(beg_date,100000)
         jdate5e = jdate5b
         timeb = REAL(beg_time)
         timee = timeb + REAL(tstep)
         if( timee .GE. 24. ) then
            timee = timee - 24.
            jdate5e = jdate5e + 1
            if( MOD(jdate5e,1000) .GT. 365 ) then
               if( MOD(INT(jdate5e/1000),4) .EQ. 0 ) then
                  if( MOD(jdate5e,1000) .EQ. 367 ) 
     &                    jdate5e = (INT(jdate5e/1000)+1)*1000 + 1
               else
                  jdate5e = (INT(jdate5e/1000)+1)*1000 + 1
               endif
            endif
         endif
         write(iounit) jdate5b,timeb,jdate5e,timee
         if( cname(1:8) .EQ. 'BOUNDARY' ) then
            do ispc=1,nspecs
              read(spcnames(ispc),'(10A1)') (spec(i),i=1,10)
               write(iounit) IONE,(spec(m),m=1,10),1,
     &                 ((conc_array(1,j,k,ispc),k=1,nlays_out),j=1,nrows)
               write(iounit) IONE,(spec(m),m=1,10),2,
     &             ((conc_array(ncols,j,k,ispc),k=1,nlays_out),j=1,nrows)
               write(iounit) IONE,(spec(m),m=1,10),3,
     &                 ((conc_array(i,1,k,ispc),k=1,nlays_out),i=1,ncols)
               write(iounit) IONE,(spec(m),m=1,10),4,
     &             ((conc_array(i,nrows,k,ispc),k=1,nlays_out),i=1,ncols)
            enddo
         else
            do ispc=1,nspecs
              read(spcnames(ispc),'(10A1)') (spec(i),i=1,10)
              do ilay=1,nlays_out
                  write(iounit) IONE,(spec(i),i=1,10),
     &                   ((conc_array(i,j,ilay,ispc),i=1,ncols),j=1,nrows)
              enddo
            enddo
         endif
      endif
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(/,A)') 'ERROR in WRT_CONC_DATA.'
      write(*,'(A)') TRIM(action)
      write(*,'(A)') 'Cannot write to FORTRAN binary file.'
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
