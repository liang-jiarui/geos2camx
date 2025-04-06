C**** WRT_FILE_HDR
c
      subroutine wrt_file_hdr(lnetcdf_out,iounit,fname,cname,cnote,
     &                        filetype,nspecs,ncols,nrows,nlayers,
     &                        p_alp,p_bet,p_gam,xcent,ycent,xorig,yorig,
     &                        xcell,ycell,itzon_in,iutmin,iproj,istag,
     &                        tstep,beg_date,beg_time,end_date,end_time,
     &                        hours_to_run,spcnames,lgas)
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
c        cname          C  name of NetCDF file type to write
c        cnote          C  file message to write to file
c        filetype       C  type of file being written
c        nspecs         I  number of spsecies to write
c        ncols          I  number of columns in grid
c        nrows          I  number of rows in grid
c        nlayers        I  number of layers to write to file
c        p_alp          R  
c        p_bet          R  
c        p_gam          R  
c        xcent          R
c        ycent          R
c        xorig          R  X-coord in X direction
c        yorig          R  Y-coord in Y direction
c        xcell          R  cell size in X direction
c        ycell          R  cell size in Y direction
c        itzon_in       I  time zone
c        itumin         I  UTM zone
c        iproj          I  projection type
c        istag          I  projection type
c        tstep          I  iteration of data time step
c        beg_date       I  beginning day in Julian
c        beg_time       R  beginning time
c        end_date       I  endign day in Julian
c        beg_time       I  endign day in Julian
c        hours_to_run   I  number of hours to write to file
c        spcnames       C  array of species names
c        lgas           L  array of flags for gas/PM species
c     Outputs:
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      logical       lnetcdf_out
      integer       iounit
      character*256 fname
      character*10  cname
      character*60  cnote
      character*256 filetype
      integer       nspecs
      integer       ncols
      integer       nrows
      integer       nlayers
      real          p_alp
      real          p_bet
      real          p_gam
      real          xcent
      real          ycent
      real          xorig
      real          yorig
      real          xcell
      real          ycell
      integer       itzon_in
      integer       iutmin
      integer       iproj
      integer       istag
      integer       tstep
      integer       beg_date
      real          beg_time
      integer       end_date
      real          end_time
      integer       hours_to_run
      character*12  spcnames(*)
      logical       lgas(*)
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
      character*245 action
      character*4   note(60), name(10)
      integer       ierr, ispc, i
c
      character*4,   allocatable   :: spec(:,:)
      character*20,  allocatable   :: spec_units(:)
      character*20,  allocatable   :: spec_long_name(:)
      character*60,  allocatable   :: spec_desc(:)
      character*60,  allocatable   :: spec_coords(:)
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
      if( lnetcdf_out ) then
c
         allocate( spec(10,nspecs)        )
         allocate( spec_units(nspecs)     )
         allocate( spec_long_name(nspecs) )
         allocate( spec_desc(nspecs)      )
         allocate( spec_coords(nspecs)    )
c
         action = 'Writing output NetCDF '//TRIM(filetype)//' file.'
         call ncf_set_vars_base()
         call ncf_set_specatt(nspecs,spcnames,lgas,spec_units,
     &                          spec_long_name, spec_desc,spec_coords)
         call ncf_set_global(cname,ncols,nrows,nlayers,p_alp,p_bet,p_gam,
     &                xcent,ycent,xorig,yorig,xcell,ycell,itzon_in,iutmin,
     &                          cnote,tstep,hours_to_run,beg_date,beg_time,
     &                                           end_date,end_time,nspecs)
         call ncf_set_tstep(tstep,beg_date,beg_time,hours_to_run)
         call ncf_wrt_dim(ierr,action,iounit,ncols,nrows,nlayers,nspecs)
         if( ierr .LE. 0 ) stop
         call ncf_wrt_global(ierr,action,iounit,nlayers,spcnames,nspecs)
         if( ierr .LE. 0 ) stop
         call ncf_wrt_vars_base(ierr,action,iounit)
         if( ierr .LE. 0 ) stop
         call ncf_wrt_vars_species(ierr,action,iounit,ncols,nrows,nspecs,
     &        spcnames,spec_units,spec_long_name,spec_desc,spec_coords)
         if( ierr .LE. 0 ) stop
         ierr = nf_enddef(iounit)
c
         deallocate( spec_units     )
         deallocate( spec_long_name )
         deallocate( spec_desc      )
         deallocate( spec_coords    )
      else
         allocate( spec(10,nspecs)        )
         do ispc=1,nspecs
            read(spcnames(ispc),'(10A1)') (spec(i,ispc),i=1,10)
         enddo
         if( cname .EQ. 'INITIAL   ' ) cname = 'AIRQUALITY'
         read(cname,'(10A1)') name
         read(cnote,'(60A1)') note
         write(iounit,ERR=7000) name,note,itzon_in,nspecs,
     &                        MOD(beg_date,100000),beg_time,
     &                            MOD(end_date,100000),end_time
         write(iounit,ERR=7000) xcent,ycent,iutmin,xorig,yorig,
     &                          xcell,ycell,ncols,nrows,nlayers,
     &                                   iproj,istag,p_alp,p_bet,0.
         write(iounit,ERR=7000) 1,1,ncols,nrows
         write(iounit,ERR=7000) ((spec(i,ispc),i=1,10),ispc=1,nspecs)
         deallocate( spec )
      endif
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(/,A)') 'ERROR in WRT_FILE_HDR.'
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
