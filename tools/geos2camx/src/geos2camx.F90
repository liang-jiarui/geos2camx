      program geos2camx
      use ncf_iodat
!c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c Copyright (C) 2006-2016  Ramboll Environ
!c
!c This program is free software; you can redistribute it and/or
!c modify it under the terms of the GNU General Public License
!c as published by the Free Software Foundation; either version 2
!c of the License, or (at your option) any later version.
!c
!c This program is distributed in the hope that it will be useful,
!c but WITHOUT ANY WARRANTY; without even the implied warranty of
!c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!c GNU General Public License for more details.
!c
!c To obtain a copy of the GNU General Public License
!c go to the Free Software Foundation at http://www.fsf.org.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
!     GEOS2CAMx converts GEOS-Chem (GC) output files into the CAMx
!     boundary condition input data, initial condition input data, or
!     top condition input data files. GEOS2CAMx also converts to CAMx
!     3D output data for surface layer.
!
!     It interpolates vertical domains,and species mapping is processed
!     based on a user specified species mapping text file (geoschem_*.txt). 
!
!     The followings are specific tasks in the main routine.
!        1. Find out the closest col and row in CAMx domain. Supporting
!           CAMx domains are, lat_lon, lcp, utm, and psp (polar secant 
!           stereograph, iproj = 3).
!        2. Read GEOS-CHEM global binary outputs and mapping table in 
!           MAP_CHEM subroutine.
!        3. Do Vertical interpolation based on either agl or msl (if
!           topo file is available).
!        4. Write out in CAMx domain. This can be BC, IC, TC and/or Surface.
!
      implicit none
!
!------------------------------------------------------------------------------
!  Include files:
!------------------------------------------------------------------------------
!
#     include "geoschem.com"
      include "netcdf.inc"
!
!------------------------------------------------------------------------------
!  Local parameters:
!------------------------------------------------------------------------------
!
      integer IONE
      parameter( IONE = 1 )
!
!------------------------------------------------------------------------------
!  Allocatable  variables:
!------------------------------------------------------------------------------
!
      character*4,    allocatable   :: spec(:,:)
      integer,        allocatable :: time_minutes(:)
      integer,        allocatable :: geos_tflag(:,:)
      integer,        allocatable :: iloc (:, :)
      integer,        allocatable :: jloc (:, :)
      real,           allocatable :: sealevel(:, :)
      real,           allocatable :: concin (:, :, :, :)     ! GEOS-Chem conc array
      real,           allocatable :: hght_in (:, :, :)       ! GEOS-Chem cell height at the center, unit is msl
      real,           allocatable :: p_geos (:, :, :)        ! GEOS-Chem pressure levels
      real,           allocatable :: concin_m (:, :, :, :)   ! CAMx array for horizontal matching
      real,           allocatable :: concout (:, :, :, :)    ! CAMx array for vertical interpolation
      real,           allocatable :: tconcout (:, :, :)      ! CAMx array for top concentration
      real,           allocatable :: rlat (:, :)
      real,           allocatable :: rlon (:, :)
      real,           allocatable :: vglvs_in (:, :, :)      ! GEOS-Chem cell height at the center, unit is msl (if CAMx topo file is provided), otherwise agl
      real,           allocatable :: vglvs_out (:, :, :)     ! CAMx vertical levels, unit is msl (if CAMx topo file is provided), otherwise agl
      real,           allocatable :: htint_in (:, :, :)      ! GEOS-Chem cell height at the top, unit is msl (if CAMx topo file is provided), otherwise agl
      real,           allocatable :: topo_m (:, :)           ! CAMx topography (m)
      real,           allocatable :: zp_time (:, :, :, :)    ! Output vertical levels with time
      real,           allocatable :: zp_now (:, :, :)        ! Current vertical levels with time
!
!  --- Grid defnition varables ---
!
      integer  gdtyp
      real*8   dble_xcent
      real*8   dble_ycent
      real*8   dble_xorig
      real*8   dble_yorig
      real*8   dble_xcell
      real*8   dble_ycell
      real*8   dble_p_alp
      real*8   dble_p_bet
      real     p_alp      ! 1st map projection parameter
      real     p_bet      ! 2nd map projection parameter
      real     p_gam      ! 3rd map projection parameter
      real     xcent      ! Longitude of coordinate system center
      real     ycent      ! Latitude of coordinate system center
      real     xorig      ! X-origin of grid
      real     yorig      ! Y-origin of grid
      real     xcell      ! X-dimension of cell (m)
      real     ycell      ! Y-dimension of cell (m)
!
!  --- File name and file unit variables ----
!
      character*60   version
      character*256  infilec
      character*256  action
      character*256  geos_infile(2)
      character*256  fname_3dmet
      character*256  fname_topo
      character*256  fname_bc
      character*256  fname_ic
      character*256  fname_tc
      character*256  fname_sfc
      character*256  fname_spcname
      character*256  fname_spcmap
      integer        geos_unit(2)            ! The 1st and 2nd main GEOS-Chem files
      integer        iinspcmap
      integer        iinmet
      integer        iintopo
      integer        ioutbc
      integer        ioutic
      integer        iouttc
      integer        ioutsfc
      integer        ifile
      integer        this_unit
      integer        ncf_cur_tstep_ic
      integer        ncf_cur_tstep_bc
      integer        ncf_cur_tstep_tc
      integer        ncf_cur_tstep_sfc
      logical        lgeos_file(2)  ! flag to determine if using 2 GeosChem files
      logical        ltopo          ! input flag to topo file
      logical        lbc            ! output flag to output bc
      logical        lic            ! output flag to output ic
      logical        lsfc           ! output flag to output sfc
      logical        ltc            ! output flag to output 2d tc
      logical        lnetcdf_out    ! flag to use NetCDF format for outputs
      logical        lnetcdf_in     ! flag to use NetCDF format for met inputs
!
!  --- Date/time variables ----
!
      character*200 time_attribute
      integer       sdate       ! starting date in model time zone,    format YYYYJJJ
      integer       sdate_utc   ! starting date in UTC
      integer       stime       ! starting time in model time zone,    format HHMMSS
      integer       stime_utc   ! starting date in UTC
      integer       this_time   ! looping variable
      integer       tstep       ! user defined time step
      integer       judate      ! user defined Julian date, YYYYJJJ
      integer       nlayers     ! number of lays in GC output file
      integer       ncols_in    ! number of columns in GoesChem domain
      integer       nrows_in    ! number of rows in GoesChem domain
      integer       ncols       ! number of columns in CAMx domain
      integer       nrows       ! number of rows in CAMx domain
      integer       nlays       ! number of lays in CAMx domain
      integer       hours_to_run
      integer       this_tstep
      integer       this_tstep_geos
      integer       this_tstep_met
      integer       this_time_tflag
      integer       this_time_etflag
      integer       nhours_in
      integer       geos_nhours(2)
      integer       this_year
      integer       this_month
      integer       this_day
      integer       this_hour
      integer       this_minute
      integer       this_julday
      integer       nhours_met
      integer       geos_start_day(2)
      integer       geos_start_time(2)
!
!  --- File description and identification variables --
!
      character*60  cnote
      character*10  metspc
      character*4   name(10)
      character*10  cname
      character*4   note(60)
      integer       this_dimid
      integer       iutmin
      integer       itzon
      integer       iproj
!
! --- Species name count and mapping variables ---
!
      character*12  camx_spec_names(500)
      character*12  geos_spec_names(500)
      character     ga_camx(500)
      integer       np                     ! number of camx species
      integer       n3dmet
      integer       n2dsrf
      integer       n_geos_spec
      integer       id_camx(500)
      integer       id_geos(500)
      integer       map_idx(500,99)
      real          map_fac(500,99) 
      real          camx_spec_mw(500)
      logical       lgas(500)
!
! --- General use variables ---
!
      integer  lvl, spc, col, row, i, j, ied, ibd, istag, ierr
      integer  this_varid, ibeg, iend, lin, lout, ioffset, iplace
      integer  this_enddate, this_endtime
      real     bt, et, xloc, yloc
      real     beg_lon, end_lon, beg_lat, beg_p1_lat, htbeg, htend, sumconc
      logical  lread_ok
!
!------------------------------------------------------------------------------
!  External functions:
!------------------------------------------------------------------------------
!
      integer ncf_get_tstep
!
!------------------------------------------------------------------------------
!  Entry point: 
!------------------------------------------------------------------------------
!
! --- end of declaration ---
! 
      iinspcmap  = 24
      iinmet     = 25
      iintopo    = 26
      ioutbc     = 30
      ioutic     = 31
      iouttc     = 32
      ioutsfc    = 33

      version = 'GEOS2CANx v4.0 01/11/2021'
      lambrt  = .FALSE.
      llatlon = .FALSE.
      lutm    = .FALSE.
      lrpolar = .FALSE.
      lpolar  = .FALSE.
      lmerc   = .FALSE.
      call ncf_set_compress_flag()
      lnetcdf_in = .FALSE. 
      nhours_met = 25
!
! --- call routine to read the options and filenames ---
!
      write(*,'(/,20X,2A,//)') 'Starting ',trim(version)
      call rdopts(lnetcdf_out,lgeos_file,geos_infile,geos_unit,   &
                  fname_spcmap,iinspcmap,lnetcdf_in,fname_3dmet,  &
                  iinmet,ltopo,fname_topo,iintopo,judate)

      action = 'Boundary Conditions'
      call rd_outfnames(lnetcdf_out,action,lbc,fname_bc,ioutbc)                         
      action = 'Initial Conditions'
      call rd_outfnames(lnetcdf_out,action,lic,fname_ic,ioutic)                         
      action = 'Top Concentations'
      call rd_outfnames(lnetcdf_out,action,ltc,fname_tc,iouttc)                         
      action = 'Surface Concentations'
      call rd_outfnames(lnetcdf_out,action,lsfc,fname_sfc,ioutsfc)                         
!
!  --- get dimensions of GeosChem grid ---
!
      infilec = geos_infile(1)
      action = 'Reading lev dimension from GeosChem input file.'
      ierr = nf_inq_dimid(geos_unit(1), "lev", this_dimid )
      if( ierr .NE. NF_NOERR ) goto 7002
      ierr = nf_inq_dimlen(geos_unit(1),this_dimid,nlayers)
      if( ierr .NE. NF_NOERR ) goto 7002

      action = 'Reading lon dimension from GeosChem input file.'
      ierr = nf_inq_dimid(geos_unit(1), "lon", this_dimid )
      if( ierr .NE. NF_NOERR ) goto 7002
      ierr = nf_inq_dimlen(geos_unit(1),this_dimid,ncols_in)
      if( ierr .NE. NF_NOERR ) goto 7002

      action = 'Reading lat dimension from GeosChem input file.'
      ierr = nf_inq_dimid(geos_unit(1), "lat", this_dimid )
      if( ierr .NE. NF_NOERR ) goto 7002
      ierr = nf_inq_dimlen(geos_unit(1),this_dimid,nrows_in)
      if( ierr .NE. NF_NOERR ) goto 7002
!
! ---- Get the time spanned by each file ---
!
      nhours_in = 0
      hours_to_run = 0
      action = 'Reading time dimension from GeosChem input file.'
      do ifile=1,2
         if( .NOT. lgeos_file(ifile) ) cycle
         infilec = geos_infile(ifile)
         geos_nhours(ifile) = 0
         ierr = nf_inq_dimid(geos_unit(ifile), "time", this_dimid )
         if( ierr .NE. NF_NOERR ) goto 7002
         ierr = nf_inq_dimlen(geos_unit(ifile),this_dimid,geos_nhours(ifile))
         if( ierr .NE. NF_NOERR ) goto 7002
         nhours_in = nhours_in + geos_nhours(ifile)
         hours_to_run = MAX( hours_to_run, geos_nhours(ifile) )
      enddo
      allocate( geos_tflag(2,nhours_in) )
      allocate( time_minutes(nhours_in) )
      ioffset = 0
      do ifile=1,2
         if( .NOT. lgeos_file(ifile) ) cycle
         infilec = geos_infile(ifile)
         action = 'Cannot find time variable from GeosChem input file.'
         ierr = nf_inq_varid(geos_unit(ifile), 'time', this_varid)
         if( ierr .NE. NF_NOERR) goto 7002
         action = 'Reading units attribute data for time variable from GeosChem input file.'
         ierr = nf_get_att_text(geos_unit(ifile),this_varid,"units",time_attribute) 
         if( ierr .NE. NF_NOERR) goto 7002
!
         iplace = INDEX( time_attribute, "minutes since" )
         read(time_attribute(iplace+14:),'(I4,4(1X,I2))') this_year, this_month, &
                         this_day, this_hour, this_minute
         this_julday = MOD(this_year,100)*10000 + this_month*100 + this_day
         call juldate( this_julday )
         geos_start_day(ifile) = this_year*1000 + MOD(this_julday,1000)
         geos_start_time(ifile) = this_hour*100 + this_minute
         action = 'Reading units attribute data for time variable from GeosChem input file.'
         ierr = nf_get_var_int(geos_unit(ifile), this_varid, time_minutes )
         if( ierr .NE. NF_NOERR) goto 7002
         do this_tstep=1,geos_nhours(ifile)
           geos_tflag(1,ioffset+this_tstep) = geos_start_day(ifile)
           this_hour = INT(REAL(time_minutes(this_tstep)/60.0)) 
           this_minute = MOD(time_minutes(this_tstep),60)
           geos_tflag(2,ioffset+this_tstep) = this_hour*10000 + this_minute*100 
           if( this_hour .GE. 24 ) then
               this_day = INT(this_hour/24)
               time_minutes(this_tstep) = time_minutes(this_tstep) - this_day * 1440
               this_hour = INT(REAL(time_minutes(this_tstep)/60.0)) 
               this_minute = MOD(time_minutes(this_tstep),60)
               geos_tflag(1,ioffset+this_tstep) = geos_tflag(1,ioffset+this_tstep) + this_day
               geos_tflag(2,ioffset+this_tstep) = this_hour*10000 + this_minute*100 
           endif
         enddo
         ioffset = geos_nhours(ifile)
         tstep = INT((REAL(time_minutes(2)) - REAL(time_minutes(1)))/60.)
      enddo
!
! --- Get domain definition of target grid from input met file ---
! 
      if( lnetcdf_in ) then
         action = 'Cannot read global attributes from 3D met NetCDF file.'
         infilec = fname_3dmet
         lread_ok = .TRUE.
         ierr = nf_get_att_text(iinmet, NF_GLOBAL, 'CAMx_NAME', cname)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_text(iinmet, NF_GLOBAL, 'NOTE', note)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_int(iinmet, NF_GLOBAL, 'ITZON', itzon)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_double(iinmet, NF_GLOBAL, 'XCENT', dble_xcent)
         xcent = REAL(dble_xcent)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_double(iinmet, NF_GLOBAL, 'YCENT', dble_ycent)
         ycent = REAL(dble_ycent)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_int(iinmet, NF_GLOBAL, 'IUTM', iutmin)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_double(iinmet, NF_GLOBAL, 'XORIG', dble_xorig)
         xorig = REAL(dble_xorig)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_double(iinmet, NF_GLOBAL, 'YORIG', dble_yorig)
         yorig = REAL(dble_yorig)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_double(iinmet, NF_GLOBAL, 'XCELL', dble_xcell)
         xcell = REAL(dble_xcell)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_double(iinmet, NF_GLOBAL, 'YCELL', dble_ycell)
         ycell = REAL(dble_ycell)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_int(iinmet, NF_GLOBAL, 'NCOLS', ncols)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_int(iinmet, NF_GLOBAL, 'NROWS', nrows)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_int(iinmet, NF_GLOBAL, 'NLAYS', nlays)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_int(iinmet, NF_GLOBAL, 'CPROJ', iproj)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_int(iinmet, NF_GLOBAL, 'ISTAG', istag)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_double(iinmet, NF_GLOBAL, 'P_ALP', dble_p_alp)
         p_alp = REAL(dble_p_alp)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_double(iinmet, NF_GLOBAL, 'P_BET', dble_p_bet)
         p_bet = REAL(dble_p_bet)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         ierr = nf_get_att_int(iinmet, NF_GLOBAL, 'NSTEPS', nhours_met)
         if( ierr .NE. NF_NOERR ) lread_ok = .FALSE.
         if( .NOT. lread_ok ) goto 7002
      else
         action = 'Cannot read header from 3D met file.'
         infilec = fname_3dmet
         read(iinmet,ERR=7002) name,note,itzon,n3dmet,ibd,bt,ied,et
         read(iinmet,ERR=7002) xcent,ycent,iutmin,xorig,yorig,xcell,ycell,ncols, &
                               nrows,nlays,iproj,istag,p_alp,p_bet
      endif
!
! --- set projection type ---
!
      if( iproj .EQ. 0 ) then
         llatlon = .TRUE.
         gdtyp = 1
      endif
      if( iproj .EQ. 1 ) then
         lutm = .TRUE.
         gdtyp = 5
      endif
      if( iproj .EQ. 2 ) then
         lambrt = .TRUE.
         gdtyp = 2
      endif
      if( iproj .EQ. 4 ) then
         lpolar = .TRUE.
         gdtyp = 6
      endif
      if( iproj .EQ. 3 ) then
         write(*,'(/,A,I5)') 'ERROR: Invalid projection type: ',iproj
         write(*,'(A,/)') 'Polar Stereographic (PSP) is not supported.'
         call flush(6)
         stop
      endif
      istag = 0
      p_gam = xcent
      if( .NOT. lnetcdf_in ) read(iinmet)
      if( .NOT. lnetcdf_in ) read(iinmet)
!
! ---  Read species mapping for the CAMx species information ---
!
      call read_chem(np,n_geos_spec,id_camx,ga_camx,             &
                         camx_spec_mw,id_geos,map_idx,map_fac,   &
                         camx_spec_names,geos_spec_names,lgas,   &
                         iinspcmap,fname_spcmap )
      rewind(iinspcmap)
!
!  --- call routine to write table of species mapping ---
!
      call wrt_spec_table(np,camx_spec_names,n_geos_spec,     &
                          geos_spec_names,map_idx,lgas)
!
! ---  Allocate matrices whose dimensions are user defined ---
!
      allocate( sealevel(ncols_in,nrows_in) )
      allocate( rlat(ncols,nrows)           )
      allocate( rlon(ncols,nrows)           )
      allocate( iloc(ncols,nrows)           )
      allocate( jloc(ncols,nrows)           )
      allocate( topo_m(ncols,nrows)         )

      allocate( hght_in(ncols_in,nrows_in,nlayers)            )
      allocate( p_geos(ncols_in,nrows_in,nlayers)             )
      allocate( vglvs_in(ncols,nrows,nlayers)         )
      allocate( vglvs_out(ncols,nrows,nlays)          )
      allocate( htint_in(ncols,nrows,nlayers)         )
      allocate( tconcout(ncols,nrows,np)              )
      allocate( concin(ncols_in,nrows_in,nlayers,np)  )
      allocate( zp_time(ncols,nrows,nlays,nhours_met) )
      allocate( zp_now(ncols,nrows,nlays)             )
      allocate( concin_m(ncols,nrows,nlayers,np)      )
      allocate( concout(ncols,nrows,nlays,np)         )
!
!  --- Read time interval record ---
!
      sdate  = judate
      stime  = 0
      sdate_utc  = sdate
      stime_utc  = stime

      ncf_cur_tstep_ic = 0
      ncf_cur_tstep_bc = 0
      ncf_cur_tstep_tc = 0
      ncf_cur_tstep_sfc = 0

      call addtime( sdate_utc, stime_utc, itzon )

      write(*,'(A,2I10)') 'Starting Date/Time in Model Time Zone ',sdate,stime
      write(*,'(A,2I10,/)') 'Starting Date/Time in UTC Time Zone ',sdate_utc,stime_utc
      call flush(6)
!
!  --- Preprocessing for headers ---
!
      allocate ( spec(10,np) )
      do spc=1,np
         read(camx_spec_names(spc),'(10A1)') (spec(i,spc),i=1,10)
      enddo
!
!  --- Write BC file header ---
!
      if( lbc ) then
         cname = 'BOUNDARY    '
         cnote = 'Boundary Conditions from '//version
         action = 'Boundary Conditions'
         call wrt_file_hdr(lnetcdf_out,ioutbc,fname_bc,cname,cnote,action,np,   &
                           ncols,nrows,nlays,p_alp,p_bet,p_gam,xcent,ycent,     &
                           xorig,yorig,xcell,ycell,itzon,iutmin,iproj,istag, &
                           tstep,sdate,REAL(stime),sdate+1,REAL(stime),         &
                           hours_to_run,camx_spec_names,lgas)

         if( .NOT. lnetcdf_out ) then
            write(ioutbc,ERR=7002) IONE,1,nrows,0,0,0,0,(2,0,0,0,i=1,nrows-2),0,0,0,0
            write(ioutbc,ERR=7002) IONE,2,nrows,0,0,0,0,(ncols-1,0,0,0,i=1,nrows-2),0,0,0,0
            write(ioutbc,ERR=7002) IONE,3,ncols,0,0,0,0,(2,0,0,0,i=1,ncols-2),0,0,0,0
            write(ioutbc,ERR=7002) IONE,4,ncols,0,0,0,0,(nrows-1,0,0,0,i=1,ncols-2),0,0,0,0
         endif
      endif
!
!  --- Write IC file header ---
!
      if( lsfc ) then
         cname = 'AVERAGE    '
         cnote = 'Gridded Surface Concentrations from '//version
         action = 'Surface Concentration'
         call wrt_file_hdr(lnetcdf_out,ioutsfc,fname_sfc,cname,cnote,action,np, &
                           ncols,nrows,1,p_alp,p_bet,p_gam,xcent,ycent,         &
                           xorig,yorig,xcell,ycell,itzon,iutmin,iproj,istag, &
                           tstep,sdate,REAL(stime),sdate+1,REAL(stime),          &
                           hours_to_run,camx_spec_names,lgas)
      endif
!
!  --- Write IC file header ---
!
      if( lic ) then
         cname = 'INITIAL   '
         cnote = 'Initial Conditions from '//version
         action = 'Initial Conditions'
         call wrt_file_hdr(lnetcdf_out,ioutic,fname_ic,cname,cnote,action,np,   &
                           ncols,nrows,nlays,p_alp,p_bet,p_gam,xcent,ycent,     &
                           xorig,yorig,xcell,ycell,itzon,iutmin,iproj,istag, &
                           tstep,sdate,REAL(stime),sdate,REAL(stime),           &
                           1,camx_spec_names,lgas)
      endif
!
!  --- Write IC file header ---
!
      if( ltc ) then
         cname = 'TOPCONC     '
         cnote = 'Top Concentrations from '//version
         action = 'Top Concentrations'
         call wrt_file_hdr(lnetcdf_out,iouttc,fname_tc,cname,cnote,action,np,   &
                           ncols,nrows,1,p_alp,p_bet,p_gam,xcent,ycent,         &
                           xorig,yorig,xcell,ycell,itzon,iutmin,iproj,istag, &
                           tstep,sdate,REAL(stime),sdate,REAL(stime),           &
                           hours_to_run,camx_spec_names,lgas)
      endif
!
! --- read layers heights from CAMx 3D Met file ---
!
      metspc = 'z         '
      cname = 'ZGRID_M   '
      action = 'Reading the input 3D met file.'
      call get_grid_data(lnetcdf_in,iinmet,fname_3dmet,cname,metspc,action,   &
                         ncols,nrows,nlays,nhours_met,n3dmet,zp_time)
!
! --- read topo data from landuse file ---
!
      if( ltopo ) then
         metspc = 'topo      '
         cname = 'TOPO_M    '
         action = 'Variable topo not found in landuse file.'
         if( .NOT. lnetcdf_in ) then
            read(iintopo,ERR=7002) name,note,i,n2dsrf,ibd,bt,ied,et
            read(iintopo,ERR=7002) 
            read(iintopo,ERR=7002) 
            read(iintopo,ERR=7002) 
         endif
         call get_grid_data(lnetcdf_in,iintopo,fname_topo,cname,metspc,action,   &
                         ncols,nrows,1,1,n2dsrf,topo_m)
      endif
!
!  --- Get the GEOS-CHEM lat & lon information ---
!
      beg_lon = -180.0 - disize/2.
      end_lon = 180.0 - disize/2.
      beg_lat = -90.
      beg_p1_lat = -90. + djsize/2.
!
!  --- Compute the closet lat & lon in coarse domain ---
!
      do i=1,ncols
        do j=1,nrows
          if( gdtyp .EQ. 6 ) then
            xloc = (xorig+(i-0.5)*xcell)/1000.
            yloc = (yorig+(j-0.5)*ycell)/1000.
            call pspgeo(1,p_gam,AMAX1(p_bet,p_alp),AMAX1(p_bet,p_alp), &
                       xloc,yloc,rlon(i,j),rlat(i,j))
          else
            call lat_lon(i,j,gdtyp,xorig,yorig,xcell,ycell,xcent,ycent, &
                         p_alp,p_bet,p_gam,rlat(i,j),rlon(i,j))
           endif
         enddo
      enddo
      do i=1,ncols
        do j=1,nrows
          iloc(i,j) = INT((rlon(i,j) - beg_lon)/disize) + 1
          if( rlon(i,j) .GT. end_lon ) iloc(i,j) = 1
          jloc(i,j) = INT((rlat(i,j) - beg_p1_lat)/djsize) + 2
          if( rlat(i,j) .LE. beg_p1_lat ) jloc(i,j) = 1
        enddo
      enddo
      write(*,*)
      write(*,'(A)') '=== GEOSCHEM GRID CELL RANGE ==='
      write(*,'(5X,4(A,I3),A)') &
       '('    ,iloc(1    ,nrows),',',jloc(1    ,nrows), &
       ') - (',iloc(ncols,nrows),',',jloc(ncols,nrows),')'
      write(*,'(5X,A)') '    |           |'
      write(*,'(5X,4(A,I3),A)') &
       '('    ,iloc(1    ,1    ),',',jloc(1    ,1    ), &
       ') - (',iloc(ncols,1    ),',',jloc(ncols,1    ),')'
      write(*,'(A,/)') '================================'
      call flush(6)
!
!  --- Start a time loop ---
!
      this_tstep_geos = 0
 333  continue
      this_tstep_geos = this_tstep_geos + 1
      if( .NOT. lbc .AND. .NOT. ltc .AND. .NOT. lsfc    &
                     .AND. this_tstep_geos .GT. 1 ) goto 444
      if( this_tstep_geos .GT. hours_to_run ) goto 444
!
!  --- Initialize GEOS-CHEM map array ---
!
      concin(:,:,:,:) = 0.0
      concout(:,:,:,:) = 0.0
      tconcout(:,:,:) = 0.0
      this_tstep = 0
      do i=1,nhours_in
        if( this_tstep .GT. 0 ) cycle
        this_enddate = geos_tflag(1,i)
        this_endtime = geos_tflag(2,i)/10000
        call addtime( this_enddate, this_endtime, tstep )
        this_endtime = this_endtime*10000
        if( sdate_utc .EQ. geos_tflag(1,i) .AND. geos_tflag(1,i)       &
                                           .EQ. this_enddate ) then
            if( stime_utc*10000 .GE. geos_tflag(2,i) .AND.           &
                              stime_utc*10000 .LT. this_endtime ) then
                 if( i .LE. geos_nhours(1) ) then
                     this_tstep = i
                     this_unit = geos_unit(1)
                 else
                     this_tstep = i-geos_nhours(1)
                     this_unit = geos_unit(2)
                 endif
            endif
        else if( sdate_utc .EQ. geos_tflag(1,i) .AND.     &
                               sdate_utc .LT. this_enddate ) then
            if( i .LE. geos_nhours(1) ) then
                this_tstep = i
                this_unit = geos_unit(1)
            else
                this_tstep = i-geos_nhours(1)
                this_unit = geos_unit(2)
            endif
        endif
      enddo
!
!  --- no data found for current hour ---
!
      if( this_tstep .EQ. 0 ) goto 7004
!
!  --- call routine to read GepsChem concentrations and
!      map the species names ----
!
      call map_chem(this_tstep,np,1,1,ncols_in,nrows_in,nlayers,      &
                    this_unit,iinspcmap,fname_spcmap,hght_in,p_geos,  &
                    concin,sealevel,camx_spec_names,fname_spcmap)
!
! --- Horizontal mapping global array onto CAMx domain ---
!
      do spc=1,np
        do lvl=1,nlayers
          do row=1,nrows
            do col=1,ncols
               concin_m(col,row,lvl,spc) &
                    = concin(iloc(col,row),jloc(col,row),lvl,spc)
               if( concin_m(col,row,lvl,spc) .LT. 0.0 ) then
                  write(*,*)'WARNING: GEOS-Chem has a negative value!'
                  write(*,*)'Setting this to a tiny positive,1.0e-6'
                  write(*,*)'GEOS-Chem (i,j,lvl,spc)=', &
                            iloc(col,row),jloc(col,row),lvl,spc
                  call flush(6)
                  concin_m(col,row,lvl,spc) = 1.0e-6
               endif
            enddo
          enddo
        enddo
      enddo
!
!-----Vertical interpolation starts
!
      write(*,*) 'Processing model time: ',sdate,stime
      write(*,*) 'Processing UTC time: ',sdate_utc,stime_utc
      write(*,*)
      write(*,*) 'Vertical interpolation start !!!!!'
      call flush(6)

      do row=1,nrows
        do col=1,ncols
          do lvl=1,nlayers
            if( ltopo ) then !vglvs_in is msl
              vglvs_in(col,row,lvl) &
                   = hght_in(iloc(col,row),jloc(col,row),lvl)
            else                                     !vglvs_in is agl
              vglvs_in(col,row,lvl) &
                 = hght_in(iloc(col,row),jloc(col,row),lvl) &
                       - sealevel(iloc(col,row),jloc(col,row))
            endif
!
! ---  The geoschem heights are at layer centers, find cell interfaces ---
!
            if(lvl .EQ. 1 ) then
              htint_in(col,row,lvl) = &
                2.0 * hght_in(iloc(col,row),jloc(col,row),lvl) &
                          - sealevel(iloc(col,row),jloc(col,row))
            else
              htint_in(col,row,lvl) = 2.0 * vglvs_in(col,row,lvl) & 
                                        - htint_in(col,row,lvl-1)
            endif
          enddo
        enddo
      enddo
!
! ---- get the Met at this hour ---
!
      if( lnetcdf_in ) then
         action = 'Finding date/time in NetCDF 3D met file.'
         this_tstep_met = ncf_get_tstep(iinmet,action,sdate,REAL(stime)*100.,             &
                       this_time_tflag,this_time_etflag,.TRUE.,.FALSE.)
      else
         this_tstep_met = this_tstep_met + 1
      endif
      this_time = stime/10000+1 ! [1-24]
!
! --- If topo info is available, vglvs_out is based on sea-level
!     instead of above ground as GEOS-Chem is based on above sea-level
!     (2014-03-17, jjung) ---
!
! --- Allocate hight_mcip to vglvs_out ---
!
      do row=1,nrows
        do col=1,ncols
          do lout=1,nlays
            if( ltopo ) then
              vglvs_out(col,row,lout) = zp_time(col,row,lout,this_time) &
                                                  + topo_m(col,row)
            else
              vglvs_out(col,row,lout) = zp_time(col,row,lout,this_time)
            endif
            zp_now(col,row,lout) = zp_time(col,row,lout,this_time)
          enddo
        enddo
      enddo

      do row=1,nrows
        do col=1,ncols
          do lout=1,nlays

            if( lout .EQ. 1 ) then
              htbeg = 0.
            else
              htbeg = vglvs_out(col,row,lout-1)
            endif

            htend = vglvs_out(col,row,lout)
            ibeg = 1
!
! --- Put values of the lowest layer of GC into the CAMx layers
!     which is even lower than the lowest layer of GC. It is
!     because there is nothing to match.
!
            do lin = 1,nlayers
              if (htint_in(col,row,lin) .GT. htbeg) then
                ibeg = lin
                goto 111
              endif
            enddo
 111        continue
            do lin=ibeg,nlayers
              if( htint_in(col,row,lin) .GT. htend ) then
                iend = lin
                goto 222
              endif
            enddo
 222        continue

            do spc=1,np
              sumconc = 0.0
              do lin=ibeg,iend
                if( ibeg .EQ. iend ) then
                  concout(col,row,lout,spc) = concin_m(col,row,lin,spc)
                  goto 555
                else
                  if( lin .EQ. ibeg ) then
                    sumconc = sumconc + (htint_in(col,row,lin)-htbeg) &
                                         * concin_m(col,row,lin,spc)
                  elseif( lin .EQ. iend ) then
                    sumconc = sumconc + (htend-htint_in(col,row,lin-1)) &
                                        * concin_m(col,row,lin,spc)
                  else
                    sumconc = sumconc + (htint_in(col,row,lin)-htint_in(col,row,lin-1)) * concin_m(col,row,lin,spc)
                  endif
                endif
              enddo             !lin loop
              concout(col,row,lout,spc) = sumconc / (htend - htbeg)
              if( lout .EQ. nlays ) then
                if( htend .GE. 0.5*(htint_in(col,row,iend)+ &
                                htint_in(col,row,iend-1))) then
                  tconcout(col,row,spc) = &
                   2.*(concin_m(col,row,iend+1,spc) &
                       - concin_m(col,row,iend,spc))/ &
                      (htint_in(col,row,iend+1) &
                       - htint_in(col,row,iend-1)) &
                     *(htend - 0.5* &
                      (htint_in(col,row,iend)+htint_in(col,row,iend-1))) &
                     + concin_m(col,row,iend,spc)
                else
                  tconcout(col,row,spc) = &
                   2.*(concin_m(col,row,iend,spc) &
                       - concin_m(col,row,iend-1,spc))/ &
                      (htint_in(col,row,iend) &
                       - htint_in(col,row,iend-2)) &
                     *(htend - 0.5* &
                      (htint_in(col,row,iend-1)+htint_in(col,row,iend-2))) &
                     + concin_m(col,row,iend-1,spc)
                endif
              endif
 555          continue
!   
!  --- Cap CAMx (cb05x) NA and PCL 
!      Sometimes GC overestimates too much near the coast lines. 
!
!cgwilson              if( camx_spec_names(spc) .EQ. 'PCL         ' ) then
!cgwilson                concout(col,row,lout,spc) = AMIN1(concout(col,row,lout,spc), 4.0)
!cgwilson                if (lout .EQ. nlays) then
!cgwilson                  tconcout(col,row,spc) = AMIN1(tconcout(col,row,spc), 4.0)
!cgwilson                endif
!cgwilson              endif
!cgwilson              if( camx_spec_names(spc) .EQ. 'NA          ' ) then
!cgwilson                concout(col,row,lout,spc) = AMIN1(concout(col,row,lout,spc), 2.5)
!cgwilson                if( lout .EQ. nlays ) then
!cgwilson                  tconcout(col,row,spc) = AMIN1(tconcout(col,row,spc), 2.5)
!cgwilson                endif
!cgwilson              endif
            enddo                   !spc loop
          enddo                     !lout loop
        enddo                       !col loop
      enddo                         !row loop

      write(*,*) 'Vertical interpolation end !!!!!'
      write(*,*)
      call flush(6)
!
!  ----- Write data to output BC file --
!
      if( lbc ) then
         cname = 'BOUNDARY    '
         action = 'Boundary Conditions'
         ncf_cur_tstep_bc = ncf_cur_tstep_bc + 1
         ncf_cur_tstep = ncf_cur_tstep_bc
         call wrt_conc_data(lnetcdf_out,ioutbc,fname_bc,action,cname,np,        &
                           ncols,nrows,nlays,nlays,xorig,yorig,xcell,ycell,     &
                           tstep,ncf_cur_tstep,sdate,REAL(stime),               &
                           camx_spec_names,rlat,rlon,topo_m,zp_now,concout)
      endif
!
!  ----- Write data to output Surface file --
!
      if( lsfc ) then
         cname = 'AVERAGE   '
         action = 'Surface Concentration'
         ncf_cur_tstep_sfc = ncf_cur_tstep_sfc + 1
         ncf_cur_tstep = ncf_cur_tstep_sfc
         call wrt_conc_data(lnetcdf_out,ioutsfc,fname_sfc,action,cname,np,      &
                           ncols,nrows,nlays,1,xorig,yorig,xcell,ycell,tstep,   &
                           ncf_cur_tstep,sdate,REAL(stime),camx_spec_names,     &
                           rlat,rlon,topo_m,zp_now,concout)
      endif
!
!  ----- Write data to output IC file --
!
      if( lic .AND. ncf_cur_tstep_ic .EQ. 0 ) then
         cname = 'AIRQUALITY'
         action = 'Inital Conditions'
         ncf_cur_tstep = 1
         call wrt_conc_data(lnetcdf_out,ioutic,fname_ic,action,cname,np,        &
                           ncols,nrows,nlays,nlays,xorig,yorig,xcell,ycell,     &
                           tstep,1,sdate,REAL(stime),camx_spec_names,rlat,      &
                           rlon,topo_m,zp_now,concout)
      endif
!
!  ----- Write data to output TC file --
!
      if( ltc ) then
         cname = 'TOPCONC   '
         action = 'Top Concentrations'
         ncf_cur_tstep_tc = ncf_cur_tstep_tc + 1
         ncf_cur_tstep = ncf_cur_tstep_tc
         call wrt_conc_data(lnetcdf_out,iouttc,fname_tc,action,cname,np,      &
                           ncols,nrows,1,1,xorig,yorig,xcell,ycell,tstep,     &
                           ncf_cur_tstep,sdate,REAL(stime),camx_spec_names,   &
                           rlat,rlon,topo_m,zp_now,tconcout)
         if( ncf_cur_tstep_tc .EQ. 1 ) then
            ierr = nf_inq_varid(iouttc,'layer',this_varid)
            if( ierr .EQ. NF_NOERR ) then
               ierr = nf_put_var_int(iouttc,this_varid,nlays+1)
            endif
         endif
      endif
!
!  ---- increase time stamp --
!
      call addtime( sdate_utc, stime_utc, tstep )
      call addtime( sdate, stime, tstep )
      ncf_cur_tstep_ic = ncf_cur_tstep_ic + 1
!
! ---- process next hour ---
!
      goto 333
!
! ---- all hours processed ---
!
 444  continue
!
! --- close output files ---
!
      if( lic .AND. .NOT. lnetcdf_out ) close (ioutic)
      if( lbc .AND. .NOT. lnetcdf_out ) close (ioutbc)
      if( ltc .AND. .NOT. lnetcdf_out ) close (iouttc)
      if( lsfc .AND. .NOT. lnetcdf_out ) close (ioutsfc)
      if( lic .AND. lnetcdf_out ) ierr = nf_close(ioutic)
      if( lbc .AND. lnetcdf_out ) ierr = nf_close(ioutbc)
      if( ltc .AND. lnetcdf_out ) ierr = nf_close(iouttc)
      if( lsfc .AND. lnetcdf_out ) ierr = nf_close(ioutsfc)
!
!  ---deallocated arrays ---
!
      deallocate( geos_tflag   )
      deallocate( time_minutes )
      deallocate( sealevel     )
      deallocate( rlat         )
      deallocate( rlon         )
      deallocate( iloc         )
      deallocate( jloc         )
      deallocate( topo_m       )
      deallocate( hght_in      )
      deallocate( p_geos       )

      deallocate( vglvs_in     )
      deallocate( vglvs_out    )
      deallocate( htint_in     )
      deallocate( tconcout     )
      deallocate( concin       )
      deallocate( zp_time      )
      deallocate( zp_now       )
      deallocate( concin_m     )
      deallocate( concout      )
      deallocate( spec         )
!
!  ---End of file reached
!
      write(*,'(/,2A)') 'Normal Completion of '//TRIM(version)
      goto 9999
!
!------------------------------------------------------------------------------
!  Error messages
!------------------------------------------------------------------------------
!
 7000 continue
      write(*,'(/,2A)') 'ERROR in ',TRIM(version)
      write(*,'(A)') TRIM(action)
      write(*,'(A)') 'Blank filename provided where filename is needed.'
      write(*,'(A,//)') 'File: ',TRIM(infilec)
      call flush(6)
      stop
!
 7001 continue
      write(*,'(/,2A)') 'ERROR in ',TRIM(version)
      write(*,'(A)') TRIM(action)
      write(*,'(A)') 'File does not exist.'
      write(*,'(A,//)') 'File: ',TRIM(infilec)
      call flush(6)
      stop
!
 7002 continue
      write(*,'(/,2A)') 'ERROR in ',TRIM(version)
      write(*,'(A)') TRIM(action)
      write(*,'(A,//)') 'File: ',TRIM(infilec)
      call flush(6)
      stop
!
 7003 continue
      write(*,'(/,2A)') 'ERROR in ',TRIM(version)
      write(*,'(A)') TRIM(action)
      write(*,'(A)') 'Output file already exists.'
      write(*,'(A,//)') 'File: ',TRIM(infilec)
      call flush(6)
      stop
!
 7004 continue
      write(*,'(/,2A)') 'ERROR in ',TRIM(version)
      write(*,'(A)') 'Cannot find data for date/time in GeosChem files.'
      this_year = INT( sdate_utc/100000 )
      sdate_utc = MOD( sdate_utc,100000 )
      write(*,*) 'Julian Date  : ',this_year*100000+sdate_utc
      call caldate( sdate_utc )
      write(*,*) 'Calendar Date: ',this_year*1000000+sdate_utc
      write(*,*) 'Time         : ',stime_utc
      call flush(6)
      stop

 9999 continue
      end program geos2camx

!ccccccccccccccccccccccccccccccccccccccccccccccccc
