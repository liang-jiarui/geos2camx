c**** NCF_SET_GLOBAL
c
      subroutine ncf_set_global(inname,ncols,nrows,nlays,p_alp,p_bet,
     &                      p_gam,xcent,ycent,xorig,yorig,xcell,ycell,
     &                       itzon,iutmin,cnote,tstep,nsteps,begin_date,
     &                          begin_time,ending_date,ending_time,nspcs)
      implicit none
c
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine sets the global file attributes for the NetCDF file
c
c      Argument description:
c       Inputs:
c         ncols       I number of columns
c         nrows       I number of rows
c         nlays       I number of layers
c         p_alp       R 1st map projection parameter
c         p_bet       R 2nd map projection parameter
c         p_gam       R 3rd map projection parameter
c         xcent       R Longitude of coordinate system center
c         ycent       R Latitude of coordinate system center
c         xorig       R X-origin of grid
c         yorig       R Y-origin of grid
c         xcell       R X-dimension of cell
c         ycell       R Y-dimension of cell
c         itzon       I time zone
c         iutmin      I UTM zone
c         cnote       C run message
c         inname      C filetype of this kind of file
c         tstep       I time step in hours
c         nsteps      I number of time steps
c         begin_date  I model begin date (YYJJJ)
c         begin_time  R model begin time
c         ending_date I model end date (YYJJJ)
c         ending_time R model end time
c         nspcs       I number of species in this file
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
      include 'ncf_iodat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer      ncols
      integer      nrows
      integer      nlays
      real         p_alp
      real         p_bet
      real         p_gam
      real         xcent
      real         ycent
      real         xorig
      real         yorig
      real         xcell
      real         ycell
      integer      itzon
      integer      iutmin
      character*60 cnote
      character*10 inname
      integer      tstep
      integer      nsteps
      integer      begin_date
      real         begin_time
      integer      ending_date
      real         ending_time
      integer      nspcs
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      real xorg_grid, yorg_grid, delx_grid, dely_grid
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- get current date/time ---
c
      call getime(ncf_cdate,ncf_ctime)
      ncf_wdate = ncf_cdate
      ncf_wtime = ncf_ctime
c
c --- get current date/time ---
c
      ncf_sdate  = begin_date
      ncf_stime  = begin_time
      ncf_tstep  = tstep * 10000
      ncf_nsteps = nsteps
      ncf_nlays  = nlays
c
c --- get current date/time ---
c
      ncf_nvars = nspcs + 1
      ncf_ncols = ncols
      ncf_nrows = nrows
      if (p_alp.gt.0.) then
        ncf_p_alp = min(p_alp,p_bet)
        ncf_p_bet = max(p_alp,p_bet)
      else
        ncf_p_alp = max(p_alp,p_bet)
        ncf_p_bet = min(p_alp,p_bet)
      endif
      ncf_p_gam = p_gam
      ncf_xcent = xcent
      ncf_ycent = ycent
c
c --- get current date/time ---
c
      ncf_xorig = xorig
      ncf_yorig = yorig
      ncf_xcell = xcell
      ncf_ycell = ycell
      if( llatlon ) then
         ncf_cproj = 0
         ncf_gdtyp = 1
      else if( lutm ) then
         ncf_cproj = 1
         ncf_gdtyp = 5
      else if( lambrt ) then
         ncf_cproj = 2
         ncf_gdtyp = 2
      else if( lrpolar ) then
         ncf_cproj = 3
         ncf_gdtyp = 4
      else if( lpolar ) then
         ncf_cproj = 4
         ncf_gdtyp = 6
      else if( lmerc ) then
         ncf_cproj = 5
         ncf_gdtyp = 7
      endif
c
c --- get current date/time ---
c
      ncf_iutm = iutmin
      ncf_istag = 1
      ncf_ftype = 1
      ncf_cproj = 2
      ncf_nthik = 1
      ncf_itzon = itzon
c
      ncf_name = '              '
      ncf_name = inname
      ncf_note = cnote
      ncf_filedesc = inname
      ncf_gdnam = 'GEOS2CAMx_v4'
      ncf_upnam = 'GEOS2CAMx_v4'
      ncf_history = "Created by GEOS2CAMx_v4.0"
c
c --- get current date/time ---
c
      ncf_vgtyp = 6
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
c
c --- get current date/time ---
c
      return
      end
 
