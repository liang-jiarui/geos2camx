c**** NCF_WRT_GLOBAL
c
      subroutine ncf_wrt_global(ierr,action,iounit,nlayer,spcnam,nspcs)
      implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine writes the Global attributes to the NetCDF file
c
c      Argument description:
c       Inputs:
c           action    C  name of file to open
c           iounit    I  NetCDF file ID of file
c           nspcs     I  numver of species in file
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
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer       nlayer
      character*12  spcnam(*)
      character*(*) action
      integer       iounit
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
      character*80             ioapi, exec_id
      character*((nspcs+1)*16) string
      character*16             varname(1000)
      integer                  ierr, i, string_length
c
      data varname(1:9)
     &                  /'X               ','Y               ',
     &                   'layer           ','TFLAG           ',
     &                   'ETFLAG          ','longitude       ',
     &                   'latitude        ','topo            ',
     &                   'z               '/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ioapi = "IOAPI-CAMx"
      exec_id = "GEOS2CAMx_V4.0"
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'SDATE', NF_INT,
     &                                                    1, ncf_sdate)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'STIME', NF_INT,
     &                                                    1, ncf_stime)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'TSTEP', NF_INT,
     &                                                    1, ncf_tstep)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'NSTEPS', NF_INT,
     &                                                 1, ncf_nsteps)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'NCOLS', NF_INT,
     &                                                 1, ncf_ncols)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'NROWS', NF_INT,
     &                                                 1, ncf_nrows)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'NLAYS', NF_INT,
     &                                                 1, ncf_nlays)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'NVARS', NF_INT,
     &                                                 1, ncf_nvars)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_double(iounit, NF_GLOBAL, 'P_ALP', NF_DOUBLE,
     &                                           1, DBLE(ncf_p_alp) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_double(iounit, NF_GLOBAL, 'P_BET', NF_DOUBLE,
     &                                           1, DBLE(ncf_p_bet) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_double(iounit, NF_GLOBAL, 'P_GAM', NF_DOUBLE,
     &                                           1, DBLE(ncf_p_gam) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_double(iounit, NF_GLOBAL, 'XCENT', NF_DOUBLE,
     &                                           1, DBLE(ncf_xcent) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_double(iounit, NF_GLOBAL, 'YCENT', NF_DOUBLE,
     &                                           1, DBLE(ncf_ycent) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_double(iounit, NF_GLOBAL, 'XORIG', NF_DOUBLE, 
     &                                           1, DBLE(ncf_xorig) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_double(iounit, NF_GLOBAL, 'YORIG', NF_DOUBLE,
     &                                           1, DBLE(ncf_yorig) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_double(iounit, NF_GLOBAL, 'XCELL', NF_DOUBLE,
     &                                           1, DBLE(ncf_xcell) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_double(iounit, NF_GLOBAL, 'YCELL', NF_DOUBLE,
     &                                           1, DBLE(ncf_ycell) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_double(iounit, NF_GLOBAL, 'XCENT', NF_DOUBLE,
     &                                           1, DBLE(ncf_xcent) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_double(iounit, NF_GLOBAL, 'YCENT', NF_DOUBLE,
     &                                           1, DBLE(ncf_ycent) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_double(iounit, NF_GLOBAL, 'XORIG', NF_DOUBLE, 
     &                                           1, DBLE(ncf_xorig) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_double(iounit, NF_GLOBAL, 'YORIG', NF_DOUBLE,
     &                                           1, DBLE(ncf_yorig) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_double(iounit, NF_GLOBAL, 'XCELL', NF_DOUBLE,
     &                                           1, DBLE(ncf_xcell) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_double(iounit, NF_GLOBAL, 'YCELL', NF_DOUBLE,
     &                                           1, DBLE(ncf_ycell) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'CPROJ', NF_INT,
     &                                                   1, ncf_cproj)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'GDTYP', NF_INT,
     &                                                   1, ncf_gdtyp)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'IUTM', NF_INT,
     &                                                   1, ncf_iutm)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'ISTAG', NF_INT,
     &                                                   1, ncf_istag)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'ITZON', NF_INT,
     &                                                    1, ncf_itzon)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit, NF_GLOBAL, 'CAMx_NAME',
     &                           LEN_TRIM(ncf_name), TRIM(ncf_name) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit, NF_GLOBAL, 'NOTE',
     &                          LEN_TRIM(ncf_note), TRIM(ncf_note) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit, NF_GLOBAL, 'FILEDESC', 
     &                  LEN_TRIM(ncf_filedesc), TRIM(ncf_filedesc) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'FTYPE', NF_INT,
     &                                                    1, ncf_ftype)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'NTHIK', NF_INT,
     &                                                 1, ncf_nthik)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'VGTYP', NF_INT,
     &                                                    1, ncf_vgtyp)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_real(iounit, NF_GLOBAL, 'VGTOP', NF_FLOAT, 
     &                                                    1, ncf_vgtop)
      if( ierr .NE. NF_NOERR ) goto 7000
c
cgwilson      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'VGLVLS', NF_INT,
cgwilson     &                                                nlayer+1, ncf_vglvls)
cgwilson      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit, NF_GLOBAL, 'GDNAM',
     &                         LEN_TRIM(ncf_gdnam), TRIM(ncf_gdnam) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit, NF_GLOBAL, 'UPNAM', 
     &                        LEN_TRIM(ncf_upnam), TRIM(ncf_upnam) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit, NF_GLOBAL, 'HISTORY', 
     &                            len_trim(ncf_history), ncf_history)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit, NF_GLOBAL, 'IOAPI_VERSION',
     &                        len_trim(ioapi), ioapi(:len_trim(ioapi)) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit, NF_GLOBAL, 'EXEC_ID',
     &                 len_trim(exec_id), exec_id(:len_trim(exec_id)) )
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'CDATE', NF_INT,
     &                                                    1, ncf_cdate)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'CTIME', NF_INT,
     &                                                    1, ncf_ctime)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'WDATE', NF_INT,
     &                                                    1, ncf_wdate)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_int(iounit, NF_GLOBAL, 'WTIME', NF_INT,
     &                                                    1, ncf_wtime)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      varname(1) = 'z               '
      do i=1,nspcs
        call jstlft( spcnam(i) ) 
        varname(i+1) = spcnam(i)
      enddo
      string = varname(1)
      string_length = 16
      do i=2,nspcs+1
        string_length = string_length + 16
        string = string(1:(i-1)*16) // TRIM(varname(i))
      enddo
      ierr = nf_put_att_text(iounit, NF_GLOBAL, 'VAR-LIST',
     &                              string_length, string(:string_length) )
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
      write(*,'(//,a)') 'ERROR in NCF_WRT_GLOBAL:'
      write(*,'(A)') TRIM(action)
      write(*,'(A)') 'Cannot write global atttributes to file.'
      stop
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
