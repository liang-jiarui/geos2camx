      subroutine map_chem
     &           ( this_timestep, np, ibeg, jbeg, iend, jend, nlayers, inunit,
     &             e6unit, fname_spcmap, height, p_geos, concin, sealevel, 
     &             camx_spec_names)

!
!******************************************************************************
!
!  Calls (1) READ_CHEM
!        (2) READ_BPCH2
!        (3) AIRQNT
!
!  Called by (1) MAIN
!******************************************************************************

      implicit none
!
! --- Program specific include files ---
!
#     include "geoschem.com"
#     include "netcdf.inc"

      integer, intent(in)  :: this_timestep
      integer, intent(in)  :: np
      integer, intent(in)  :: ibeg, jbeg, iend, jend, nlayers
      integer, intent(in)  :: inunit   ! input unit number for GEOS-Chem file
      integer, intent(in)  :: e6unit   ! input unit number for species mapping
      real*4,  intent(out) :: height( iend, jend, nlayers )
      real*4,  intent(out) :: p_geos( iend, jend, nlayers )
      real*4,  intent(out) :: concin( iend, jend, nlayers, np )
      real*4               :: sealevel( iend, jend )
!
! --- Fundamental Constants: ( Source: CRC76, pp. 1-1 to 1-6) ---
!
      REAL        AVO ! Avogadro's Constant [ number/mol ]
      PARAMETER ( AVO = 6.0221367 E23 )
!
! ---- Local variables ---
!
      logical, save      :: first = .TRUE.
      integer            :: ii, jj, ll, lidx
      integer            :: c,  r, col, row, lvl, spc
      integer            :: i,  j,  l,   n,  ios, m, k, ierr
      integer            :: idx, camx_id, geos_id
      integer, save      :: n_camx_spec, n_geos_spec
      integer, save      :: id_geos(500), id_camx(500)
      integer, save      :: map_idx(500,99) 

      real*4, save       :: map_fac(500,99) 
      real*4             :: fac
      real*4             :: camx_spec_mw(500)
      character(len=1), save   :: ga_camx(500)
      character(len=16)  :: pname
      character(len=12)  :: camx_spec_names(500)  !et
      character(len=12)  :: geos_spec_names(500)  !et
      character(len=256) :: fname_spcmap

      real*4, allocatable :: con( :, :, :, : )
      real*4, allocatable :: airtemp( :, :, : )
      real*4, allocatable :: psurf( :, : )
      real*4, allocatable :: aird( :, :, : )
      real*4, allocatable :: array( :, :, : )
      real*4, allocatable :: pedge( :, :, : )

      real*4, allocatable :: tmpconc( :, :, : )
   
      integer ispec
      integer data_start(4)
      integer data_count(4)
      integer this_varid
      character*(24) this_varname

      logical lgas(500)

      allocate(tmpconc(iend,jend,nlayers))
      allocate(airtemp(iend,jend,nlayers))
      allocate(psurf(iend,jend))
      allocate(aird(iend,jend,nlayers))
      allocate(array(iend,jend,nlayers))
      allocate(pedge(iend,jend,nlayers+1))

      if ( first ) call read_chem(n_camx_spec,n_geos_spec,id_camx,
     &                             ga_camx,camx_spec_mw,id_geos,
     &                             map_idx,map_fac,camx_spec_names,
     &                             geos_spec_names,lgas,e6unit,
     &                             fname_spcmap )
      first = .FALSE.

      allocate(con(iend,jend,nlayers,n_geos_spec), stat=ios)
!
! ---- loop over model species and read concentrations from NetCDF file ---
!
      data_start(1) = 1
      data_count(1) = iend
      data_start(2) = 1
      data_count(2) = jend
      data_start(3) = 1
      data_count(3) = nlayers
      data_start(4) = this_timestep
      data_count(4) = 1
      do ispec=1,n_geos_spec
        this_varname = 'SpeciesConc_'//geos_spec_names(ispec)
        ierr = nf_inq_varid(inunit,this_varname,this_varid)
        if( ierr .NE. NF_NOERR ) cycle
        ierr = nf_get_vara_real(inunit,this_varid,data_start,
     &                                           data_count,tmpconc)
        if( ierr .NE. NF_NOERR ) then
           write(*,'(/,A)') 'ERROR in MAP_CHEM: '  
           write(*,'(3A,/)') 'Cannot read data for variable for ',
     &                   'model species: ',TRIM(geos_spec_names(ispec))
           stop
        endif
        con(:,:,:,ispec) = tmpconc
      enddo
!
! ---- load surface pressue data into the array ---
!
      ierr = nf_inq_varid(inunit,'Met_PMIDDRY',this_varid)
      if( ierr .NE. NF_NOERR ) then
         write(*,'(/,A)') 'ERROR in MAP_CHEM: '  
         write(*,'(2A,/)') 'Cannot find variable for pressure fields: Met_PMIDDRY',
     &                                                       ' in GeosChem file.'
         stop
      endif
      ierr = nf_get_vara_real(inunit,this_varid,data_start,
     &                                           data_count,tmpconc)
      if( ierr .NE. NF_NOERR ) then
         write(*,'(/,A)') 'ERROR in MAP_CHEM: '  
         write(*,'(2A,/)') 'Cannot read data for variable for ',
     &                            'pressure fields: Met_PMIDDRY'
         stop
      endif
      psurf(:,:) = tmpconc(:,:,1)
!
!  --- load air temperature (K) into array ---
!
      ierr = nf_inq_varid(inunit,'Met_T',this_varid)
      if( ierr .NE. NF_NOERR ) then
         write(*,'(/,A)') 'ERROR in MAP_CHEM: '  
         write(*,'(2A,/)') 'Cannot find variable for air temperature: Met_T',
     &                                                    ' in GeosChem file.'
         stop
      endif
      ierr = nf_get_vara_real(inunit,this_varid,data_start,
     &                                       data_count,airtemp)
      if( ierr .NE. NF_NOERR ) then
         write(*,'(/,A)') 'ERROR in MAP_CHEM: '  
         write(*,'(2A,/)') 'Cannot read data for variable for ',
     &                            'air temperature: Met_T'
         stop
      endif
!
!  --- calculate the AIRMOLDEN from other variables --
!
      aird(:,:,:) = tmpconc(:,:,:) * 100. / 8.3145 / airtemp(:,:,:)
!
!  --- calculate heights ---
!
      modelname = 'GEOS5'
      call airqnt(ibeg,jbeg,iend,jend,nlayers,psurf,airtemp,height,p_geos,sealevel)
! 
! ---- Apply factor to the IJAVG ---
!
      do i=1,n_camx_spec
        camx_id = id_camx(i)
        do j=1,99 ! allow more right-hand side terms - bkoo (3/25/09)
          if( map_idx(i,j) > 0 ) then
            idx = map_idx(camx_id,j)
            fac = map_fac(camx_id,j)
            concin(:,:,:,camx_id) = concin(:,:,:,camx_id)
     &                              + con(:,:,:,idx)*fac
          endif
        enddo
      enddo
      if( allocated(con) ) deallocate(con)
c        
c-----For unit conversion (GAS in ppmv and Aerosol in ug/m3)
c         
      do n=1,n_camx_spec
        camx_id = id_camx(n)
!
! ---- gas phase ---
!
        if( ga_camx(camx_id) == 'G' ) then
          fac = 1.E+6
          concin(:,:,:,camx_id) = concin(:,:,:,camx_id) * fac
!
! ---- aerosol ---
!
        elseif( ga_camx(camx_id) == 'A' ) then
          do l=1,nlayers
            do j=1,jend
              do i=1,iend
                fac = aird(i,j,l)*camx_spec_mw(camx_id)*1.0e+6
                concin(i,j,l,camx_id) = concin(i,j,l,camx_id) * fac
              enddo
            enddo
          enddo
        else
          stop 'error Undefine species'
          write(*,'(/,A)') 'ERROR in MAP_CHEM: '  
          write(*,'(A,/)') 'Invalid species found: ',TRIM(ga_camx(camx_id))
          stop
        endif
      enddo

      if( allocated(airtemp)  ) deallocate( airtemp  )
      if( allocated(psurf)    ) deallocate( psurf    )
      if( allocated(aird)     ) deallocate( aird     )
      if( allocated(array)    ) deallocate( array    )
      if( allocated(pedge)    ) deallocate( pedge    )

      end subroutine map_chem
