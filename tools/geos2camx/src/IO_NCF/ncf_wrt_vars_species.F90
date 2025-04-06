!**** NCF_WRT_VARS_SPECIES
!
      subroutine ncf_wrt_vars_species(ierr,action,iounit,numcols,numrows, &
                           nspcs,spnames,spunits,splong,spdesc,spcoord )
      use ncf_iodat
      implicit none
!
!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!   This routine writes the variable definitions and descriptions to 
!    the NetCDF file
!
!      Argument description:
!       Inputs:
!           action   C  name of file to open
!           iounit   I  NetCDF file ID of file
!           numcols  I  number of columns in the grid
!           numrows  I  number of columns in the grid
!           numcols  I  number of columns in the grid
!           nspcs    I  number of species in the file
!           spnames  C  names of each species
!           spunits  C  units for each species
!           splong   C  long name for each species
!           spdesc   C  description of each species
!           spcoord  C  description of each species
!       Outputs:
!           ierr     I  error code
!
!-----------------------------------------------------------------------
!    LOG:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Include files:
!-----------------------------------------------------------------------
!
      include 'netcdf.inc'
!
!-----------------------------------------------------------------------
!    Argument declarations:
!-----------------------------------------------------------------------
!
      integer       ierr
      character*(*) action
      integer       iounit
      integer       numcols
      integer       numrows
      integer       numlays
      integer       nspcs
      character*(*) spnames(nspcs)
      character*(*) spunits(nspcs)
      character*(*) splong(nspcs)
      character*(*) spdesc(nspcs)
      character*(*) spcoord(nspcs)
!
!-----------------------------------------------------------------------
!    External functions:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Local variables:
!-----------------------------------------------------------------------
!
      character*14 this_species
      integer      ispc, spec_dimid(4), spec_chunk(4), spec_varid
!
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------
!
      spec_chunk(1) = INT(REAL(numcols)/NCF_CHUNK_SIZE_VAR_X)
      if( spec_chunk(1) .GT. numcols ) goto 7002
      spec_chunk(2) = INT(REAL(numrows)/NCF_CHUNK_SIZE_VAR_Y)
      if( spec_chunk(2) .GT. numrows ) goto 7003
      spec_chunk(3) = 1
      spec_chunk(4) = 1
!
      spec_dimid(1) = ncf_col_dimid
      spec_dimid(2) = ncf_row_dimid
      spec_dimid(3) = ncf_lay_dimid
      spec_dimid(4) = ncf_tstep_dimid
!
      do ispc=1,nspcs
!
!  --- define everything for this species ---
!
        this_species = TRIM(spnames(ispc))
        ncf_species_units = spunits(ispc)
        ncf_species_long_name = splong(ispc)
        ncf_species_var_desc = spdesc(ispc)
        ncf_species_coordinates = spcoord(ispc)
!
!  --- define the variable ---
!
        ierr = nf_def_var(iounit,TRIM(this_species),           &
                          NF_FLOAT, 4, spec_dimid, spec_varid)
        if( ierr .NE. NF_NOERR ) goto 7000
!
!  --- add the attributes ---
!
        ierr = nf_put_att_text(iounit,spec_varid,'long_name',   &
                           LEN_TRIM(ncf_species_long_name),     &
                             TRIM(ncf_species_long_name) )
        if( ierr .NE. NF_NOERR ) goto 7000

        ierr = nf_put_att_text(iounit,spec_varid,'units',      &
                        LEN_TRIM(ncf_species_units),           &
                            TRIM(ncf_species_units) )
        if( ierr .NE. NF_NOERR ) goto 7000

        ierr = nf_put_att_text(iounit,spec_varid,'var_desc', &
                       LEN_TRIM(ncf_species_var_desc),       &
                          TRIM(ncf_species_var_desc) )
        if( ierr .NE. NF_NOERR ) goto 7000

        ierr = nf_put_att_text(iounit,spec_varid,'coordinates', &
                       LEN_TRIM(ncf_species_coordinates),       &
                             TRIM(ncf_species_coordinates) )
        if( ierr .NE. NF_NOERR ) goto 7000
!
!  --- chunkers go here ... Irie! ----
!
#ifdef CHUNK
        if( ncf_compress ) then
          ierr = nf_def_var_chunking(iounit, spec_varid, &
                                             NF_CHUNKED, spec_chunk)
          if( ierr .NE. NF_NOERR ) goto 7001
!
          ierr = nf_def_var_deflate(iounit, spec_varid, NCF_SHUFFLE,  &
                                      NCF_DEFLATE, NCF_DEFLATE_LEVEL )
          if( ierr .NE. NF_NOERR ) goto 7001
        endif
#endif
!
      enddo
!
      ierr = 1
      goto 9999
!
!-----------------------------------------------------------------------
!    Error messages:
!-----------------------------------------------------------------------
!
 7000 continue
      write(*,'(//,A)') 'ERROR in NCF_WRT_VARS_SPECIES:'
      write(*,'(A)') TRIM(action)
      write(*,'(2A)') 'Cannot create file variable for species: ', &
                                                TRIM(this_species)
      ierr = -1
      goto 9999
!
 7001 continue
      write(*,'(//,A)') 'ERROR in NCF_WRT_VARS_SPECIES:'
      write(*,'(A)') TRIM(action)
      write(*,'(2A)') 'Cannot set chunk parameters for species: ', &
                                              TRIM(this_species)
      ierr = -1
      goto 9999
!
 7002 continue
      write(*,'(//,A)') 'ERROR in NCF_IODAT.INC:'
      write(*,'(A)') TRIM(action)
      write(*,'(A)') 'Cannot set chunk parameters for this file.'
      write(*,'(2A,/,A)') 'The NCF_CHUNK_SIZE_VAR_X parameter causes the ', &
           'chunk value to be larger','than the number of columns.'
      write(*,'(A,I3,A,I4)') 'Number of columns: ',numcols
      ierr = -1
      goto 9999
!
 7003 continue
      write(*,'(//,A)') 'ERROR in NCF_IODAT.INC:'
      write(*,'(A)') TRIM(action)
      write(*,'(A)') 'Cannot set chunk parameters for this file.'
      write(*,'(2A,/,A)') 'The NCF_CHUNK_SIZE_VAR_Y parameter causes the ', &
           'chunk value to be larger','than the number of rows.'
      write(*,'(A,I3,A,I4)') 'Number of rows: ',numrows
      ierr = -1
      goto 9999
!
!-----------------------------------------------------------------------
!    Return point:
!-----------------------------------------------------------------------
!
 9999 continue
      return
      end
