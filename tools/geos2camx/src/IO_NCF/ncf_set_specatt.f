c**** NCF_SET_SPECATT
c
      subroutine ncf_set_specatt(nspecs,spcnam,lgas,
     &               spec_units,spec_long_name,spec_desc,spec_coords)
      implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine sets the species varaible attributes for the average
c   concentration output NetCDF file
c
c      Argument description:
c       Inputs:
c            nspecs         I number of species
c            spcnam         C species names
c            lgas           L true if species is gas species
c       Outputs:
c            spec_units     C array of units for this each species
c            spec_long_name C array of "long names" for each each species
c            spec_desc      C array of desciption for this each species
c            spec_coords    C array of coordinates for this each species
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
       integer      nspecs
       logical      lgas(*)
       character*10 spcnam(*)
       character*20 spec_units(*)
       character*20 spec_long_name(*)
       character*60 spec_desc(*)
       character*60 spec_coords(*)
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer l
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      do l = 1,nspecs
        if( lgas(l) ) then
           spec_units(l) = "ppmv"
        else
           spec_units(l) = "micrograms m-3"
        endif
        spec_long_name(l) = spcnam(l)
        spec_desc(l) = TRIM(spcnam(l))//" air concentration"
        spec_coords(l) = "latitude longitude"
      enddo
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
      return
      end
 
