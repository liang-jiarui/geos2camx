c*** NCF_IODAT
c
      Module ncf_iodat
      include 'ncf_iodat.inc'
c
      Contains
c
c-----------------------------------------------------------------------
c    BEGIN SUBROUTINE NCF_ALLOC_TSTEP
c-----------------------------------------------------------------------
c
         subroutine ncf_alloc_tstep(num_tsteps)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
         implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     This routine allocates the array to store the NetCDF time step
c     varaibles
c
c     Input:
c       num_steps I number of time steps in this simulation
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
        integer :: num_tsteps
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
       if( .NOT. allocated(ncf_tflag) ) allocate( ncf_tflag(2,num_tsteps) )
       if( .NOT. allocated(ncf_etflag) ) allocate( ncf_etflag(2,num_tsteps) )
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c    END SUBROUTINE NCF_ALLOC_TSTEP
c-----------------------------------------------------------------------
c
      end Module
