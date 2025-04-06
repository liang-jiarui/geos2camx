subroutine ncf_set_compress_flag()
use ncf_iodat
!
!     This routine just sets the compression flag for NetCDF based on the
!     the compiler directive. It needs to be a F90 routine so the preprocssor
!
!     Modifications:
!        none
!
!     Input arguments:
!        none
!
!     Output arguments:
!        none
!
!     Routines called:
!        none
!
!
!     Called by:
!        READNML
!
implicit none

ncf_compress = .TRUE.
#ifndef CHUNK
ncf_compress = .FALSE.
#endif

return
end

