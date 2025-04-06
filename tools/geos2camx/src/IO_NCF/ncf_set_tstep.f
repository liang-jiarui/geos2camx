c**** NCF_SET_TSTEP
c
      subroutine ncf_set_tstep(time_skip,begin_date,begin_time,numsteps)
      use ncf_iodat
      implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine sets the global file attributes for the NetCDF file
c
c      Argument description:
c       Inputs:
c         time_skip   I hours to skip between records
c         begin_date  I model begin date (YYJJJ)
c         begin_time  R model begin time
c         numsteps    I number of timesteps in the file
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
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer time_skip
      integer begin_date
      real    begin_time
      integer numsteps
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer this_date, date_now, this_tstep, i
      real    time_now
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- allocate the arrays ---
c
      call ncf_alloc_tstep(numsteps)
c
c  --- now walk through time again to fill the NetCDF variable ---
c
      date_now = begin_date
      time_now = begin_time
c
c  --- load first time step into arrays ---
c
      ncf_tflag(1,1) = date_now
      ncf_tflag(2,1) = INT(time_now*10000)
c
c  --- loop over remaining number of steps and calculate the
c      tmestamps for this hour ---
c
      do this_tstep=2,numsteps
         time_now = time_now + time_skip
         this_date = date_now
         if( time_now .GE. 24. ) then
            time_now = time_now - 24.
            this_date = this_date + 1
            if( MOD(this_date,1000) .GT. 365 ) then
              if( MOD(INT(this_date/1000),4) .EQ. 0 ) then
              if( MOD(this_date,1000) .EQ. 367 )
     &                   this_date = (INT(this_date/1000)+1)*1000 + 1
              else
                 this_date = (INT(this_date/1000)+1)*1000 + 1
              endif
            endif
         endif
         ncf_tflag(1,this_tstep) = this_date
         ncf_tflag(2,this_tstep) = INT(time_now*10000)
         ncf_etflag(1,this_tstep-1) = ncf_tflag(1,this_tstep)
         ncf_etflag(2,this_tstep-1) = ncf_tflag(2,this_tstep)
      enddo
c
c  --- finish with the ending time
c
      time_now = time_now + time_skip
      this_date = date_now
      if( time_now .GE. 24. ) then
         time_now = time_now - 24.
         this_date = this_date + 1
         if( MOD(this_date,1000) .GT. 365 ) then
           if( MOD(INT(this_date/1000),4) .EQ. 0 ) then
              if( MOD(this_date,1000) .EQ. 367 )
     &                      this_date = (INT(this_date/1000)+1)*1000 + 1
           else
              this_date = (INT(this_date/1000)+1)*1000 + 1
           endif
         endif
      endif
      ncf_etflag(1,numsteps) = this_date
      ncf_etflag(2,numsteps) = INT(time_now*10000)
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
 
