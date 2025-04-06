!     SUBROUTINE addtime
!
      subroutine addtime (sdate, stime, tstep)

      integer         addday      ! define external function

      integer         sdate       ! starting date,    format YYYYJJJ
      integer         stime       ! starting time,    format HHMMSS
      integer         tstep       ! user defined time step

      stime = stime + tstep
      if( stime .GE. 24) then
        stime = stime - 24
        sdate = addday(sdate)
      endif

      return
      end
