      integer function addday(idate)
      implicit none
      integer idate,iyr,idy
      iyr = idate/1000
      idy = idate - iyr*1000
      if( (MOD(iyr,4) .EQ. 0 .AND. idy .EQ. 366) .OR.
     &          (MOD(iyr,4) .NE. 0 .AND. idy .EQ. 365) ) then
        iyr = iyr + 1
        addday = iyr*1000 + 1
      else
        addday = idate + 1
      endif
      idate = addday
      end
