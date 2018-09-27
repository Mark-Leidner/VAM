c!#   $Id: yymmdd2julian.F,v 1.4 1998/05/15 19:50:59 stanr Exp $
c!#   $Log: yymmdd2julian.F,v $
c!#   Revision 1.4  1998/05/15 19:50:59  stanr
c!#   type declaration for external function leapyr
c!#
c!#   Revision 1.3  1997/06/05 16:33:56  leidner
c!#   Added check for leap years
c!#
c!#	Revision 1.2  1997/04/17  16:56:54  leidner
c!#	now set up for non-leap years
c!#
c!#	Revision 1.1  1997/02/21  23:45:10  leidner
c!#	Initial revision
c!#
**************************************************************
      subroutine yymmdd2julian(idate,julday)
*************************************************************
c
c     This subroutine converts yymmdd to Julian day
c
      external leapyr
      logical leapyr

      dimension mo(12)
      data mo /31,28,31,30,31,30,31,31,30,31,30,31/

      julday = 0
c
c     Break yymmdd into Year Month and Day
c
      iyy = int(idate/10000+.001)
      imm = int(idate/100-iyy*100+.001)
      idd = int(Idate-iyy*10000+.001-imm*100+.001)

c
c     Check for leap year (setup for years between 1951-2050)
c
      if (iyy .gt. 50 .and. iyy .le. 99) then
        icentury = 1900
      else if (iyy .ge. 0 .and. iyy .le. 50) then
        icentury = 2000
      else
        write(*,*) 'leapyr check not set up for years outside',
     &      ' 1951-2050'
        stop 1999
      endif
      if (leapyr(icentury+iyy)) mo(2) = 29

c
c     Sum days over the months up to imm
c
      do 1 m=1,imm-1
      julday=julday+mo(m)
1     continue

c
c     Add the days of the month imm to the sum
c
      julday=julday+idd

      return
      end
