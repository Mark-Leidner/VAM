c!#   $Id: getdat.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: getdat.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
C***********************************************************************
      SUBROUTINE GETDAT(INDATE,INTIME,NDATE)
C***********************************************************************
C
      INTEGER NDATE(4)
C
      IYEAR = INDATE / 10000
      IY0000 = IYEAR * 10000
      IMMDD = INDATE - IY0000
      IMONTH = IMMDD / 100
      IM00   = IMONTH * 100
      IDAY = IMMDD - IM00
      IHOUR = INTIME / 10000
C
      NDATE(1) = IMONTH
      NDATE(2) = IDAY
      NDATE(3) = IYEAR
      NDATE(4) = IHOUR
C
      RETURN
      END
