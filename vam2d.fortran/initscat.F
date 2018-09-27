c!#   $Id: initscat.F,v 1.1 1997/02/10 21:12:25 leidner Exp $
c!#   $Log: initscat.F,v $
c!#   Revision 1.1  1997/02/10 21:12:25  leidner
c!#   Initial revision
c!#
      subroutine initscat

#include "vam.h"

      integer kset,kalter,kprint
      namelist /input/ kset,kalter,kprint

      kset   = 0
      kalter = 0
      kprint = 0
      read (IUNML,input,end=1520,err=1520)
      write (6,input)

      call jscsALL(IUNML,6,6,0,kset,kalter,kprint)
      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 1520 continue
      call errsea (520,6HINITSC)
      go to 999

  999 return
      end 
