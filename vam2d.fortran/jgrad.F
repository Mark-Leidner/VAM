c!#   $Id: jgrad.F,v 1.3 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: jgrad.F,v $
c!#   Revision 1.3  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.2  1997/02/10  19:34:17  leidner
c!#	changed single-quoted include statements to double-quoted
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#

c     ------------------------------------------------------------------
      subroutine Jgrad(N, x, g)

      real x(N), g(N)

#include "vam.h"
#include "funct.h"

c     perform calculation
      call Jxfer(N, x)
      ngcall = ngcall + 1

c     move results back
      do 20 i=1,N
         g(i)=dJdx(i)
 20   continue 

c$$$      write (*, *) ' Gradient evaluation ', ngcall, ' complete, J = ', J

      return
      end
