c!#   $Id: funct.h,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: funct.h,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision (filename originally funct.blk)
c!#

c     The functional J, its derivatives with respect to x 
c     and flags indicating if these have been evaluated for 
c     the copy of x currently stored here.
c     All these are scaled.
c     Also the total number of function, gradient and actual
c     calls are maintained.

      real J, dJdx, xlast
      logical LJ, LdJdx
      integer nfcall, ngcall, nacall

      common /funct/ J,xlast(2*MAXLONS*MAXLATS),dJdx(2*MAXLONS*MAXLATS),
     &      LJ, LdJdx, nfcall, ngcall, nacall
