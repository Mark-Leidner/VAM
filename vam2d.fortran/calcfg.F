c!#   $Id: calcfg.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: calcfg.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#

c     ------------------------------------------------------------------

      subroutine calcfg(N, x, f, g)

      real x(N), f, g(N)

      call sscgr (N, x, f, g)

      return
      end
