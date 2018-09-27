c!#   $Id: jxfer.F,v 1.3 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: jxfer.F,v $
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
      subroutine Jxfer(N, x)

      real x(N)

#include "vam.h"
#include "funct.h"

      logical lfirst,Lxsame
      save lfirst
      data lfirst/.true./

c     Initialization
      if (lfirst) then
         lfirst = .false.
         LJ = .false.
         LdJdx = .false.
         nacall = 0
         nfcall = 0
         ngcall = 0
         Lxsame = .false.
      else
         Lxsame = .true.
         do 100 i=1, N
            Lxsame = Lxsame .and. x(i) .eq. xlast(i)
 100     continue 
      endif

c     We always calculate both the function and its gradient.
c     If x is unchanged and these have been calculated return.

      if (Lxsame .and. LJ .and. LdJdx) return

      nacall = nacall + 1
c$$$      write (*, *) ' Actual evaluation ', nacall, ' begins.'

c     perform calculation
      call SSCGR(N, X, J, dJdx)

      LJ = .true.
      LdJdx = .true.

c     Save current x.
      do 200 i=1,N
         xlast(i) = x(i)
 200  continue 

      return
      end
