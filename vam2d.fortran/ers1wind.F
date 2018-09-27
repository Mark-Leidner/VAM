c!#   $Id: ers1wind.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: ers1wind.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function ers1wind(icell,vsol,dsol,rsol)
c**********************************************************************
c English Name: ERS1 Winds 
c -------------
c
c Purpose: Retrieves ambiguous winds for current ERS1 observation
c -------- (see "iobuf common").
c
c Notes: 1) "iobuf" common must be set before calling this routine.
c ------
c
c Variable Description:
c ---------------------
c
c     Argument List
c     -------------
c
c     integer ers1wind,icell
c     real vsol(4),dsol(4),rsol(4)
c
c     ers1wind (O) - function return value: number of retrieved
c                    aliases.
c     icell (I) - cell number designating current ERS1 observation
c                 within a 19-cell row (see "oibuf common").
c     vsol (O) - ambiguous wind speeds (m/s).
c     dsol (O) - ambiguous wind directions (degrees).
c     rsol (O) - probability of correctness for ambiguous solutions
c                (fraction).
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: October 4, 1994 - created.
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      integer icell
      real vsol(4),dsol(4),rsol(4)
c
c     Local Variables
c     ---------------
c
      integer nalias
      integer*2 ms(3),pcd
      real s0(3),ai(3),ba(3),kp(3)
c
c     Common Block - "IO Buffer"
c     --------------------------
c
      integer*4 ibuf(MAXREC)
      real*4 buffer(475),rbuf(MAXREC-475)
      real*4 rtime(19),rlat(19),rlon(19),rspd(19),rdir(19)
      real*4 rthtf(19),razif(19),rsigf(19),rkpf(19)
      real*4 rthtm(19),razim(19),rsigm(19),rkpm(19)
      real*4 rthta(19),razia(19),rsiga(19),rkpa(19)
      real*4 qland(19),qgood(19),qrange(19),qamb(19)
      real*4 qmeth(19),qfore(19),qmid(19),qaft(19)

      common /iobuf/ rtime,rlat,rlon,rspd,rdir,
     &               rthtf,razif,rsigf,rkpf,
     &               rthtm,razim,rsigm,rkpm,
     &               rthta,razia,rsiga,rkpa,
     &               qland,qgood,qrange,qamb,
     &               qmeth,qfore,qmid,qaft,rbuf,ibuf

      ers1wind = 0
      if (icell .lt. 1 .or. icell .gt. 19) return
c
c     Retrieve ambibuous winds for current observation.
c     =================================================
c
      s0(1) = rsigf(icell)
      s0(2) = rsigm(icell)
      s0(3) = rsiga(icell)

      ai(1) = rthtf(icell)
      ai(2) = rthtm(icell)
      ai(3) = rthta(icell)

      ba(1) = razif(icell)
      ba(2) = razim(icell)
      ba(3) = razia(icell)

      kp(1) = rkpf(icell)
      kp(2) = rkpm(icell)
      kp(3) = rkpa(icell)

      ms(1) = 0
      ms(2) = 0
      ms(3) = 0

      pcd   = 0
      call winret(s0,ai,ba,kp,ms,pcd,vsol,dsol,rsol,nalias)

      ers1wind = nalias

      return
      end
