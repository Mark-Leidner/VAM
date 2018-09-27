c!#   $Id: wgp_wrgrid.F,v 1.2 1997/02/12 19:41:08 leidner Exp $
c!#   $Log: wgp_wrgrid.F,v $
c!#   Revision 1.2  1997/02/12 19:41:08  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function WGP_wrgrid(idate,itime)
c**********************************************************************
c English Name: Write Grid (WGP - Write Grid in Phoenix)
c -------------
c
c Purpose: Writes U and V gridded wind fields from VAM data buffer
c -------- (see "grdta.h") to output data set in Phoenix format.
c
c Notes:
c ------
c
c Variable Description:
c ---------------------
c
c     Argument List
c     -------------
c
c     integer WGP_wrgrid,idate,itime
c
c     WGP_wrgrid (O) - function return value:
c
c          WGP_wrgrid =  0 (normal)
c                       -1 (error returned)
c                       -2 (error reading namelist)
c
c     idate (I) - date in yymmdd format.
c     itime (I) - time in hhmmss format.
c
c Programmer: Joseph V. Awrizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddawr Space Flight Center)
c
c Modified: April 13, 1995 - created. 
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      integer idate,itime
c
c     Local Variables
c     ---------------
c
      real rlev
      integer i,n,id,jd

      real*4 rdum
      character*1 cdum(8),ddum(40)
      integer*4 ndfld,nlev,nlay,nbefaf,im,jnp
c
c     Common Blocks
c     -------------
c
#include "gparm.h"
c
c     Common Block - "IO Buffer"
c     --------------------------
c
      real*4 rbuf(MAXREC)
      integer*4 ibuf(MAXREC)

      common /iobuf/ rbuf,ibuf
c
c     Namelist
c     --------
c
      integer iu
      real*4 level
      character*80 xlabel
      character*40 descr(2)
      character*8 job,name(2)
      integer*4 nymd,nhms,nymd0,nhms0,ks,ku,ibefaf

      namelist /input/ iu,job,xlabel,nymd,nhms,nymd0,nhms0,ks,ku,
     &                 level,ibefaf,name,descr

      data ndfld/0/
      data ptop/0.0/
      data nlay/-1/
      data nbefaf/1/
      data cdum/8*' '/
      data ddum/40*' '/
      data rdum/0.0/
c
c     Retrieve namelist parameters / initialize
c     =========================================
c
      WGP_wrgrid = 0

      nymd     = idate
      nhms     = itime
      im       = int(360.0 / delx)
      jnp      = int(180.0 / dely) + 1

      iu       = 0
      nymd0    = 0
      nhms0    = 0
      xlabel   = '        '
      ks       = 2
      ku       = 0
      nlev     = 0
      level    = 0.0
      ibefaf   = 0 
      job      = 'VAM ANA '
      name(1)  = 'U10M    '
      name(2)  = 'V10M    '
      descr(1) = 'U-WIND AT 10 METERS (M/S)               '
      descr(2) = 'V-WIND AT 10 METERS (M/S)               '

      read(IUNML,input,end=902,err=902)
      write(*,input)

      ks   = min(2,ks)
      ku   = min(2,ku)
      if (ku .gt. 0) nlev = 1
      if (ks .gt. 0 .and. ku .gt. 0) ks = 0
      if (ks .le. 0 .and. ku .le. 0) ks = 2
c
c     Establish I/O link
c     ==================
c
 5    continue
      if (ioopen(iu) .ne. 0) go to 901
c
c     Write Phoenix Header.
c     =====================
c
      write(iu) job,nymd,nhms,nymd0,nhms0,xlabel,
     &          im,jnp,ks,ku,ndfld,ptop,
     &          nlev,(level,n=1,nlev),nlay,
     &          nbefaf,ddum,ibefaf,rdum,cdum

      write(iu) (name(n),descr(n),n=1,ks),
     &          (name(n),descr(n),n=1,ku)
c
c     Write Data
c     ==========
c     
      id    = im
      jd    = jnp
      rlev  = level
      call WGP_wrdata(iu,rlev,name(1),name(2),id,jd,rbuf)

      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 901  continue
      WGP_wrgrid = -1
      go to 999

 902  continue
      WGP_wrgrid = -2
      print *, 'vam(WGP_wrgrid): error reading namelist'
      go to 999

 999  return
      end
