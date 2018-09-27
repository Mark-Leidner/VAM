      subroutine dump_ptdta (iuvam, hdr_fname, bin_fname, ierr)
c!#
c!#   Routine to write out NSCAT ambiguous winds data in VAM99 format, from
c!#   common blocks ns_winds and ptdat
c!#     
#include "vam.h"
#include "gparm.h"
#include "ns_winds.h"
#include "ptdta.h"
      
      integer iuvam, ierr
      integer*4 liuvam, lierr
      character*(*) hdr_fname, bin_fname

      integer iobs_u, iobs_v, iconst_uoesd, iconst_voesd
      integer*4 nvar, nocc
      parameter (iobs_u=1, iobs_v=2, iconst_uoesd=1, iconst_voesd=2,
     &     nvar=2, nconst=3, nocc=4)
c!# ...Direct access file record length parameter: recl/(no of words)
      parameter (parecl=4)
      real*4 uoesd, voesd
      parameter (dfac_const=0.25, uoesd=1, voesd=1, lname=10)

      real*4 vconst(nconst), time(ptmax), latdeg(ptmax), londeg(ptmax),
     &     data(nvar,nocc,ptmax), uint(ptmax), vint(ptmax)
      real*4 ufgat(ptmax), vfgat(ptmax), alpha_fgat(ptmax)
      integer*4 numocc(ptmax), recid(ptmax)
      character*(lname) cobsid, namcon(nconst), namvar(nvar)
      integer*4 nloc, recl
      logical*4 qcflag(nocc,ptmax)

      data cobsid/'ambiguous '/
      data namcon/'dfac', 'sd(u)', 'sd(v)'/, namvar/'u_wind','v_wind'/

      vconst(1) = dfac_const
      vconst(2) = uoesd
      vconst(3) = voesd
      liuvam = iuvam
c!#   Fill up local (s.p.) arrays from ptdta.h common block
      nloc = nptsa
      if (nloc .le. 0) then
         print *,'No data in ptdta, nothing written to ',
     &        hdr_fname, bin_fname
         ierr=-1
      endif
      do 100 n=1, nloc
         time(n) = 0
         uint(n) = 0
         vint(n) = 0
         ufgat(n) = 0
         vfgat(n) = 0
         alpha_fgat(n) = 1
         numocc(n) = nwinds(n)
         do 50 iocc=1,nocc
            qcflag(iocc,n) = wgt(n) .le. 0 .or. iocc .gt. nwinds(n)
   50    continue
         latdeg(n) = ys + (jll(n) - 1 + ycoord(n)) * dely
         londeg(n) = xs + (ill(n) - 1 + xcoord(n)) * delx
         recid(n) = 100*nsw_row(n) + nsw_col(n)
         do 90 iocc=1,nocc
            data(iobs_u,iocc,n) = uobs(iocc,n)
            data(iobs_v,iocc,n) = vobs(iocc,n)
   90    continue
  100 continue

c!#   Write data to file
      recl = nloc * parecl
      call wrtobs(liuvam, hdr_fname, bin_fname,
     &     cobsid, nconst, nloc, nocc, nvar,
     &     namcon, vconst, namvar, recl,
     &     numocc, recid, time, latdeg, londeg,
     &     uint, vint, ufgat, vfgat, alpha_fgat,
     &     qcflag, data,
     &     lierr)
      ierr = lierr
      if (ierr .eq. 0) then
         print *,'Dump_ptdta: wrote obs to ',hdr_fname,bin_fname
      else
         print *,'Dump_ptdta: wrtobs error ierr= ',ierr
         print *,'writing obs to ',hdr_fname,bin_fname
      endif
      return
      end
