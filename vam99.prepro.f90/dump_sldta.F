      subroutine dump_sldta (iuvam, hdr_fname, bin_fname, ierr)
c
c!#   Routine to write out conventional winds data in VAM99 format, from
c!#   common block sldta
c     
#include "vam.h"
#include "gparm.h"
#include "sldta.h"
      
      integer iuvam, ierr
      integer*4 liuvam, lierr
      character*(*) hdr_fname, bin_fname

      integer iobs_u, iobs_v, iconst_uoesd, iconst_voesd
      integer*4 nvar, nocc
      parameter (iobs_u=1, iobs_v=2, iconst_uoesd=1, iconst_voesd=2,
     &     nvar=2, nconst=2, nocc=1)
      real*4 uoesd, voesd
      parameter (uoesd=1, voesd=1, lname=10)
c!# ...Direct access file record length parameter: recl/(no of words)
      parameter (parecl=4)

      real*4 vconst(nconst), time(slmax), latdeg(slmax), londeg(slmax),
     &     data(nvar,nocc,slmax), uint(slmax), vint(slmax)
      real*4 ufgat(slmax), vfgat(slmax), alpha_fgat(slmax)
      integer*4 numocc(slmax), recid(slmax)
      character*(lname) cobsid, namcon(nconst), namvar(nvar)
      integer*4 nloc, recl
      logical qcflag(nocc,ptmax)

      data cobsid/'convention'/
      data namcon/'sd(u)', 'sd(v)'/, namvar/'u_wind','v_wind'/

      vconst(1) = uoesd
      vconst(2) = voesd

      liuvam = iuvam
c!#   Fill up local (s.p.) arrays from sldta.h common block
      nloc = nptc
      if (nloc .le. 0) then
         print *,'No data in sldta, nothing written to ',hdr_fname, bin_fname
         ierr=-1
      endif
      do 100 n=1, nloc
         time(n) = 0
         uint(n) = 0
         vint(n) = 0
         ufgat(n) = 0
         vfgat(n) = 0
         alpha_fgat(n) = 1
         qcflag(1,n) = wconv(n) .le. 0
         numocc(n) = 1
         latdeg(n) = ys + (jlc(n) - 1 + yconv(n)) * dely
         londeg(n) = xs + (ilc(n) - 1 + xconv(n)) * delx
         recid(n) = n
         data(iobs_u,1,n) = uconv(n)
         data(iobs_v,1,n) = vconv(n)
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
         print *,'Dump_sldta: wrote obs to ',hdr_fname, bin_fname
      else
         print *,'Dump_sldta: wrtobs error ierr= ',ierr
         print *,'writing obs to ',hdr_fname, bin_fname
      endif
      return
      end
