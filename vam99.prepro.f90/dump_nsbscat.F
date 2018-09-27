      subroutine dump_nsbscat (iuvam, hdr_fname, bin_fname, ierr)
c!#
c!#   Routine to write out NSCAT backscatter data in VAM99 format, from
c!#   common blocks ns_bscat
c!#     
#include "vam.h"
#include "gparm.h"
#include "ns_bscat.h"
      
      integer iuvam, ierr
      integer*4 liuvam, lierr
      character*(*) hdr_fname, bin_fname

      integer iobs_hv_polar, iobs_incidence, iobs_azimuth,
     &     iobs_sigma0, iobs_kpa, iobs_kpb, iobs_kpc,
     &     iconst_satid, iconst_modfn, iconst_sdcalc, iconst_ndcalc
      integer*4 nvar, nocc
      parameter (iobs_hv_polar=1, iobs_incidence=2, iobs_azimuth=3,
     &     iobs_sigma0=4, iobs_kpa=5, iobs_kpb=6, iobs_kpc=7,
     &     iconst_satid=1, iconst_modfn=2, iconst_sdcalc=3,
     &     iconst_ndcalc=4,
     &     nvar=7, nconst=4, nocc=1)
c!# ...Direct access file record length parameter: recl/(no of words)
      parameter (parecl=4)
      real*4 satid_const, modfn_const, sdcalc_const, ndcalc_const
      parameter (satid_const=1, modfn_const=3,
     &     sdcalc_const=2, ndcalc_const=1,
     &     lname=10)

      real*4 vconst(nconst), time(ns_mobs),
     &     latdeg(ns_mobs), londeg(ns_mobs),
     &     data(nvar,nocc,ns_mobs), uint(ns_mobs), vint(ns_mobs)
      real*4 ufgat(ns_mobs), vfgat(ns_mobs), alpha_fgat(ns_mobs)
      integer*4 numocc(ns_mobs), recid(ns_mobs)
      character*(lname) cobsid, namcon(nconst), namvar(nvar)
      integer*4 nloc, recl
      logical*4 qcflag(nocc,ns_mobs)

      data cobsid/'sigma0    '/
      data namcon/'satid', 'modfn', 'sdcalc', 'ndcalc'/
      data namvar/'hv-polar', 'incidence', 'azimuth', 'sigma0',
     &     'kpa', 'kpb', 'kpc'/

      vconst(1) = satid_const
      vconst(2) = modfn_const
      vconst(3) = sdcalc_const
      vconst(4) = ndcalc_const
      liuvam = iuvam
c!#   Fill up local (s.p.) arrays from ns_bscat.h common block
      nloc = ns_nobs
      if (nloc .le. 0) then
         print *,'No data in ns_bscat, nothing written to ',
     &        hdr_fname, bin_fname
         ierr=-1
      endif
      do 100 n=1, nloc
         time(n) = ns_time(n)
         uint(n) = 0
         vint(n) = 0
         numocc(n) = 1
         ufgat(n) = 0
         vfgat(n) = 0
         alpha_fgat(n) = 1
         qcflag(1,n) = .FALSE.
         latdeg(n) = ns_lat(n)
         londeg(n) = ns_lon(n)
         recid(n) = n	!until something more meaningful can be figured out!#
         data(iobs_hv_polar,1,n) = ns_pol(n)
         data(iobs_incidence,1,n) = ns_theta(n)
         data(iobs_azimuth,1,n) = ns_azim(n)
         data(iobs_sigma0,1,n) = ns_s0obs(n)
         data(iobs_kpa,1,n) = ns_kpa(n)
         data(iobs_kpb,1,n) = ns_kpb(n)
         data(iobs_kpc,1,n) = ns_kpc(n)
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
         print *,'Dump_nsbscat: wrote obs to ',hdr_fname,bin_fname
      else
         print *,'Dump_nsbscat: wrtobs error ierr= ',ierr
         print *,'writing obs to ',hdr_fname,bin_fname
      endif
      return
      end
