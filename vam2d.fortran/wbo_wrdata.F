c!#   $Id: wbo_wrdata.F,v 1.2 1997/02/12 19:19:28 leidner Exp $
c!#   $Log: wbo_wrdata.F,v $
c!#   Revision 1.2  1997/02/12 19:19:28  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function WBO_wrdata(iu,itag)
c**********************************************************************
c English Name: Write Data (WBO - Write Backscatter in OI Repack)
c -------------
c
c Purpose: Writes backscatter stored in VAM Backscatter data buffer
c -------- (see "er_bscat.h") to output OI repack data set according
c          to the specified options.
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
c     integer WBO_wrdata,iu,itag(MAXTAG)
c
c     WBO_wrdata (O) - function return value:
c
c           WBO_wrdata >= 0 (number of reports written)
c
c     iu (I) - fortran unit number assigned to output OI repack data.
c     itag (I) - data identifier tags designating which observations
c                are to be written (see "ALLTYPE" in vam.h).
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 17, 1995 - created.
c ---------
c**********************************************************************
      real PI
      parameter (PI = 3.1415927)
#include "vam.h"

      real OIFILL
      parameter (OIFILL = 999.9)
c
c     Argument List
c     -------------
c
      integer iu,itag(MAXTAG)
c
c     Local Variables
c     ---------------
c
      real LOGTEN
      logical skipobs
      integer i,j,ii,jj,n,nrep,nsearch,nbuff
c
c     Common Blocks
c     -------------
c
#include "er_bscat.h"
#include "gparm.h"
#include "grdta.h"
c
c     Common Block - "IO Buffer"
c     --------------------------
c
      integer*4 ibuf(MAXREC)
      real*4 form,buff,buffer(475),rbuf(MAXREC-477)
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
     &               qmeth,qfore,qmid,qaft,form,buff,rbuf,ibuf
      equivalence(buffer(1),rtime(1))

      LOGTEN = log(10.0)
c
c     Count the number of observations to be written.
c     ===============================================
c
      nrep = 0
      ntag = isrch(MAXTAG,itag,ALLTYPE) - 1
      if (ntag .lt. 0) ntag = MAXTAG
      
      if (ntag .eq. 0) then 
         nrep = er_nobs
      else
         do n = 1,er_nobs
            if (isrch(ntag,itag,er_tag(n)) .ne. 0) nrep = nrep + 1
         end do
      endif
c
c     Create OI Repack sub-header records.
c     ====================================
c
      nbuff = nrep / 19
      if (mod(nrep,19) .ne. 0) nbuff = nbuff + 1

      form = 7.0
      buff = float(nbuff)
      write(iu) form,buff
c
c     Create OI Repack buffer.
c     ========================
c
      n = 0
      do i = 1,nrep,19

         nsearch = min(19,nrep-i+1)
         call setcon4(475,buffer,OIFILL)

         do j = 1,nsearch
 
            skipobs = .true.

            do while (skipobs) 

               n = n + 1

               if (ntag.eq.0 .or. isrch(ntag,itag,er_tag(n)).ne.0) then 

                  skipobs = .false.
 
                  ii   = er_ic(n)
                  jj   = er_jc(n)
c
c                 Save Data.
c                 ----------
c
                  rlat(j)  = ys + (jj - 1 + er_yc(n)) * dely
                  rlon(j)  = xs + (ii - 1 + er_xc(n)) * delx
                  rtime(j) = er_time(n) / 60.0
                  qland(j) = er_qc(n)
                  rthtf(j) = er_theta(n,1) * 180.0/PI
                  rthtm(j) = er_theta(n,2) * 180.0/PI
                  rthta(j) = er_theta(n,3) * 180.0/PI
                  rkpf(j)  = er_kp (n,1)
                  rkpm(j)  = er_kp (n,2)
                  rkpa(j)  = er_kp (n,3)
                  rspd(j)  = er_vel5(n)
                  rdir(j)  = er_dir5(n)

                  razif(j) = er_azim(n,1) * 180.0/PI - 180.0
                  if (razif(j) .lt. 0.0) razif(j) = razif(j) + 360.0
                  razim(j) = er_azim(n,2) * 180.0/PI - 180.0
                  if (razim(j) .lt. 0.0) razim(j) = razim(j) + 360.0
                  razia(j) = er_azim(n,3) * 180.0/PI - 180.0
                  if (razia(j) .lt. 0.0) razia(j) = razia(j) + 360.0

                  if (rkpf(j) .gt. 0.0) then 
                     rkpf(j) = rkpf(j) * 100.0
                     rsigf(j) = 10.0 * log(er_s05(n,1)) / LOGTEN
                  endif 

                  if (rkpm(j) .gt. 0.0) then 
                     rkpm(j) = rkpm(j) * 100.0
                     rsigm(j) = 10.0 * log(er_s05(n,2)) / LOGTEN
                  endif 

                  if (rkpa(j) .gt. 0.0) then 
                     rkpa(j) = rkpa(j) * 100.0
                     rsiga(j) = 10.0 * log(er_s05(n,3)) / LOGTEN
                  endif 

               endif

            end do

         end do
c
c        Write OI Repack Buffer.
c        -----------------------
c
         write(iu) buffer

      end do

c     Create OI Repack trailer records.
c     =================================
c
      form = 0.0
      buff = 0.0
      write(iu) form,buff

      WBO_wrdata = nrep

      return
      end
