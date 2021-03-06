c!#   $Id: wco_wrdata.F,v 1.2 1997/02/12 19:19:28 leidner Exp $
c!#   $Log: wco_wrdata.F,v $
c!#   Revision 1.2  1997/02/12 19:19:28  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function WCO_wrdata(iu,itag,assign_spd,assign_dir)
c**********************************************************************
c English Name: Write Data (WCO - Write Conventional in OI Repack)
c -------------
c
c Purpose: Writes observations stored in VAM conventional data buffer
c -------- (see "sldta.h") to output OI repack data set according to
c          the specified options.
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
c     integer WCO_wrdata,iu,itag(MAXTAG)
c     logical assign_spd,assign_dir
c
c     WCO_wrdata (O) - function return value:
c
c           WCO_wrdata >= 0 (number of reports written)
c
c     iu (I) - fortran unit number assigned to output OI repack data.
c     itag (I) - data identifier tags designating which observations
c                are to be written (see "ALLTYPE" in vam.h).
c
c     assign_spd (I) - flag indicating that the analyzed speed is to be
c                      assigned to each report:
c
c             assign_spd = .true. (assign analyzed speed)
c                         .false. (do not assign speed)
c
c     assign_dir (I) - flag indicating that the analyzed direction is
c                      to be assigned to each report:
c
c             assign_dir = .true. (assign analyzed direction)
c                         .false. (do not assign direction)
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 17, 1995 - created.
c ---------
c**********************************************************************
#include "vam.h"

      real OIFILL
      parameter (OIFILL = 999.9)
c
c     Argument List
c     -------------
c
      integer iu,itag(MAXTAG)
      logical assign_spd,assign_dir
c
c     Local Variables
c     ---------------
c
      logical skipobs
      integer i,j,ii,jj,n,nrep,nsearch,nbuff
      real rlat,rlon,u0i,v0i,uo,vo,adirec,aspeed,odirec,ospeed
c
c     Common Blocks
c     -------------
c
#include "sldta.h"
#include "gparm.h"
#include "grdta.h"
c
c     Common Block - "IO Buffer"
c     --------------------------
c
      integer*4 ibuf(MAXREC)
      real*4 rbuf(MAXREC-1019)
      real*4 buffer(1017),form,buff
      real*4 sptim(113),spkx(113),splat(113),splon(113)
      real*4 psl(113),us(113),vs(113),qpsl(113),qws(113)

      common /iobuf/ sptim,spkx,splat,splon,psl,us,vs,qpsl,qws,
     &               form,buff,rbuf,ibuf

      equivalence(buffer(1),sptim(1))
c
c     Count the number of observations to be written.
c     ===============================================
c
      nrep = 0
      ntag = isrch(MAXTAG,itag,ALLTYPE) - 1
      if (ntag .lt. 0) ntag = MAXTAG
      
      if (ntag .eq. 0) then 
         nrep = nptc
      else
         do n = 1,nptc
            if (isrch(ntag,itag,iconv(n)) .ne. 0) nrep = nrep + 1
         end do
      endif
c
c     Create OI Repack sub-header records.
c     ====================================
c
      form = 1.0
      buff = 0.0
      write(iu) form,buff

      nbuff = nrep / 113
      if (mod(nrep,113) .ne. 0) nbuff = nbuff + 1

      form = 2.0
      buff = float(nbuff)
      write(iu) form,buff
c
c     Create OI Repack buffer.
c     ========================
c
      n = 0
      do i = 1,nrep,113

         nsearch = min(113,nrep-i+1)
         call setcon4(1024,buffer,OIFILL)

         do j = 1,nsearch
 
            skipobs = .true.

            do while (skipobs) 

               n = n + 1

               if (ntag.eq.0 .or. isrch(ntag,itag,iconv(n)).ne.0) then 

                  skipobs = .false.
 
                  ii   = ilc(n)
                  jj   = jlc(n)
                  rlat = ys + (jj - 1 + yconv(n)) * dely
                  rlon = xs + (ii - 1 + xconv(n)) * delx
c
c                 Interpolate analysis grid to observation.
c                 -----------------------------------------
c
                  u0i = bilin (xconv(n),yconv(n),u(ii,jj),idim)
                  v0i = bilin (xconv(n),yconv(n),v(ii,jj),idim)
                  call winds(u0i,v0i,aspeed,adirec,.false.)
c
c                 Assign speed/direction if desired.
c                 ----------------------------------
c
                  call winds(uconv(n),vconv(n),ospeed,odirec,.false.)
                  if (assign_dir) odirec = adirec
                  if (assign_spd) ospeed = aspeed
                  call winds(uo,vo,ospeed,odirec,.true.)
c
c                 Save Data.
c                 ----------
c
                  sptim(j) = 0.0
                  spkx(j)  = iconv(n)
                  splat(j) = rlat
                  splon(j) = rlon
                  psl(j)   = sl_qc(n)
                  us(j)    = uo
                  vs(j)    = vo
                  qpsl(j)  = 1.0
                  qws(j)   = 0.0

                  if (splon(j) .ge. 180.0) splon(j) = splon(j) - 360.0

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
      form = 3.0
      buff = 0.0
      write(iu) form,buff
      form = 4.0
      buff = 0.0
      write(iu) form,buff
      form = 0.0
      buff = 0.0
      write(iu) form,buff

      WCO_wrdata = nrep

      return
      end
