c!#   $Id: wbo_wrnwind.F,v 1.1 1997/02/21 23:45:10 leidner Exp $
c!#   $Log: wbo_wrnwind.F,v $
c!#   Revision 1.1  1997/02/21 23:45:10  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function WBO_wrnwind(iu,itag,assign_spd,assign_dir)
c**********************************************************************
c English Name: Write Wind (WBO - Write Backscatter in OI Repack)
c -------------
c
c Purpose: Writes winds stored in VAM Backscatter data buffer
c -------- (see "ns_bscat.h") to output OI repack data set according
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
c     integer WBO_wrnwind,iu,itag(MAXTAG)
c     logical assign_spd,assign_dir
c
c     WBO_wrnwind (O) - function return value:
c
c           WBO_wrnwind >= 0 (number of reports written)
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
#include "ns_bscat.h"
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
         nrep = ns_nWVC
      else
         do n = 1,ns_nWVC
            if (isrch(ntag,itag,ns_tag(n)) .ne. 0) nrep = nrep + 1
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

c              if (ntag.eq.0 .or. isrch(ntag,itag,ns_tag(n)).ne.0) then 

                  skipobs = .false.
 
                  ii   = WVC_ic(n)
                  jj   = WVC_jc(n)
c
c                 Interpolate analysis grid to observation.
c                 -----------------------------------------
c
                  u0i = bilin (WVC_xc(n),WVC_yc(n),u(ii,jj),idim)
                  v0i = bilin (WVC_xc(n),WVC_yc(n),v(ii,jj),idim)
                  call winds(u0i,v0i,aspeed,adirec,.false.)
c
c                 Assign speed/direction if desired.
c                 ----------------------------------
c
                  ospeed = ns_vel5(n)
                  odirec = ns_dir5(n)
                  if (assign_dir) odirec = adirec
                  if (assign_spd) ospeed = aspeed
                  call winds(uo,vo,ospeed,odirec,.true.)
c
c                 Save Data.
c                 ----------
c
                  sptim(j) = 0.0
                  spkx(j)  = 28.0
                  splat(j) = WVC_Lat(n)
                  splon(j) = WVC_Lon(n)
c                 psl(j)   = ns_qc(n)
                  us(j)    = uo
                  vs(j)    = vo
                  qpsl(j)  = 1.0
                  qws(j)   = 0.0

                  if (splon(j) .ge. 180.0) splon(j) = splon(j) - 360.0

c              endif

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

      WBO_wrnwind = nrep

      return
      end
