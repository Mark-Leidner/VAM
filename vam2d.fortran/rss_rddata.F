c!#   $Id: rss_rddata.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: rss_rddata.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function RSS_rddata(iu,nbuff,speedonly,scfact,wfact,alias,itag)
c**********************************************************************
c English Name: Read Data (RSS - Read SASS from SASS Repack)     
c -------------
c
c Purpose: Reads in SASS data from SASS repack data set. Reports are
c -------- stored in the VAM SASS data buffers (see "ptdta.h").
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
c     real scfact,wfact
c     logical speedonly,alias(4)
c     integer RSS_rddata,iu,nbuff,itag
c
c     RSS_rddata (O) - function return value:
c
c        RSS_rddata >= 0 (number of reports stored)
c                     -1 (unexpected EOF) 
c                     -2 (max dimensions exceeded)
c
c     iu (I) - fortran unit number assigned to input SASS repack data.
c     nbuff (I) - number of records to be read (see "RPK_rdskip").
c     itag (I) - arbitrary data identifier tag useful for isolating 
c                data types. 
c     speedonly (I) - flag indicating if directional information is to
c                     be ignored:
c
c              speedonly = .true. (directional information ignored).
c                          .false. (directional information saved).
c
c     scfact (I) - wind speed scaling factor for adjusting data to
c                  analysis height.
c
c     wfact (I) - VAM weighting factor (0-1).
c
c     alias (I) - contains flags for each alias indicating if data is
c                 to be accepted:
c
c              alias(ia) = .true. (alias accepted)
c                          .false. (alias ignored)
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)
c             (NASA Goddard Space Flight Center)
c
c Modified: February 7, 1995 - created.
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      real scfact,wfact
      integer iu,nbuff,itag
      logical speedonly,alias(4)
c
c     Local Variables
c     ---------------
c
      logical goodwind
      integer i,ia,na,ipt,nrep,igrid,jgrid
      real rlon,rlat,xgrid,ygrid,speed,direc,vavg,factor,fill
c
c     Common Blocks
c     -------------
c
#include "ptdta.h"
#include "gparm.h"
c
c     Common Block - "IO Buffer"
c     --------------------------
c
      real*4 rpkbuf(187)
      integer*4 ibuf(MAXREC)
      real*4 rbuf(MAXREC-187)
      real*4 rpkspd(17,4),rpklat(17),rpklon(17),rpkdir(17,4),rpkals(17)

      common /iobuf/ rpklat,rpklon,rpkspd,rpkdir,rpkals,rbuf,ibuf

      equivalence (rpkbuf(1  ),rpklat),
     &            (rpkbuf(18 ),rpklon),
     &            (rpkbuf(35 ),rpkspd),
     &            (rpkbuf(103),rpkdir),
     &            (rpkbuf(171),rpkals)

      data fill/999.9/
      data factor/1.74532925199432946D-2/
c
c     Begin reading SASS buffers.
c     ===========================
c
      nrep = 0
      do n = 1,nbuff

         read(iu,end=900,err=900) rpkbuf

         do 5 i = 1,17

            if (rpkals(i) .eq. 0.0) go to 5
c
c           Check that ob is in analysis region.
c           ------------------------------------
c
            rlon = rpklon(i)
            rlat = rpklat(i)
            if (rlon .lt. 0.0) rlon = rlon + 360.0
            call gridx(rlon,xs,delx,iper,xmin,xmax,igrid,xgrid,*5)
            call gridx(rlat,ys,dely,   0,ymin,ymax,jgrid,ygrid,*5)

            ipt = npts + nrep + 1
            if (ipt .gt. PTMAX) go to 901
c
c           Save desired/valid aliases.
c           ---------------------------
c
            nwinds(ipt) = 0
            vbar(ipt)   = 0.0
            vavg        = 0.0

            do ia = 1,4

               if (alias(ia)) then 

                  speed = rpkspd(i,ia)
                  direc = rpkdir(i,ia)

                  goodwind = (speed .ne. 0.0)
                  goodwind = (goodwind .and. .not. FTEST(speed,fill))
                  goodwind = (goodwind .and. .not. FTEST(direc,fill))

                  if (goodwind) then 
 
                     nwinds(ipt) = nwinds(ipt) + 1
                     na          = nwinds(ipt)
                     speed       = speed * scfact
                     direc       = direc * factor
                     vbar(ipt)   = vbar(ipt) + speed**2
                     vavg        = vavg + speed

                     call winds(uobs(na,ipt),vobs(na,ipt),
     &                                             speed,direc,.true.)
                  endif

               endif
                     
            end do
c
c           If valid aliases were located, save remaining data.
c           ---------------------------------------------------
c
            if (nwinds(ipt) .gt. 0) then 
            
               nrep = nrep + 1

               ill(ipt)    = igrid
               jll(ipt)    = jgrid
               xcoord(ipt) = xgrid
               ycoord(ipt) = ygrid
               iobs(ipt)   = itag
               wgt(ipt)    = wfact
               pt_qc(ipt)  = 0
               vbar(ipt)   = sqrt(vbar(ipt) / na)
               vavg        = vavg / na

               if (speedonly) then 
                  nwinds(ipt) = 0
                  uobs(1,ipt) = vavg
                  vobs(1,ipt) = 0.0
                  vbar(ipt)   = vavg
               endif

            endif

 5       continue

      end do

      RSS_rddata = nrep
      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue
      RSS_rddata = -1
      print *, 'vam(RSS_rddata): unexpected EOF'
      go to 999

 901  continue
      RSS_rddata = -2
      print *, 'vam(RSS_rddata): max dimensions exceeded.'
      go to 999

 999  return
      end
