c!#   $Id: rso_rdback.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: rso_rdback.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function RSO_rdback(iu,nbuff,speedonly,scfact,wfact,alias,itag,
     &                            dbmin,dbmax,minsig0,minprob,nthin)
c**********************************************************************
c English Name: Read Backscatter (RSO - Read SASS from OIrepack)
c -------------
c
c Purpose: Reads in data from OI repack backscatter buffer (form=7).
c -------- Reports are stored in VAM SASS data buffers 
c          (see "ptdta.h").
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
c     logical speedonly,alias(4)
c     real dbmin,dbmax,minprob,scfact,wfact
c     integer RSO_rdback,iu,nbuff,nthin,itag,minsig0
c
c     RSO_rdback (O) - function return value:
c
c        RSO_rdback >= 0 (number of reports stored)
c                     -1 (unexpected EOF)
c                     -2 (max dimensions exceeded)
c                     -3 (illegal thinning increment)
c
c     iu (I) - fortran unit number assigned to input OI repack data.
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
c     dbmin (I) - minimum acceptable value for s0 (DB)
c     dbmax (I) - maximum acceptable value for s0 (DB)
c     minsig0 (I) - minimum number of valid s0's needed to accept
c                   the report
c     minprob (I) - minimum probability of correctness for accepting
c                   alias (%).
c     nthin (I) - specifies how to thin (reduce) the data:
c
c                    nthin = 1 (every report used; every swath)
c                            2 (every other report used> 19,17,15,...
c                               from every other swath)
c                            .               .
c                            .               .
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
      logical speedonly,alias(4)
      real dbmin,dbmax,minprob,scfact,wfact
      integer iu,nbuff,nthin,itag,minsig0
c
c     Local Variables
c     ---------------
c
      real vsol(4),dsol(4),rsol(4),prob
      logical goodwind,forb,midb,aftb,reject
      integer i,k,ia,na,ipt,nrep,ic,jc,ngood,nreject
      real xc,yc,speed,direc,factor,vavg,rlon8,rlat8
c
c     Common Blocks
c     -------------
c
#include "gparm.h"
#include "ptdta.h"
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
      equivalence(buffer(1),rtime(1))

      data factor/1.74532925199432946D-2/
c
c     Begin reading OI buffers
c     ========================
c
      nrep    = 0
      nreject = 0
      if (nthin .le. 0) go to 902

      do 10 k = 1,nbuff

         read(iu,end=900,err=900) buffer
         if (mod(k,nthin) .ne. 0) go to 10

         do 5 i = 19,1,-nthin
c
c           Perform Gross Quality Check
c           ---------------------------
c
            if (FTEST(rlat(i),ER_FILL)) go to 5
            if (FTEST(rlon(i),ER_FILL)) go to 5
            if (qland(i) .ne. 0.0) go to 5

            reject = .false.
            if (rsigf(i).lt.dbmin.or.rsigf(i).gt.dbmax) reject = .true.
            if (rsigm(i).lt.dbmin.or.rsigm(i).gt.dbmax) reject = .true.
            if (rsiga(i).lt.dbmin.or.rsiga(i).gt.dbmax) reject = .true.

            if (reject) then
               nreject = nreject + 1
               go to 5
            endif

            forb = (rkpf(i) .gt. 0)
            midb = (rkpm(i) .gt. 0)
            aftb = (rkpa(i) .gt. 0)

            ngood = 0
            if (forb) ngood = ngood + 1
            if (midb) ngood = ngood + 1
            if (aftb) ngood = ngood + 1
            if (ngood .lt. minsig0) go to 5
c
c           Transform (long,lat) to grid coordinates.
c           -----------------------------------------
c
            if (rlon(i) .lt. 0.0) rlon(i) = rlon(i) + 360.0
            rlon8 = rlon(i)
            rlat8 = rlat(i)
            call gridx(rlon8,xs,delx,iper,xmin,xmax,ic,xc,*5)
            call gridx(rlat8,ys,dely,   0,ymin,ymax,jc,yc,*5)
c
c           Retrieve ambiguous winds.
c           -------------------------
c
            nwind = ers1wind(i,vsol,dsol,rsol)
            if (nwind .lt. 2 .or. nwind .gt. 4) go to 5

            ipt  = npts + nrep + 1
            if (ipt .gt. PTMAX) go to 901
c
c           Save desired/valid aliases.
c           ---------------------------
c
            nwinds(ipt) = 0
            vbar(ipt)   = 0.0
            vavg        = 0.0

            do ia = 1,nwind

               if (alias(ia)) then 

                  speed = vsol(ia)
                  direc = dsol(ia)
                  prob  = rsol(ia) * 100.0

                  goodwind = (speed .ne. 0.0)
                  goodwind = (goodwind .and. prob .gt. minprob)

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

               ill(ipt)    = ic
               jll(ipt)    = jc
               xcoord(ipt) = xc
               ycoord(ipt) = yc
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

 10   continue

      RSO_rdback = nrep
      write(*,100) nreject
      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue
      RSO_rdback = -1
      print *, 'vam(RSO_rdback): unexpected EOF'
      go to 999

 901  continue
      RSO_rdback = -2
      print *, 'vam(RSO_rdback): max dimensions exceeded.'
      go to 999

 902  continue
      RSO_rdback = -3
      print *, 'vam(RSO_rdback): illegal thinning increment: ',nthin
      go to 999

 100  format(/,1x,'vam(RSO_rdback): ',i6,' obs rejected.')

 999  return
      end
