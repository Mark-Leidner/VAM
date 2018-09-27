c!#   $Id: rso_rdukmo.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: rso_rdukmo.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function RSO_rdukmo(iu,nbuff,speedonly,scfact,wfact,alias,itag,
     &                                                  minprob,nthin)
c**********************************************************************
c English Name: Read UKMO (RSO - Read SASS from OIrepack)
c -------------
c
c Purpose: Reads in data from OI repack UKMO buffer (form=8).
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
c     real minprob,scfact,wfact
c     integer RSO_rdukmo,iu,nbuff,nthin,itag
c
c     RSO_rdukmo (O) - function return value:
c
c        RSO_rdukmo >= 0 (number of reports stored)
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
      real minprob,scfact,wfact
      integer iu,nbuff,nthin,itag
c
c     Local Variables
c     ---------------
c
      logical goodwind
      integer i,k,ia,na,ipt,nrep,ic,jc
      real xc,yc,speed,direc,factor,vavg,prob,rlon8,rlat8
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
      real*4 buffer(323),rbuf(MAXREC-323)
      real*4 rtime(19),rlat(19),rlon(19),rspd(19),rdir(19)
      real*4 vsol(4,19),dsol(4,19),rsol(4,19)

      common /iobuf/ rtime,rlat,rlon,rspd,rdir,
     &               vsol,dsol,rsol,rbuf,ibuf
      equivalence(buffer(1),rtime(1))

      data factor/1.74532925199432946D-2/
c
c     Begin reading OI buffers
c     ========================
c
      nrep    = 0
      if (nthin .le. 0) go to 902

      do 10 k = 1,nbuff

         read(iu,end=900,err=900) buffer
         if (mod(k,nthin) .ne. 0) go to 10

         do 5 i = 19,1,-nthin
c
c           Transform (long,lat) to grid coordinates.
c           -----------------------------------------
c
            if (rlon(i) .lt. 0.0) rlon(i) = rlon(i) + 360.0
            rlon8 = rlon(i)
            rlat8 = rlat(i)
            call gridx(rlon8,xs,delx,iper,xmin,xmax,ic,xc,*5)
            call gridx(rlat8,ys,dely,   0,ymin,ymax,jc,yc,*5)

            ipt  = npts + nrep + 1
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

                  speed = vsol(ia,i)
                  direc = dsol(ia,i)
                  prob  = rsol(ia,i) * 100.0

                  goodwind = (.not. FTEST(speed,51.0))
                  goodwind = (goodwind .and. .not. FTEST(direc,510.0))
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

      RSO_rdukmo = nrep
      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue
      RSO_rdukmo = -1
      print *, 'vam(RSO_rdukmo): unexpected EOF'
      go to 999

 901  continue
      RSO_rdukmo = -2
      print *, 'vam(RSO_rdukmo): max dimensions exceeded.'
      go to 999

 902  continue
      RSO_rdukmo = -3
      print *, 'vam(RSO_rdukmo): illegal thinning increment: ',nthin
      go to 999

 999  return
      end
