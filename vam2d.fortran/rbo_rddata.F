c!#   $Id: rbo_rddata.F,v 1.3 1998/01/26 16:46:55 leidner Exp $
c!#   $Log: rbo_rddata.F,v $
c!#   Revision 1.3  1998/01/26 16:46:55  leidner
c!#   added polynomial Kp model as default ers1 Kp
c!#
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function RBO_rddata(iu,nbuff,dbmin,dbmax,minsig0,nthin,itag,
     O     iKpType)
c**********************************************************************
c English Name: Read Data (RBO - Read Backscatter from OIrepack)
c -------------
c
c Purpose: Reads in data from OI repack backscatter buffer (form=7).
c -------- Reports are stored in VAM backscatter data buffers 
c          (see "er_bscat.h").
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
c     real dbmin,dbmax
c     integer RBO_rddata,iu,nbuff,nthin,itag,minsig0
c
c     RBO_rddata (O) - function return value:
c
c        RBO_rddata >= 0 (number of reports stored)
c                     -1 (unexpected EOF)
c                     -2 (max dimensions exceeded)
c                     -3 (illegal thinning increment)
c
c     iu (I) - fortran unit number assigned to input OI repack data.
c     nbuff (I) - number of records to be read (see "RPK_rdskip").
c     dbmin - minimum acceptable value for s0 (DB)
c     dbmax - maximum acceptable value for s0 (DB)
c     minsig0 - minimum number of valid s0's needed to accept the report
c     nthin - specifies how to thin (reduce) the data:
c
c                    nthin = 1 (every report used; every swath)
c                            2 (every other report used> 19,17,15,...
c                               from every other swath)
c                            .               .
c                            .               .
c     itag (I) - arbitrary data identifier tag useful for isolating 
c                data types. 
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)
c             (NASA Goddard Space Flight Center)
c
c Modified: February 7, 1995 - created.
c ---------
c**********************************************************************
      parameter (PI = 3.1415927)
#include "vam.h"
c
c     Argument List
c     -------------
c
      real dbmin,dbmax
      integer iu,nbuff,nthin,itag,minsig0
c
c     Local Variables
c     ---------------
c
      logical forb,midb,aftb,reject
      real xc,yc,azia,azif,azim,rlon8,rlat8
      integer i,k,n,nrep,ic,jc,ngood,nreject
c
c     Common Blocks
c     -------------
c
#include "gparm.h"
#include "er_bscat.h"
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

      data fill/999.0/
c
c     ------------------------------------------------------------------
c
c**   0. Define more local variables
c
c     Constants for model of Kp model based on Ad's estimates
      real C0, CS0, CTh, CS0Th, CS0Th2
      parameter (C0 = 17.81416/100, CS0 = -2.693393/100,
     &     CTh = -0.4566886/100, CS0Th = 0.1087795/100,
     &     CS0Th2 = -0.001338454/100)

c     Degrees   Radians to degrees (180/Pi)
      real Degrees
      parameter (Degrees = 180/0.314159265358979d+01)

c!#~   iKpTpye   Method of calculation of Kp
c!#~   iKpType.  (1=>data set values, 2=>Ad Stoffelen's model)
      integer iKpType

      real ThDeg, S0dB, Kp

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

            nrep = nrep + 1
            n    = er_nobs + nrep
            if (n .gt. ER_MOBS) go to 901
c
c           Store current observation.
c           --------------------------
c
            er_time(n)    = rtime(i) * 60.0
            er_lat(n)     = rlat(i)
            er_lon(n)     = rlon(i)
            er_qc(n)      = 0
            er_ic(n)      = ic
            er_xc(n)      = xc
            er_jc(n)      = jc
            er_yc(n)      = yc
            er_tag(n)     = itag
            er_theta(n,1) = rthtf(i) * PI/180.0
            er_theta(n,2) = rthtm(i) * PI/180.0
            er_theta(n,3) = rthta(i) * PI/180.0
            azif          = razif(i) + 180.0
            if (azif .ge. 360.0) azif = azif - 360.0
            azim          = razim(i) + 180.0
            if (azim .ge. 360.0) azim = azim - 360.0
            azia          = razia(i) + 180.0
            if (azia .ge. 360.0) azia = azia - 360.0
            er_azim (n,1) = azif * PI/180.0
            er_azim (n,2) = azim * PI/180.0
            er_azim (n,3) = azia * PI/180.0
            er_s0obs(n,1) = ER_FILL
            er_s0obs(n,2) = ER_FILL
            er_s0obs(n,3) = ER_FILL
            er_kp (n,1)   = 0.0
            er_kp (n,2)   = 0.0
            er_kp (n,3)   = 0.0
            er_s0sd(n,1)  = 1.0
            er_s0sd(n,2)  = 1.0
            er_s0sd(n,3)  = 1.0

            if (forb) then
               er_s0obs(n,1) = 10.0**(rsigf(i)/10.0)
               er_kp (n,1)   = rkpf(i) / 100.0
            endif

            if (midb) then
               er_s0obs(n,2) = 10.0**(rsigm(i)/10.0)
               er_kp (n,2)   = rkpm(i) / 100.0
            endif

            if (aftb) then
               er_s0obs(n,3) = 10.0**(rsiga(i)/10.0)
               er_kp (n,3)   = rkpa(i) / 100.0
            endif

c!#   Calculate kp with a function polynomial in theta and s0

            do 3 jb = 1, ER_MBEAM
               if (iKpType .eq. 2) then
                  ThDeg = Degrees*er_theta(n, jb)
                  S0dB = 10*LOG10( ABS( er_s0obs(n, jb) ) )
                  Kp = C0 + CTh*ThDeg +
     &                 (CS0 + (CS0Th + CS0Th2*ThDeg)*ThDeg)*S0dB
                  er_kp(n, jb) = MAX(Kp, er_kp(n, jb))
               endif
 3          continue 

 5       continue

 10   continue

      RBO_rddata = nrep
      write(*,100) nreject
      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue
      RBO_rddata = -1
      print *, 'vam(RBO_rddata): unexpected EOF'
      go to 999

 901  continue
      RBO_rddata = -2
      print *, 'vam(RBO_rddata): max dimensions exceeded.'
      go to 999

 902  continue
      RBO_rddata = -3
      print *, 'vam(RBO_rddata): illegal thinning increment: ',nthin
      go to 999

 100  format(/,1x,'vam(RBO_rddata): ',i6,' obs rejected.')

 999  return
      end
