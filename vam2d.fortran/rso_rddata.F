c!#   $Id: rso_rddata.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: rso_rddata.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function RSO_rddata(iu,nbuff,speedonly,scfact,wfact,alias,itag,
     &                                                     kx,dirinc)
c**********************************************************************
c English Name: Read Data (RSO - Read SASS from OIrepack)
c -------------
c
c Purpose: Reads in data from OI repack surface buffer (form=2).
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
c     real scfact,wfact,dirinc(4)
c     integer RSO_rddata,iu,nbuff,itag,kx
c
c     RSO_rddata (O) - function return value:
c
c        RSO_rddata >= 0 (number of reports stored)
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
c
c     kx (I) - OI Repack data type index designating which reports are
c              to be accepted.
c
c     dirinc (I) - directional increment for each alias. For each 
c                  desired alias, this is the clockwise increment from
c                  the primary direction read from the data set (i.e.
c                  there is only one direction stored in an OI repack
c                  surface buffer). Essentially, this allows one to
c                  bogus aliases to create ambiguous winds:
c
c                  dirinc = 0,180.0; alias = T,T,F,F
c  
c                  This creates two aliases in opposite directions, the
c                  first alias being the extracted wind direction and
c                  speed with the second having the same speed but 180
c                  degree opposite direction.
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)
c             (NASA Goddard Space Flight Center)
c
c Modified: April 24, 1995 - created.
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      logical speedonly,alias(4)
      real scfact,wfact,dirinc(4)
      integer iu,nbuff,itag,kx
c
c     Local Variables
c     ---------------
c
      integer i,k,ia,na,ipt,nrep,ic,jc,kindex
      real rlat,rlon,xc,yc,speed,direc,ospeed,odirec,factor,vavg,fill
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
      real*4 buffer(1017)
      integer*4 ibuf(MAXREC)
      real*4 rbuf(MAXREC-1017)
      real*4 sptim(113),spkx(113),splat(113),splon(113)
      real*4 psl(113),us(113),vs(113),qpsl(113),qws(113)

      common /iobuf/ sptim,spkx,splat,splon,psl,us,vs,qpsl,qws,rbuf,ibuf

      equivalence(buffer(1),sptim(1))

      data fill/999.0/
      data factor/1.74532925199432946D-2/
c
c     Begin reading OI buffers
c     ========================
c
      nrep    = 0
      do 10 k = 1,nbuff

         read(iu,end=900,err=900) buffer

         do 5 i = 1,113

            if (abs(spkx(i)) .ge. FILL) go to 5
c
c           Transform (long,lat) to grid coordinates.
c           -----------------------------------------
c
            rlon = splon(i)
            rlat = splat(i)
            if (rlon .lt. 0.0) rlon = rlon + 360.0
            call gridx(rlon,xs,delx,iper,xmin,xmax,ic,xc,*5)
            call gridx(rlat,ys,dely,   0,ymin,ymax,jc,yc,*5)
c
c           Check that ob is valid.
c           -----------------------
c
            kindex   = int(spkx(i))
            if (kindex .ne. kx) go to 5
            if (abs(us(i)) .ge. FILL) go to 5
            if (abs(vs(i)) .ge. FILL) go to 5

            call winds(us(i),vs(i),ospeed,odirec,.false.)
            odirec = odirec / factor
            if (ospeed .le. 0.0) go to 5

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

                  speed = ospeed
                  direc = odirec + dirinc(ia)
                  if (direc .ge. 360.0) direc = direc - 360.0
 
                  nwinds(ipt) = nwinds(ipt) + 1
                  na          = nwinds(ipt)
                  speed       = speed * scfact
                  direc       = direc * factor
                  vbar(ipt)   = vbar(ipt) + speed**2
                  vavg        = vavg + speed

                  call winds(uobs(na,ipt),vobs(na,ipt),
     &                                             speed,direc,.true.)

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

      RSO_rddata = nrep
      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue
      RSO_rddata = -1
      print *, 'vam(RSO_rddata): unexpected EOF'
      go to 999

 901  continue
      RSO_rddata = -2
      print *, 'vam(RSO_rddata): max dimensions exceeded.'
      go to 999

 999  return
      end
