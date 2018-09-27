c!#   $Id: rco_rddata.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: rco_rddata.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function RCO_rddata(iu,nbuff,nkx,kx,scfact,wfact,itag)
c**********************************************************************
c English Name: Read Conventional (RCO -Read Conventional from OIrepack)
c -------------
c
c Purpose: Reads in conventional wind observations from OI repack
c -------- surface data buffer. Reports are stored in VAM conventional
c          data buffers (see "sldta.h").
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
c     real scfact(*),wfact(*)
c     integer RCO_rddata,iu,nbuff,nkx,kx(*),itag(*)
c
c     RCO_rddata (O) - function return value:
c
c        RCO_rddata >= 0 (number of reports stored)
c                     -1 (unexpected EOF)
c                     -2 (max dimensions exceeded)
c                     -3 (illegal kx encountered)
c
c     iu (I) - fortran unit number assigned to input OI repack data.
c     nbuff (I) - number of records to be read (see "RPK_rdskip").
c     nkx (I) - number of desired observation types.
c     kx (I) - desired OI repack observation types.
c     scfact (I) - wind speed adjustment factor for each specified
c                  observation type. This array is indexed according to
c                  each kx value specified:
c
c              ex) nkx = 1; kx=28; scfact(28) = 0.987
c
c     wfact (I) - VAM weighting factor for each specified observation
c                 type (0-1). This array is indexed according to
c                 each kx value specified:
c
c              ex) nkx = 1; kx=28; wfact(28) = 1.0
c
c     itag (I) - data identifier tags for each specified observation
c                type. This array is indexed according to each kx
c                value specified:
c
c              ex) nkx = 1; kx=28; itag(28) = 1
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
      real scfact(*),wfact(*)
      integer iu,nbuff,nkx,kx(*),itag(*)
c
c     Local Variables
c     ---------------
c
      logical goodwind
      real rlon,rlat,xgrid,ygrid,fill
      integer i,k,n,ipt,nrep,igrid,jgrid,kindex
c
c     Common Blocks
c     -------------
c
#include "sldta.h"
#include "gparm.h"
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
c
c     Begin reading OI buffers
c     ========================
c
      nrep = 0
      do n = 1,nbuff

         read(iu,end=900,err=900) buffer

         do 5 i = 1,113

            if (abs(spkx(i)) .ge. FILL) go to 5
c
c           Check that ob is in analysis region.
c           ------------------------------------
c
            rlon = splon(i)
            rlat = splat(i)
            if (rlon .lt. 0.0) rlon = rlon + 360.0
            call gridx(rlon,xs,delx,iper,xmin,xmax,igrid,xgrid,*5)
            call gridx(rlat,ys,dely,   0,ymin,ymax,jgrid,ygrid,*5)
c
c           Check that ob is valid.
c           -----------------------
c
            kindex   = int(spkx(i))
            if (kindex .gt. MAXKX .or. kindex .lt. 0) go to 902
            k        = isrch(nkx,kx,kindex)

            goodwind = (k .ne. 0)
            goodwind = (goodwind .and. abs(us(i)) .lt. FILL)
            goodwind = (goodwind .and. abs(vs(i)) .lt. FILL)
c
c           Save ob in conventional data buffers.
c           -------------------------------------
c
            if (goodwind) then
            
               nrep = nrep + 1
               ipt = nptc + nrep
               if (ipt .gt. SLMAX) go to 901

               ilc(ipt)   = igrid
               jlc(ipt)   = jgrid
               xconv(ipt) = xgrid
               yconv(ipt) = ygrid
               uconv(ipt) = us(i) * scfact(kindex)
               vconv(ipt) = vs(i) * scfact(kindex)
               iconv(ipt) = itag(kindex)
               wconv(ipt) = wfact(kindex)
               sl_qc(ipt) = 0

            endif

 5       continue

      end do

      RCO_rddata = nrep
      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue
      RCO_rddata = -1
      print *, 'vam(RCO_rddata): unexpected EOF'
      go to 999

 901  continue
      RCO_rddata = -2
      print *, 'vam(RCO_rddata): max dimensions exceeded.'
      go to 999

 902  continue
      RCO_rddata = -3
      print *, 'vam(RCO_rddata): illegal kx encountered.'
      go to 999

 999  return
      end
