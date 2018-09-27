c!#   $Id: wss_wrdata.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: wss_wrdata.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function WSS_wrdata(iu,iform,itag,alias,assign_spd,assign_dir)
c**********************************************************************
c English Name: Write Data (WSS - Write SASS in SASS Repack)
c -------------
c
c Purpose: Writes observations stored in VAM SASS data buffer (see
c -------- "ptdta.h") to output SASS Repack data set according to
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
c     integer WSS_wrdata,iu,iform,itag(MAXTAG)
c     logical alias(4),assign_spd,assign_dir
c
c     WSS_wrdata (O) - function return value:
c
c           WSS_wrdata >= 0 (number of reports written)
c
c     iu (I) - fortran unit number assigned to output SASS repack data.
c     iform (I) - SASS repack buffer type.
c     itag (I) - data identifier tags designating which observations
c                are to be written (see "ALLTYPE" in vam.h).
c     alias (I) - flags indicating which aliases are to be considered:
c
c             alias = .false. (ignore alias)
c                      .true. (accept alias)
c
c     assign_spd (I) - flag indicating that the analyzed speed is to be
c                      assigned to each alias:
c
c             assign_spd = .true. (assign analyzed speed)
c                         .false. (do not assign speed)
c
c     assign_dir (I) - flag indicating that the analyzed direction is
c                      to be assigned to each alias:
c
c             assign_dir = .true. (assign analyzed direction)
c                         .false. (do not assign direction)
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 12, 1995 - created.
c ---------
c**********************************************************************
#include "vam.h"

      real SASFILL
      parameter (SASFILL = 999.9)
c
c     Argument List
c     -------------
c
      integer iu,iform,itag(MAXTAG)
      logical alias(4),assign_spd,assign_dir
c
c     Local Variables
c     ---------------
c
      logical skipobs
      real rlat,rlon,vsol(4),dsol(4)
      integer i,j,n,ia,nrep,nsearch,nbuff
c
c     Common Blocks
c     -------------
c
#include "ptdta.h"
c
c     Common Block - "IO Buffer"
c     --------------------------
c
      integer*4 ibuf(MAXREC)
      real*4 form,buff,rpkbuf(204),rbuf(MAXREC-206)
      real*4 rpkspd(17,4),rpklat(17),rpklon(17)
      real*4 rpkdir(17,4),rpkals(17),rpkflg(17)

      common /iobuf/ rpklat,rpklon,rpkspd,rpkdir,rpkals,rpkflg,
     &               form,buff,rbuf,ibuf

      equivalence (rpkbuf(1  ),rpklat),
     &            (rpkbuf(18 ),rpklon),
     &            (rpkbuf(35 ),rpkspd),
     &            (rpkbuf(103),rpkdir),
     &            (rpkbuf(171),rpkals),
     &            (rpkbuf(188),rpkflg)
c
c     Count the number of observations to be written.
c     ===============================================
c
      nrep = 0
      ntag = isrch(MAXTAG,itag,ALLTYPE) - 1
      if (ntag .lt. 0) ntag = MAXTAG
      
      if (ntag .eq. 0) then 
         nrep = npts
      else
         do n = 1,npts
            if (isrch(ntag,itag,iobs(n)) .ne. 0) nrep = nrep + 1
         end do
      endif
c
c     Create SASS Repack sub-header record.
c     =====================================
c
      nbuff = nrep / 17
      if (mod(nrep,17) .ne. 0) nbuff = nbuff + 1

      form = float(iform)
      buff = float(nbuff)
      write(iu) form,buff
c
c     Create SASS Repack buffer.
c     ==========================
c
      n = 0
      do i = 1,nrep,17

         nsearch = min(17,nrep-i+1)
         call setcon4(204,rpkbuf,SASFILL)
         call setcon4(17,rpkals,0.0)

         do j = 1,nsearch
 
            skipobs = .true.

            do while (skipobs) 

               n = n + 1

               if (ntag.eq.0 .or. isrch(ntag,itag,iobs(n)).ne.0) then 

                  skipobs = .false.
c
c                 Save Data
c                 ---------
c
                  rpkals(j) = PT_dealias(n,alias,assign_spd,assign_dir,
     &                                      rlat,rlon,vsol,dsol,SASFILL)

                  rpklat(j) = rlat
                  rpklon(j) = rlon
                  rpkflg(j) = pt_qc(n)

                  do ia = 1,4
                     rpkspd(j,ia) = vsol(ia)
                     rpkdir(j,ia) = dsol(ia)
                  end do

               endif

            end do

         end do
c
c        Write SASS Repack Buffer.
c        -------------------------
c
         write(iu) rpkbuf

      end do

c     Create SASS Repack trailer record.
c     ==================================
c
      form = 0.0
      buff = 0.0
      write(iu) form,buff

      WSS_wrdata = nrep

      return
      end
