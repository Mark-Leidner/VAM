c!#   $Id: vam.F,v 1.2 1998/09/29 11:58:19 leidner Exp $
c!#   $Log: vam.F,v $
c!#   Revision 1.2  1998/09/29 11:58:19  leidner
c!#   added call to system timing function
c!#
c!#   Revision 1.1  1997/02/10 21:06:53  leidner
c!#   Initial revision
c!#
c**********************************************************************
c English Name: VAM (Variational Analysis Method)
c -------------
c
c Purpose: A gridded surface wind analysis is obtained by minimizing an
c -------- objective function, the magnitude of which measures the
c          error made by the gridded analysis in fitting satellite
c          wind speed data, SASS ambiguous winds, ERS1 backscatter,
c          conventional surface wind observations and the forecast
c          surface wind field.
c
c Notes: 1) Execution of this program is completely guided by namelist
c ------    parameters (see "vam.namelist.doc").
c
c Usage: vam [namelist] [iolist] [sdate] [stime] [edate] [etime] [hinc]
c ------
c
c   namelist - name of file containing control parameters for executing
c              the program (see "vam.namelist.doc" for information on 
c              building namelists).
c
c   iolist - name of file containing input and output data set names
c            along with fortran unit numbers and format control
c            parameters (see "vam.io.doc"). Data sets are referenced by
c            unit number in the namelist file.
c        
c Variable Description:
c ---------------------
c
c Principal Programmer: Dr. Ross N. Hoffman
c --------------------- (Atmospheric and Environmental Research)
c                       (840 Memorial Dr.)
c                       (Cambridge, MA 02139)
c
c Programmer: Joseph V. Ardizzone **
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c      **Responsible for developing I/O routines and re-writting 
c        the main driver.
c
c Modified: April 14, 1995 - new I/O, driver. (J.A.)
c ---------
c**********************************************************************
#include "vam.h"
c
c     Local Variables
c     ---------------
c
      logical more_to_do
      integer getargs,istat,ierr,idate,itime
      integer indate,intime,iedate,ietime,inchrs
      character*132 fname1,fname2
      character*52 procedure
      real*4 e, etime, t(2)
c
c     Retrieve Input Arguments
c     ========================
c
      if (getargs(fname1,fname2,indate,intime,iedate,
     &                         ietime,inchrs) .ne. 0) go to 999
c
c     Open VAM procedure file and I/O list.
c     =====================================
c
      open(unit=IUNML,file=fname1,status='old',err=900)
      open(unit=IULST,file=fname2,form='formatted',status='old',err=901)

      call iodisable(1,7)
      call iodisable(IUNML,IUNML)
      call iodisable(IULST,IULST)
      if (ioload(IULST) .ne. 0) go to 998
c
c     Begin Variational Analysis
c     ==========================
c
      do while (indate .lt. iedate .or. (indate .eq. iedate .and.
     &                                           intime .le. ietime))
c
c        Initialize
c        ==========
c
         rewind IUNML
         idate = indate
         itime = intime
         call initvam
         more_to_do = .true.
c
c        Execute VAM control procedures as directed by namelist.
c        =======================================================
c
         do while (more_to_do)

            read (IUNML,fmt='(a52)',iostat=istat) procedure
            if (istat .eq. 0) write(*,100) procedure,idate,itime
c
c           No more procedures.
c           -------------------
c
            if (istat .ne. 0 .or. procedure(1:3) .eq. 'END') then 

               more_to_do = .false.
c
c           Event Type: Read
c           ----------------
c
            elseif (procedure(1:4) .eq. 'READ') then 

               ierr = read_event(procedure,idate,itime)
               if (ierr .lt. 0) go to 998
               if (ierr .ne. 0) more_to_do = .false.
c
c           Event Type: Write
c           -----------------
c
            elseif (procedure(1:5) .eq. 'WRITE') then 

               ierr = write_event(procedure,idate,itime)
               if (ierr .lt. 0) go to 998
               if (ierr .ne. 0) more_to_do = .false.
c
c           Event Type: Execute
c           -------------------
c
            else

               ierr = execute_event(procedure,idate,itime)
               if (ierr .lt. 0) go to 998
               if (ierr .ne. 0) more_to_do = .false.

            endif

         end do

         call inctime(indate,intime,inchrs,0,0)

         
      end do

      e = etime( t )

      print *
      print *, 'elapsed:', e, ', user:', t(1), ', sys:', t(2)
      print *

      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue
      print *, 'vam: unable to access namelist file.'
      call exit(1)
      go to 999

 901  continue
      print *, 'vam: unable to access IO list file.'
      call exit(1)
      go to 999

 998  continue
      call exit(1)
      go to 999

 100  format(/,1x,132('*'),
     &       //,1x,'Procedure: "',a52,'"','; Date/Time: ',2i8)

 999  continue
      call exit(0)
      stop
      end
