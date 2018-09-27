c!#   $Id: execute_event.F,v 1.3 1997/04/17 18:57:42 rnh Exp $
c!#   $Log: execute_event.F,v $
c!#   Revision 1.3  1997/04/17 18:57:42  rnh
c!#   Added test event.
c!#
c!#	Revision 1.2  1997/04/04  15:38:19  rnh
c!#	Merging NSCATo and VAM1
c!#
c!#	Revision 1.1.2.1  1997/02/21  19:42:27  leidner
c!#	added "NSCAT BACKSCATTER QC", "COPY NSCAT BACKSCATTER", and
c!#	"REGRID" (procedure #'s 7, 10, and 12)
c!#
c!#     Revision 1.1.1.1  1997/02/21  19:36:59  rnh
c!#     added "REGRID" (procedure #10)
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function execute_event(procedure,idate,itime)
c**********************************************************************
c English Name: Execute Event
c -------------
c
c Purpose: Event handler for all VAM processing procedures.
c --------
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
c     character*52 procedure
c     integer execute_event,idate,itime
c
c     execute_event (O) - function return value:
c
c        execute_event = 0 (normal)
c                      -1 (fatal error from called routine)
c                      -2 (unknown procedure)
c                     > 0 (non-fatal error from called routine)
c
c     idate (I) - current date in YYMMDD format.
c     itime (I) - current time in HHMMSS format.     
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 14, 1995 - created.
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      integer idate,itime
      character*52 procedure
c
c     Local Variables
c     ---------------
c
      integer ierr
      character*52 procs(13)

      data procs/'MINIMIZE                                            ',
     &           'SUM OF SQUARES                                      ',
     &           'SORT DATA                                           ',
     &           'CONVENTIONAL QC                                     ',
     &           'SASS QC                                             ',
     &           'BACKSCATTER QC                                      ',
     &           'NSCAT BACKSCATTER QC                                ',
     &           'RESET                                               ',
     &           'COPY BACKSCATTER                                    ',
     &           'COPY NSCAT BACKSCATTER                              ',
     &           'INITIALIZE BACKSCATTER                              ',
     &           'REGRID                                              ',
     &           'TEST                                              '/
c
c     Queue the Event
c     ===============
c     
      ierr = 0
c
c     Minimize
c     --------
c
      if (procedure .eq. procs(1)) then 

         call solve(.true.)
c
c     Sum of Squares
c     --------------
c
      elseif (procedure .eq. procs(2)) then 

         call solve(.false.)
c
c     Sort Data 
c     ---------
c
      elseif (procedure .eq. procs(3)) then 

         call sdata
c
c     Conventional QC
c     ---------------
c
      elseif (procedure .eq. procs(4)) then 

         call residc
c
c     SASS QC 
c     -------
c
      elseif (procedure .eq. procs(5)) then 

         call resid
c
c     Backscatter QC
c     --------------
c
      elseif (procedure .eq. procs(6)) then 

         call resids
c
c     NSCAT Backscatter QC
c     --------------------
c
      elseif (procedure .eq. procs(7)) then 

         call residn
c
c
c     Reset
c     -----
c
      elseif (procedure .eq. procs(8)) then 

         call reset
c
c     Copy Backscatter
c     ----------------
c
      elseif (procedure .eq. procs(9)) then 

         call copyers1
c
c     Copy NSCAT Backscatter
c     ----------------
c
      elseif (procedure .eq. procs(10)) then 

         call copynscat
c
c     Initialize Backscatter
c     ----------------------
c
      elseif (procedure .eq. procs(11)) then 

         call initscat
c
c     Regrid all data
c     ---------------
c
      elseif (procedure .eq. procs(12)) then 

         call regrid
c
c     Test procedure
c     --------------
c
      elseif (procedure .eq. procs(13)) then 

         call test
c
c     Unknown Procedure
c     -----------------
c
      else

         execute_event = -2
         print *, 'vam(execute_event): unknown event : "',procedure,'"'
         return

      endif

      if (ierr .lt. 0) ierr = -1
      execute_event = ierr

      return
      end
