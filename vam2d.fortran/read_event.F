c!#   $Id: read_event.F,v 1.6 1998/03/04 14:20:40 leidner Exp $
c!#   $Log: read_event.F,v $
c!#   Revision 1.6  1998/03/04 14:20:40  leidner
c!#   added NSCAT 25km mgdr file handling
c!#
c!#   Revision 1.5  1997/08/11 17:48:01  leidner
c!#   added ssmi los read routine
c!#
c!#	Revision 1.4  1997/04/11  17:42:18  leidner
c!#	added NSCAT data types to read function calls
c!#
c!#	Revision 1.3  1997/04/08  20:14:08  leidner
c!#	dump_restore_grid now declared as integer
c!#
c!#	Revision 1.1.2.1  1997/02/21  21:48:03  leidner
c!#	added "READ GRID FROM GDAS",
c!#	      "READ NSCAT BACKSCATTER FROM LEVEL 1.7 HDF DATA FILE",
c!#	      "READ NSCAT WINDS FROM LEVEL 2.0 HDF DATA FILE",
c!#	      "READ GRID FROM VAM RESTART" (procedure #'s 3, 15, 16 and 17)
c!#
c!#	Revision 1.1.1.1  1997/02/21  21:44:55  rnh
c!#	added procedure READ GRID FROM VAM RESTART (proc #14)
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function read_event(procedure,idate,itime)
c**********************************************************************
c English Name: Read Event
c -------------
c
c Purpose: Event handler for all VAM input (read) procedures.
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
c     integer read_event,idate,itime
c
c     read_event (O) - function return value:
c
c        read_event = 0 (normal)
c                    -1 (fatal error from called routine)
c                    -2 (unknown procedure)
c                   > 0 (non-fatal error from called routine)
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
      character*52 procs(19)

      data procs/'READ GRID FROM V8INTERP                             ',
     &           'READ GRID FROM PHOENIX                              ',
     &           'READ GRID FROM GDAS                                 ',
     &           'READ CONVENTIONAL FROM OI REPACK                    ',
     &           'READ CONVENTIONAL USING ERS1 FROM OI REPACK         ',
     &           'READ CONVENTIONAL USING UKMO FROM OI REPACK         ',
     &           'READ SASS FROM SASS REPACK                          ',
     &           'READ SASS USING SSMI FROM SASS REPACK               ',
     &           'READ SASS USING ERS1 FROM SASS REPACK               ',
     &           'READ SASS USING BACKSCATTER FROM OI REPACK          ',
     &           'READ SASS USING ERS1 FROM OI REPACK                 ',
     &           'READ SASS USING SSMI FROM OI REPACK                 ',
     &           'READ SASS USING UKMO FROM OI REPACK                 ',
     &           'READ BACKSCATTER FROM OI REPACK                     ',
     &           'READ NSCAT BACKSCATTER FROM LEVEL 1.7 HDF DATA FILE ',
     &           'READ NSCAT WINDS FROM LEVEL 2.0 HDF DATA FILE       ',
     &           'READ GRID FROM VAM RESTART                          ',
     &           'READ SSMI LOS WINDS                                 ',
     &           'READ NSCAT 25KM MERGED DATA FILE                    '/

      integer RGG_rdgrid,dump_restore_grid,rsl_rdlos,rbm_rdback
c
c     Queue the Event
c     ===============
c     
      ierr = 0
c
c     Read Grid from V8Interp
c     -----------------------
c
      if (procedure .eq. procs(1)) then 

         ierr = RGV_rdgrid(idate,itime)
c
c     Read Grid from Phoenix
c     ----------------------
c
      elseif (procedure .eq. procs(2)) then 

         ierr = RGP_rdgrid(idate,itime)
c
c     Read Grid from GDAS
c     -------------------
c
      elseif (procedure .eq. procs(3)) then 

         ierr = RGG_rdgrid(idate,itime)
c
c     Read Conventional from OI Repack.
c     ---------------------------------
c
      elseif (procedure .eq. procs(4)) then 

         ierr = RCO_rdconv(idate,itime,STANDARD)
c
c     Read Conventional using ERS1 from OI Repack.
c     --------------------------------------------
c
      elseif (procedure .eq. procs(5)) then 

         ierr = RCO_rdconv(idate,itime,ERS1)
c
c     Read Conventional using UKMO from OI Repack.
c     --------------------------------------------
c
      elseif (procedure .eq. procs(6)) then 

         ierr = RCO_rdconv(idate,itime,UKMO)
c
c     Read SASS from SASS Repack.
c     ---------------------------
c
      elseif (procedure .eq. procs(7)) then 

         ierr = RSS_rdsass(idate,itime,STANDARD)
c
c     Read SASS using SSMI from SASS Repack.
c     --------------------------------------
c
      elseif (procedure .eq. procs(8)) then 

         ierr = RSS_rdsass(idate,itime,SSMI)
c
c     Read SASS using ERS1 from SASS Repack.
c     --------------------------------------
c
      elseif (procedure .eq. procs(9)) then 

         ierr = RSS_rdsass(idate,itime,ERS1)
c
c     Read SASS using Backscatter from OI Repack
c     ------------------------------------------
c
      elseif (procedure .eq. procs(10)) then 

         ierr = RSO_rdsass(idate,itime,BACKSCATTER)
c
c     Read SASS using ERS1 from OI Repack
c     -----------------------------------
c
      elseif (procedure .eq. procs(11)) then 

         ierr = RSO_rdsass(idate,itime,ERS1)
c
c     Read SASS using SSMI from OI Repack
c     -----------------------------------
c
      elseif (procedure .eq. procs(12)) then 

         ierr = RSO_rdsass(idate,itime,SSMI)
c
c     Read SASS using UKMO from OI Repack
c     -----------------------------------
c
      elseif (procedure .eq. procs(13)) then 

         ierr = RSO_rdsass(idate,itime,UKMO)
c
c     Read Backscatter from OI Repack.
c     --------------------------------
c
      elseif (procedure .eq. procs(14)) then 

         ierr = RBO_rdback(idate,itime,STANDARD)
c
c     Read NSCAT Backscatter from Level 1.7 JPL data from HDF file.
c     ----------------------------------------------------------------
c
      elseif (procedure .eq. procs(15)) then 

         ierr = RNB_rdback(idate,itime,STANDARD_NSCAT)
c
c     Read NSCAT Winds from Level 2.0 JPL data from HDF file.
c     ----------------------------------------------------------------
c
      elseif (procedure .eq. procs(16)) then 

         ierr = RNW_rdwind(idate,itime,NSCAT_WINDS)
c
c     Read Grid in Restart
c     ---------------------
c
      elseif (procedure .eq. procs(17)) then
       
         ierr = dump_restore_grid(idate,itime,.FALSE.)
c
c     Read SSMI Line of Sight Winds
c     ------------------------------
c
      elseif (procedure .eq. procs(18)) then
       
         ierr = rsl_rdlos(idate,itime)

c
c     Read NSCAT 25 km Merged Geophysical Data Records from files
c     -----------------------------------------------------------
c
      elseif (procedure .eq. procs(19)) then
       
         ierr = rbm_rdback(idate,itime,STANDARD_NSCAT)

c
c     Unknown Procedure
c     -----------------
c
      else

         read_event = -2
         print *, 'vam(read_event): unknown event : "',procedure,'"'
         return

      endif

      if (ierr .lt. 0) ierr = -1
      read_event = ierr

      return
      end
