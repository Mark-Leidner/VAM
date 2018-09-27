c!# CSU IDENTIFICATION : write_event.F
c!#   $Id: write_event.F,v 1.8 1998/05/15 19:37:02 stanr Exp $

c!## PURPOSE : Event handler for all VAM output (write) procedures.

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## INPUT/OUTPUT INTERFACE :

      function write_event(procedure,idate,itime)     !#

#include "vam.h"                                      !#

c!# Input:
c!#~	procedure	Procedure name
c!#~	idate		Current date in YYMMDD format
c!#~	itime		Current time in HHMMSS format
      character*52 procedure    !#
      integer idate, itime      !#

c!# Output:
c!#~	write_event	Function return value
c!#			=  0 (normal)
c!#			= -1 (fatal error from called routine)
c!#			= -2 (unknown procedure)
c!#			>  0 (non-fatal error from called routine)
c     integer write_event       !#

c!## DATA CONVERSION :

c!## ALGORITHMS :

c!## REFERENCES :

c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 14, 1995 - created.
c ---------

c!## LIMITATIONS :

c!## CHANGE LOG : 
c!#	$Log: write_event.F,v $
c!#	Revision 1.8  1998/05/15 19:37:02  stanr
c!#	comment out function type declaration (also is in vam.h)
c!#	
c!#	Revision 1.7  1997/10/23 20:59:33  leidner
c!#	updated documentation, added type declarations for functions
c!#
c!#	Revision 1.6  1997/08/11 17:49:01  leidner
c!#	added ssmi los ascii write routine
c!#
c!#	Revision 1.5  1997/06/23  17:53:25  leidner
c!#	added wna_wrback
c!#
c!#	Revision 1.4  1997/04/23  16:56:58  leidner
c!#	added write for JPL winds
c!#
c!#	Revision 1.3  1997/04/08  20:31:31  leidner
c!#	two function types declared
c!#
c!#	Revision 1.2  1997/04/04  19:38:58  rnh
c!#	Merging NSCATo and VAM1
c!#
c!#	Revision 1.1.2.1  1997/02/21  22:17:44  leidner
c!#	added "WRITE CONVENTIONAL WINDS IN ASCII",
c!#	      "WRITE NSCAT BACKSCATTER IN OI REPACK",
c!#	      "WRITE NSCAT WINDS IN OI REPACK",
c!#	      "WRITE NSCAT WINDS IN ASCII",
c!#	      "WRITE GRID IN ASCII",
c!#	      "WRITE GRID IN VAM RESTART"
c!#	      (procedure #'s 6, 10, 11, 12, 13, 14)
c!#
c!#	Revision 1.1.1.1  1997/02/21  22:13:57  rnh
c!#	added "WRITE GRID IN ASCII" and "WRITE GRID IN VAM RESTART"
c!#	(procedure #'s 9, 10)
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

c!## LOCAL DATA ELEMENTS :

c!# Integer:
c!#~	dump_restore_grid	Function return value
c!#~	ierr			Error flag
c!#~	WBO_wrback		Function return value (delcared in "vam.h")
c!#~	WBO_wrnback		Function return value
c!#~	WCA_wrwind		Function return value
c!#~	WCO_wrconv		Function return value (delcared in "vam.h")
c!#~	WGA_wrgrid		Function return value
c!#~	WGP_wrgrid		Function return value (delcared in "vam.h")
c!#~	WJA_wrwind		Function return value
c!#~	WNA_wrback		Function return value
c!#~	WNA_wrwind		Function return value
c!#~	WSL_wrlos		Function return value
c!#~	WSS_wrsass		Function return value (delcared in "vam.h")
      integer ierr, dump_restore_grid, WBO_wrnback, WCA_wrwind             !#
      integer WGA_wrgrid, WJA_wrwind, WNA_wrback, WNA_wrwind               !#
      integer WSL_wrlos                                                    !#

c!# Character:
c!#~	procs			List of all VAM write (output) procedure names
      character*52 procs(17)    !#

      data procs/'WRITE SASS IN SASS REPACK                           ',
     &           'WRITE SSMI FROM SASS IN SASS REPACK                 ',
     &           'WRITE ERS1 FROM SASS IN SASS REPACK                 ',
     &           'WRITE GRID IN PHOENIX                               ',
     &           'WRITE CONVENTIONAL IN OI REPACK                     ',
     &           'WRITE CONVENTIONAL WINDS IN ASCII                   ',
     &           'WRITE ERS1 FROM CONVENTIONAL IN OI REPACK           ',
     &           'WRITE BACKSCATTER IN OI REPACK                      ',
     &           'WRITE ERS1 FROM BACKSCATTER IN OI REPACK            ',
     &           'WRITE NSCAT BACKSCATTER IN OI REPACK                ',
     &           'WRITE NSCAT WINDS IN OI REPACK                      ',
     &           'WRITE NSCAT WINDS IN ASCII                          ',
     &           'WRITE GRID IN ASCII                                 ',
     &           'WRITE GRID IN VAM RESTART                           ',
     &           'WRITE JPL WINDS IN ASCII                            ',
     &           'WRITE NSCAT BACKSCATTER IN ASCII                    ',
     &           'WRITE SSMI LOS WINDS IN ASCII                       '/

c!## LOCAL DATA STRUCTURES :

c!## DATA FILES :

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1.  Queue the Event
      ierr = 0

c!#   2.1.  Write SASS in SASS Repack
      if (procedure .eq. procs(1)) then
         ierr = WSS_wrsass(idate,itime,STANDARD)

c!#   2.2.  Write SSMI from SASS in SASS Repack
      elseif (procedure .eq. procs(2)) then 
         ierr = WSS_wrsass(idate,itime,SSMI)

c!#   2.3.  Write ERS1 from SASS in SASS Repack
      elseif (procedure .eq. procs(3)) then
         ierr = WSS_wrsass(idate,itime,ERS1)

c!#   2.4.  Write Grid in Phoenix
      elseif (procedure .eq. procs(4)) then 
         ierr = WGP_wrgrid(idate,itime)

c!#   2.5.  Write Conventional in OI Repack
      elseif (procedure .eq. procs(5)) then 
         ierr = WCO_wrconv(idate,itime,STANDARD)

c!#   2.6.  Write Conventional winds in Ascii
      elseif (procedure .eq. procs(6)) then
         ierr = WCA_wrwind(idate,itime)

c!#   2.7.  Write ERS1 from Conventional in OI Repack
      elseif (procedure .eq. procs(7)) then 
         ierr = WCO_wrconv(idate,itime,ERS1)

c!#   2.8.  Write Backscatter in OI Repack
      elseif (procedure .eq. procs(8)) then 
         ierr = WBO_wrback(idate,itime,STANDARD)

c!#   2.9.  Write ERS1 from Backscatter in OI Repack
      elseif (procedure .eq. procs(9)) then 
         ierr = WBO_wrback(idate,itime,ERS1)

c!#   2.10.  Write NSCAT Backscatter in OI Repack
      elseif (procedure .eq. procs(10)) then 
         ierr = WBO_wrnback(idate,itime,STANDARD_NSCAT)

c!#   2.11.  Write NSCAT Winds in OI Repack
      elseif (procedure .eq. procs(11)) then 
         ierr = WBO_wrnback(idate,itime,NSCAT_WINDS)

c!#   2.12.  Write NSCAT winds in Ascii
      elseif (procedure .eq. procs(12)) then
         ierr = WNA_wrwind(idate,itime)

c!#   2.13.  Write Grid in Ascii
      elseif (procedure .eq. procs(13)) then
         ierr = WGA_wrgrid(idate,itime)

c!#   2.14.  Write Grid in Restart
      elseif (procedure .eq. procs(14)) then 
         ierr = dump_restore_grid(idate,itime,.TRUE.)

c!#   2.15.  Write JPL winds in Ascii
      elseif (procedure .eq. procs(15)) then
         ierr = WJA_wrwind(idate,itime)

c!#   2.16.  Write JPL winds in Ascii
      elseif (procedure .eq. procs(16)) then
         ierr = WNA_wrback(idate,itime)

c!#   2.17.  Write SSMI LOS winds in Ascii
      elseif (procedure .eq. procs(17)) then
         ierr = wsl_wrlos(idate,itime)

c!#   2.X.  Unknown Procedure; set function return value
      else
         write_event = -2
         print *, 'vam(write_event): unknown event : "',procedure,'"'
         return
      endif

c!## ERROR HANDLING :
      if (ierr .lt. 0) ierr = -1
      write_event = ierr
      return
      end
