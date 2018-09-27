c!# CSU IDENTIFICATION : dump_event.F
c!#   $Id: dump_event.F,v 1.4 2000/01/21 16:16:11 trn Exp $

c!## PURPOSE : Event handler for all VAM output (write) procedures.

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## INPUT/OUTPUT INTERFACE :

      integer function dump_event(procedure,idate,itime,iuvam)     !#

#include "vam.h"                                      !#

c!# Input:
c!#~	procedure	Procedure name
c!#~	idate		Current date in YYMMDD format
c!#~	itime		Current time in HHMMSS format
      character*52 procedure    !#
      integer idate, itime, iuvam      !#

c!# Output:
c!#~	dump_event	Function return value
c!#			=  0 (normal)
c!#			= -1 (fatal error from called routine)
c!#			= -2 (unknown procedure)
c!#			>  0 (non-fatal error from called routine)
c     integer dump_event       !#

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
c!#	$Log: dump_event.F,v $
c!#	Revision 1.4  2000/01/21 16:16:11  trn
c!#	Add support for NSCAT scatterometer data
c!#	
c!#	Revision 1.3  1999/10/08 14:30:07  trn
c!#	Updated vam_obs data file structure, some
c!#	reorganization of modules, other stylistic changes.
c!#
c!#	Revision 1.2  1999/08/03 15:58:14  trn
c!#	Add code for ambiguous winds (dump_ptdta)
c!#
c!#	Revision 1.1  1999/07/21 19:23:54  trn
c!#	Initial revision
c!#
c!#  write_event:
c!#	Revision 1.8  1998/05/15 19:37:02  stanr
c!#	comment out function type declaration (also is in vam.h)
c!#
c!#	Revision 1.7  1997/10/23 20:59:33  leidner
c!#	updated documentation, added type declarations for functions
c!#(...)
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

c!## LOCAL DATA ELEMENTS :

c!#  Namelist
      character*255 hdr_fname, bin_fname
      namelist /dump/ hdr_fname, bin_fname

c!# Integer:
c!#~	dump_restore_grid	Function return value
c!#~	ierr			Error flag
      integer ierr

c!# Character:
c!#~	procs			List of all VAM write (output) procedure names
      character*52 procs(3)    !#
      data procs/'DUMP CONVENTIONAL                                  ',
     &           'DUMP AMBIGUOUS WINDS                               ',
     &           'DUMP NSCAT BACKSCATTER                             '/

c!## LOCAL DATA STRUCTURES :

c!## DATA FILES :

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1.  Queue the Event
      ierr = 0

c!#   Get filename from namelist
      read (IUNML,dump,iostat=ierr) 
      if (ierr .ne. 0) then
         print *,'Error reading dump_event namelist'
         goto 900
      endif

c!#   2.1.  Dump conventional
      if (procedure .eq. procs(1)) then
         call dump_sldta(iuvam, hdr_fname, bin_fname, ierr)
      elseif (procedure .eq. procs(2)) then
         call dump_ptdta(iuvam, hdr_fname, bin_fname, ierr)
      elseif (procedure .eq. procs(3)) then
         call dump_nsbscat(iuvam, hdr_fname, bin_fname, ierr)
      else
         dump_event = -2
         print *, 'vam(dump_event): unknown event : "',procedure,'"'
         return
      endif

c!## ERROR HANDLING :
      if (ierr .lt. 0) ierr = -1
  900 dump_event = ierr
      return
      end
