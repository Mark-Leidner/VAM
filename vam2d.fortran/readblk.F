c!# CSU IDENTIFICATION : readblk.F
c!#     $Id: readblk.F,v 1.3 1997/08/11 15:32:37 leidner Exp $

c!## PURPOSE : Reads one block of data from version-4 SSMI data sets
c!# from Frank Wentz (RSS).

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## INPUT/OUTPUT INTERFACE :

      subroutine readblk                                                !#
     c    ( iu,                                                         !#
     i    iblock,                                                       !#
     o    lblk, ieof )                                                  !#

c!# iu               unit number of SSMI data file
c!# iblock           record number for direct read
c!# lblk             retrieved data block (30 rows).  
c!# ieof             end of file flag:
c!#                     = 0 (normal)
c!#                       1 (eof reached, next call will access new file)
      integer iblock, ieof, iu                                          !#
      character*1 lblk(994,30)                                          !#

c!## DATA CONVERSION :

c!## ALGORITHMS :

c!## REFERENCES :

c!## LIMITATIONS :

c!## CHANGE LOG : $Log: readblk.F,v $
c!## CHANGE LOG : Revision 1.3  1997/08/11 15:32:37  leidner
c!## CHANGE LOG : updated documentation and removed file open code to readssmi.F
c!## CHANGE LOG :
c!#	Revision 1.2  1997/04/16  15:44:11  leidner
c!#	removed mail text and added rec # to sequential, direct read
c!#
c!#	Revision 1.1  1997/04/16  14:53:58  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

c!## LOCAL DATA ELEMENTS :

c!## LOCAL DATA STRUCTURES :

c!## DATA FILES :

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c
c     ------------------------------------------------------------------
c
c!#   1.  Initialize eof flag to "end of file reached" value.
c
      ieof = 1
c
c     ------------------------------------------------------------------
c
c!#   2.  Read record iblock from ssmi data file.
c
      read(iu,iostat=istat,rec=iblock,err=100) lblk
      goto 101
 100  print *, 'Trouble reading ssmi file, iostat=', istat
c
c     ------------------------------------------------------------------
c
c!#   3.  If no read errors and istat is nonzero (-1), we have reached
c!#       eof mark.  Return.
c
 101  if (istat .ne. 0) return
c
c     ------------------------------------------------------------------
c
c!#   4.  Set eof flag to normal value (not at end of file).
c
      ieof = 0
c
c     ------------------------------------------------------------------
c
c!## ERROR HANDLING :
      return
      end
