c!#   $Id: stshft.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: stshft.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      subroutine stshft(str,dir)
c**********************************************************************
c English Name: String Shift
c -------------
c
c Purpose: Right or left justifies a character string.
c --------
c
c Notes:
c ------
c
c Variable Description:
c ---------------------
c
c Programmer: Joseph V. Ardizzone
c ----------- (Centel Federal Services Corporation)
c
c Modified: February 25, 1990 - created.
c ---------
c**********************************************************************
      integer LEFT,RIGHT
      parameter (LEFT=-1,RIGHT=1)
c
c     Argument List
c     -------------
c
      character*(*) str
      integer dir

      istart = 0
      ilen = len(str)
c
c     Left shift.     
c     ===========
c
      if (dir .eq. LEFT) then

         i=1
         do while (istart .eq. 0)
            if (str(i:i) .ne. ' ' .or. i .eq. ilen) istart = i
            i = i + 1
         end do

         k = 1
         if (istart .eq. 1) go to 999

         do 5 i = istart,ilen
            str(k:k) = str(i:i)
            str(i:i) = ' '
            k = k + 1
 5       continue
c
c     Right shift
c     ===========
c
      elseif (dir .eq. RIGHT) then

         i = ilen
         do while (istart .eq. 0)
            if (str(i:i) .ne. ' ' .or. i .eq. 1) istart = i
            i = i - 1
         end do

         k = ilen
         if (istart .eq. ilen) go to 999

         do 10 i = istart,1,-1
            str(k:k) = str(i:i)
            str(i:i) = ' '
            k = k - 1
 10      continue

       endif

 999  return
      end
