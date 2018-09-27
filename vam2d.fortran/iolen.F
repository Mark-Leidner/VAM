c!#   $Id: iolen.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: iolen.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
       function iolen(str)
c**********************************************************************
c English Name: I/O String Length 
c -------------
c
c Purpose: Given a character string, this routine searches for the
c -------- first trailing blank and returns the string length.
c
c Notes: Blanks are expected to mark the end of the string.
c ------
c
c Variable Description:
c ---------------------
c
c Programmer: Joseph V. Ardizzone
c ----------- (Centel Federal Services Corporation) 
c
c Modified: April 3, 1995 - created.
c --------- 
c**********************************************************************
c
c     Argument List
c     -------------
c
      character*(*) str
c
c     Determine length of base string.
c     ================================
c
      i = len(str)
      iolen = 0

      do while (i .gt. iolen)

         if (str(i:i) .ne. ' ') then 
            iolen = i
         else
            i = i - 1
         endif

       end do   
      
      return
      end
