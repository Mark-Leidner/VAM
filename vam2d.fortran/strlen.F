c!#   $Id: strlen.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: strlen.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
       function strlen(str)
c**********************************************************************
c English Name: String Length
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
c Modified: July 1990 - created.
c --------- 10/4/90 - took out "maxlen" argument. Using "len" function
c                     instead to get the max length of string.
c**********************************************************************
c
c     Argument List
c     -------------
c
      integer strlen
      character*(*) str
c
c     Determine length of base string.
c     ================================
c
      i = len(str)
      strlen = 0

      do while (i .gt. strlen)

         if (str(i:i) .ne. ' ') then
            strlen = i
         else
            i = i - 1
         endif

       end do

      return
      end
