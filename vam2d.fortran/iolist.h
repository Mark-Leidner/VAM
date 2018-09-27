c!#   $Id: iolist.h,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: iolist.h,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision (filename originally iolist.com)
c!#
c
c     Common Block - "iolist" (I/O List)
c     ----------------------------------
c
c     integer*2 iomap(MAXUNIT)
c     character*132 iodata(MAXDSET)
c     common /iolist/ iomap,iodata
c
c     iomap - contains index pointer into I/O control list containing
c             data set parameters associated with each fortran unit
c             number:
c
c             iomap(iu) =    0  (no data sets)
c                        IOFILL (iu is reserved;see iosubs.h)
c                           >0 (index pointer into I/O control list)
c
c     iodata - contains I/O parameters for all data sets to be accessed
c              during execution. Parameters are exclamation separated
c              fields within a character string:
c
c           iodata(i) = 'unit number!filename!format'
c
c                 ex.) '12!test.data!-F f77 -N ieee'
c

      integer ionum,iomap(MAXUNIT)
      character*132 iodata(MAXDSET)
      common /iolist/ ionum,iomap,iodata
