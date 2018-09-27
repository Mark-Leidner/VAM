c!#   $Id: iolink.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: iolink.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function iolink(iocntl)
c**********************************************************************
c English Name: I/O Link
c -------------
c
c Purpose: Extracts I/O parameters from specified control string and
c -------- connects the data set for I/O.
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
c     integer iolink
c     character*(*) iocntl
c
c     iolink (O) - function return value:
c
c             iolink =  0 (normal)
c                      -1 (error occurred linking data set).
c                       
c     iocntl (I) - I/O control string with exclamation separated fields:
c
c           iocntl = 'unit number!filename!format'
c
c           ex.) '12!test.data!-F f77 -N ieee'
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: March 31, 1995 - created.
c ---------
c**********************************************************************
#include "iosubs.h"
c
c     Argument List
c     -------------
c
      character*(*) iocntl
c
c     Local Variables
c     ---------------
c
      character*6 form
      integer i1,i2,i3,iunit
      character*132 cunit,cname,cform

      iolink = 0
c
c     Parse string into I/O components.
c     =================================
c
      i1 = ioparse(iocntl,1,cunit)
      i2 = ioparse(iocntl,2,cname)
      i3 = ioparse(iocntl,3,cform)
c
c     Open the data set.
c     ==================
c
      form = '(i   )'
      write(unit=form(3:5),fmt='(i3)') i1
      read(unit=cunit(1:i1),fmt=form) iunit
      open(unit=iunit,file=cname(1:i2),form=cform(1:i3),
     &                             status='unknown',err=900)
      print *, 'open(unit=',cunit(1:i1),',file=',cname(1:i2),
     &         ',form=',cform(1:i3),')'
      go to 999

 900  continue
      iolink = -1
      print *, 'iosubs(iolink): Error opening data set.'
      print *, 'iosubs(iolink): ', iocntl(1:i1+i2+i3+2)

 999  return
      end
