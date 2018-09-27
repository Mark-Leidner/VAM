c!#   $Id: getargs.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: getargs.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c Revision 1.1  1997/02/10  16:30:04  leidner
c Initial revision
c
c**********************************************************************
      function getargs(file1,file2,indate,intime,iedate,ietime,inchrs)
c**********************************************************************
c English Name: Get Arguments
c -------------
c
c Purpose: Retrieves command line arguments and stores them in the
c -------- proper variable types.
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
c     character*(*) file1,file2
c     integer getargs,indate,intime,iedate,ietime,inchrs
c
c     getargs (O) - function return value:
c
c               =  0 (normal)
c                 -1 (incorrect number of arguments)
c                 -2 (invalid hourly increment)
c
c     file1 (O) - retrieved filename argument-1.
c     file2 (O) - retrieved filename argument-2.
c     indate (O) - retrieved initial date argument (yymmdd).
c     intime (O) - retrieved initial time argument (hhmmss).
c     iedate (O) - retrieved ending date argument (yymmdd).
c     ietime (O) - retrieved ending time argument (hhmmss).
c     inchrs (O) - retrieved hourly increment argument (hh).
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 3, 1995 - created.
c ---------
c**********************************************************************
c
c     Argument List
c     -------------
c
      character*(*) file1,file2
      integer getargs,indate,intime,iedate,ietime,inchrs
c
c     Local Variables
c     ---------------
c
      integer strlen
      character*5 form
      character*130 str

      data form/'(i  )'/
c
c     Check Usage
c     ===========
c
      getargs = 0
      if (iargc() .ne. 7) then 

         getargs = -1
         print *, 'Usage: vam [procname] [iolist] [indate]',
     &            ' [intime] [iedate] [ietime] [inchrs]'
         go to 999
      endif
c
c     Retrieve arguments.
c     ===================
c
      call getarg(1,file1)
      call getarg(2,file2)

      call getarg(3,str)
      ilen = strlen(str)
      write(unit=form(3:4),fmt='(i2)') ilen
      read(unit=str(1:ilen),fmt=form) indate

      call getarg(4,str)
      ilen = strlen(str)
      write(unit=form(3:4),fmt='(i2)') ilen
      read(unit=str(1:ilen),fmt=form) intime

      call getarg(5,str)
      ilen = strlen(str)
      write(unit=form(3:4),fmt='(i2)') ilen
      read(unit=str(1:ilen),fmt=form) iedate

      call getarg(6,str)
      ilen = strlen(str)
      write(unit=form(3:4),fmt='(i2)') ilen
      read(unit=str(1:ilen),fmt=form) ietime

      call getarg(7,str)
      ilen = strlen(str)
      write(unit=form(3:4),fmt='(i2)') ilen
      read(unit=str(1:ilen),fmt=form) inchrs
      if (inchrs .le. 0) go to 900

      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue
      getargs = -2
      print *, 'vam(getargs): invalid hourly increment'
      go to 999

 999  return
      end
