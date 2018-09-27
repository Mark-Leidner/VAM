      subroutine scan_bufr (infile, imess)
C
C**** *SCAN_BUFR*
C
C     $Id: scan_bufr.f,v 1.1 2001/08/10 17:58:34 mcc Exp $
C     $Log: scan_bufr.f,v $
C     Revision 1.1  2001/08/10 17:58:34  mcc
C     Initial revision.
C
C 
C
C     PURPOSE.
C     --------
C
C     Scan a BUFR file and return the # of messages.
C
C
C**   INTERFACE.
C     ----------
C
C       INPUT  :
C
C         infile - input - BUFR filename
C
C
C       OUTPUT :
C
C         imess   - output- number of BUFR messages in infile
C
C     METHOD.
C     -------
C
C     Open and read through the BUFR file, counting messages.
C     When finished, close the file and return the # of messages.
C
C     EXTERNALS.
C     ----------
C
C         CALL PBOPEN                 |  from ECMWF BUFR software
C         CALL PBBUFR                 |  from ECMWF BUFR software
C         CALL PBCLOSE                |  from ECMWF BUFR software
C
C
C     REFERENCE.
C     ----------
C
C     NONE.
C
C
C     AUTHOR.
C     -------
C
C          M. LEIDNER       *AER*         23 March 2001
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
C     Declare interface variables.
      character*256 infile
      integer imess

C     Declare local variables.
      parameter (jbufl=20000,jbyte=80000)
      dimension kbuff(jbufl)
      integer iunit, iret, kbufl
      logical lopened

C--------------------------------------------------------------
C
C*    0.  Initialize output variables.
C
      imess = 0

C
C*    1.  Open file containing bufr data.
C
C*    1.1  Look for an available unit number.
C
      iunit = 20
  5   inquire(iunit, opened=lopened)
      if (lopened) then
        iunit = iunit + 1
        goto 5
      endif
C
C*    1.2  Open the file.
C
      iret=0 
      call pbopen(iunit,infile(1:index(infile,' ')-1),'r',iret)
      if (iret.eq.-1) stop 'scan_bufr: Open failed'
      if (iret.eq.-2) stop 'scan_bufr: Invalid input file name'
      if (iret.eq.-3) stop 'scan_bufr: Invalid open mode specified'

C
C*    2. Read BUFR messages, keeping a count.
C
  10  continue
      iret=0
      call pbbufr(iunit,kbuff,jbyte,kbufl,iret) 
      if(iret.eq.-1) goto 20
      if(iret.eq.-2) stop 'scan_bufr: file handling problem' 
      if(iret.eq.-3) stop 'scan_bufr: array too small for product'
      imess = imess + 1
      goto 10

C
C*    3. Print diagnostics; close input file.
C
  20  continue
      print*, 'scan_bufr: Input file "',
     &    infile(1:index(infile,' ')-1),'" contains',
     &    imess, ' BUFR messages'
      iret=0 
      call pbclose(iunit,iret)
      if(iret .ne. 0) stop 'scan_bufr: error closing file'

      return
      end
