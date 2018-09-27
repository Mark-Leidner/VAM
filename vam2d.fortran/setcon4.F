c!#   $Id: setcon4.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: setcon4.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      subroutine setcon4(idim,buf,const)
c**********************************************************************
c English Name: Set Constant (4-byte words)
c -------------
c
c Purpose: Initializes an array of "idim" 4-byte floating point words to
c -------- the value, "const".
c
c Notes:
c ------
c
c Variable Description:
c ---------------------
c
c    Argument List
c    -------------
c
c    integer idim 
c    real*4 buf(*)
c    real const
c
c    idim (I) - dimension of data vector.
c    buf (O) - on output, this is an array of 4-byte words set to
c              constant values ("const").
c    const (I) - value representing the constant.
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 17, 1995 - created.
c ---------
c**********************************************************************
c
c     Argument List
c     -------------
c
      integer idim 
      real*4 buf(*)
      real const
c
c     Initialize array to constants.
c     ==============================
c
      do 5 i = 1,idim
        buf(i) = const
 5    continue

      return
      end
