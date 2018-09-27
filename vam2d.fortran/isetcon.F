c!#   $Id: isetcon.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: isetcon.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      subroutine isetcon(idim,ibuf,iconst)
c**********************************************************************
c English Name: Set Constant
c -------------
c
c Purpose: Initializes an array of "idim" integer words to the value,
c -------- "iconst".
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
c    integer ibuf(*),iconst
c
c    idim (I) - dimension of data vector.
c    ibuf (O) - on output, this is an array of constants ("iconst").
c    iconst (I) - value representing the constant.
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 14, 1995 - created.
c ---------
c**********************************************************************
c
c     Argument List
c     -------------
c
      integer idim 
      integer ibuf(*),iconst
c
c     Initialize array to constants.
c     ==============================
c
      do 5 i = 1,idim
        ibuf(i) = iconst
 5    continue

      return
      end
