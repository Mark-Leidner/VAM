c!#   $Id: grname.h,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: grname.h,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision (filename originally grname.com)
c!#
      integer iu,ibefaf
      character*8 qu,qv
      real qlev,grid_height

      common /grname/ grid_height,qlev,iu,ibefaf,qu,qv
