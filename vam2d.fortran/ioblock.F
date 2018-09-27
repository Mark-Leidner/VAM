c!#   $Id: ioblock.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: ioblock.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      block data ioblock
c**********************************************************************
#include "iosubs.h"
c
c     Common Blocks
c     -------------
c
#include "iolist.h"

      data ionum/0/
      data iomap/MAXUNIT * 0/

      end
