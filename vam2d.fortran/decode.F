c!#   $Id: decode.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: decode.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      subroutine decode(icode,item1,item2,item3)
c**********************************************************************
      item1 = icode / 10000
      item2 = (icode - (item1*10000)) / 100
      item3 = icode - (item1*10000 + item2*100)
      return
      end
