c!#   $Id: totsec.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: totsec.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c***********************************************************************
      function totsec(iy)
c***********************************************************************
      integer totsec
      logical leapyr

      leapyr = ((mod(iy,4) .eq. 0 .and. mod(iy,100) .ne. 0) .or.
     >                                        mod(iy,400) .eq. 0)

      totsec = 365 * 86400
      if (leapyr) totsec = totsec + 86400

      return
      end
