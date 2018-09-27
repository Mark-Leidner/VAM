c!#   $Id: dirdif.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: dirdif.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c*********************************************************************
      function dirdif(dir1,dir2)
c*********************************************************************

      dir1a = dir1
      dir1b = dir1
      if (dir1 .ge. 180.0) dir1b = dir1 - 360.0

      dir2a = dir2
      dir2b = dir2
      if (dir2 .ge. 180.0) dir2b = dir2 - 360.0

      diffwd = abs(dir1a-dir2a)
      difbck = abs(dir1b-dir2b)
      diff = min(diffwd,difbck)

      if (diffwd .lt. difbck) then
         if (dir1a .lt. dir2a) diff = diffwd
         if (dir1a .gt. dir2a) diff = -diffwd
      else
         if (dir1b .lt. dir2b) diff = difbck
         if (dir1b .gt. dir2b) diff = -difbck
      endif

      dirdif = diff

      return
      end
