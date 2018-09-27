c!#   $Id: weight.h,v 1.2 1997/04/17 18:57:42 rnh Exp $
c!#   $Log: weight.h,v $
c!#   Revision 1.2  1997/04/17 18:57:42  rnh
c!#   Added ssmi los wind cost function
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision (filename originally weight.com)
c!#
c-----common block describing constants and values of objective function
      integer nsos
      parameter (nsos=11)
      integer iteration
      real lamda(nsos), norm(nsos), scale(nsos), ssq(nsos), lscale(nsos)
      common /weight/ iteration, lamda, norm, scale, ssq, lscale
