c!#   $Id: ckwk.F,v 1.2 1997/02/18 17:40:49 leidner Exp $
c!#   $Log: ckwk.F,v $
c!#   Revision 1.2  1997/02/18 17:40:49  leidner
c!#   replaced IDIMW, JDIMW, MAXVEC workspace size variables with WBF (defined
c!#   in "vam.h")
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#	
      SUBROUTINE CKWK (NVEC,NDIM)
#include "vam.h"
C***********************************************************************
C*****CKWK - ALLOCATES AND CHECKS AMOUNT OF WORK SPACE
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : ERRSEA ...... ...... ...... ...... ...... ...... ......
C-----FORT : IBCOM# ...... ...... ...... ...... ...... ...... ......
C-----
      COMMON /WORK/ WORK(WBF)
C-----
      IF (NVEC*NDIM .LE. WBF) RETURN
      WRITE (6,101) NVEC,NDIM
  101 FORMAT ('0THESE IS NOT ENOUGH STORAGE FOR ',I4,
     +    ' VECTORS OF LENGTH ',I6/)
      WRITE (6,102) WBF
  102 FORMAT ('0WORKSPACE ALLOCATED IS ',I8/)
      WRITE (6,103) NVEC*NDIM
  103 FORMAT ('0WORKSPACE REQUESTED IS ',I8/)
      CALL ERRSEA (521,6HCKWK  )
      ENTRY WORKS2
      WRITE (6,102) WBF
      RETURN
      END
