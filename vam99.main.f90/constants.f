
      MODULE constants

      IMPLICIT NONE
      PRIVATE

c     -----------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

c!#   Numerical constants:
c!#~   pi        half circumfrence of unit circle
      REAL, PUBLIC, PARAMETER ::  pi=3.1415926535897932D0

c!#   Physical constants: 
c!#~   a         radius of earth (meters)
      REAL, PUBLIC, PARAMETER :: a = 6371 * 1000
c!#~   omega     rotation rate of earth (radians/sec)
      REAL, PUBLIC, PARAMETER :: omega = 7.292D-5
  
      END MODULE constants
