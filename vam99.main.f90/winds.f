c!#   $Id: winds.f,v 1.3 1999/10/08 13:45:13 trn Exp $
c!#   $Log: winds.f,v $
c!#   Revision 1.3  1999/10/08 13:45:13  trn
c!#   Updated vam_obs data file structure, revised gridops and vamobs namelists, some
c!#   reorganization of modules, other stylistic changes.
c!#
c!#   Revision 1.2  1999/09/28 18:47:30  trn
c!#   Use code from test module - only stylistically different
c!#
c!#   Revision 1.1  1999/08/30 15:38:51  trn
c!#   Initial revision
c!#
c!#   Adapted from vam.src/vam2d.fortran/winds.F, 
c!#                version: 1.1 1997/02/10 16:39:08 leidner

      SUBROUTINE  WINDS (u,v,vel,theta,l2uv)

c     -----------------------------------------------------------------

C*****WINDS TRANSFORMS FROM (U,V) TO (VEL,THETA) AND VICE VERSA
      USE constants, ONLY: pi

      implicit none

c-----Specifications

c     theta, measured from north clockwise, is the direction wind comes
c     from. theta is restricted to range 0 to 2*pi on output.
      real, intent(inout) :: u,v,vel,theta
      LOGICAL, intent(in) :: l2uv

c-----Transform to u and v
      IF (l2uv) THEN
        u=-vel*SIN(theta)
        v=-vel*COS(theta)
      ELSE
c-----Transform to vel and theta
        vel=SQRT(v*v+u*u)
        IF (vel.GT.0) THEN
          theta=ATAN2(u,v) + pi
        ELSE
          theta=0
        END IF
      END IF

      END SUBROUTINE winds
