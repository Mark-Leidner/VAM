
c!##  CONSTRAINTS :

c     Polar, periodic options maintained but not tested (10/98)

      MODULE grid_mod

      USE types, ONLY : len_fname

      IMPLICIT NONE
      PRIVATE

c!#   Basic grid description:
c!#~   lon0       longitude of first grid point
c!#~   dlon       longitude increment
c!#~   nlon       number of longitude grid points
c!#~   lat0       latitude of first grid point
c!#~   dlat       latitude increment
c!#~   nlat       number of latitude grid points
      REAL, PUBLIC, SAVE :: lon0=0,dlon=1,lat0=0,dlat=1
      INTEGER, PUBLIC, SAVE :: nlon=0,nlat=0

c!#  Identification (these default to the filenames)
c!#~   name0     name of the background wind field
c!#~   name5     name of the analysis wind field
      CHARACTER(len=len_fname), PUBLIC, SAVE :: name0=' ', name5=' '

c!#  Valid date/time (UTC):
c!#~   idate     valid date (yyyymmdd) of the wind field
c!#~   itime     valid time (hhmmss) of the wind field
      INTEGER, PUBLIC, SAVE :: idate=0, itime=0

c Gridded fields will be DIMENSION(nlon, nlat)
c!#~   u0,v0     wind field (background)
c!#~   u5,v5     wind field (trajectory)
c!#~   u,v       wind field (adjoint)

      REAL, PUBLIC, SAVE, ALLOCATABLE, DIMENSION(:,:)::u0,v0,u5,v5,u,v

c Except currently f is DIMENSION(nlat)
c!#~   f         Coriolis term

      REAL, PUBLIC, SAVE, ALLOCATABLE, DIMENSION(:) :: f

c And all drag coeficients are equal
c!#~   cf        friction coefficient = drag coefficient / PBL height(=1km)

      REAL, PUBLIC, SAVE :: cf=-1E-3/1000

c * Setup grids

c!#~  du5,dv5  adjustment wind (trajectory)
c!#~  du,dv    adjustment wind (adjoint)
c!#~  ua5,va5  adjusted wind (trajectory)
c!#~  ua,va    adjusted wind (adjoint)
      REAl, PUBLIC, SAVE, ALLOCATABLE, DIMENSION(:,:) :: du5,dv5,
     &   du,dv,ua5,va5,ua,va

c * Definition of FCA modes

c We define four modes of operation wrt to the FCA technique

c 0. fca_none: fca_mode=0
c    This is the standard problem with no fca.
c    The control vector x = (u,v). 
c    Jb compares (u5,v5) to (u0,v0).
c    The adjustment winds and the adjusted winds are not present.
c    Conceptually (du5,dv5)=0, so (ua5,va5)=(u0,v0).

c 1. fca_only: fca_mode=1
c    An analysis based on fca only.
c    The control vector x = (du,dv). 
c    Jb = 0 (or is absent) since the analysis is the adjusted wind.
c    Jc compares (du,dv) to (0,0).
c    Conceptually (u5,v5) = (ua5,va5).

c 2. fca_too: fca_mode=2
c    A combined data analysis including fca and analysis increments.
c    The control vector x = (u,v,du,dv). 
c    Jb compares (u5,v5) to (ua5,va5), but (ua5,va5) depend on (du,dv).
c    Jc compares (du,dv) to (0,0).

c 3. fca_val: fca_mode=3
c    Use fca for validation: match analysis and background using fca.
c    The control vector x = (du,dv). 
c    Jb compares (u5,v5) to (ua5,va5).
c    Jc compares (du,dv) to (0,0).
c    In this mode (u5,v5) is fixed and Jo is absent.

c!#~ fca_mode   control of fca processing (0,1,2,3 as defined above)
c!#~ cv_uv      does control vector include (u,v)
c!#~ cv_fca     does control vector include (du,dv)
c!#~ nactive    number of active grid points
c!#~ nactive.   control vector is either 2* or 4*nactive in length

      INTEGER, PUBLIC, SAVE :: fca_mode=0, nactive=0

      LOGICAL, PUBLIC, SAVE :: cv_uv=.TRUE., cv_fca=.FALSE.
  
      END MODULE grid_mod
