
      MODULE solve_mod

      USE types, ONLY: accumulator
      USE cgr_mod, ONLY: sscgr0,cv2g,g2cv
      USE vam_obs_mod, ONLY: regrid_obs

      IMPLICIT NONE
      PRIVATE
      PUBLIC solve, solve_init

c Background term indices
c!#~   ibVWM     index of component for previous forecast
c!#~   ibLAP     index of component for J2 filter
c!#~   ibDIV     index of component for divergence
c!#~   ibVOR     index of component for vorticity 
c!#~   ibDYN     index of component for (D/DT) of vorticity
c!#~   ibDDDT    index of component for (D/DT) of divergence
      INTEGER, PARAMETER :: ibVWM=1, ibLAP=2, ibDIV=3, ibVOR=4,
     &     ibDYN=5,ibDDDT=6

c     Use of vector subscript iback maps lamda ordering
c     to that needed in jb_mod.
c!#~   mback    number of lamdas in namelist for Jb and Jc
c!#~   nback    number of lamdas used in jb_mod
c!#~   iback    list of lamdas in namelist for Jb
c!#~   jback    list of lamdas ordered for use in jb_mod for Jb
      INTEGER, PARAMETER :: mback=6, nback=8
      INTEGER, PARAMETER, DIMENSION(mback) ::
     &    iback=(/ibVWM,ibLAP,ibDIV,ibVOR,ibDYN,ibDDDT/)
      INTEGER, PARAMETER, DIMENSION(nback) ::
     &    jback=(/ibVWM,ibVWM,ibDIV,ibVOR,ibLAP,ibLAP,ibDYN,ibDDDT/)
c!# ***************************************************************************
c!#   NOTE: jback and jcons must agree with the delu, delv, div etc
c!#   parameters in jb_mod for proper indexing of the lamda values.
c!# ***************************************************************************

c Constraint term indices
c!#~   icVWM     index of component for previous forecast
c!#~   icLAP     index of component for J2 filter
c!#~   icDIV     index of component for divergence
c!#~   icVOR     index of component for vorticity 
c!#~   icDYN     index of component for (D/DT) of vorticity
c!#~   icDDDT    index of component for (D/DT) of divergence
c!#~   icons     list of lamdas in namelist for Jc
c!#~   jcons     list of lamdas ordered for use in jb_mod for Jc
      INTEGER, PARAMETER :: icVWM=7, icLAP=8, icDIV=9, icVOR=10,
     &     icDYN=11,icDDDT=12

c These are exactly analogous to those for the background term
      INTEGER, PARAMETER, DIMENSION(mback) ::
     &    icons=(/icVWM,icLAP,icDIV,icVOR,icDYN,icDDDT/)
      INTEGER, PARAMETER, DIMENSION(nback) ::
     &    jcons=(/icVWM,icVWM,icDIV,icVOR,icLAP,icLAP,icDYN,icDDDT/)

c Obs operator indices
c!#~   ioCONV    index of component for conventional data
c!#~   ioSSMI    index of component for SSMI data
c!#~   ioTMI     index of component for TMI data
c!#~   ioAMSR    index of component for AMSR data
c!#~   ioUNIQ    index of component for unique wind data
c!#~   ioAMB     index of component for ambiguous wind data
c!#~   ioSPD     index of component for velocity magnitude
c!#~   ioNRCS    index of component for sigma0 data

c     Use of vector subscript iback maps lamda ordering
c     to that needed in jb_mod.
c!#~   m_obs    number of lamdas in namelist for Jo
c!#~   n_obs    number of lamdas used in vam_obs_mod
c!#~   i_obs    list of lamdas in namelist for Jo
c!#~   j_obs    list of lamdas ordered for use in vam_obs_mod
      INTEGER, PARAMETER :: ioCONV=13, ioSSMI=14, ioTMI=15,
     &    ioAMSR=16, ioUNIQ=17, ioAMB=18, ioSPD=19, ioNRCS=20
      INTEGER, PARAMETER :: m_obs=8, n_obs=7
      INTEGER, PARAMETER, DIMENSION(m_obs) :: i_obs = (/ioCONV,
     &    ioSSMI, ioTMI, ioAMSR, ioUNIQ, ioAMB, ioSPD, ioNRCS/)
      INTEGER, PARAMETER, DIMENSION(n_obs) :: j_obs =
     &    (/ioCONV, ioAMB, ioNRCS, ioSSMI, ioTMI, ioAMSR, ioUNIQ/)
      REAL, DIMENSION (n_obs) :: norm_obs
c!# ***************************************************************************
c!#   NOTE: j_obs must order the lamda values to agree with names_obsid
c!#   in vam_obs_mod for proper indexing.
c!# ***************************************************************************

c     Sums of squares components, weights, etc.
c!#~   nsos      number of sums of squares components
c!#~   lamda     lamda weight for each component
c!#~   lscale    dimensional scale for each component
c!#~   rmsg      rms gradient for each component and total
c!#~   sosc      sum of squares (SS) for each component and total
c!#~   norm      sum of weights (W) or total area for each component
c!#~   norm.     It is defined such that SS/W is a mean square error
c!#~   norm..    (scaled by lscale)
c!#~   slamda    scaled lamda weights (=lamda*lscale)
      INTEGER, PARAMETER :: nsos=2*mback + m_obs
c     These are namelist parameters:
      REAL, SAVE, DIMENSION(nsos) :: lamda=-1, lscale
c     These are calculated by solve:
      REAL, DIMENSION(nsos+1) :: rmsg, sosc
      REAL, DIMENSION(nsos) :: norm, slamda

c!#~   ctitle    short title for each component
      CHARACTER*20, PARAMETER, DIMENSION(nsos+1) :: ctitle = (/
     1    'Jb:   Prior estimate',
     2    'Jb:        J2 filter',
     3    'Jb:       Divergence',
     4    'Jb:        Vorticity',
     5    'Jb:   D/DT vorticity',
     6    'Jb:  D/DT divergence',
     7    'Jc:   Prior estimate',
     8    'Jc:        J2 filter',
     9    'Jc:       Divergence',
     &    'Jc:        Vorticity',
     1    'Jc:   D/DT vorticity',
     2    'Jc:  D/DT divergence',
     3    'Jo:  Ship+buoy winds',
     4    'Jo:      SSMI speeds',
     5    'Jo:       TMI speeds',
     6    'Jo:      AMSR speeds',
     7    'Jo:     Unique winds',
     8    'Jo:  Ambiguous winds',
     9    'Jo:      SCAT speeds',
     &    'Jo:        NRCS (S0)',
     1    '               Total' /)

c!#~  ncv       size of the control vector
c!#~  iteration number of time sum of squares has been calculated
c!#~  domain    area of integration domain
      INTEGER, SAVE :: iteration=0, ncv
      REAL, SAVE :: domain

      CONTAINS

c     -----------------------------------------------------------------

      SUBROUTINE solve_init  (xmin,xmax,ymin,ymax)

      USE constants, ONLY: a
      USE grid_mod, ONLY: fca_mode,nactive

c!#~   xmin      minimum value of longitude
c!#~   xmax      maximum value of longitude
c!#~   ymin      minimum value of latitude
c!#~   ymax      maximum value of latitude
c xmin, xmax, ymin, ymax are used to calculate the (integration) area
      REAL, INTENT(IN) :: xmin,xmax,ymin,ymax

c     Control vector is (u,v) and (du,dv) for fca_mode.EQ.2
c     Otherwise it is one or the other
      ncv=2*nactive
      If (fca_mode.EQ.2) ncv = 4*nactive

c     note d(area)=a^2 cos(y) dx dy.
      domain=a**2*(xmax-xmin)*(sin(ymax)-sin(ymin))       

      END SUBROUTINE solve_init

c     -----------------------------------------------------------------

      SUBROUTINE solve (iuvam, obs_data)

      USE grid_mod, ONLY: nlat,nlon,name5,name0,u5,v5
      USE types, ONLY: obs_data_typ

c*****solve calculates the sum of squares or minimum

c     !#~   iuvam     reserved i/o unit for vam file operations.
c     !#~   obs_data  linked list of observation data
      INTEGER iuvam
      TYPE(obs_data_typ), POINTER :: obs_data

c!# Namelist input:
      REAL, SAVE :: fcrit=1E-3, eps=1E-6
      INTEGER, SAVE ::  maxfn=25, mdim=6, iprint(2)
      DATA iprint/-1,0/
      LOGICAL :: minimize
      NAMELIST /input/ lamda, lscale, fcrit, eps, maxfn, mdim, iprint,
     &    minimize
c     !#~   minimize  should the obs function be minimized?
c     !#~   minimize.  (otherwise evaluate it only)
c     !#~   maxfn     maximum function calls allowed
c     !#~   mdim      number of work vectors allowed for VA15AD
c     !#~   iprint    controls printout from VA15AD
c     !#~   fcrit     critical fraction of sos reduction needed for restart
c     !#~   eps       stopping parameter for VA15AD 

c     Local variables
c     !#~   ipass     number of times objective function has been minimized
c     !#~   ifun      number of function calls for this use of VA15AD
c     !#~   iflag     error flag from VA15AD
c     !#~   ipoint    work space for VA15AD 
c     !#~   iter      work space for VA15AD
c     !#~   ierr      error code
c     !#~   sos       sum of squares
c     !#~   sos0      previous sum of squares
      INTEGER, SAVE :: ipass=0
      INTEGER ifun,iflag,ipoint,iter,ierr
      REAL sos, sos0

c     Workspace variables for VA15AD
c Note: x, g, diag are large.
c On Palm (Altix SGI) at Goddard stack size was exceeded.
c Use command line "limit stacksize unlimited" to avoid this.
c     !#~   x         control vector
c     !#~   g         gradient of sum of squares
c     !#~   diag      work space for VA15AD
c     !#~   swork     work space for VA15AD
c     !#~   ywork     work space for VA15AD
c     !#~   wwork     work space for VA15AD
      REAL, DIMENSION(ncv) :: x, g, diag
      REAL, ALLOCATABLE, DIMENSION(:) :: swork, ywork, wwork
c     
c     Set defaults for weight parameters
c     ----------------------------------
c     
      IF (lamda(1).LT.0) THEN

        lamda(:)  = 0.0
        lscale(:) = 1.0

        lscale( (/ibVWM, icVWM/) ) = (10.)**(-10)
        lscale( (/ibLAP, icLAP/) ) = (10.)**(+10)
        lscale( (/ibDYN, icDYN/) ) = (10.)**(+8)
        lscale((/ibDDDT, icDDDT/)) = (10.)**(+8)
        
      END IF

      WRITE (*,*) 'Forecast grid is ',trim(name0),
     &    '. First guess grid is ',trim(name5),'.'

c-----Read in namelist
      minimize = .FALSE.
      READ (*,input,IOSTAT=ierr)
      WRITE (*,input)
      IF (ierr .NE. 0) THEN
        WRITE (*,*) 'Error ', ierr,
     &      ' reading namelist /input/ from file standard input.'
        STOP 'solve: Error reading namelist input'
      END IF

c     Scaled lamda weights
      slamda(:)=lscale(:)*lamda(:)

c     Define the control vector from the gridded data
      CALL g2cv(ncv, x)

c     !#   NOTE: sscgr1 is always called before sscgr.  We rely on this 
c     !#         in the intialization of norm(iback) in sscgr1.
      IF (minimize) THEN
        ipass = ipass + 1
        name5='Minimize'

c-----Allocate workspace
        ALLOCATE (swork(mdim*ncv), ywork(mdim*ncv),
     &      wwork(ncv+2*mdim), STAT=ierr)
        IF (ierr.NE.0) STOP 'solve: ALLOCATE (swork(mdim*ncv), ...'

c-----Print out sums and calculate sos
        CALL sscgr1 (ncv,x,sos,g,obs_data)

        sos0=sos ; ifun=0 ; iflag=0

        DO WHILE ((iflag.EQ.1 .AND. ifun.LE.maxfn)
     &      .OR. (iflag.EQ.0 .AND. ifun.EQ.0))
          CALL sscgr(ncv,iprint,x,sos,g,obs_data)
          CALL va15ad(ncv, mdim, x, sos, g,
     &        .FALSE., diag, iprint, eps,
     &        swork,ywork,ipoint,wwork,iflag,iter)
          ifun = ifun + 1

          IF (iflag.LT.0) THEN
            WRITE (*,*) 'VAM: Solve: VA15AD error flag: ', iflag
c-----For some errors we restart the minimization  
            IF ((iflag.EQ.-4 .OR. iflag.EQ.-5)
     &          .AND. sos0-sos.GT.fcrit*sos0) THEN
              WRITE (*,*) 'VAM: Solve: restarting minimization: ',
     &            sos0, sos, fcrit
              sos0=sos ; ifun=0 ; iflag=0
            END IF
          END IF

        END DO

c-----Store result
        CALL cv2g (ncv, x)
c     !#  Update interpolated values u_int, v_int
        CALL regrid_obs(obs_data)

c-----Free work space
        ierr = 0
        DEALLOCATE (swork,ywork,wwork,STAT=ierr)
        IF (ierr.NE.0) STOP 'solve: DEALLOCATE (swork,ywork,...'
      END IF

c-----Print out sums
      CALL sscgr1 (ncv,x,sos,g,obs_data)

      END SUBROUTINE solve

c     -----------------------------------------------------------------

      SUBROUTINE sscgr1 (ncv,x,sos,g,obs_data)
C*****SSCGR1 provides linkage, printing each J component, and total.

      USE types, ONLY: obs_data_typ

c!#~   ncv       size of the control vector
c!#~   x         control vector
c!#~   sos       sum of squares
c!#~   g         gradient of sum of squares
c!#~   obs_data  linked list of observation data
      INTEGER, INTENT(IN) :: ncv
      REAL, INTENT(IN) :: x(ncv)
      REAL, INTENT(OUT) :: sos, g(ncv)
      TYPE(obs_data_typ), POINTER :: obs_data

c!#~   ss        sum of squares (accumulator)
c!#~   mlamda    phony vector of lamda weights
      REAL(accumulator) ss
      REAL mlamda(nsos)

c!#~   k         index over components
      INTEGER k, ktest, kobs

c*****Calculates sum of squares
c!#   NOTE: norm(iback) is only initialized here because:
c!#         (1) norm is only used within sscgr1 for printout
c!#         (2) norm is properly defined anyways since sscgr1 is always 
c!#             called before sscgr
c!#         norm(i_obs) defined after every call to sscgr0
      norm(iback) = domain
      norm(icons) = domain
      WRITE (*,200) 'Component', 'Lamda',
     &    'RMS(Gradient)', 'Sum of Squares', 'Sum of Weights'
      DO k=1,nsos
        IF (lamda(k).GT.0) THEN

          mlamda(:)=0
          mlamda(k)=1

          CALL sscgr0(ncv,x,ss,g,mlamda(jback),mlamda(jcons),
     &       mlamda(j_obs),norm_obs, obs_data)
      
C!#       Map norm_obs back to norm(j_obs) if k is for an obs ss:
          kobs = 0
          do ktest=1,m_obs
             if (k .eq. j_obs(ktest)) kobs=ktest
          enddo
          if (kobs .ne. 0) norm(k) = norm_obs(kobs)

          sosc=lscale(k)*ss
          rmsg(k)=slamda(k)*SQRT(SUM(g(:)**2)/ncv)
          WRITE (*,201) ctitle(k),lamda(k),rmsg(k),sosc(k),norm(k)
        END IF
      END DO

c     Now calculate the totals

      CALL sscgr0(ncv,x,ss,g,slamda(jback),slamda(jcons),
     &    slamda(j_obs),norm_obs,obs_data)

      sos=ss

      norm(j_obs) = norm_obs

      sosc(nsos+1)=ss

      rmsg(nsos+1)=SQRT(SUM(g(:)**2)/ncv)

      iteration = iteration + 1

      WRITE (*,202) 'Total',iteration,rmsg(nsos+1),sosc(nsos+1)

  200 FORMAT (20('====')/A20,1X,A10  ,3A16  /20('----'))
  201 FORMAT (           A20,1X,F10.3,3E16.7           )
  202 FORMAT (20('----')/A20,1X,I10  ,2E16.7/20('----'))

      END SUBROUTINE sscgr1

c     -----------------------------------------------------------------

      SUBROUTINE sscgr (ncv,iprint,x,sos,g,obs_data)
C*****SSCGR provides linkage, printing total J.

      USE types, ONLY: obs_data_typ

c!#~   ncv       size of the control vector
c!#~   x         control vector
c!#~   sos       sum of squares
c!#~   g         gradient of sum of squares
c!#~   obs_data  linked list of observation data
      INTEGER, INTENT(IN) :: ncv, iprint(2)
      REAL, INTENT(IN) :: x(ncv)
      REAL, INTENT(OUT) :: sos, g(ncv)
      TYPE(obs_data_typ), POINTER :: obs_data

c!#~   ss        sum of squares (accumulator)
      REAL(accumulator) ss

c*****Calculates sum of squares

      CALL sscgr0(ncv,x,ss,g,slamda(jback),slamda(jcons),
     &    slamda(j_obs),norm_obs,obs_data)

      sos=ss

      norm(j_obs) = norm_obs

      sosc(nsos+1)=ss

      rmsg(nsos+1)=SQRT(SUM(g(:)**2)/ncv)

      iteration = iteration + 1

      IF (iprint(1) .LE. 0)
     &     WRITE (*,203) iteration,rmsg(nsos+1),sosc(nsos+1)
  203 FORMAT (           20X,1X,I10  ,2E16.7           )

      END SUBROUTINE sscgr

      END MODULE solve_mod
