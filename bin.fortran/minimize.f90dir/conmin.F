      subroutine conmin(n,x,f,g,ifun,iter,eps,nflag,mxfun,w,
     1iout,mdim,idev,acc,nmeth)
c!#   $Id: conmin.F,v 1.1 2000/11/07 20:16:30 mcc Exp $
c!#   $Log: conmin.F,v $
c!#   Revision 1.1  2000/11/07 20:16:30  mcc
c!#   Routine added to build minimize.f90dir lib. Initial revision
c!#
c!#   Revision 1.1  1997/02/10 14:52:21  leidner
c!#   Initial revision
c!#
c
c purpose:    subroutine conmin minimizes an unconstrained nonlinear
c             scalar valued function of a vector variable x
c             either by the bfgs variable metric algorithm or by a
c             beale restarted conjugate gradient algorithm.
c
c usage:      call conmin(n,x,f,g,ifun,iter,eps,nflag,mxfun,w,
c             iout,mdim,idev,acc,nmeth)
c
c parameters: n      the number of variables in the function to
c                    be minimized.
c             x      the vector containing the current estimate to
c                    the minimizer. on entry to conmin,x must contain
c                    an initial estimate supplied by the user.
c                    on exiting,x will hold the best estimate to the
c                    minimizer obtained by conmin. x must be double
c                    precisioned and dimensioned n.
c             f      on exiting from conmin,f will contain the lowest
c                    value of the object function obtained.
c                    f is real.
c             g      on exiting from conmin,g will contain the
c                    elements of the gradient of f evaluated at the
c                    point contained in x. g must be double
c                    precisioned and dimensioned n.
c             ifun   upon exiting from conmin,ifun contains the
c                    number of times the function and gradient
c                    have been evaluated.
c             iter   upon exiting from conmin,iter contains the
c                    total number of search directions calculated
c                    to obtain the current estimate to the minizer.
c             eps    eps is the user supplied convergence parameter.
c                    convergence occurs when the norm of the gradient
c                    is less than or equal to eps times the maximum
c                    of one and the norm of the vector x. eps
c                    must be real.
c             nflag  upon exiting from conmin,nflag states which
c                    condition caused the exit.
c                    if nflag=0, the algorithm has converged.
c                    if nflag=1, the maximum number of function
c                       evaluations have been used.
c                    if nflag=2, the linear search has failed to
c                       improve the function value. this is the
c                       usual exit if either the function or the
c                       gradient is incorrectly coded.
c                    if nflag=3, the search vector was not
c                       a descent direction. this can only be caused
c                       by roundoff,and may suggest that the
c                       convergence criterion is too strict.
c             mxfun  mxfun is the user supplied maximum number of
c                    function and gradient calls that conmin will
c                    be allowed to make.
c             w      w is a vector of working storage.if nmeth=0,
c                    w must be dimensioned 5*n+2. if nmeth=1,
c                    w must be dimensioned n*(n+7)/2. in both cases,
c                    w must be real.
c             iout   iout is a user  supplied output parameter.
c                    if iout = 0, there is no printed output from
c                    conmin. if iout > 0,the value of f and the
c                    norm of the gradient squared,as well as iter
c                    and ifun,are written every iout iterations.
c             mdim   mdim is the user supplied dimension of the
c                    vector w. if nmeth=0,mdim=5*n+2. if nmeth=1,
c                    mdim=n*(n+7)/2.
c             idev   idev is the user supplied number of the output
c                    device on which output is to be written when
c                    iout>0.
c             acc    acc is a user supplied estimate of machine
c                    accuracy. a linear search is unsuccessfully
c                    terminated when the norm of the step size
c                    becomes smaller than acc. in practice,
c                    acc=10.d-20 has proved satisfactory. acc is
c                    real.
c             nmeth  nmeth is the user supplied variable which
c                    chooses the method of optimization. if
c                    nmeth=0,a conjugate gradient method is
c                    used. if nmeth=1, the bfgs method is used.
c
c remarks:    in addition to the specified values in the above
c             argument list, the user must supply a subroutine
c             calcfg which calculates the function and gradient at
c             x and places them in f and g(1),...,g(n) respectively.
c             the subroutine must have the form:
c                    subroutine calcfg(n,x,f,g)
c                    real x(n),g(n),f
c
c             an example subroutine for the rosenbrock function is:
c
c                    subroutine calcfg(n,x,f,g)
c                    real x(n),g(n),f,t1,t2
c                    t1=x(2)-x(1)*x(1)
c                    t2=1.0-x(1)
c                    f=100.0*t1*t1+t2*t2
c                    g(1)=-400.0*t1*x(1)-2.0*t2
c                    g(2)=200.0*t1
c                    return
c                    end
c
      real x(n),g(n),w(mdim)
      real f,fp,fmin,alpha,at,ap,gsq,dg,dg1
      real dp,step,acc,dal,u1,u2,u3,u4,eps
      real xsq,rtst,sqrt,min1,max1,abs
      logical rsw
c
c initialize iter,ifun,nflag,and ioutk,which counts output iterations.
c
      iter=0
      ifun=0
      ioutk=0
      nflag=0
c
c set parameters to extract vectors from w.
c w(i) holds the search vector,w(nx+i) holds the best current
c estimate to the minimizer,and w(ng+i) holds the gradient
c at the best current estimate.
c
      nx=n
      ng=nx+n
c
c test which method is being used.
c if nmeth=0, w(nry+i) holds the restart y vector and
c w(nrd+i) holds the restart search vector.
c
      if(nmeth.eq.1)go to 10
      nry=ng+n
      nrd=nry+n
      ncons=5*n
      ncons1=ncons+1
      ncons2=ncons+2
      go to 20
c
c if nmeth=1,w(ncons+i) holds the approximate inverse hessian.
c
10    ncons=3*n
c
c  calculatethe function and gradient at the initial
c point and initialize nrst,which is used to determine
c whether a beale restart is being done. nrst=n means that this
c iteration is a restart iteration. initialize rsw,which indicates
c that the current search direction is a gradient direction.
c
20    call calcfg(n,x,f,g)
      ifun=ifun+1
      nrst=n
      rsw=.true.
c
c calculate the initial search direction , the norm of x squared,
c and the norm of g squared. dg1 is the current directional
c derivative,while xsq and gsq are the squared norms.
c
      dg1=0.
      xsq=0.
      do 30 i=1,n
        w(i)=-g(i)
        xsq=xsq+x(i)*x(i)
30    dg1=dg1-g(i)*g(i)
      gsq=-dg1
c
c test if the initial point is the minimizer.
c
      if(gsq.le.eps*eps*max1(1.0,xsq))return
c
c begin the major iteration loop. ncalls is used to guarantee that
c at least two points have been tried when nmeth=0. fmin is the
c current function value.
c
40    fmin=f
      ncalls=ifun
c
c if output is desired,test if this is the correct iteration
c and if so, write output.
c
      if(iout.eq.0)go to 60
      if(ioutk.ne.0)go to 50
      write(idev,500)iter,ifun,fmin,gsq
50    ioutk=ioutk+1
      if(ioutk.eq.iout)ioutk=0
c
c begin linear search. alpha is the steplength.
c set alpha to the nonrestart conjugate gradient alpha.
c
60    alpha=alpha*dg/dg1
c
c if nmeth=1 or a restart has been performed, set alpha=1.0.
c
      if(nrst.eq.1.or.nmeth.eq.1)alpha=1.0
c
c if a gradient direction is used, set alpha=1.0/sqrt(gsq),
c which scales the initial search vector to unity.
c
      if(rsw)alpha=1.0/sqrt(gsq)
c
c the linear search fits a cubic to f and dal, the function and its
c derivative at alpha, and to fp and dp,the function
c and derivative at the previous trial point ap.
c initialize ap ,fp,and dp.
c
      ap=0.
      fp=fmin
      dp=dg1
c
c save the current derivative to scale the next search vector.
c
      dg=dg1
c
c update the iteration.
c
      iter=iter+1
c
c calculate the current steplength  and store the current x and g.
c
      step=0.
      do 70 i=1,n
        step=step+w(i)*w(i)
        nxpi=nx+i
        ngpi=ng+i
        w(nxpi)=x(i)
70    w(ngpi)=g(i)
      step=sqrt(step)
c
c begin the linear search iteration.
c test for failure of the linear search.
c
80    if(alpha*step.gt.acc)go to 90
c
c test if direction is a gradient direction.
c
      if(.not.rsw)go to 20
      nflag=2
      return
c
c calculate the trial point.
c
90    do 100 i=1,n
        nxpi=nx+i
100   x(i)=w(nxpi)+alpha*w(i)
c
c evaluate the function at the trial point.
c
      call calcfg(n,x,f,g)
c
c test if the maximum number of function calls have been used.
c
      ifun=ifun+1
      if(ifun.le.mxfun)go to 110
      nflag=1
      return
c
c compute the derivative of f at alpha.
c
110   dal=0.0
      do 120 i=1,n
120   dal=dal+g(i)*w(i)
c
c test whether the new point has a negative slope but a higher
c function value than alpha=0. if this is the case,the search
c has passed through a local max and is heading for a distant local
c minimum.
c
      if(f.gt.fmin.and.dal.lt.0.)go to 160
c
c if not, test whether the steplength criteria have been met.
c
      if(f.gt.(fmin+.0001*alpha*dg).or.abs(dal/dg)
     1.gt.(.9))go to 130
c
c if they have been met, test if two points have been tried
c if nmeth=0 and if the true line minimum has not been found.
c
      if((ifun-ncalls).le.1.and.abs(dal/dg).gt.eps.and.
     1nmeth.eq.0)go to 130
      go to 170
c
c a new point must be tried. use cubic interpolation to find
c the trial point at.
c
130   u1=dp+dal-3.0*(fp-f)/(ap-alpha)
      u2=u1*u1-dp*dal
      if(u2.lt.0.)u2=0.
      u2=sqrt(u2)
      at=alpha-(alpha-ap)*(dal+u2-u1)/(dal-dp+2.*u2)
c
c test whether the line minimum has been bracketed.
c
      if((dal/dp).gt.0.)go to 140
c
c the minimum has been bracketed. test whether the trial point lies
c sufficiently within the bracketed interval.
c if it does not, choose at as the midpoint of the interval.
c
      if(at.lt.(1.01*min1(alpha,ap)).or.at.gt.(.99*max1
     1(alpha,ap)))at=(alpha+ap)/2.0
      go to 150
c
c the minimum has not been bracketed. test if both points are
c greater than the minimum and the trial point is sufficiently
c smaller than either.
c
140   if(dal .gt.0.0.and.0.0.lt.at.and.at.lt.(.99*min1(ap,alpha)))
     1go to 150
c
c test if both points are less than the minimum and the trial point
c is sufficiently large.
c
      if(dal.le.0.0.and.at.gt.(1.01*max1(ap,alpha)))go to 150
c
c if the trial point is too small,double the largest prior point.
c
      if(dal.le.0.)at=2.0*max1(ap,alpha)
c
c if the trial point is too large, halve the smallest prior point.
c
      if(dal.gt.0.)at=min1(ap,alpha)/2.0
c
c set ap=alpha, alpha=at,and continue search.
c
150   ap=alpha
      fp=f
      dp=dal
      alpha=at
      go to 80
c
c a relative max has been passed.reduce alpha and restart the search.
c
160   alpha=alpha/3.
      ap=0.
      fp=fmin
      dp=dg
      go to 80
c
c the line search has converged. test for convergence of the algorithm.
c
170   gsq=0.0
      xsq=0.0
      do 180 i=1,n
        gsq=gsq+g(i)*g(i)
180   xsq=xsq+x(i)*x(i)
      if(gsq.le.eps*eps*max1(1.0,xsq))return
c
c search continues. set w(i)=alpha*w(i),the full step vector.
c
      do 190 i=1,n
190   w(i)=alpha*w(i)
c
c compute the new search vector. first test whether a
c conjugate gradient or a variable metric vector is used.
c
      if(nmeth.eq.1)go to 330
c
c conjugate gradient update section.
c test if a powell restart is indicated.
c
      rtst=0.
      do 200 i=1,n
        ngpi=ng+i
200   rtst=rtst+g(i)*w(ngpi)
      if(abs(rtst/gsq).gt.0.2)nrst=n
c
c if a restart is indicated, save the current d and y
c as the beale restart vectors and save d'y and y'y
c in w(ncons+1) and w(ncons+2).
c
      if(nrst.ne.n)go to 220
      w(ncons+1)=0.
      w(ncons+2)=0.
      do 210 i=1,n
        nrdpi=nrd+i
        nrypi=nry+i
        ngpi=ng+i
        w(nrypi)=g(i)-w(ngpi)
        w(nrdpi)=w(i)
        w(ncons1)=w(ncons1)+w(nrypi)*w(nrypi)
210   w(ncons2)=w(ncons2)+w(i)*w(nrypi)
c
c calculate  the restart hessian times the current gradient.
c
220   u1=0.0
      u2=0.0
      do 230 i=1,n
        nrdpi=nrd+i
        nrypi=nry+i
        u1=u1-w(nrdpi)*g(i)/w(ncons1)
230   u2=u2+w(nrdpi)*g(i)*2./w(ncons2)-w(nrypi)*g(i)/w(ncons1)
      u3=w(ncons2)/w(ncons1)
      do 240 i=1,n
        nxpi=nx+i
        nrdpi=nrd+i
        nrypi=nry+i
240   w(nxpi)=-u3*g(i)-u1*w(nrypi)-u2*w(nrdpi)
c
c if this is a restart iteration,w(nx+i) contains the new search
c vector.
c
      if(nrst.eq.n)go to 300
c
c not a restart iteration. calculate the restart hessian
c times the current y.
c
250   u1=0.
      u2=0.
      u3=0.
      u4=0.
      do 260 i=1,n
        ngpi=ng+i
        nrdpi=nrd+i
        nrypi=nry+i
        u1=u1-(g(i)-w(ngpi))*w(nrdpi)/w(ncons1)
        u2=u2-(g(i)-w(ngpi))*w(nrypi)/w(ncons1)
     1  +2.0*w(nrdpi)*(g(i)-w(ngpi))/w(ncons2)
260   u3=u3+w(i)*(g(i)-w(ngpi))
      step=0.
      do 270 i=1,n
        ngpi=ng+i
        nrdpi=nrd+i
        nrypi=nry+i
        step=(w(ncons2)/w(ncons1))*(g(i)-w(ngpi))
     1  +u1*w(nrypi)+u2*w(nrdpi)
        u4=u4+step*(g(i)-w(ngpi))
270   w(ngpi)=step
c
c calculate the doubly updated hessian times the current
c gradient to obtain the search vector.
c
      u1=0.0
      u2=0.0
      do 280 i=1,n
        u1=u1-w(i)*g(i)/u3
        ngpi=ng+i
280   u2=u2+(1.0+u4/u3)*w(i)*g(i)/u3-w(ngpi)*g(i)/u3
      do 290 i=1,n
        ngpi=ng+i
        nxpi=nx+i
290   w(nxpi)=w(nxpi)-u1*w(ngpi)-u2*w(i)
c
c calculate the derivative along the new search vector.
c
300   dg1=0.
      do 310 i=1,n
        nxpi=nx+i
        w(i)=w(nxpi)
310   dg1=dg1+w(i)*g(i)
c
c if the new direction is not a descent direction,stop.
c
      if (dg1.gt.0.)go to 320
c
c update nrst to assure at least one restart every n iterations.
c
      if(nrst.eq.n)nrst=0
      nrst=nrst+1
      rsw=.false.
      go to 40
c
c roundoff has produced a bad direction.
c
320   nflag=3
      return
c
c a variable metric algoritm is being used. calculate y and d'y.
c
330   u1=0.0
      do 340 i=1,n
        ngpi=ng+i
        w(ngpi)=g(i)-w(ngpi)
340   u1=u1+w(i)*w(ngpi)
c
c if rsw=.true.,set up the initial scaled approximate hessian.
c
      if(.not.rsw)go to 380
c
c calculate y'y.
c
      u2=0.
      do 350 i=1,n
        ngpi=ng+i
350   u2=u2+w(ngpi)*w(ngpi)
c
c calculate the initial hessian as h=(p'y/y'y)*i
c and the initial u2=y'hy and w(nx+i)=hy.
c
      ij=1
      u3=u1/u2
      do 370 i=1,n
        do 360 j=i,n
          ncons1=ncons+ij
          w(ncons1)=0.0
          if(i.eq.j)w(ncons1)=u3
360     ij=ij+1
        nxpi=nx+i
        ngpi=ng+i
370   w(nxpi)=u3*w(ngpi)
      u2=u3*u2
      go to 430
c
c calculate w(nx+i)=hy and u2=y'hy.
c
380   u2=0.0
      do 420 i=1,n
        u3=0.0
        ij=i
        if(i.eq.1)go to 400
        ii=i-1
        do 390 j=1,ii
          ngpj=ng+j
          ncons1=ncons+ij
          u3=u3+w(ncons1)*w(ngpj)
390     ij=ij+n-j
400     do 410 j=i,n
          ncons1=ncons+ij
          ngpj=ng+j
          u3=u3+w(ncons1)*w(ngpj)
410     ij=ij+1
        ngpi=ng+i
        u2=u2+u3*w(ngpi)
        nxpi=nx+i
420   w(nxpi)=u3
c
c calculate the updated approximate hessian.
c
430   u4=1.0+u2/u1
      do 440 i=1,n
        nxpi=nx+i
        ngpi=ng+i
440   w(ngpi)=u4*w(i)-w(nxpi)
      ij=1
      do 450 i=1,n
        nxpi=nx+i
        u3=w(i)/u1
        u4=w(nxpi)/u1
        do 450 j=i,n
          ncons1=ncons+ij
          ngpj=ng+j
          w(ncons1)=w(ncons1)+u3*w(ngpj)-u4*w(j)
450   ij=ij+1
c
c calculate the new search direction w(i)=-hg and its derivative.
c
      dg1=0.0
      do 490 i=1,n
        u3=0.0
        ij=i
        if(i.eq.1)go to 470
        ii=i-1
        do 460 j=1,ii
          ncons1=ncons+ij
          u3=u3-w(ncons1)*g(j)
460     ij=ij+n-j
470     do 480 j=i,n
          ncons1=ncons+ij
          u3=u3-w(ncons1)*g(j)
480     ij=ij+1
        dg1=dg1+u3*g(i)
490   w(i)=u3
c
c test for a downhill direction.
c
      if(dg1.gt.0.)go to 320
      rsw=.false.
      go to 40
500   format(10h iteration,i5,20h      function calls,i6/5h f = ,
     1d15.8,13h g-squared = ,d15.8/)
      end
