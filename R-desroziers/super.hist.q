
##############################################

super.hist <-
  function (x, y, breaks=hist(x,plot=F)$breaks, trim=0)
{
  # for each x bin calculate simple statistics
  nx<-length(x)
  nb<-length(breaks)-1
  # exception handling
  if (length(y)!=nx) stop('super.hist: x and y length mismatch, nx=', nx, ', ny=', length(y))
  if (any(is.na(x))) stop('super.hist: NAs in x')
  if (any(is.na(y))) stop('super.hist: NAs in y')
  # remove any infinities and also points out of bounds
  select<-(-Inf)<x+y & x+y<Inf
  select<-select & breaks[1]<=x & x<=breaks[nb+1]
  x<-x[select] ; y<-y[select]
  outofbounds <- nx - length(x)
  nx <- length(x)
  ntrim <- 0
  # sort by y and trim
  # trim the entire sample, not bin by bin
  if (trim > 0 & trim < 0.5) {
    i<-order(y)
    imin<-nx*trim ; imax<-nx*(1-trim) ; i<-i[imin:imax]
    x<-x[i] ; y<-y[i]
    ntrim <- nx - length(x)
    nx <- length(x) }
  # sort by x
  i<-order(x)
  x<-x[i] ; y<-y[i]
  # find indices of break points
  i<-cut(x,breaks=breaks,labels=F,include.lowest=T)
  start<-c(1,(1:nx)[c(F,diff(i)!=0)])
  end<-c(start[-1]-1,nx)
  j<-i[start] # bins that we will fill
  nj<-length(j)
  # exception handling
  if (nj>nb) stop('super.hist: unexpected length mismatch, nb=', nb, ', nj=', nj)
  if (nj<nb) warning('super.hist: ', nb-nj, ' empty bins')
  # determine sums
  n<-rep(0,nb) ; sx<-n ; sxx<-n ; sy<-n ; syy<-n
  n[j]<-end - c(0,end[-nj])
  cs<-cumsum(x) ; sx[j]<-cs[end] - c(0,cs[end[-nj]])
  cs<-cumsum(x*x) ; sxx[j]<-cs[end] - c(0,cs[end[-nj]])
  cs<-cumsum(y) ; sy[j]<-cs[end] - c(0,cs[end[-nj]])
  cs<-cumsum(y*y) ; syy[j]<-cs[end] - c(0,cs[end[-nj]])
  # calculate statistics
  list(breaks=breaks, nx=nx, ntrim=ntrim, nb=nb, nj=nj, outofbounds=outofbounds,
       n=n, sx=sx, sxx=sxx, sy=sy, syy=syy,
       xbar=sx/n, sdx=sqrt((sxx-sx^2/n)/(n-1)),
       ybar=sy/n, sdy=sqrt((syy-sy^2/n)/(n-1)))
}

##############################################

super.hist.print <-
  function(sh, nsd=1/3, sdev=T, digits=3)
{  
  for (i in names(sh)) assign(i,sh[[i]])
  for (i in names(b)) assign(i,b[[i]])
  cat('Sample size is',nx,'for',name,'::\n')
  if (sdev) means<-ifelse(means<=0,0,sqrt(means))
  print(unlist(list(ntrim=ntrim,nb=nb,nj=nj,outofbounds=outofbounds,mean=means)))
  heading<-paste(breaks[-length(breaks)],breaks[-1],sep='-')
  table<-rbind(n=n)
  dimnames(table)[[2]]<-heading
  print(table)
  table<-rbind(percent=100*n/nx,xbar=xbar)
  for (x in c('b','o','a')) {
    ybar<-sh[[x]]$ybar; d<-nsd*sh[[x]]$sdy ; ypd<-ybar+d ; ymd<-ybar-d
    if (sdev) {
      ybar<-sqrt(ifelse(ybar<=0,NA,ybar))
      ypd<-sqrt(ifelse(ypd<=0,0,ypd))
      ymd<-sqrt(ifelse(ymd<=0,0,ymd)) 
    }
    y<-rbind(ypd=ypd,ybar=ybar,ymd=ymd)
    dimnames(y)[[1]]<-paste(x,dimnames(y)[[1]],sep='.')
    table<-rbind(table,y) }
  dimnames(table)[[2]]<-heading
  print(round(table,digit=digits))
}

##############################################

plot.super.hist <-
  function (sh, xlab='x', ylab='y', main='', add=F, nsd=1, sem=F, sdev=F,
            histo=T, super=T,
            xrange=range(breaks), yrange=range(c(y+d,y-d)),
            line.col=4, lwd=3, add.lines=F,
            rgbc=col2rgb(line.col),transparency=0.25,
            poly.col=rgb(rgbc['red',1],rgbc['green',1],rgbc['blue',1],
              alpha=255*transparency,max=255),
            ...)
{
  # plot the super histogram
  # unpack the sh list
  for (i in names(sh)) assign(i,sh[[i]])
  x<-rep(NA,2*nb+1)
  x[seq(2,2*nb,by=2)]<-xbar
  x[seq(1,2*nb+1,by=2)]<-breaks
  y<-rep(NA,2*nb+1)
  y[seq(2,2*nb,by=2)]<-ybar
  y[seq(3,2*nb-1,by=2)]<-ybar[-nb] + (breaks[-c(1,nb+1)]-xbar[-nb])*(ybar[-1]-ybar[-nb])/(xbar[-1]-xbar[-nb])
  dbar<-nsd*sdy/(if(sem) sqrt(n) else 1)
  d<-rep(NA,2*nb+1)
  d[seq(2,2*nb,by=2)]<-dbar
  d[seq(3,2*nb-1,by=2)]<-dbar[-nb] + (breaks[-c(1,nb+1)]-xbar[-nb])*(dbar[-1]-dbar[-nb])/(xbar[-1]-xbar[-nb])
  # constant extrapolation to missing break points
  for (i in seq(1,2*nb-1,by=2)) if (is.na(y[i])) {y[i]<-y[i+1] ; d[i]<-d[i+1]}
  for (i in seq(3,2*nb+1,by=2)) if (is.na(y[i])) {y[i]<-y[i-1] ; d[i]<-d[i-1]}
  # determine y+d, y-d and take sqrt if desired
  ypd<-y+d ; ymd<-y-d
  if (sdev) {
    y<-sqrt(ifelse(y<=0,NA,y))
    ypd<-sqrt(ifelse(ypd<=0,0,ypd))
    ymd<-sqrt(ifelse(ymd<=0,0,ymd)) 
  }
  # plot the variation of y with x
  if (!add) plot(xrange, yrange, type='n', xlab=xlab, ylab=ylab, main=main, ...)
  if (super) {
    for (i in 1:(length(y)-1))
      if (!is.na(ymd[i]) && !is.na(ymd[i+1]) && !is.na(ypd[i]) && !is.na(ypd[i+1]))
        polygon(c(x[i],x[i+1],x[i+1],x[i]),c(ymd[i],ymd[i+1],ypd[i+1],ypd[i]),
                border=NA,col=poly.col)
    lines(x,y,lwd=lwd,col=line.col)
    if (add.lines) lines(x,ypd,lwd=max(1,lwd-1),col=line.col)
    if (add.lines) lines(x,ymd,lwd=max(1,lwd-1),col=line.col)
  }
  # add histogram as lollipop plot
  if (histo) {
  # scale n for this plot so that full y axis goes from 0 to 100 %
    ymin<-par('usr')[3] ; ymax<-par('usr')[4]
    abline(h=ymin+(ymax-ymin)/nb,col=1,lty=3)
    ns<-ymin+(ymax-ymin)*n/nx
  # note that points with zero counts will have xbar=NA and will not be plotted
    segments(xbar,ymin,xbar,ns)
    points(xbar,ns,pch=19,xpd=T)
  # annotate axes
  # tick marks for breaks
    axis(1,breaks,F,tcl=1/3)
  # percents on right axis
  # typically requires par(mar=c(5,4,4,2)+0.1)
    axis(4,ymin+(ymax-ymin)*c(1,2,5,10,20,50)/100,c('','','5','10','20','50'))
    mtext('Percent',4,1,adj=1)
  }
}

##############################################
