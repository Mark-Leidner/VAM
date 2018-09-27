# # Plot superhist for gamma, delta in one plot per treatment
# # x variables include o, b, a, a-b, lon, platform, dt, |dt|, time, gamma
# # These parameters must be set main='Title'; sdev=T ; nsd=1/3
# # Assumes all.data and select are already in the work space

# # ============= Unpack selected all.data, calculate Cx
  for (i in dimnames(all.data)[[2]]) assign(i,all.data[select,i])
  gamma<-(o-b)/(a-b)
  delta<-(a-b)/(o-b)
# # ============= Initialize plotting
  yrange=c(-5,5)
  ylab='gamma, delta'
  pdf('gamma.pdf',width=8,height=10,onefile=T,title='gamma plot')
  par(mfrow=c(2,1))
  trim<-0; np<-length(platforms)
  means<-c(gamma=mean(gamma,trim=trim),delta=mean(delta,trim=trim))
  line.col=c('red','blue')
# # ============= Observation wind speed (m/s)
  xlab='Observation wind speed [m/s]' ; name<-paste(basename,'vs','o',sep='.')
  x<-o
  breaks=c(0,2,4,6,8,10,12,16,20,Inf)
  xrange=c(0,25)
  plot.gd.super.hist(x, gamma, delta, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,name=name)
# # ============= Background wind speed (m/s)
  xlab='Background wind speed [m/s]' ; name<-paste(basename,'vs','b',sep='.')
  x<-b
  breaks=c(0,2,4,6,8,10,12,16,20,Inf)
  xrange=c(0,25)
  plot.gd.super.hist(x, gamma, delta, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,name=name)
# # ============= Analysis wind speed (m/s)
  xlab='Analysis wind speed [m/s]' ; name<-paste(basename,'vs','a',sep='.')
  x<-a
  breaks=c(0,2,4,6,8,10,12,16,20,Inf)
  xrange=c(0,25)
  plot.gd.super.hist(x, gamma, delta, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,name=name)
# # ============= A-B (m/s)^2
  xlab='(A-B)^2 [(m/s)^2]' ; name<-paste(basename,'vs','amb2',sep='.')
  x<-(a-b)^2
  breaks=c(0,0.1,0.25,0.5,seq(1,4,0.5),5:8,10,12,14,16,Inf)
  xrange=c(0,16)
  plot.gd.super.hist(x, gamma, delta, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,name=name)
# # ============= O-B (m/s)^2
  xlab='(O-B)^2 [(m/s)^2]' ; name<-paste(basename,'vs','omb2',sep='.')
  x<-(o-b)^2
  breaks=c(0,0.1,0.25,0.5,seq(1,4,0.5),5:8,10,12,14,16,Inf)
  xrange=c(0,16)
  plot.gd.super.hist(x, gamma, delta, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,name=name)
# # ============= Longitude
  xlab='Longitude (degrees)' ; name<-paste(basename,'vs','lon',sep='.')
  x<-lon
  breaks=seq(0,360,30)
  xrange=c(0,360)
  plot.gd.super.hist(x, gamma, delta, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,name=name)
# # ============= Platform
  xlab='Platform' ; name<-paste(basename,'vs','platform',sep='.')
  x<-platform
  breaks=0.5+0:np
  xrange=c(0,np)+0.5
  plot.gd.super.hist(x, gamma, delta, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,name=name)
  text(1:np,yrange[2],pos=1,unlist(platforms),cex=1)
# # ============= Time (minutes relative to synoptic time)
  xlab='Time (minutes relative to synoptic time)' ; name<-paste(basename,'vs','dt',sep='.')
  x<-dt
  breaks=seq(-180,180,by=15)
  xrange=c(-180,180)
  plot.gd.super.hist(x, gamma, delta, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,name=name)
# # ============= ABS(Time) (minutes relative to synoptic time)
  xlab='|Time| (minutes relative to synoptic time)' ; name<-paste(basename,'vs','absdt',sep='.')
  x<-abs(dt)
  breaks=seq(0,180,by=15)
  xrange=c(0,180)
  plot.gd.super.hist(x, gamma, delta, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,name=name)
# # ============= Time (days relative New Year 2004)
  xlab='Time (days relative New Year 2004)' ; name<-paste(basename,'vs','time',sep='.')
  x<-time
  daysInMonth<-c(31,29,31,30,31,30,31,31,30,31,30,31)
  breaks<-c(0,cumsum(daysInMonth))
  xrange=range(breaks)
  plot.gd.super.hist(x, gamma, delta, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,name=name)
  midbreaks<-0.5*(breaks[-1]+breaks[-length(breaks)])
  months<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  text(midbreaks,yrange[2],pos=1,months,cex=1)
# # ============= Gamma
  xlab='(O-B)/(A-B)' ; name<-paste(basename,'vs','gamma',sep='.')
  x<-(o-b)/(a-b)
  breaks=c(-Inf,seq(-4,6,0.5),Inf)
  xrange=c(-5,7)
  plot.gd.super.hist(x, gamma, delta, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,name=name)
# # ============= End of plotting
  dev.off(dev.cur())
