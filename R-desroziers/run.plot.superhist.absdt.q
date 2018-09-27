# # Plot superhist for B, O, and A in one plot per treatment
# # x variables include o, b, a, a-b, lon, platform, dt, time, gamma
# # These parameters must be set main='Title'; sdev=T ; nsd=1/3
# # Assumes all.data and select are already in the work space

# # ============= Unpack selected all.data, calculate Cx
  for (i in dimnames(all.data)[[2]]) assign(i,all.data[select,i])
  Ca<-(a-b)*(o-a)
  Cb<-(a-b)*(o-b)
  Co<-(o-a)*(o-b)
# # ============= Initialize plotting
  wind.range=if (sdev) c(-0.25,3) else c(-2,8)
  yrange= if (sdev) c(-0.25,2.0) else c(-1,3)
# yrange=wind.range
  ylab= if (sdev) 'sqrt(Cx) [m/s]' else '(Cx) [(m/s)^2]'
  par(mfrow=c(2,1))
  trim<-0; np<-length(platforms)
  means<-c(B=mean(Cb,trim=trim),O=mean(Co,trim=trim),A=mean(Ca,trim=trim))
  line.col=c('cyan','red','blue')
# # ============= Observation wind speed (m/s)
  xlab='Observation wind speed [m/s]' ; name<-paste(basename,'vs','o',sep='.')
  x<-o
  breaks=c(0,2,4,6,8,10,12,16,20,Inf)
  xrange=c(0,25)
  plot.VC.super.hist(x, Cb, Co, Ca, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=wind.range,xlab=xlab,line.col=line.col,name=name)
# # ============= ABS(Time) (minutes relative to synoptic time)
  xlab='|Time| (minutes relative to synoptic time)' ; name<-paste(basename,'vs','absdt',sep='.')
  x<-abs(dt)
  breaks=seq(0,180,by=15)
  xrange=c(0,180)
  plot.VC.super.hist(x, Cb, Co, Ca, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,name=name)
