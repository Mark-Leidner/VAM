# # Plot superhist for B, O, and A in one plot per treatment
# # x variables include o, b, a, a-b, lon, platform, dt, time, gamma
# # These parameters must be set main='Title'; sdev=T ; nsd=1/3; basename='VCQC.all' or similar.
  sdev=F ; nsd=1/3
  main<-paste('B, O, A error variance, ', lat.nameNS, ', 2004 data, VC QC, +/- 1/3 sdev',sep='')
# # Assumes all.data and select are already in the work space

# # ============= Unpack selected all.data, calculate Cx
  for (i in dimnames(all.data)[[2]]) assign(i,all.data[select,i])
  Ca<-(a-b)*(o-a)
  Cb<-(a-b)*(o-b)
  Co<-(o-a)*(o-b)
# # ============= Naive estimates of slopes
  gamma<-(o-b)/(a-b)
  delta<-1/gamma
  gammaBar<-boxplot.stats(gamma,do.conf=F,do.out=F)$stats[3] # whisker, 1st quartile, median, 3rd quartile, whisker
  gammaSD<-diff(boxplot.stats(gamma,do.conf=F,do.out=F)$stats[c(2,4)])/diff(qnorm(c(1,3)/4)) # approximate standard deviation
  gammaSlopes<-unlist(list(B=gammaBar,O=(gammaBar-1)*gammaBar+gammaSD^2,A=gammaBar-1))
  deltaBar<-boxplot.stats(delta,do.conf=F,do.out=F)$stats[3] # whisker, 1st quartile, median, 3rd quartile, whisker
  deltaSD<-diff(boxplot.stats(delta,do.conf=F,do.out=F)$stats[c(2,4)])/diff(qnorm(c(1,3)/4)) # approximate standard deviation
  deltaSlopes<-unlist(list(B=deltaBar,O=1-deltaBar,A=deltaBar*(1-deltaBar)-deltaSD^2))
# # ============= Initialize plotting
  ylab= if (sdev) 'sqrt(Cx) [m/s]' else '(Cx) [(m/s)^2]'
  pdf('gamma.pdf',width=8,height=10,onefile=T,title='gamma plot')
  par(mfrow=c(1,1)) ; trim<-0; np<-length(platforms)
  means<-c(B=mean(Cb,trim=trim),O=mean(Co,trim=trim),A=mean(Ca,trim=trim))
  line.col=c('cyan','red','blue')
  breaks=c(0,0.1,0.25,0.5,seq(1,4,0.5),5:8,10,12,14,16,Inf)
  xrange=c(0,16) ; yrange=c(-2,20)
# # ============= A-B (m/s)^2
  xlab='(A-B)^2 [(m/s)^2]' ; name<-paste(basename,'vs','amb2',sep='.')
  x<-(a-b)^2
  plot.VC.super.hist(x, Cb, Co, Ca, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,name=name)
  for (i in 1:3) abline(0,gammaSlopes[i],lty=2,lwd=2,col=line.col[i])
# # ============= O-B (m/s)^2
  xlab='(O-B)^2 [(m/s)^2]' ; name<-paste(basename,'vs','omb2',sep='.')
  x<-(o-b)^2
  plot.VC.super.hist(x, Cb, Co, Ca, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,name=name)
  for (i in 1:3) abline(0,deltaSlopes[i],lty=2,lwd=2,col=line.col[i])
# # ============= O-A (m/s)^2
  xlab='(O-A)^2 [(m/s)^2]' ; name<-paste(basename,'vs','oma2',sep='.')
  x<-(o-a)^2
  plot.VC.super.hist(x, Cb, Co, Ca, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,name=name)
# # ============= End of plotting
  dev.off(dev.cur())
