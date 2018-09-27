  # requires all.data
  # also breaks, histo, nsdVC, VC.sd, VC.means

  trim<-0; np<-1
  line.col=c('cyan','red','blue')
  xlab='Longitude (degrees)'
  # breaks=seq(125,225,5)
  # histo=F
  xrange=c(125,225)

  for (i in dimnames(all.data)[[2]]) assign(i,all.data[,i])
  aVC<-(a-b)*(o-a)
  bVC<-(a-b)*(o-b)
  oVC<-(o-a)*(o-b)
  means<-c(B=mean(bVC,trim=trim),O=mean(oVC,trim=trim),A=mean(aVC,trim=trim))
  select.a<-abs(aVC-VC.means$a)<nsdVC*VC.sd$a
  select.b<-abs(bVC-VC.means$b)<nsdVC*VC.sd$b
  select.o<-abs(oVC-VC.means$o)<nsdVC*VC.sd$o
  select<-select.a & select.b & select.o
  unlist(list(Lat=as.real(lat.name),Total=length(select),Selected=sum(select),QCedPercent=round(100*(1-sum(select)/length(select)),digits=2)))
  select.qscat<-platform==5
  select.amsre<-platform==1

  select<-select.a & select.b & select.o & select.qscat
  unlist(list(Lat=as.real(lat.name),Total=length(select),Selected=sum(select),QCedPercent=round(100*(1-sum(select)/length(select)),digits=2)))
  for (i in dimnames(all.data)[[2]]) assign(i,all.data[select,i])
  aVC<-(a-b)*(o-a)
  bVC<-(a-b)*(o-b)
  oVC<-(o-a)*(o-b)

  par(mfrow=c(2,1))
  sdev=T ; nsd=1/3
  yrange= if (sdev) c(-0.25,2.0) else c(-1,3)
  ylab= if (sdev) 'sqrt(VC) [m/s]' else '(VC) [(m/s)^2]'
  main<-paste('A, B, O error st. dev., ', lat.nameNS, ', 2004 data, VC QC, QSCAT, +/- 1/3 sdev',sep='')
  plot.VC.super.hist(lon, bVC, oVC, aVC, breaks,nsd=nsd,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col,histo=histo)

  sdev=F ; nsd=1/3
  yrange=c(4,12)
  ylab='O (m/s)'
  main<-paste('Observed wind speed ', lat.nameNS, ', 2004 data, VC QC, QSCAT, +/- 1/3',sep='')
  plot(xrange,yrange,xlab=xlab,ylab=ylab,main=main,type='n')
  plot.super.hist(super.hist(lon,o,breaks=breaks,trim=trim),add=T,sdev=sdev,line.col=line.col[2],nsd=nsd,histo=histo)
  abline(h=0,col=2) ; abline(h=mean(o,trim=trim),col=4,lwd=2)
  legend('topright',leg=paste('Mean=',round(mean(o,trim=trim),digits=2),sep=''),bty='n',cex=2)

  par(mfrow=c(2,1))
  sdev=F ; nsd=1/3
  yrange=c(0,180)
  ylab='|dt| (min)'
  main<-paste('Time ', lat.nameNS, ', 2004 data, VC QC, QSCAT, +/- 1/3',sep='')
  plot(xrange,yrange,xlab=xlab,ylab=ylab,main=main,type='n')
  plot.super.hist(super.hist(lon,abs(dt),breaks=breaks,trim=trim),add=T,sdev=sdev,line.col=line.col[1],nsd=nsd,histo=histo)
  abline(h=0,col=2) ; abline(h=mean(abs(dt),trim=trim),col=4,lwd=2)
  legend('topright',leg=paste('Mean=',round(mean(abs(dt),trim=trim),digits=2),sep=''),bty='n',cex=2)

  # Repeat for just the lollipops alone
  plot(xrange,yrange,xlab=xlab,ylab=ylab,main=main,type='n')
  plot.super.hist(super.hist(lon,abs(dt),breaks=breaks,trim=trim),add=T,sdev=sdev,line.col=line.col[1],nsd=nsd,histo=T,super=F)

  qc2<-2*abs(o-a)/(o+a)
  yrange=c(0,.20)
  ylab='|Speed QC|'
  main<-paste('Speed QC ', lat.nameNS, ', 2004 data, VC QC, QSCAT, +/- 1/3',sep='')
  plot(xrange,yrange,xlab=xlab,ylab=ylab,main=main,type='n')
  plot.super.hist(super.hist(lon,abs(qc2),breaks=breaks,trim=trim),add=T,sdev=sdev,line.col=line.col[1],nsd=nsd,histo=histo)
  abline(h=0,col=2) ; abline(h=mean(abs(qc2),trim=trim),col=4,lwd=2)
  legend('topright',leg=paste('Mean=',round(mean(abs(qc2),trim=trim),digits=2),sep=''),bty='n',cex=2)

  select<-select.a & select.b & select.o & select.amsre
  unlist(list(Lat=as.real(lat.name),Total=length(select),Selected=sum(select),QCedPercent=round(100*(1-sum(select)/length(select)),digits=2)))
  for (i in dimnames(all.data)[[2]]) assign(i,all.data[select,i])
  aVC<-(a-b)*(o-a)
  bVC<-(a-b)*(o-b)
  oVC<-(o-a)*(o-b)

  par(mfrow=c(2,1))
  sdev=T ; nsd=1/3
  yrange= if (sdev) c(-0.25,2.0) else c(-1,3)
  ylab= if (sdev) 'sqrt(VC) [m/s]' else '(VC) [(m/s)^2]'
  main<-paste('A, B, O error st. dev., ', lat.nameNS, ', 2004 data, VC QC, AMSRE, +/- 1/3 sdev',sep='')
  plot.VC.super.hist(lon, bVC, oVC, aVC, breaks,nsd=nsd,histo=histo,sdev=sdev,ylab=ylab,main=main,means=means,xrange=xrange,yrange=yrange,xlab=xlab,line.col=line.col)

  sdev=F ; nsd=1/3
  yrange=c(4,12)
  ylab='O (m/s)'
  main<-paste('Observed wind speed ', lat.nameNS, ', 2004 data, VC QC, AMSRE, +/- 1/3',sep='')
  plot(xrange,yrange,xlab=xlab,ylab=ylab,main=main,type='n')
  plot.super.hist(super.hist(lon,o,breaks=breaks,trim=trim),add=T,sdev=sdev,line.col=line.col[2],nsd=nsd,histo=histo)
  abline(h=0,col=2) ; abline(h=mean(o,trim=trim),col=4,lwd=2)
  legend('topright',leg=paste('Mean=',round(mean(o,trim=trim),digits=2),sep=''),bty='n',cex=2)

  par(mfrow=c(2,1))
  sdev=F ; nsd=1/3
  yrange=c(0,180)
  ylab='|dt| (min)'
  main<-paste('Time ', lat.nameNS, ', 2004 data, VC QC, AMSRE, +/- 1/3',sep='')
  plot(xrange,yrange,xlab=xlab,ylab=ylab,main=main,type='n')
  plot.super.hist(super.hist(lon,abs(dt),breaks=breaks,trim=trim),add=T,sdev=sdev,line.col=line.col[1],nsd=nsd,histo=histo)
  abline(h=0,col=2) ; abline(h=mean(abs(dt),trim=trim),col=4,lwd=2)
  legend('topright',leg=paste('Mean=',round(mean(abs(dt),trim=trim),digits=2),sep=''),bty='n',cex=2)

  # Repeat for just the lollipops alone
  plot(xrange,yrange,xlab=xlab,ylab=ylab,main=main,type='n')
  plot.super.hist(super.hist(lon,abs(dt),breaks=breaks,trim=trim),add=T,sdev=sdev,line.col=line.col[1],nsd=nsd,histo=T,super=F)

  qc2<-2*abs(o-a)/(o+a)
  yrange=c(0,.20)
  ylab='|Speed QC|'
  main<-paste('Speed QC ', lat.nameNS, ', 2004 data, VC QC, AMSRE, +/- 1/3',sep='')
  plot(xrange,yrange,xlab=xlab,ylab=ylab,main=main,type='n')
  plot.super.hist(super.hist(lon,abs(qc2),breaks=breaks,trim=trim),add=T,sdev=sdev,line.col=line.col[1],nsd=nsd,histo=histo)
  abline(h=0,col=2) ; abline(h=mean(abs(qc2),trim=trim),col=4,lwd=2)
  legend('topright',leg=paste('Mean=',round(mean(abs(qc2),trim=trim),digits=2),sep=''),bty='n',cex=2)
