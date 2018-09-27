meranti<-c(0,0.0000,0.0000,0.0000,1,0.4023,0.3919,0.3786,2,0.6008,0.5890,0.5605,3,0.6533,0.6441,0.6116,4,0.5820,0.5797,0.5511,5,0.3711,0.3795,0.3592,6,0.0000,0.0000,0.0000)
meranti<-matrix(meranti,byrow=T,nrow=7,dimnames=list(0:6,c("hr","u","v","wspd")))

title<-"RMS errors (m/s)"
parab<-function(x,ymax=0.6,dx=3){ymax*(1-(x/dx-1)^2)}

postscript('fgat-meranti.ps',paper='letter',horizontal=F,onefile=T,title='fgat')
par(mfrow=c(2,2),mar=c(3,3,1,1)+0.1,cex=1.5)
plot(c(0,6),c(0,0.7),type='n',xlab='',ylab='')
title(xlab='Time (h)',ylab=title,line=2)
lines(0:100/6,parab(0:100/6,1/sqrt(2)),lwd=2)
lines(0:100/6,parab(0:100/6,1/sqrt(3)),lwd=2)
points(0:6,meranti[,4],col=4,pch=15,cex=2)
points(0:6,meranti[,3],col=3,pch=16,cex=1.5)
points(0:6,meranti[,2],col=2,pch=18,cex=1.5)
legend('bottom',c('u','v','W'),pch=c(18,16,15),col=2:4,cex=1,inset=0.05)
dev.off(dev.cur())

