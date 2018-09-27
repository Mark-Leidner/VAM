postscript('front-tz.ps',paper='letter',horizontal=F,onefile=F)

xmin<-0 ; xmax<-8
ymin<-0 ; ymax<-4
x1<-3 ; x2<-5 ; xf<-3.5 ; yf<-1
charsize <- 3
aspect((xmax-xmin)/(ymax-ymin))

par(lwd=2,mfrow=c(1,1))
tmin<-0 ; tmax<-8
zmin<-(-0.10); zmax<-1.10
t1<-2 ; t2<-6 ; tf<-3.5
plot(c(tmin,tmax),c(zmin,zmax),type='n',axes=F,xlab='',ylab='')
axis(1,c(tmin,t1,tf,t2,tmax),c('','','','',''),lwd=2,tick=T)
axis(1,c(t1,tf,t2),c(expression(t[1]),expression(t[f]),expression(t[2])),cex.axis=charsize,lwd=2,tick=F,line=1.0)
axis(1,tmax,expression(t %->% phantom(0)),cex.axis=charsize,lwd=2,tick=F,line=0.5)
axis(2,c(0,1),c('0','1'),cex.axis=charsize,lwd=2,tick=T)
axis(2,0.5,expression(Z %->% phantom(0)),cex.axis=charsize,lwd=2,tick=F,line=0.2,hadj=0.4)
lines(c(tmin,t1,t2,tmax),c(0,0,1,1))
lines(c(tmin,tf,tf,tmax),c(0,0,1,1),lty=2)

dev.off()
