postscript('archtypes.ps',paper='letter',horizontal=F,onefile=F)

xmin<-0 ; xmax<-8
ymin<-0 ; ymax<-4
x1<-3 ; x2<-5 ; xf<-3.5 ; yf<-1
charsize <- 3
aspect((xmax-xmin)/(ymax-ymin))

par(lwd=2,mfrow=c(1,1))
tmin<-0 ; tmax<-8
zmin<-(-0.10); zmax<-1.10
plot(c(tmin,tmax),c(zmin,zmax),type='n',axes=F,xlab='',ylab='')
t0<-0; t1<-0.25 ; t2<-2.25; t3<-2.50
for (offset in c(0:2)*(2.75)) axis(1,offset+c(t1,t2),c('',''),lwd=2,tick=T)
for (offset in c(0:2)*(2.75)) axis(1,offset+c(t0,t1,t2,t3),c('',expression(t[1]),expression(t[2]),''),cex.axis=charsize,lwd=2,tick=F,line=1)
axis(1,(tmax-tmin)/2+tmin,expression(t %->% phantom(0)),cex.axis=charsize,lwd=2,tick=F,hadj=0.4,line=2.4)
axis(2,c(0,1),c('',''),cex.axis=charsize,lwd=2,tick=T)
axis(2,0.5,expression(Error %->% phantom(0)),cex.axis=charsize,lwd=2,tick=F,hadj=0.4)
text(c(0.55,3.5,6.25),1,c('a','b','c'),pos=1,cex=charsize)

x<-seq(from=0,to=1,length=101)
x0<-0
lines(x0+(t1+t2)/2+(t2-(t1+t2)/2)*x,1-x^2)
lines(x0+(t1+t2)/2-((t1+t2)/2-t1)*x,1-x^2)
x0<-2.75
lines(x0+(t1+t2)/2+(t2-(t1+t2)/2)*x,1-x)
lines(x0+(t1+t2)/2-((t1+t2)/2-t1)*x,1-x)
x0<-2*2.75
lines(x0+t1+((t1+t2)/2-t1)*x,(exp(x)-1)/(exp(1)-1))
lines(x0+t2+((t1+t2)/2-t2)*x,(exp(x)-1)/(exp(1)-1))

dev.off()
