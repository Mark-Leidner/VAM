postscript('front-xy.ps',paper='letter',horizontal=F,onefile=F)

xmin<-0 ; xmax<-8
ymin<-0 ; ymax<-4
x1<-3 ; x2<-5 ; xf<-3.5 ; yf<-1
charsize <- 3
par(lwd=1,mfrow=c(1,1))
aspect((xmax-xmin)/(ymax-ymin))
plot(c(xmin,xmax),c(ymin,ymax),type='n',xaxt='n',yaxt='n',xlab='',ylab='')
shading<-color.scale(c(x1,x2),c('darkgrey','white'),saturate=T)
up<-par('usr')
values<-seq(from=up[1],to=up[2],length=201)
add.color.gradient(shading,top2bottom=F,zvalues=values)
par(lwd=2)
abline(v=c(x1,x2))
points(xf,yf,pch=3,cex=charsize)
box()
axis(1,x1,expression(lambda[1]),cex.axis=charsize,lwd=2,line=1,tick=F)
axis(1,x2,expression(lambda[2]),cex.axis=charsize,lwd=2,line=1,tick=F)
axis(1,xmax,expression(lambda %->% phantom(0)),cex.axis=charsize,lwd=2,line=0.5,tick=F)
axis(2,ymax,expression(phi %->% phantom(0)),cex.axis=charsize,lwd=2,tick=F,hadj=0.6,line=0)

dev.off()
