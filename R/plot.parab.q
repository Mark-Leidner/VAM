
parab<-function(x,ymax=0.6,dx=3){ymax*(1-(x/dx-1)^2)}

plot.parab.uvw <-
  function(tuvw,ylab="RMS errors (m/s)",ymax=1.25,nt=dim(tuvw)[1],
           tmax=as.real(dimnames(tuvw)[[1]][nt]),ptmax=tmax,dx=(nt-1)/2,
           peaks=range(tuvw[dx+1,-1]),times=tmax*(0:100)/100,
           add=F,linear=F,col=2:4,charsize=1.5)
  # use peaks=NA to suppress reference parabola
{if (!add) {
  plot(c(0,ptmax),c(0,ymax),type='n',xlab='',ylab='',cex.axis=charsize)
  title(xlab='Time (h)',ylab=ylab,line=2.25,cex.lab=1.33*charsize) }
 for (i in peaks) if (!linear) lines(times,parab(times,i,dx),lwd=2)
 else lines(c(0,1,2)*dx,c(0,i,0),lwd=2)
 points(0:tmax,tuvw[,4],col=col[3],pch=15,cex=2)
 points(0:tmax,tuvw[,3],col=col[2],pch=16,cex=1.5)
 points(0:tmax,tuvw[,2],col=col[1],pch=18,cex=1.5)
 if (!add) legend('bottom',c('u','v','W'),pch=c(18,16,15),col=col,cex=charsize,inset=0.05)
}
