
##############################################
########### Read latitude data ###############
##############################################

readInputDataByLat <-
  function (lat='42.375', year = 2004,
            platforms=list(amsre='AMSRE',f13='F13',f14='F14',f15='F15',qscat='QSCAT',tmi='TMI'),
            dir='/Users/rhoffman/Desktop/P1736-NASA-OceanWInds/CCMP/gammaInputDataByLat/',
            columns=c('date','time','dt','lat','lon','a','b','o'),
            daysInMonth=c(31,29,31,30,31,30,31,31,30,31,30,31),
            daysTilMonth=c(0,cumsum(daysInMonth)[-12]))
  { # Leap year for 2004 for now.
    first.file<-T
    cat('In directory',dir,'\n')
    for (i in 1:length(platforms)) {
      dataname<-paste(names(platforms)[i],'speed',year,lat,sep='.')
      filename<-paste(dir,dataname,'.txt',sep='')
      cat('File',paste(dataname,'.txt: ',sep=''))
      if (file.exists(filename)) {
        xx<-matrix(scan(filename,skip=1),ncol=length(columns),byrow=T,dimnames=list(NULL,columns))
        datetime<-xx[,'date']*100+xx[,'time']/10000
        month<-(datetime%%1000000)%/%10000
        day<-(datetime%%10000)%/%100
        hour<-datetime%%100
        time<-daysTilMonth[month]+day+hour/24-1
        gamma<-(xx[,'o']-xx[,'b'])/(xx[,'a']-xx[,'b'])
      # Eliminate infinities
        gamma[gamma>777]<-777; gamma[gamma<(-777)]<-(-777)
        yy<-cbind(platform=i,datetime=datetime,time=time,xx[,3:dim(xx)[2]],gamma=gamma)
        if (first.file) {
          zz<-yy ; first.file<-F
        }
        else zz<-rbind(zz,yy) }
      else cat('Does not exist\n') }
    # order by lon, then time, then platform
    zz<-zz[order(zz[,'lon'],zz[,'time'],zz[,'platform']),]
    # identify groups for same time at each longitude
    dtime<-c(1,diff(zz[,'time']))
    # these parameters are by group, below use rep(...,numIndex) to expand
    startIndex<-c((1:length(dtime))[dtime!=0],1+length(dtime))
    end<-startIndex[-1] ; begin<-startIndex[-length(startIndex)]
    numIndex<-end-begin
    # calculate group averages
    cumdt<-c(0,cumsum(zz[,'dt']))
    meandt<-(cumdt[end]-cumdt[begin])/numIndex
    cumgamma<-c(0,cumsum(zz[,'gamma']))
    meangamma<-(cumgamma[end]-cumgamma[begin])/numIndex
    cumavc<-c(0,cumsum((zz[,'a']-zz[,'b'])*(zz[,'o']-zz[,'a'])))
    avc<-cumavc[end]-cumavc[begin]
    cbind(zz,nobs=rep(numIndex,numIndex),
          dtbar=rep(meandt,numIndex),
          gammabar=rep(meangamma,numIndex),
          avc=rep(avc,numIndex))
  }

##############################################
############ Plotting for Cx #################
##############################################

plot.VC.super.hist <-
  function (x, bVC, oVC, aVC, breaks, trim=0, nsd=0.5, overplot=T, sdev=F,
            histo=T, super=T, line.col=c('yellow','red','blue'),
            means=c(b=mean(bVC,trim=trim),o=mean(oVC,trim=trim),a=mean(aVC,trim=trim)),
            main='title', name=NULL,
            xrange=range(x), yrange=range(c(bVC,oVC,aVC)),
            xlab='x', ylab='y')
{
  plot(xrange,yrange,xlab=xlab,ylab=ylab,main=main,type='n')
  SH.b<-super.hist(x,bVC,breaks=breaks,trim=trim)
  SH.o<-super.hist(x,oVC,breaks=breaks,trim=trim)
  SH.a<-super.hist(x,aVC,breaks=breaks,trim=trim)
  if (!is.null(name)) assign(name,list(b=SH.b,o=SH.o,a=SH.a,means=means,name=name),pos=1)
  plot.super.hist(SH.b,add=T,nsd=nsd,sdev=sdev,histo=histo,super=super,line.col=line.col[1])
  plot.super.hist(SH.o,add=T,nsd=nsd,sdev=sdev,histo=F,super=super,line.col=line.col[2])
  plot.super.hist(SH.a,add=T,nsd=nsd,sdev=sdev,histo=F,super=super,line.col=line.col[3])
  abline(h=0,col=1)
  if (sdev) means<-ifelse(means<=0,0,sqrt(means))
  abline(h=means,col=line.col,lwd=2,lty=3)
  legend('topright',leg=paste(names(means),round(means,digits=2),sep=' = '),lwd=2,lty=1,col=line.col,title='Mean values',cex=1)
  if (overplot) {
    plot.super.hist(SH.b,add=T,nsd=0,sdev=sdev,histo=F,super=super,line.col=line.col[1])
    plot.super.hist(SH.o,add=T,nsd=0,sdev=sdev,histo=F,super=super,line.col=line.col[2])
    plot.super.hist(SH.a,add=T,nsd=0,sdev=sdev,histo=F,super=super,line.col=line.col[3])
  }
}

##############################################
######## Plotting for gamma delta ############
##############################################

plot.gd.super.hist <-
  function (x, gamma, delta, breaks, trim=0, nsd=0.5, overplot=T, sdev=F,
            histo=T, super=T, line.col=c('red','blue'),
            means=c(gamma=mean(gamma,trim=trim),delta=mean(delta,trim=trim)),
            main='title', name=NULL,
            xrange=range(x), yrange=range(c(gamma,delta)),
            xlab='x', ylab='y')
{
  plot(xrange,yrange,xlab=xlab,ylab=ylab,main=main,type='n')
  SH.gamma<-super.hist(x,gamma,breaks=breaks,trim=trim)
  SH.delta<-super.hist(x,delta,breaks=breaks,trim=trim)
  if (!is.null(name)) assign(name,list(gamma=SH.gamma,delta=SH.delta,means=means,name=name),pos=1)
  plot.super.hist(SH.gamma,add=T,nsd=nsd,sdev=sdev,histo=histo,super=super,line.col=line.col[1])
  plot.super.hist(SH.delta,add=T,nsd=nsd,sdev=sdev,histo=F,super=super,line.col=line.col[2])
  abline(h=0,col=1)
  if (sdev) means<-ifelse(means<=0,0,sqrt(means))
  abline(h=means,col=line.col,lwd=2,lty=3)
  legend('topright',leg=paste(names(means),round(means,digits=2),sep=' = '),lwd=2,lty=1,col=line.col,title='Mean values',cex=1)
  if (overplot) {
    plot.super.hist(SH.gamma,add=T,nsd=0,sdev=sdev,histo=F,super=super,line.col=line.col[1])
    plot.super.hist(SH.delta,add=T,nsd=0,sdev=sdev,histo=F,super=super,line.col=line.col[2])
  }
}

##############################################
############ To generate tables ##############
##############################################

gamma.stats <-
  function (x,n=dim(x)[1],
            bar.omb=sum(omb)/n,
            bar.oma=sum(oma)/n,
            bar.amb=sum(amb)/n,
            Mb=bar.amb*bar.omb,
            Mo=bar.oma*bar.omb,
            Ma=bar.amb*bar.oma,
            Bb=((a-b)-bar.amb) * ((o-b)-bar.omb),
            Bo=((o-a)-bar.oma) * ((o-b)-bar.omb),
            Ba=((a-b)-bar.amb) * ((o-a)-bar.oma) )
{
  for (i in dimnames(x)[[2]]) assign(i,x[,i])
  xplus<-cbind(x,Bb=Bb,Bo=Bo,Ba=Ba,Mb=Mb,Mo=Mo,Ma=Ma)
  sx<-apply(xplus,2,sum)
  sxx<-apply(xplus^2,2,sum)
  mean<-sx/n
  var<-pmax(sxx/n-mean^2,0)
# For sqrt of bar(Cx) use sqrtMean<-sqrt(pmax(mean,0))))
  list(n=n,mean=mean,stdev=sqrt(var))
}

gamma.stats.print <-
  function(gs, name=lat.nameNS, digits=c(2,1,3), CxSQRT=T)
{  
  cat('Sample size is',gs$n,'for',name,'::\n')
  x<-rbind(mean=gs$mean,stDev=gs$stdev)
  if (CxSQRT) x<-rbind(x,sqrtMean=sqrt(pmax(gs$mean,0)))
  print(gamma.stats.rounder(x,digits))
}

gamma.stats.rounder <-
  function(x, digits=c(2,1,3))
{
  y<-round(x,digits=digits[1])
  boa<-match(c('b','o','a'),dimnames(y)[[2]])
  boa<-boa[!is.na(boa)]
  if (length(boa)>0) y[,boa]<-round(x[,boa],digits=digits[2])
  Xa<-match(c('Ca','Ba','Ma'),dimnames(y)[[2]])
  Xa<-Xa[!is.na(Xa)]
  if (length(Xa)>0) y[,Xa]<-round(x[,Xa],digits=digits[3])
  y
}

##############################################
# Calculate the mean and variance of a series
##############################################
delta.stats <-
  function (y, q=1, n=length(y), trim=0)
{
    y<-sort(y)
    mean<-mean(y,trim=trim)
    imin<-round(n*pnorm(-q)) ; imax<-round(n*pnorm(q))
    stdev<-(y[imax]-y[imin])/(2*q)
    unlist(list(n=n,mean=mean,stdev=stdev,sqrtMean=sqrt(max(mean,0))))
}

##############################################
