
##############################################
######## Read data at Good/Bad Location ######
##############################################

readGoodBad <-
  function (name='Good', year = 2004,
            platforms=list(amsr="AMSRE",f13="F13",f14="F14",f15="F15",qscat="QSCAT"),
            dir='/Users/rhoffman/Desktop/P1326-NASA-OceanWinds/ErrorAnalysis/GoodBadData/',
            columns=c('date','time','dt','a','b','o'),
            daysInMonth=c(31,29,31,30,31,30,31,31,30,31,30,31),
            daysTilMonth=c(0,cumsum(daysInMonth)[-12]))
  { # tmi="TMI" left off for now.
    # Leap year for 2004 for now.
    for (i in 1:length(platforms)) {
      dataname<-paste('wspd',platforms[[i]],year,name,sep='')
      filename<-paste(dir,dataname,'.txt',sep='')
      xx<-matrix(scan(filename,skip=4),ncol=6,byrow=T,dimnames=list(NULL,columns))
      datetime<-xx[,'date']*100+xx[,'time']/10000
      month<-(datetime%%1000000)%/%10000
      day<-(datetime%%10000)%/%100
      hour<-datetime%%100
      time<-daysTilMonth[month]+day+hour/24-1
      gamma<-(xx[,'o']-xx[,'a'])/(xx[,'b']-xx[,'a'])
      # Eliminate infinities
      gamma[gamma>777]<-777; gamma[gamma<(-777)]<-(-777)
      yy<-cbind(platform=i,datetime=datetime,time=time,xx[,3:dim(xx)[2]],gamma=gamma)
      if (i==1) zz<-yy else zz<-rbind(zz,yy) }
    # order by time, then platform
    zz<-zz[order(zz[,'time'],zz[,'platform']),]
    # identify groups for same time
    dtime<-c(1,diff(zz[,'time']))
    # these parameters are by group, below use rep(...,numIndex) to expand
    startIndex<-c((1:length(dtime))[dtime>0],1+length(dtime))
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
####### Selection at Good/Bad Location #######
##############################################

gamma.select <-
  function (goodBad, main='', platformSelect=0, nobsSelect=0, maxGamma=0,
            gammaCriticalRange=c(-1/2,1/4), wind='', windRange=c(5,10),
            platforms)
{
 # First unpack goodBad to get name, year, lat, lon, data
  for (i in names(goodBad)) assign(i,goodBad[[i]])
  nameyear<-paste(name,year)
  latlon<-paste('(',latitude,'N ',longitude,'E)',sep='')
  # Then unpack data matrix by columns
  for (i in dimnames(data)[[2]]) assign(i,data[,i])
  # Determine subset
  select<-time>0
  if (platformSelect>0) {
    select<-select & platform==platformSelect;
    main<-paste(main,platforms[[platformSelect]],' only',sep='')}
  else main<-paste(main,'All platforms',sep='')
  if (nobsSelect>0) {
    select<-select & nobs==nobsSelect
    main<-paste(main,', Nobs=',nobsSelect,sep='')}
  if (maxGamma>0) {
    select<-select & abs(gamma)<maxGamma
    main<-paste(main,', |gamma|<',maxGamma,sep='')}
  if (maxGamma<0) { # select on gammaCriticalRange
    select<-select & gammaCriticalRange[1]<gamma & gamma<gammaCriticalRange[2]
    main<-paste(main,', ',gammaCriticalRange[1], '<gamma<',
                gammaCriticalRange[2],sep='')}
  if (wind=='o') { # select on obs wind range
    select<-select & windRange[1]<o & o<windRange[2]
    main<-paste(main,', ',windRange[1], '<O<',windRange[2],sep='')}
  if (wind=='b') { # select on background wind range
    select<-select & windRange[1]<b & b<windRange[2]
    main<-paste(main,', ',windRange[1], '<B<',windRange[2],sep='')}
  if (wind=='amb') { # select on |a-b| wind range
    select<-select & windRange[1]<abs(a-b) & abs(a-b)<windRange[2]
    main<-paste(main,', ',windRange[1], '<|A-B|<',windRange[2],sep='')}
  list(select=select, main=main, nameyear=nameyear, latlon=latlon)
}

##############################################
############# Plot gamma #####################
##############################################

# The 3-panel plots of gamma contain the following:

# Panel 1: Gamma versus time (days, relative to New Year)
# Title gives the overall selection criteria.
# Top left of this panel gives the percent of all obs that are selected.

# Panel 2: Histogram of gamma
# Title gives location name, year, and size of selected sample
# Legends in this panel give the color and symbol usage in the other panels

# Panel 3: Gamma versus time (minutes, relative to the synoptic time)
# Title gives lat, lon location

# All panels have red lines bounding the gamma critical range (nominally [-1/2, 1/4] where we expect most of the gamma values to lie.
# Top right of panel 1 gives the percentage of the selected sample in the critical range.

# Top and bottom panels have a "super-histogram" plotted under the data.
# Bottom left of panel 1 gives the trim values used by super.hist.
# Bins in panel 1 are months of the year.
# Bins in panel 3 are 15 minutes.

# =======================================

# Super-histogram
# Connects the bin means with a thick black line.
# Puts grey shading within one bin standard deviations of the bin means.
# The sample histogram is given by the small lollipops.
# These are plotted at the bottom of the plot with a scale on the right.
# All bin statistics are plotted at the mean x value for the bin sample.
# The bin means and standard deviations are linearly extrapolated
# to bin edges from adjacent bins that have samples.
# Otherwise these are constant extrapolated to bin edges.

# =======================================

# The 3-panel plots of VC(A) are the same as the 3-panel plots of gamma,
# except that:
# In panel 1 the right hand annotations indicate the percent of
# the selected sample that fall above and below the plot limits.
# A red line is plotted at the zero value of VC(A) instead of plotting
# red lines for the critical gamma values.

##############################################

plotGoodBadGamma <-
  function (goodBad,gammaRange=c(-5,5),
            gammaCriticalRange=c(-1/2,1/4), trim=0.025,
            platforms=list(amsr="AMSRE",f13="F13",f14="F14",f15="F15",qscat="QSCAT"),
            daysInMonth=c(31,29,31,30,31,30,31,31,30,31,30,31),
            ...)
{
  # First unpack goodBad to get name, year, lat, lon, data
  # Then unpack data matrix by columns
  # Determine subset
  subset<-gamma.select(goodBad,platforms=platforms,gammaCriticalRange=gammaCriticalRange,...)
  # Then unpack subset to get select and plot titles (main, nameyear, latlon)
  for (i in names(subset)) assign(i,subset[[i]])
  # Now unpack selected data matrix by columns
  for (i in dimnames(goodBad$data)[[2]]) assign(i,goodBad$data[select,i])
  # Reset out of bounds gamma
  gamma.truncated<-pmin(pmax(gammaRange[1],gamma),gammaRange[2])
  # Ready to plot or process
  par(pty='m')
  # Plot time series
  plot(range(time),gammaRange,xlab='Time (days since New Year)',
       ylab='(O-A)/(B-A)',type='n',main=main)
  plot.super.hist(super.hist(time,gamma,breaks=c(0,cumsum(daysInMonth)),trim=trim),add=T)
  points(time,gamma.truncated,col=platform,pch=nobs)
  abline(h=gammaCriticalRange,col=2)
  legend('topleft',leg=paste(round(100*sum(select)/length(select)),'%',sep=''),bty='n',cex=2)
  select<-gammaCriticalRange[1]<gamma & gamma<gammaCriticalRange[2]
  legend('topright',leg=paste(round(100*sum(select)/length(select)),'%',sep=''),bty='n',cex=2)
  legend('bottomleft',leg=paste('Trim=',round(100*trim,digits=1),'%',sep=''),bty='n',cex=2)
  # Plot histogram
  hist(gamma.truncated,breaks=seq(gammaRange[1],gammaRange[2],1/4),xlab='(O-A)/(B-A)',main=paste(nameyear, ', N=',length(gamma),sep=''))
  abline(v=gammaCriticalRange,col=2)
  add.legend(platforms,c('topleft','topright'))
  # Plot gamma relative to dt
  plot(c(-180,180),gammaRange, xlab='Time (minutes relative to synoptic time)',
       ylab='(O-A)/(B-A)',main=latlon,type='n')
  plot.super.hist(super.hist(dt,gamma,breaks=seq(-180,180,by=15),trim=trim),add=T)
  points(dt,gamma.truncated,col=platform,pch=nobs)
  abline(h=gammaCriticalRange,col=2)
}

plotGoodBadVC <-
  function (goodBad,VCRange=c(-4,4),
            gammaCriticalRange=c(-1/2,1/4), trim=0.025,
            VClab='aVC=(A-B)(O-A)',
            platforms=list(amsr="AMSRE",f13="F13",f14="F14",f15="F15",qscat="QSCAT"),
            daysInMonth=c(31,29,31,30,31,30,31,31,30,31,30,31),
            ...)
{
  # First unpack goodBad to get name, year, lat, lon, data
  # Then unpack data matrix by columns
  # Determine subset
  subset<-gamma.select(goodBad,platforms=platforms,gammaCriticalRange=gammaCriticalRange,...)
  # Then unpack subset to get select and plot titles (main, nameyear, latlon)
  for (i in names(subset)) assign(i,subset[[i]])
  # Now unpack selected data matrix by columns
  for (i in dimnames(goodBad$data)[[2]]) assign(i,goodBad$data[select,i])
  # Recalculate avc for individual obs, not individual synoptic times
  VC<-(a-b)*(o-a)
  # Reset out of bounds avc and store in VC
  VC.truncated<-pmin(pmax(VCRange[1],VC),VCRange[2])
  # Ready to plot or process
  par(pty='m')
  # Plot time series
  plot(range(time),VCRange,xlab='Time (days since New Year)',
       ylab=VClab,type='n',main=main)
  plot.super.hist(super.hist(time,VC,breaks=c(0,cumsum(daysInMonth)),trim=trim),add=T)
  points(time,VC.truncated,col=platform,pch=nobs)
  abline(h=0,col=2)
  legend('topleft',leg=paste(round(100*sum(select)/length(select)),'%',sep=''),bty='n',cex=2)
  legend('bottomleft',leg=paste('Trim=',round(100*trim,digits=1),'%',sep=''),bty='n',cex=2)
  select<-VC.truncated<VC
  legend('topright',leg=paste(round(100*sum(select)/length(select)),'%',sep=''),bty='n',cex=2)
  select<-VC.truncated>VC
  legend('bottomright',leg=paste(round(100*sum(select)/length(select)),'%',sep=''),bty='n',cex=2)
  # Plot histogram
  hist(VC.truncated,breaks=seq(VCRange[1],VCRange[2],1/4),xlab=VClab,main=paste(nameyear, ', N=',length(VC),sep=''))
  abline(v=0,col=2)
  add.legend(platforms,c('topleft','topright'))
  # Plot relative to synoptic time
  plot(c(-180,180),VCRange, xlab='Time (minutes relative to synoptic time)',
       ylab=VClab,main=latlon,type='n')
  plot.super.hist(super.hist(dt,VC,breaks=seq(-180,180,by=15),trim=trim),add=T)
  points(dt,VC.truncated,col=platform,pch=nobs)
  abline(h=0,col=2)
}

##############################################

plotGoodBadPairs <-
  function (goodBad,gammaRange=c(-5,5),range=5,
            platforms=list(amsr="AMSRE",f13="F13",f14="F14",
              f15="F15",qscat="QSCAT"),...)
{
  # First unpack goodBad to get name, year, lat, lon, data
  # Then unpack data matrix by columns
  # Determine subset
  subset<-gamma.select(goodBad,platforms=platforms,...)
  # Then unpack data matrix by columns
  for (i in names(subset)) assign(i,subset[[i]])
  # Now unpack selected data matrix by columns
  for (i in dimnames(goodBad$data)[[2]]) assign(i,goodBad$data[select,i])
  # Reset out of bounds gamma
  gamma<-pmin(pmax(gammaRange[1],gamma),gammaRange[2])
  # Ready to plot or process
  par(pty='s')
  plot.pairs(a-b,o-b,'a-b','o-b','Background',main,range,platform,nobs)
  add.legend(platforms,c('topleft','nextto'))
  plot.pairs(o-b,o-a,'o-b','o-a','Observation',nameyear,range,platform,nobs)
  add.legend(platforms,c('topleft','nextto'))
  plot.pairs(a-b,o-a,'a-b','o-a','Analysis',latlon,range,platform,nobs)
  add.legend(platforms,c('topleft','nextto'))
  }

plotGoodBadCase <-
  function (goodBad,casedatetime,caseTitle='',
            gammaRange=c(-5,5),
            gammaCriticalRange=c(-1/2,1/4), dtRange=c(-180,180),
            platforms=list(amsr="AMSRE",f13="F13",f14="F14",f15="F15",qscat="QSCAT"),
            platform.colors=c(3,4,4,4,2),         
            names.colors=c(SSMI=4,QSCAT=2,AMSRE=3),
            xsymbols=c(A=2,B=12,O=19), ...)
{
  # First unpack goodBad to get name, year, lat, lon, data
  # Then unpack data matrix by columns
  # Determine subset
  subset<-gamma.select(goodBad,platforms=platforms,gammaCriticalRange=gammaCriticalRange,...)
  # Then unpack subset to get select and plot titles (main, nameyear, latlon)
  for (i in names(subset)) assign(i,subset[[i]])
  # Overwrite select for this casedatetime
  select<-casedatetime==goodBad$data[,'datetime']
  # Now unpack selected data matrix by columns
  for (i in dimnames(goodBad$data)[[2]]) assign(i,goodBad$data[select,i])
  # Reset out of bounds gamma
  g<-pmin(pmax(gammaRange[1],gamma),gammaRange[2])
  # Ready to plot or process
  par(pty='m')
  # Plot gamma relative to dt
  plot(dtRange,1.53*gammaRange,
       xlab=paste('Time (minutes relative to ',casedatetime,')',sep=''),
       ylab='g=(x-A)/(B-A)',type='n')
  # Generate title
  caseTitle<-''
  caseTitle<-paste(caseTitle,'<g>=',signif(gammabar[1],digits=2),sep='')
  caseTitle<-paste(caseTitle,', <dt>=',round(dtbar[1]),sep='')
  caseTitle<-paste(caseTitle,', S(VC(A))=',signif(sum((a-b)*(o-a)),digits=2),sep='')
  title(main=caseTitle)
  # Ornamentation
  xp<-par('usr') ; yp<-gammaCriticalRange
  polygon(c(xp[1],xp[2],xp[2],xp[1],xp[1]),c(yp[1],yp[1],yp[2],yp[2],yp[1]),border=NA,col=8)
  abline(h=c(0,gammaRange),v=0,col=1) ; abline(h=1, col=5)
  abline(v=dt,col=platform.colors[platform])
#  i<-add.cases.legend(names.colors,xsymbols,c('topleft','nextto'))
  add.table.legend(a,b,o,dt,platform.colors[platform],xsymbols,platforms[platform])
  # Plot points
  points(dt,g,,col=platform.colors[platform],pch=xsymbols['O'],cex=2)
  points(dt,rep(0,length(dt)),col=platform.colors[platform],pch=xsymbols['A'],cex=2)
  points(dt,rep(1,length(dt)),col=platform.colors[platform],pch=xsymbols['B'],cex=2)
#  position<-ifelse(dt>0,2,4)
#  text(dt,g,paste('O',signif(o,digits=2),sep='='),pos=position,col=platform.colors[platform])
#  text(dt,rep(0,length(dt)),paste('A',signif(a,digits=2),sep='='),pos=position,col=platform.colors[platform])
#  text(dt,rep(1,length(dt)),paste('B',signif(b,digits=2),sep='='),pos=position,col=platform.colors[platform])
  position<-ifelse(g>0,1,3)
  select<-abs(gamma)>5
  if (sum(select)>0) text(dt[select],g[select],paste('g',signif(gamma[select],digits=2),sep='='),pos=position[select],col=platform.colors[platform[select]],xpd=T)
  if (length(dt)>1) {
      g<-pmin(pmax(gammaRange[1],gammabar),gammaRange[2])
      points(dtbar[1:2],g[1:2],pch=c(1,3),col=6,cex=3,xpd=T)
      position<-ifelse(g>0,1,3)
      if (abs(gammabar[1])>5)
        text(dtbar[1],g[1],
             paste('g',signif(gammabar[1],digits=2),sep='='),
             pos=position[1],col=6,xpd=T) }
}

##############################################

plot.pairs <-
  function (x, y, xlab, ylab, name, subtitle='Test',range=5,platform,
            nobs,digits=2)
  { variance<-mean(x*y)
    BCvariance<-variance - mean(x)*mean(y) # bias corrected
    yonx<-variance/mean(x*x)
    xony<-mean(y*y)/variance
    plot(c(-range,range),c(-range,range),xlab=xlab,ylab=ylab,main=paste(name,'variance estimate'),type='n')
    x[-range>x]<--range;x[x>range]<-range
    y[-range>y]<--range;y[y>range]<-range
    points(x, y, col=platform, pch=nobs)
    title(subtitle,line=0.5)
    abline(0,variance,col=2)
    abline(0,BCvariance,col=4)
    abline(0,yonx,col=3)
    abline(0,xony,col=3)
    abline(h=0,v=0)
    stdDev<-round(sign(variance)*sqrt(abs(variance)),digits=digits)
    SDBC<-round(sign(BCvariance)*sqrt(abs(BCvariance)),digits=digits)
    leg<-c(paste('StdDev',stdDev,sep='='),paste('SD-BC',SDBC,sep='='),
           'Fits thru (0,0)',paste('N',length(x),sep='='))
    legend('bottomright',leg,lty=1,col=c(2,4,3,NA))
  }

##############################################

add.legend <-
  function (platforms, location=c('topleft','topright'))
  {
    i<-legend(location[1],leg=unlist(platforms),col=1:5,title='Platforms',pch=19)
    if (location[2] != 'nextto') {
      legend(location[2],leg=1:5,pch=1:5,title='Nobs',col=1)}
    if (location[2] == 'nextto') {
      #if (location[1] == 'topleft') {
        x<-i$rect$left+i$rect$w; y<-i$rect$top# } 
      legend(x,y,leg=1:5,pch=1:5,title='Nobs',col=1)}}

add.cases.legend <-
  function (names.colors, xsymbols, location=c('topleft','topright'))
  {
    i<-legend(location[1],leg=names(names.colors),
              fill=names.colors,
              title='Platforms',bg='white')
    if (location[2] != 'nextto') {
      x<-location[2] ; y<-NULL }
    else {
      x<-i$rect$left+i$rect$w; y<-i$rect$top}
    legend(x,y,leg=names(xsymbols),pch=xsymbols,title='x',col=1,bg='white')}

add.table.legend <-
  function (a,b,o,dt,colors,xsymbols,platforms)
  {
    par(cex=0.67)
    torder<-order(dt)
    a<-a[torder]; b<-b[torder]; o<-o[torder]
    dt<-dt[torder]; colors<-colors[torder]; platforms<-platforms[torder]
    i<-legend('topleft',leg=dt,title='Time',fill=colors,bg='white')
    x<-i$rect$left+i$rect$w; y<-i$rect$top
    i<-legend(x,y,leg=round(a,digits=2),pch=xsymbols['A'],title='A',col=colors,bg='white')
    x<-i$rect$left+i$rect$w; y<-i$rect$top
    i<-legend(x,y,leg=round(b,digits=2),pch=xsymbols['B'],title='B',col=colors,bg='white')
    x<-i$rect$left+i$rect$w; y<-i$rect$top
    i<-legend(x,y,leg=round(o,digits=2),pch=xsymbols['O'],title='O',col=colors,bg='white')
    x<-i$rect$left+i$rect$w; y<-i$rect$top
    i<-legend(x,y,,leg=round((o-a)/(b-a),digits=2),pch=xsymbols['O'],title='g',col=colors,bg='white')
    i<-legend('bottomleft',leg=platforms,title='Platform',fill=colors,bg='white')
    x<-i$rect$left+i$rect$w; y<-i$rect$top
    i<-legend(x,y,leg=round((a-b)*(o-a),digits=2),pch=xsymbols['A'],title='VC(A)',col=colors,bg='white')
    x<-i$rect$left+i$rect$w; y<-i$rect$top
    i<-legend(x,y,leg=round((a-b)*(o-b),digits=2),pch=xsymbols['B'],title='VC(B)',col=colors,bg='white')
    x<-i$rect$left+i$rect$w; y<-i$rect$top
    i<-legend(x,y,leg=round((o-a)*(o-b),digits=2),pch=xsymbols['O'],title='VC(O)',col=colors,bg='white')
    par(cex=1.0)
  }

##############################################

calc.errors <-
  function(xx,gamma1=-0.25,delta=0.25)
  { a<-xx[,'A']
    b<-xx[,'B']
    o<-xx[,'O']
    gamma<-(o-a)/(b-a)
    select<-(gamma>gamma1-delta) & (gamma<delta)
    if (delta==F) select<-rep(T,length(gamma))
    n<-sum(select)
    a<-xx[select,'A']
    b<-xx[select,'B']
    o<-xx[select,'O']
    data=as.character(sys.call())[2] # get the actual name for xx
    rbind(unlist(calc.pairs(a-b,o-b,'Background',n,data,gamma1,delta)),
    unlist(calc.pairs(o-a,o-b,'Observation',n,data,gamma1,delta)),
    unlist(calc.pairs(a-b,o-a,'Analysis',n,data,gamma1,delta)))
  }

calc.pairs <-
  function (x, y, name, n=length(x), data='good', gamma1=-.25, delta=0.25, digits=3)
  { variance<-mean(x*y)
    biasCorrectedVariance<-variance - mean(x)*mean(y)
    stdDev<-round(sign(variance)*sqrt(abs(variance)),digits=digits)
    biasCorrectedStdDev<-round(sign(biasCorrectedVariance)*sqrt(abs(biasCorrectedVariance)),digits=digits)
    list(data=data,name=name,n=n,stdDev=stdDev,biasCorrected=biasCorrectedStdDev,delta=delta,gamma1=gamma1)
  }

##############################################

normalize <- function (x)
  { xbar<-mean(x)
    sdevx<-sqrt(mean(x^2)-xbar^2)
    (x-xbar)/sdevx
  }

##############################################
