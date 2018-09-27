# Source function written by Ross -- not all need
source('~/NWP/vam/R-desroziers/plot.gamma.q')
source('~/NWP/vam/R-desroziers/super.hist.q')
source('~/NWP/vam/R-desroziers/plot.Cx.q')
library(plotrix)
library(chron)

# set parameters to be used later
platforms<-list(amsre="AMSRE",f13="F13",f14="F14",f15="F15",qscat="QSCAT",tmi='TMI')
vam.vals <- list(a='a',b='b',o='o')
# QC parameters
VC.sd<-list(b=2.0,o=0.9,a=0.66)
VC.means<-list(b=1.3,o=0.40,a=0.075)
nsdVC<-6

# load the data from text files
lat.name<-'42.375'
lat.nameNS<-paste(abs(as.real(lat.name)),ifelse(as.real(lat.name)<0,'S','N'))
all.data <- readInputDataByLat(lat.name,dir='/Users/bwoods/NWP/vam/CCMP/gammaInputDataByLat/')
# data is in a matrix [row,variable]

# dinames(all.data)[[1]] would return the names of the rows of the matrix
dimnames(all.data)[[2]]
# [1] "platform" "datetime" "time"     "dt"       "lat"      "lon"      "a"        "b"        "o"        "gamma"   
#[11] "nobs"     "dtbar"    "gammabar" "avc"     

# create variables in the workspae with the name listed above 
for (i in dimnames(all.data)[[2]]) assign(i,all.data[,i])

#summary(lon)
  ##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  ## 29.88  179.10  216.90  237.30  313.10  349.60 

# *******************************************************************
# Apply quality control
# *******************************************************************
Ca<-(a-b)*(o-a)
Cb<-(a-b)*(o-b)
Co<-(o-a)*(o-b)
# ncdVC = 6    is a 6-sigma quality control check
# VC.sd$a      standard deviation of a for QC us
# now check for QC on each of a, b, o
select.a<-abs(Ca-VC.means$a)<nsdVC*VC.sd$a
select.b<-abs(Cb-VC.means$b)<nsdVC*VC.sd$b
select.o<-abs(Co-VC.means$o)<nsdVC*VC.sd$o
# get indices where a, b, and o all fall within the 6 sigma QC
select<-select.a & select.b & select.o
QC.data <- all.data[select,]
# update the name space to include just the QC'ed data
rm(all.data)
for (i in dimnames(QC.data)[[2]]) assign(i,QC.data[,i])
#summary(lon)
  ##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  ## 29.88  179.90  217.10  237.60  313.40  349.60 



# get unique longitudes (we only have loaded one latitude)
unq.lon <- unique(lon)
## # create a blank matrix for the variances at each grid cell for each instrument
## variances <- array(0,dim=c(3,length(platforms),length(unq.lon)))
## dimnames(variances) <- list(vam.vals,platforms,as.character(unq.lon))
## averages <- variances # copy the empty variances to create averages
## # fill the variance matrix
## for (x in as.character(unq.lon)){
##   for(p in platforms){
##     if(p == "TMI") next
##     print(c(x,p))
##     slice <- QC.data[lon == x & unlist(platforms[platform]) == p,unlist(vam.vals)]
##     variances[,p,x] <- apply(slice,2,var)
##     averages[,p,x] <- apply(slice,2,mean)
##   }
## }
## save(variances, averages, file = "variances.RData")
## # load the averages and variances object
load(file = "variances.RData")

# create super hist plots for each instrument vs latitude
pdf(paste('plots/','platforms_by_longitude.pdf',sep=''))
trim <- 0
ylabel <- expression(paste("wind speed (m s"^{-1},")"))
# first process all of the instruments together
means<-c(B=mean(b,trim=trim),O=mean(o,trim=trim),A=mean(a,trim=trim))
plot.VC.super.hist(lon, b, o, a, seq(25,350,5),nsd=1/3,sdev=F,ylab=ylabel,main="all platforms",means=means,yrange=c(0,12),xlab="Longitude",line.col=c('aquamarine','red','blue'))
for(p in 1:(length(platforms))){ # now process each individual platform
  inds <- (platform == p) # find all the points corresponding to that instrument
  if(!any(inds)) next # skip to next instrument if there are no measurements
  means<-c(B=mean(b[inds],trim=trim),O=mean(o[inds],trim=trim),A=mean(a[inds],trim=trim))
plot.VC.super.hist(lon[inds], b[inds], o[inds], a[inds], seq(25,350,5),nsd=1/3,sdev=F,ylab=ylabel,main=paste(platforms[p],' observations'),means=means,yrange=c(0,12),xlab="Longitude",line.col=c('aquamarine','red','blue'))
}
dev.off()

for(v in vam.vals){
  for(p in platforms){
    if(p == "TMI") next
    print(paste(v,p))
    pdf(paste('plots/',p,'_',v,'_lonavg.pdf',sep=''))
    v1d = averages[v,p,]
    v1d[v1d==0] <- NA
    plot(unq.lon,v1d,xlab='longitude',ylab=paste(p,'average',v))
    dev.off()
    pdf(paste('plots/',p,'_',v,'_lonvar.pdf',sep=''))
    v1d = variances[v,p,]
    v1d[v1d==0] <- NA
    plot(unq.lon,v1d,xlab='longitude',ylab=paste(p,'varaince',v))
    dev.off()
  }
}

# already have unique longitudes: unq.lon
unq.time <- unique(time)
# loop through each unique time and location to check for the same a-b
# result: a-b does not agree for each of the nobs
pdf(paste('plots/dt_lines.pdf',sep=''))
for(ti in unq.time){
  print(ti)
    for(x in unq.lon){
    i = which(time == ti & lon == x)
    if(length(i) > 3) {
      p = platform[i]
      # get subset of dt, o, b, i at valid time and location
      dti <- dt[i] ; oi <- o[i] ; ai <- a[i] ; bi <- b[i]
      yrange <- range(c(o,b,a))
      ylabel <- expression(paste("wind speed (m s"^{-1},")"))
      xlabel <- 'difference from synoptic time (min)'
      mainstr <- paste(paste(month.day.year(ti,origin=c(1,1,2004)),collapse='-'), times(ti-floor(ti)), 'lat', lat[i][1], 'lon', x)
      # add scatter points with shape by platform and color by (b,o,a)
      plot(dti,bi,pch=p,xlab=xlabel,ylab=ylabel,col='aquamarine',main=mainstr,xlim=c(-180,180),ylim=yrange)
      points(dti,oi,pch=p,col='red')
      points(dti,ai,pch=p,col='blue')
      abline(v=0, lty=2) # dashed vertical line at dt = 0
      # add linear regression for (b,o,a) to left and right sides
      oir <- oi[dti>0] ; oil <- oi[dti<0]
      air <- ai[dti>0] ; ail <- ai[dti<0]
      bir <- bi[dti>0] ; bil <- bi[dti<0]
      dtr <- dti[dti>0] ; dtl <- dti[dti<0]
      a.minus.b <- mean(ai-bi) # a-b offset constant for dt -180 to 180 min
      ltyreg <- 2 ; ltyoff <- 1
      if(length(unique(dtr)) > 1) {
        lmc <- lm(oir~dtr)$coef
        ablineclip(reg=lmc,col='red',lty=ltyreg,x1=0,x2=180)
        lmc <- lm(air~dtr)$coef
        ablineclip(reg=lmc,col='blue',lty=ltyreg,x1=0,x2=180)
        lmc <- lm(bir~dtr)$coef
        ablineclip(reg=lmc,col='aquamarine',lty=ltyreg,x1=0,x2=180)
        lmc[1] <- lmc[1]+a.minus.b # add a-b offset to b regression
        ablineclip(reg=lmc,col='aquamarine',lty=ltyoff,x1=0,x2=180)
      }
      else if (length(unique(dtr)) == 1) {
        ablineclip(h=oir,col='red',lty=ltyreg,x1=0,x2=180)
        ablineclip(h=air,col='blue',lty=ltyreg,x1=0,x2=180)
        ablineclip(h=bir,col='aquamarine',lty=ltyreg,x1=0,x2=180)
        ablineclip(h=bir+a.minus.b,col='aquamarine',lty=ltyoff,x1=0,x2=180)
      }
      if(length(unique(dtl)) > 1) {
        lmc <- lm(oil~dtl)$coef
        ablineclip(reg=lmc,col='red',lty=ltyreg,x1=-180,x2=0)
        lmc <- lm(ail~dtl)$coef
        ablineclip(reg=lmc,col='blue',lty=ltyreg,x1=-180,x2=0)
        lmc <- lm(bil~dtl)$coef
        ablineclip(reg=lmc,col='aquamarine',lty=ltyreg,x1=-180,x2=0)
        lmc[1] <- lmc[1]+a.minus.b # add a-b offset to b regression
        ablineclip(reg=lmc,col='aquamarine',lty=ltyoff,x1=-180,x2=0)
      }
      else if (length(unique(dtl)) == 1) {
        ablineclip(h=oil,col='red',lty=ltyreg,x1=-180,x2=0)
        ablineclip(h=ail,col='blue',lty=ltyreg,x1=-180,x2=0)
        ablineclip(h=bil,col='aquamarine',lty=ltyreg,x1=-180,x2=0)
        ablineclip(h=bil+a.minus.b,col='aquamarine',lty=ltyoff,x1=-180,x2=0)
      }
      # add legends
      legend('topright',leg=paste(platforms[unique(p)]),pch=unique(p),title='Platforms',cex=1)
      legend('topleft',leg=c('b','o','a'),pch=1,col=c('aquamarine','red','blue'),title='Fields',cex=1)
    }
  }
}
dev.off()

