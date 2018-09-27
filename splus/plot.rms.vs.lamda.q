
plot.rms.vs.lamda <- function(windsdir,sigma0dir)

{
# dir is the path to vam output files for a given time and various
# lamda weights

# Set lamda weights
	lamda.amb<-c(0.0009765625,0.00390625,0.015625,0.0625,0.25,1,4,16,64,256,1024)
	lamda.sig<-c(0.0009765625,0.00390625,0.015625,0.0625,0.25,1,4,16,64,256,1024)
#	lamda.amb<-c(0.0009765625,0.00390625,0.015625,0.0625,0.25,1,4,16)
#	lamda.sig<-c(0.0009765625,0.00390625,0.015625,0.0625,0.25,1,4,16)

        rev <- 1025
#        rev <- 907

# Make array for storing rms values
	arr<-rep(NA, 2 * ( length(lamda.amb) + 1 ) )
	dim(arr)<-c(length(lamda.amb) + 1, 2)
	dimnames(arr)<-list(c(),c("wind","sigma0"))
	print(arr)

# Read in JPL winds
	wov <- scan(paste(windsdir,"/lamda",lamda.amb[1],"/r",rev,"/MGDRwinds-JPLranked.txt",sep=""))
#	wov <- scan(paste(windsdir,"/lamda",lamda.amb[1],"/r",rev,"/L20winds-JPLranked.txt",sep=""))
	jpl<-make.jpl.winds(wov)

# Read in background winds interpolated to nscat WVC's
	wov <- scan(paste(windsdir,"/lamda",lamda.amb[1],"/r",rev,"/background@nscat.winds.txt", sep="")) 
	backnscat <- make.nscat.winds(wov)
#	arr[1,1]<-calc.vam.wspd.err(backnscat, jpl) # use MLE
#	arr[1,1]<-calc.vam.wspd.err(backnscat, jpl, use.closest = T) # use closest
	arr[1,1]<-calc.vam.wspd.err(backnscat, jpl, use.selected = T) # use delected
	arr[1,2]<-arr[1,1]
	print(arr)

# Read in vam winds interpolated to nscat WVC's from analyses using ambiguous winds
        for (i in 1:length(lamda.amb)) {
	  wov <- scan(paste(windsdir,"/lamda",lamda.amb[i],"/r",rev,"/analysis@nscat.winds.txt", sep=""))
	  assign(paste("analnscat.",lamda.amb[i],sep=""), make.nscat.winds(wov))
          analnscat<- make.nscat.winds(wov)
          print(paste("Computing rms for winds analysis with lamda=",lamda.amb[i]))
#          arr[i+1,1]<-calc.vam.wspd.err(analnscat, jpl)
#          arr[i+1,1]<-calc.vam.wspd.err(analnscat, jpl, use.closest = T)
          arr[i+1,1]<-calc.vam.wspd.err(analnscat, jpl, use.selected = T)

	}

# Read in vam winds interpolated to nscat WVC's from analyses using sig0
        for (i in 1:length(lamda.sig)) {
	  wov <- scan(paste(sigma0dir,"/lamda",lamda.sig[i],"/r",rev,"/analysis@nscat.winds.txt", sep="")) 
	  assign(paste("analnscat.sig.",lamda.sig[i],sep=""), make.nscat.winds(wov))
          analnscat<- make.nscat.winds(wov)
#          arr[i+1,2]<-calc.vam.wspd.err(analnscat, jpl)
#          arr[i+1,2]<-calc.vam.wspd.err(analnscat, jpl, use.closest = T)
          arr[i+1,2]<-calc.vam.wspd.err(analnscat, jpl, use.selected = T)
	}

	print(arr)

# Plot results
	plot(log10(lamda.amb),arr[(1:length(lamda.amb))+1,"wind"],xlim=c(-3.5,3.5),ylim=c(0,4),xlab="",ylab="",pch="w")
	lines(log10(lamda.amb),arr[(1:length(lamda.amb))+1,"wind"])
	points(log10(lamda.sig),arr[(1:length(lamda.amb))+1,"sigma0"],pch="s")
	lines(log10(lamda.sig),arr[(1:length(lamda.amb)+1),"sigma0"])

# Annotate
	mtext("rms(Vvam - Vselected) [m/s]", line = 3, side = 2)
	mtext("log10(lambda.wind)", line = 2.5, side = 1)
#	mtext("log10(1024*lambda.sigma0)", line = 3.5, side = 1)

	title("fit to 25km ambiguous winds and sig0 vs lambda weight; 961028")
	timestamp(line=4)

 }
