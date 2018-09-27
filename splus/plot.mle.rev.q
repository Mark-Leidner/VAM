plot.mle.rev <- function (jpl, s0, skip=50)

{

# Postscript options
	make.ps<-T
	if (make.ps) {
	ps.options (colors=HeatB.lines.ps)
	ps.options (image.colors=visible.light.image.colors)
	path<-"/carmine/nscat/functional.plots/rev1096/"
	file<-paste(path,"sssass.ps",sep="")
	postscript(file=file,print.it=F) }

# Initialize/set local variables
	mrow <- 820
	mcol <- 24
	nwvc <- 0

# Loop over all possible rows in rev
	nrow <- 0
	while (nrow <= mrow ) {
 	  nrow <- nrow + 1

# Loop over all possible columns
	  ncol <- 0
	  while (ncol <= mcol) {
	    ncol <- ncol + 1

# Check to see if this cell exists
	    data <-jpl$lat[jpl$row==nrow & jpl$col==ncol]
#	    print(paste("Checking for data: row cell length(data) nwvc",
#		nrow,ncol,length(data),nwvc,sep=" "),quo=F)
  	    if (length(data) != 0) {

	      nwvc <- nwvc + 1

	      if (nwvc%%skip == 0) {

# Check to see if this cell has any ambiguities or sigma0's
	        amb<-length(jpl$namb[jpl$row==nrow & jpl$col==ncol]) # if 0, no matches
  	        nsig<-length(s0$s0obs[s0$row==nrow & s0$col==ncol])   # if 0, no matches
		print(paste("Processing data: row cell nwvc amb nsig",
			nrow,ncol,nwvc,amb,nsig,sep=" "),quo=F)

# Calculate and plot mle
#	        if (nsig > 0) {
## Make sub-title
		sub<-paste("Rev", 1096," Row", nrow, " Col", ncol,sep=" ")
## Make Postscript file name
#		path<-"/carmine/nscat/functional.plots/rev1096/"
#		file<-paste(path,"row",nrow,".col",ncol,".mle.ps",sep="")
#		print(file)
#		mle(jpl,s0,irow=nrow,icol=ncol,
#			range=T,uran=c(-20,20),vran=c(-20,20),
#			plot=T,ncont=10,step=0.5,
#			sub=sub,ps.file=file,make.ps=F)
#		dev.off()
#	        }

# Calculate and plot sssass functional
	        if (amb > 0) {
# Make sub-title
		sub<-paste("Rev", 1096," Row", nrow, " Col", ncol,sep=" ")
# Make Postscript file name
#		path<-"/carmine/nscat/functional.plots/rev1096/"
#		file<-paste(path,"row",nrow,".col",ncol,".sssass.ps",sep="")
#		print(file)
		sssass(jpl,irow=nrow,icol=ncol,
			range=T,uran=c(-20,20),vran=c(-20,20),
			plot=T,ncont=10,step=0.5,
			sub=sub,ps.file=file,make.ps=F)
#		dev.off()
	        }
# Calculate and plot Tomassini dual ambiguity functional

	        if (amb > 0) {
		sub<-paste("Rev", 1096," Row", nrow, " Col", ncol,sep=" ")
		ssdual(jpl,irow=nrow,icol=ncol,
			range=T,uran=c(-20,20),vran=c(-20,20),
			plot=T,ncont=10,step=0.5,
			sub=sub,ps.file=file,make.ps=F)
	         }
	      } 
	    }
	  }
	}

	dev.off()
}



