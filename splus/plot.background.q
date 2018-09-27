#!#   $Id: plot.background.q,v 1.8 2005/07/14 18:43:54 jhalland Exp $
#!#   $Log: plot.background.q,v $
#!#   Revision 1.8  2005/07/14 18:43:54  jhalland
#!#   Updated for plotting across the dateline.
#!#
#!#   Revision 1.7  2005/06/13 17:18:50  leidner
#!#   updated line types/thicknesses for bounding plotting area and map outlines.
#!#
#!#   Revision 1.6  1999/10/29 16:57:17  jmh
#!#   Updated to plot GM and EQ
#!#
#!#   Revision 1.5  1999/08/27 21:47:58  jmh
#!#   Corrected axis labelling bug
#!#
#!#   Revision 1.4  1999/08/26 16:36:48  jmh
#!#   jmh: Changed axes labelling strategy
#!#   jmh: Heavily updated version; use with local plot.ESTEC.q in /orchid/jmh/ambig/splus
#!#
#!#   Revision 1.3  1999/06/16 19:10:19  jmh
#!#   This entry updates file using M. Leidner's changes
#!#
#!#	Revision 1.2  1997/04/24  21:49:14  leidner
#!#	added code needed for plotting map backgrounds
#!#
#!#	Revision 1.1  1997/04/22  20:20:00  leidner
#!#	Initial revision
#!#
plot.background <- function (wf, initialize=T, annotate=T,
	drawbox=T, drawmap = drawmap, main = '', sub='', setpin = T,
	lns = wf$xs, lne = wf$xe, delln = wf$delln, interior = F,
	lts = wf$ys, lte = wf$ye, dellt = wf$dellt, labsiz=1) 
{
# Note: library(maps) is needed if drawmap is T
# Note: This version assumes lns < lne and all longitudes are
# in (-180, 180) to support maps.  Latitudes are in (-90, 90).
#
# Parameters:
#
#            wf - gridded (wind field) object which provides defaults
#      annotate - LOGICAL: if true add axes, labels
#    initialize - LOGICAL: if true setup the plot
#       drawmap - LOGICAL; draw a map background?
# Note: will not work if plot region crosses date line.
#       drawbox - LOGICAL; draw a box around the plot area?
#        setpin - set plot area (pin)
#           lns - starting longitude
#           lne - ending longitude
#         delln - longitude increment for annotation
#           lts - starting latitude
#           lte - ending latitude
#         dellt - latitude increment for annotation
#          main - plot title
#           sub - sub title
#
        library(maps)
#

# Set normal line type, line width, and clipping
	par(lty = 1, lwd = 1, xpd = F)
	if (initialize) {
#
# Define the coordinates of the plot region for square aspect ratio
#rnh   	par( xaxs = "i", yaxs = "i")
	if (setpin) {
#  	par(pty = 'm')
	aspect( (lne - lns)/(lte - lts) ) }
#
# Setup for plotting
	plot( c(lns, lne), c(lts, lte),
		type = "n", las = 1, tck = 1, xaxt = "n", yaxt = "n", 
		xlab = "", ylab = "", bty = "o", lty = 1, lwd = 2, xaxs='i', yaxs='i', col=2) }
#
	if (annotate) {
#
# Generate y-axis labels every dellt within the range (lts,lte)
# making sure that 0 is a tic mark
#	y <- seq(dellt,90,dellt)
       
        y <- seq(lts,90,dellt) # start latitude labels from beginning of plot range

#	yval <- c(-rev(y),0,y)
          yval <- c(y[y<0],0,y[y>0])
        print(yval)

#	ylab <- c(paste(rev(y),"S",sep=""),"EQ",paste(y,"N",sep=""))
###        ylab <- c(paste(-yval[yval<0],"S",sep=""),paste(yval[yval>0],"N",sep=""))
  	ylab <- paste(abs(yval),ifelse(yval>0,"N","S"),sep="")
#        print(ylab)
        ylab[yval==0] <- 'EQ'
#        print(ylab)
	select <- lts <= yval & yval <= lte

 	yval <- yval[select]
	ylab <- ylab[select]

# Generate x-axis labels every delln within the range (lns,lne)
#	x <- seq(delln,360,delln)
        x <- seq(lns,lne,delln)

#	xval <- c(-rev(x),0,x)
        if (any(x<0) && any(x>0)) {
	xval <- c(x[x<0],0,x[x>0])
 	} else xval <- x
 print(xval)
# temporarily normalize to -180<xval<=180 in order to generate labels
#	xval[180<xval] <- xval[180<xval] - 360
#	xval[xval<=(-180)] <- xval[xval<=(-180)] + 360

 	xlab <- paste(abs(xval),ifelse(xval>0,"E","W"),sep="")
        
##        xlab <- c(paste(-xval[xval<0],"W",sep=""),"GM",paste(xval[xval>0],"E",sep=""))
  
###        xlab <- c(paste(-xval[xval<0],"W",sep=""),paste(xval[xval>0],"E",sep=""))
         
#print(xlab)
        xlab[xval==0] <- 'GM'
 print(xlab)
#	xlab[xval==0]<-"GM"
	xlab[xval==180 | xval==-180]<-"DL"
        
#	xval <- c(-rev(x),0,x) # restore xval numerical sequence

	select <- lns <= xval & xval <= lne

	xval <- xval[select]
	xlab <- xlab[select]
# Note: X-axis labels (i.e., longitudes) will plot as densely as space will allow; no over-
# plotting occurs, even for "GM"

#
# Plot axes, axes labels, map background, caption and title.
	if (length(yval)>0) { par(mgp = c(0, labsiz+0.2, 0))
	axis(2,at=yval,tck=1,labels=ylab,lty=2,lwd=0.75,cex=labsiz,yaxs='i',col=2)
	axis(2,at=yval,tck=F,labels=ylab,lty=1,lwd=2,yaxs='i',col=1) }
	if (length(xval)>0) { par(mgp = c(0, 0.5, 0))
	axis(1,at=xval,tck=1,labels=xlab,lty=2,lwd=0.75,cex=labsiz,xaxs='i',col=2)
	axis(1,at=xval,tck=F,labels=xlab,lty=1,lwd=2,xaxs='i',col=1) }
	par(mgp = c(0, labsiz+0.2, 0))
 	mtext(main, line = 1.5, cex = labsiz*1.2)
 	mtext(sub, line = 0.5, cex=labsiz) }
# map fails crossing dateline, so avoid this.
 	if (drawmap & !(lns > lne)) {
                print('plotting map ...')
#		map("world.thin",xlim=c(lns,lne),ylim=c(lts,lte),add=T,lwd=1,col=2,interior=F)
# 		map("state",xlim=c(lns,lne),ylim=c(lts,lte),add=T,lwd=2,col=2)
 		map("world",xlim=c(lns,lne),ylim=c(lts,lte),add=T,lwd=2,col=2)
 		if(lne > 180) {
                    cat ('lne=',lne,'will not plot any map features beyond 180\n')
                }
        }
 	invisible( if (drawbox) box(lty = 1, col=1, lwd=2) )
}


