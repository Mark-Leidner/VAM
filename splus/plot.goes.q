#!#   $Id: plot.goes.q,v 1.1 1999/05/17 18:12:41 jmh Exp $
#!#   $Log: plot.goes.q,v $
#!#   Revision 1.1  1999/05/17 18:12:41  jmh
#!#   Initial revision
#!#
plot.goes.tb <- function (go,
	usr=par('usr'), xs=usr[1], xe=usr[2], ys=usr[3], ye=usr[4],
	sel= !is.na(go$tb4) & (xs<=go$lon & go$lon<=xe) &  (ys<=go$lat & go$lat<=ye),
	lat=go$lat[sel], lon=go$lon[sel], tb4=go$tb4[sel],
	polyscale=0.18, nseg=4, min=180, max=300, labsiz=1 )
{
#
# Parameters:
#                wo - wind observation data object
# (all.lat,all.lon) - location of all data in wo object
#     (xs,xe,ys,ye) - lat/lon bounds of plotting region
#         (lat,lon) - location of ssmi data within plotting region
#             (u,v) - wind components for data within plotting region
#              wspd - scaler wind speed
#         polyscale - scaling factor for polygon plotting size
#              nseg - number of line segments used to draw circle
#   plot.trajectory - logical flag specifying trajectory or obs ssmi wind speed
#
	nsegp <- nseg + 1
	n <- length(lat)

	if (n==0) {
           warning ('No goes data in region; exiting plot.goes.q')
           return()
        }
# Replicate lats/lons nseg+1 times
	times<-rep(nsegp,n)
	x<-rep(lon,times=times)
	y<-rep(lat,times=times)
# Create plotting coords for polygons
	a<-seq(1,nsegp,1)*2*pi/(nseg)+pi/4
#	a<-seq(1,nsegp,1)*2*pi/(nseg)
	x <- x + rep(polyscale*cos(a),n)
	y <- y + rep(polyscale*sin(a),n)
# Assign NA's to mark breaks between polygons
	breaks<-seq(nsegp,n*nsegp,nsegp)
	x[breaks]<-NA
	y[breaks]<-NA
# Use goes brightness temp (tb) to define polygon colors.
# Colors 8-48 in ssmi.winds color table are used to fill in
# goes tb polygons. The first seven colors are used for
# colors of lines and text in the plot. goes tb
# <= min K corresponds to color 8 while goes tb >= max K use color 48.
	tb4.scaled<-tb4-min
	limits<-max-min
	colors<-as.integer(round((40/limits)*(tb4.scaled))) + 8

# set colors greater than max m/s to top of color scale
	index.hi<-tb4>max
	colors[index.hi]<-48
	index.lo<-tb4<min
	colors[index.lo]<-8

# Plot ssmi winds as filled polygons
	polygon(x,y,border=F,col=colors)
# Plot legend
	par(cex=labsiz)
	plot.add.image.legend(zlim=c(min,max),lines=c(-3.5,-3),axisloc=c(0.2,0.8))
###1	plot.add.image.legend(zlim=c(min,max),lines=c(-2.5,-2),axisloc=c(0.3,0.7))

###	plot.add.image.legend(zlim=c(min,max),lines=c(-3.,-2),axisloc=c(0.3,0.7))
###	plot.add.image.legend(zlim=c(min,max),lines=c(-3.5,-2.5))
	mtext('GOES brightness temperature (K)', side=1, line=3.5)
        par(cex=1) }
###	mtext('(K)', side=1, line=3.5, at=xs+(xe-xs)*0.2)  }
###      }


plot.goes.al <- function (go,
	usr=par('usr'), xs=usr[1], xe=usr[2], ys=usr[3], ye=usr[4],
	sel= !is.na(go$tb4) & (xs<=go$lon & go$lon<=xe) &  (ys<=go$lat & go$lat<=ye),
	lat=go$lat[sel], lon=go$lon[sel], al=go$al[sel],
	polyscale=0.18, nseg=4, min=0, max=100 )
{
#
# Parameters:
#                wo - wind observation data object
# (all.lat,all.lon) - location of all data in wo object
#     (xs,xe,ys,ye) - lat/lon bounds of plotting region
#         (lat,lon) - location of ssmi data within plotting region
#             (u,v) - wind components for data within plotting region
#              wspd - scaler wind speed
#         polyscale - scaling factor for polygon plotting size
#              nseg - number of line segments used to draw circle
#   plot.trajectory - logical flag specifying trajectory or obs ssmi wind speed
#
	nsegp <- nseg + 1
	n <- length(lat)

	if (n==0) {
           warning ('No goes data in region; exiting plot.goes.q')
           return()
        } 
# Replicate lats/lons nseg+1 times
	times<-rep(nsegp,n)
	x<-rep(lon,times=times)
	y<-rep(lat,times=times)
# Create plotting coords for polygons
	a<-seq(1,nsegp,1)*2*pi/(nseg)+pi/4
#	a<-seq(1,nsegp,1)*2*pi/(nseg)
	x <- x + rep(polyscale*cos(a),n)
	y <- y + rep(polyscale*sin(a),n)
# Assign NA's to mark breaks between polygons
	breaks<-seq(nsegp,n*nsegp,nsegp)
	x[breaks]<-NA
	y[breaks]<-NA
# Use goes albedo to define polygon colors.
# Colors 8-48 in ssmi.winds color table are used to fill in
# goes albedo polygons. The first seven colors are used for
# colors of lines and text in the plot. goes albedo
# <= min corresponds to color 8 while goes albedo >= max K use color 48.
	al.scaled<-al-min
	limits<-max-min
	colors<-as.integer(round((40/limits)*(al.scaled))) + 8

# set colors greater than max m/s to top of color scale
	index.hi<-al>max
	colors[index.hi]<-48
	index.lo<-al<min
	colors[index.lo]<-8

# Plot ssmi winds as filled polygons
	polygon(x,y,border=F,col=colors)
# Plot legend
	plot.add.image.legend(zlim=c(min,max),lines=c(-2.5,-2),axisloc=c(0.3,0.7))
	mtext('(%)', side=1, line=3.5, at=xs+(xe-xs)*0.2)  }

