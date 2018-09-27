plot.ssmi <- function (winds,
            dellt=2, delln=2, plot.bg=T, 
	    xs=-90, xe=-30, ys=0, ye=60, sub="a", subsub=1,
	    make.ps=T, drawmap=T, file,
	    plot.time=F, plot.velm=F,
            outdir = "/burst/p651a/p651/vam-fgat/splus",
            thin=1,title="test title", horizontal=F,
	    barbscale=NULL, vam.factor=2, colours=c(3,4),
            polyscale=0.18, nseg=15, my.select=NULL, zlim=c(0,30),
            diff=0 )
{
#!# $Id: plot.ssmi.q,v 1.3 2005/07/25 19:18:40 leidner Exp $
#!# Plot SSMI parameters
#!# $Log: plot.ssmi.q,v $
#!# Revision 1.3  2005/07/25 19:18:40  leidner
#!# improved color scale plotting using new argument, "diff"; function is
#!# now dependent on three sets of color tables corresponding to wind
#!# speed (diff=0; default) , signed wind speed differences (diff=1), and
#!# pos def wind speed differences (diff=2).
#!#
#!# Revision 1.2  2005/07/14 18:51:21  jhalland
#!# First working version.
#!#
#!# Revision 1.1  2005/06/13 17:23:22  leidner
#!# First plotting software for SSMI data.
#!#
###
###   Parameters
###
###         winds  wind object with VAM FGAT information included
###        xs, xe  starting and ending longitudes of the region of interest
###        ys, ye  starting and ending latitudes of the region of interest
###           sub  name of subregion of interest
###        subsub  name of sub-subregion of interest
###       make.ps  logical; create a PostScript version of the plot?
###       drawmap  logical; draw map background?
###         dellt  latitude increment for annotation
###         delln  longitude increment for annotation
###          file  name of PostScript file
###      plot.vam  logical; plot VAM-selected ambiguities?
###      plot.jpl  logical; plot JPL-selected ambiguities?
###       plot.bg  logical whether or not to plot background map
###    horizontal  orientation of image in postcript file, if T will
###                print landscape, if F will print portrait
###          diff  indicator for type of wind difference:
###                0 - field is not a difference field; plot as a full field
###                1 - field is a signed difference field (+/- values)
###                2 - field is a positive def difference field (+ values only)
###


### Make outdir for Postscript files
 print(paste('outdir',outdir,sep=' '))
 if (missing(outdir)) {
  outdir <- paste ("./", sep="")
  unix(paste ("mkdir -p ", outdir, sep=""))
}

  if (plot.velm) {
#    title <- paste ("SSMI velocity magnitude",  sep="")
    if (missing(file)) 
      file <- paste (outdir, "/ssmi.velm.ps", sep="") 
    else
      file <- paste (outdir, file, sep="/")
  }

# set colour tables for plotting VAM FGAT params
# ps.options (colors=ssmi.winds.lines.colors)
# ps.options (image.colors=ssmi.winds.image.colors)

   if (diff == 0) {
      ps.options (colors=mark.windspeed.line.colors)
      ps.options (image.colors=mark.windspeed.image.colors)
    } else if (diff == 1) {
      ps.options (colors=mark.signed.windspeed.diff.line.colors)
      ps.options (image.colors=mark.signed.windspeed.diff.image.colors)
    } else {
      ps.options (colors=mark.windspeed.diff.line.colors)
      ps.options (image.colors=mark.windspeed.diff.image.colors)
    }

   if (make.ps) { # open ps file if requested
      postscript(file=file, horizontal = horizontal, print.it=F)
      print(list(ps.file=file)) 
   }

  if (plot.bg) {
	plot.background(back,main=title,lns=xs,lne=xe,lts=ys,lte=ye,
			delln=delln,dellt=dellt,initialize=T,
			drawmap=drawmap,sub=sub)
	

  }

  n <- length(winds$lat)
 if (n==0) {
   warning ('No SSMI data in region; exiting plot.ssmi')
   return()
 }
 
 if (plot.velm) { plot.ssmi.vel(winds, polyscale=polyscale, nseg=nseg,
                                select=my.select, zlim=zlim) }

  timestamp(line=2)

  if (make.ps) {
     print('closing postscript file')
     dev.off() 
}

}




# -----------------------------------------------
#     BEGIN FIELD SPECIFIC PLOTTING FUNCTIONS
# -----------------------------------------------

plot.ssmi.vel <- function (ssmi,
	all.lat=ssmi$lat, all.lon=NULL,
	usr=par('usr'), xs=usr[1], xe=usr[2], ys=usr[3], ye=usr[4],
	select=(xs<=all.lon & all.lon<=xe) & (ys<=all.lat & all.lat<=ye) &
              ssmi$velm>0,
	lat=all.lat[select], lon=all.lon[select],
	velm=ssmi$velm[select], velm5=ssmi$velm5[select],
	polyscale=0.13, nseg=6, plot.trajectory=F , zlim=c(0,30))
{
#
# Parameters:
#              ssmi - ssmi data object
# (all.lat,all.lon) - location of all data in ssmi object
#     (xs,xe,ys,ye) - lat/lon bounds of plotting region
#         (lat,lon) - location of ssmi data within plotting region
#              velm - ssmi wind speed
#             velm5 - trajectory ssmi wind speed
#         polyscale - scaling factor for polygon plotting size
#              nseg - number of line segments used to draw circle
#   plot.trajectory - logical flag specifying trajectory or obs ssmi wind speed
#
if (is.null(all.lon)) {
 if (xe > 180) {
    all.lon <- ifelse(ssmi$lon<0,ssmi$lon+360,ssmi$lon)
 } else {
    all.lon <- ssmi$lon
 }	
}
	n <- sum(select)
#	print(list(select=select,all.lat=all.lat,all.lon=all.lon,nseg=nseg,n=n,bounds=c(xs,xe,ys,ye)))
	{if (n==0) warning ('No ssmi wind data in region.')}
	if (plot.trajectory) velm <- velm5
# Use ssmi wind speed to define polygon colors.
# Colors 8-48 in ssmi.winds color table are used to fill in
# ssmi wind speed polygons. The first seven colors are used for
# colors of lines and text in the plot. ssmi wind speed
# = 0 m/s corresponds to color 8 while ssmi winds >= 20 m/s use color 48.
       #colors<-as.integer(round(2*velm)) + 8
# set colors greater than 20 m/s to top of color scale
       #index.hi<-velm>20
       #colors[index.hi]<-48
# Use ssmi wind speed to define polygon colors.
# Colors 8-136 in mark.windspeed.line.colors color table are used to fill in
# ssmi wind speed polygons. The first seven colors are used for
# S colors of lines and text in the plot. ssmi wind speed
# = zlim[1] m/s corresponds to color 8 while ssmi winds >= zlim[2] m/s use color 136.
         color.hi <- 136
         color.lo <- 8
         numcols <- color.hi - color.lo + 1
        colors <- as.integer( round( (numcols/(zlim[2]-zlim[1])) * velm ) ) + 8

       if (zlim[1] < 0) 
         colors  <-as.integer( round( (numcols/(zlim[2]-zlim[1])) * (velm+zlim[2]) ) ) + 8

       if (zlim[1] > 0) 
         colors  <-as.integer( round( (numcols/(zlim[2]-zlim[1])) * (velm-zlim[1]) ) ) + 8
        print(range(colors))

# set colors greater than zlim[2] and less than zlim[1] to gray
       index.out<-colors > color.hi | colors < color.lo
       colors[index.out]<- 2

        if (nseg >= 3) {
# Replicate lats/lons nseg+1 times
	  nsegp <- nseg + 1
	  times<-rep(nsegp,n)
	  x<-rep(lon,times=times)
	  y<-rep(lat,times=times)
# Create plotting coords for polygons
	  a<-seq(1,nsegp,1)*2*pi/(nseg)
	  x <- x + rep(polyscale*cos(a),n)
	  y <- y + rep(polyscale*sin(a),n)
# Assign NA's to mark breaks between polygons
	  breaks<-seq(nsegp,n*nsegp,nsegp)
	  x[breaks]<-NA
	  y[breaks]<-NA
# Plot ssmi winds as filled polygons
	  polygon(x,y,border=F,col=colors)
        } else {
	  points(lon, lat, type="p", pch=".", cex=6.5/(ye-ys), col=colors)
	}

# Plot legend
	plot.add.image.legend(zlim=zlim,lines=c(-2.5,-2),axisloc=c(0.15,0.85))
	mtext('(m/s)', side=1, line=3.85, at=xe-(xe-xs)*0.03)
}
#!#   $Id: plot.ssmi.q,v 1.3 2005/07/25 19:18:40 leidner Exp $
#!#   $Log: plot.ssmi.q,v $
#!#   Revision 1.3  2005/07/25 19:18:40  leidner
#!#   improved color scale plotting using new argument, "diff"; function is
#!#   now dependent on three sets of color tables corresponding to wind
#!#   speed (diff=0; default) , signed wind speed differences (diff=1), and
#!#   pos def wind speed differences (diff=2).
#!#
#!#   Revision 1.2  2005/07/14 18:51:21  jhalland
#!#   First working version.
#!#
#!#   Revision 1.1  2005/06/13 17:23:22  leidner
#!#   First plotting software for SSMI data.
#!#
plot.ssmi.los <- function (ssmi,
	all.lat=ssmi$lat, all.lon=ssmi$lon,
	usr=par('usr'), xs=usr[1], xe=usr[2], ys=usr[3], ye=usr[4],
	select=(xs<=all.lon & all.lon<=xe) &  (ys<=all.lat & all.lat<=ye),
	lat=all.lat[select], lon=all.lon[select],
	losm=ssmi$losm[select], losm5=ssmi$losm5[select],
	px=ssmi$px[select], py=ssmi$py[select],
	polyscale=0.13, nseg=6, plot.trajectory=F )
{
#
# Parameters:
#              ssmi - ssmi data object
# (all.lat,all.lon) - location of all data in ssmi object
#     (xs,xe,ys,ye) - lat/lon bounds of plotting region
#         (lat,lon) - location of ssmi data within plotting region
#              losm - los wind speed
#             losm5 - trajectory los wind speed
#                px - sin(satellite look angle)
#                py - cos(satellite look angle)
#         polyscale - scaling factor for polygon plotting size
#              nseg - number of line segments used to draw circle
#   plot.trajectory - logical flag specifying trajectory or obs los winds
#
	nsegp <- nseg + 1
	n <- sum(select)
	{if (n==0) warning ('No los wind data in region.')}
	if (plot.trajectory) losm <- losm5
# Replicate lats/lons nseg+1 times
	times<-rep(nsegp,n)
	breaks<-seq(nsegp,n*nsegp,nsegp)
	x<-rep(lon,times=times)
	y<-rep(lat,times=times)
# Create plotting coords for polygons
	if (nseg == 3) { #  Triangles are a special case
		nn<-0
	 	for (i in losm) {
			nn<-nn+1
			j<-nn*4
			toward<-sign(i)
			x[j-3]<-x[j-3]-toward*polyscale*2*px[nn]/3
			y[j-3]<-y[j-3]-toward*polyscale*2*py[nn]/3
			tempx<-x[j-2]+toward*polyscale*px[nn]/3
			tempy<-y[j-2]+toward*polyscale*py[nn]/3
			x[j-2]<-tempx+polyscale*py[nn]
			y[j-2]<-tempy-polyscale*px[nn]
			x[j-1]<-tempx-polyscale*py[nn]
			y[j-1]<-tempy+polyscale*px[nn] } }
	else { #  Create n-sided polygons
		a<-seq(1,nsegp,1)*2*pi/(nseg)
		x <- x + rep(polyscale*cos(a),n)
		y <- y + rep(polyscale*sin(a),n) }
# Assign NA's to mark breaks between polygons
	x[breaks]<-NA
	y[breaks]<-NA
# Use los wind speed ro define polygon colors.
# Colors 8-48 in ssmi.winds color table are used to fill in
# ssmi los wind speed polygons. The first seven colors are used for
# colors of lines and text in the plot. ssmi los winds
# = 0 m/s corresponds to color 8 while los winds >= 20 m/s use color 48.
	abs.losm<-abs(losm)
	colors<-as.integer(round(2*abs.losm)) + 8
# set colors greater than 20 m/s to top of color scale
	index.hi<-abs.losm>20
	colors[index.hi]<-48
# Plot los winds as filled polygons
	polygon(x,y,border=F,col=colors)
	if (nseg==3) { #  For triangles, plot a straight where los winds are zero
		zero<-losm==0
		polygon(x,y,density=0,border=zero,col=1,lwd=1,xpd=T) }
	else { #  For other polygons, overplot a "+" character for los winds pointing toward satellite
		toward<-losm>0
		points(lon[toward],lat[toward],type="p",cex=6.5/(ye-ys),pch="+",col=1) }
# Plot legend
	plot.add.image.legend(zlim=c(0,20),lines=c(-2.5,-2),axisloc=c(0.3,0.7))
	mtext('(m/s)', side=1, line=3.5, at=xs+(xe-xs)*0.2)
# Plot legend labels for los brightness temp
	mtext('(K)', side=1, line=1.5, at=xs+(xe-xs)*0.2)
	mtext('0.0',side=1, line=1.5, at=xs+(xe-xs)*0.3)
	mtext('+/-2.4', side=1, line=1.5, at=xs+(xe-xs)*0.7)
}
#!#   $Id: plot.ssmi.q,v 1.3 2005/07/25 19:18:40 leidner Exp $
#!#   $Log: plot.ssmi.q,v $
#!#   Revision 1.3  2005/07/25 19:18:40  leidner
#!#   improved color scale plotting using new argument, "diff"; function is
#!#   now dependent on three sets of color tables corresponding to wind
#!#   speed (diff=0; default) , signed wind speed differences (diff=1), and
#!#   pos def wind speed differences (diff=2).
#!#
#!#   Revision 1.2  2005/07/14 18:51:21  jhalland
#!#   First working version.
#!#
#!#   Revision 1.1  2005/06/13 17:23:22  leidner
#!#   First plotting software for SSMI data.
#!#
plot.ssmi.tice <- function (ssmi,
	all.lat=ssmi$lat, all.lon=ssmi$lon,
	usr=par('usr'), xs=usr[1], xe=usr[2], ys=usr[3], ye=usr[4],
	select=(xs<=all.lon & all.lon<=xe) &  (ys<=all.lat & all.lat<=ye),
	lat=all.lat[select], lon=all.lon[select],
	tice=ssmi$tice[select],
	polyscale=0.18, nseg=6, max=100)
{
#
# Parameters:
#              ssmi - ssmi data object
# (all.lat,all.lon) - location of all data in ssmi object
#     (xs,xe,ys,ye) - lat/lon bounds of plotting region
#         (lat,lon) - location of ssmi data within plotting region
#              tice - percent total sea ice in ssmi cell
#         polyscale - scaling factor for polygon plotting size
#              nseg - number of line segments used to draw circle
#
	nsegp <- nseg + 1
	n <- sum(select)
	{if (n==0) warning ('No ssmi wind data in region.')}
# Replicate lats/lons nseg+1 times
	times<-rep(nsegp,n)
	x<-rep(lon,times=times)
	y<-rep(lat,times=times)
# Create plotting coords for polygons
	a<-seq(1,nsegp,1)*2*pi/(nseg)
	x <- x + rep(polyscale*cos(a),n)
	y <- y + rep(polyscale*sin(a),n)
# Assign NA's to mark breaks between polygons
	breaks<-seq(nsegp,n*nsegp,nsegp)
	x[breaks]<-NA
	y[breaks]<-NA
# Use ssmi integrated clooud water to define polygon colors.
# Colors 8-48 in ssmi.winds color table are used to fill in
# ssmi integrated cloud water polygons. The first seven colors are used for
# colors of lines and text in the plot. ssmi integrated cloud water
# = 0 mm corresponds to color 8 while icw >= 5 m/s use color 48.
	colors<-as.integer(round((40/max)*tice)) + 8
# set colors greater than 20 m/s to top of color scale
# Plot ssmi winds as filled polygons
	polygon(x,y,border=F,col=colors)
# Plot legend
	plot.add.image.legend(zlim=c(0,max),lines=c(-2.5,-2),axisloc=c(0.3,0.7))
	mtext('(%)', side=1, line=3.5, at=xs+(xe-xs)*0.2)
}
#!#   $Id: plot.ssmi.q,v 1.3 2005/07/25 19:18:40 leidner Exp $
#!#   $Log: plot.ssmi.q,v $
#!#   Revision 1.3  2005/07/25 19:18:40  leidner
#!#   improved color scale plotting using new argument, "diff"; function is
#!#   now dependent on three sets of color tables corresponding to wind
#!#   speed (diff=0; default) , signed wind speed differences (diff=1), and
#!#   pos def wind speed differences (diff=2).
#!#
#!#   Revision 1.2  2005/07/14 18:51:21  jhalland
#!#   First working version.
#!#
#!#   Revision 1.1  2005/06/13 17:23:22  leidner
#!#   First plotting software for SSMI data.
#!#
plot.ssmi.iwv <- function (ssmi,
	all.lat=ssmi$lat, all.lon=ssmi$lon,
	usr=par('usr'), xs=usr[1], xe=usr[2], ys=usr[3], ye=usr[4],
	select=(xs<=all.lon & all.lon<=xe) &  (ys<=all.lat & all.lat<=ye),
	lat=all.lat[select], lon=all.lon[select],
	iwv=ssmi$iwv[select],
	polyscale=0.13, nseg=6, max=40)
{
#
# Parameters:
#              ssmi - ssmi data object
# (all.lat,all.lon) - location of all data in ssmi object
#     (xs,xe,ys,ye) - lat/lon bounds of plotting region
#         (lat,lon) - location of ssmi data within plotting region
#               iwv - ssmi integrated water vapor
#         polyscale - scaling factor for polygon plotting size
#              nseg - number of line segments used to draw circle
#
	nsegp <- nseg + 1
	n <- sum(select)
	{if (n==0) warning ('No ssmi wind data in region.')}
# Replicate lats/lons nseg+1 times
	times<-rep(nsegp,n)
	x<-rep(lon,times=times)
	y<-rep(lat,times=times)
# Create plotting coords for polygons
	a<-seq(1,nsegp,1)*2*pi/(nseg)
	x <- x + rep(polyscale*cos(a),n)
	y <- y + rep(polyscale*sin(a),n)
# Assign NA's to mark breaks between polygons
	breaks<-seq(nsegp,n*nsegp,nsegp)
	x[breaks]<-NA
	y[breaks]<-NA
# Use ssmi integrated water vapor to define polygon colors.
# Colors 8-48 in ssmi.winds color table are used to fill in
# ssmi wind speed polygons. The first seven colors are used for
# colors of lines and text in the plot. ssmi wind speed
# = 0 m/s corresponds to color 8 while ssmi winds >= 20 m/s use color 48.
	colors<-as.integer(round((40/max)*iwv)) + 8
# set colors greater than 20 m/s to top of color scale
	index.hi<-iwv>max
	colors[index.hi]<-48
# Plot ssmi winds as filled polygons
	polygon(x,y,border=F,col=colors)
# Plot legend
	plot.add.image.legend(zlim=c(0,max),lines=c(-2.5,-2),axisloc=c(0.3,0.7))
	mtext('(mm)', side=1, line=3.5, at=xs+(xe-xs)*0.2)
}
#!#   $Id: plot.ssmi.q,v 1.3 2005/07/25 19:18:40 leidner Exp $
#!#   $Log: plot.ssmi.q,v $
#!#   Revision 1.3  2005/07/25 19:18:40  leidner
#!#   improved color scale plotting using new argument, "diff"; function is
#!#   now dependent on three sets of color tables corresponding to wind
#!#   speed (diff=0; default) , signed wind speed differences (diff=1), and
#!#   pos def wind speed differences (diff=2).
#!#
#!#   Revision 1.2  2005/07/14 18:51:21  jhalland
#!#   First working version.
#!#
#!#   Revision 1.1  2005/06/13 17:23:22  leidner
#!#   First plotting software for SSMI data.
#!#
plot.ssmi.icw <- function (ssmi,
	all.lat=ssmi$lat, all.lon=ssmi$lon,
	usr=par('usr'), xs=usr[1], xe=usr[2], ys=usr[3], ye=usr[4],
	select=(xs<=all.lon & all.lon<=xe) &  (ys<=all.lat & all.lat<=ye),
	lat=all.lat[select], lon=all.lon[select],
	icw=ssmi$icw[select],
	polyscale=0.18, nseg=6, max=5)
{
#
# Parameters:
#              ssmi - ssmi data object
# (all.lat,all.lon) - location of all data in ssmi object
#     (xs,xe,ys,ye) - lat/lon bounds of plotting region
#         (lat,lon) - location of ssmi data within plotting region
#               icw - ssmi integrated water vapor
#         polyscale - scaling factor for polygon plotting size
#              nseg - number of line segments used to draw circle
#
	nsegp <- nseg + 1
	n <- sum(select)
	{if (n==0) warning ('No ssmi wind data in region.')}
# Replicate lats/lons nseg+1 times
	times<-rep(nsegp,n)
	x<-rep(lon,times=times)
	y<-rep(lat,times=times)
# Create plotting coords for polygons
	a<-seq(1,nsegp,1)*2*pi/(nseg)
	x <- x + rep(polyscale*cos(a),n)
	y <- y + rep(polyscale*sin(a),n)
# Assign NA's to mark breaks between polygons
	breaks<-seq(nsegp,n*nsegp,nsegp)
	x[breaks]<-NA
	y[breaks]<-NA
# Use ssmi integrated clooud water to define polygon colors.
# Colors 8-48 in ssmi.winds color table are used to fill in
# ssmi integrated cloud water polygons. The first seven colors are used for
# colors of lines and text in the plot. ssmi integrated cloud water
# = 0 mm corresponds to color 8 while icw >= 5 m/s use color 48.
	colors<-as.integer(round((40/max)*icw)) + 8
# set colors greater than 20 m/s to top of color scale
	index.hi<-icw>max
	colors[index.hi]<-48
# Plot ssmi winds as filled polygons
	polygon(x,y,border=F,col=colors)
# Plot legend
	plot.add.image.legend(zlim=c(0,max),lines=c(-2.5,-2),axisloc=c(0.3,0.7))
	mtext('(mm)', side=1, line=3.5, at=xs+(xe-xs)*0.2)
}
#!#   $Id: plot.ssmi.q,v 1.3 2005/07/25 19:18:40 leidner Exp $
#!#   $Log: plot.ssmi.q,v $
#!#   Revision 1.3  2005/07/25 19:18:40  leidner
#!#   improved color scale plotting using new argument, "diff"; function is
#!#   now dependent on three sets of color tables corresponding to wind
#!#   speed (diff=0; default) , signed wind speed differences (diff=1), and
#!#   pos def wind speed differences (diff=2).
#!#
#!#   Revision 1.2  2005/07/14 18:51:21  jhalland
#!#   First working version.
#!#
#!#   Revision 1.1  2005/06/13 17:23:22  leidner
#!#   First plotting software for SSMI data.
#!#
plot.ssmi.rainr <- function (ssmi,
	all.lat=ssmi$lat, all.lon=ssmi$lon,
	usr=par('usr'), xs=usr[1], xe=usr[2], ys=usr[3], ye=usr[4],
	select=(xs<=all.lon & all.lon<=xe) &  (ys<=all.lat & all.lat<=ye),
	lat=all.lat[select], lon=all.lon[select],
	rainr=ssmi$rain[select],
	polyscale=0.18, nseg=6, max=10)
{
#
# Parameters:
#              ssmi - ssmi data object
# (all.lat,all.lon) - location of all data in ssmi object
#     (xs,xe,ys,ye) - lat/lon bounds of plotting region
#         (lat,lon) - location of ssmi data within plotting region
#             rainr - ssmi rain rate (mm/h)
#         polyscale - scaling factor for polygon plotting size
#              nseg - number of line segments used to draw circle
#
	nsegp <- nseg + 1
	n <- sum(select)
	{if (n==0) warning ('No ssmi wind data in region.')}
# Replicate lats/lons nseg+1 times
	times<-rep(nsegp,n)
	x<-rep(lon,times=times)
	y<-rep(lat,times=times)
# Create plotting coords for polygons
	a<-seq(1,nsegp,1)*2*pi/(nseg)
	x <- x + rep(polyscale*cos(a),n)
	y <- y + rep(polyscale*sin(a),n)
# Assign NA's to mark breaks between polygons
	breaks<-seq(nsegp,n*nsegp,nsegp)
	x[breaks]<-NA
	y[breaks]<-NA
# Use ssmi rain rate to define polygon colors.
# Colors 8-48 in ssmi.winds color table are used to fill in
# ssmi rain rate polygons. The first seven colors are used for
# colors of lines and text in the plot. ssmi rain rate
# = 0 mm/h corresponds to color 8 while rain rate >= 10 mm/h use color 48.
	colors<-as.integer(round((40/max)*rainr)) + 8
# set colors greater than 20 m/s to top of color scale
	index.hi<-rainr>max
	colors[index.hi]<-48
# set colors greater than 20 m/s to top of color scale
# Plot ssmi winds as filled polygons
	polygon(x,y,border=F,col=colors)
# Plot legend
	plot.add.image.legend(zlim=c(0,max),lines=c(-2.5,-2),axisloc=c(0.3,0.7))
	mtext('(mm/h)', side=1, line=3.5, at=xs+(xe-xs)*0.2)
}


plot.ssmi.sst <- function (ssmi,
	all.lat=ssmi$lat, all.lon=ssmi$lon,
	usr=par('usr'), xs=usr[1], xe=usr[2], ys=usr[3], ye=usr[4],
	select=(xs<=all.lon & all.lon<=xe) &  (ys<=all.lat & all.lat<=ye),
	lat=all.lat[select], lon=all.lon[select],
	sst=ssmi$sst[select],
	polyscale=0.18, nseg=6, max=32)
{
#
# Parameters:
#              ssmi - ssmi data object
# (all.lat,all.lon) - location of all data in ssmi object
#     (xs,xe,ys,ye) - lat/lon bounds of plotting region
#         (lat,lon) - location of ssmi data within plotting region
#               sst - climatological sea sfc temp (deg C)
#         polyscale - scaling factor for polygon plotting size
#              nseg - number of line segments used to draw circle
#
	nsegp <- nseg + 1
	n <- sum(select)
	{if (n==0) warning ('No ssmi wind data in region.')}
# Replicate lats/lons nseg+1 times
	times<-rep(nsegp,n)
	x<-rep(lon,times=times)
	y<-rep(lat,times=times)
# Create plotting coords for polygons
	a<-seq(1,nsegp,1)*2*pi/(nseg)
	x <- x + rep(polyscale*cos(a),n)
	y <- y + rep(polyscale*sin(a),n)
# Assign NA's to mark breaks between polygons
	breaks<-seq(nsegp,n*nsegp,nsegp)
	x[breaks]<-NA
	y[breaks]<-NA
# Use climatological sea sfc temp to define polygon colors.
# Colors 8-48 in ssmi.winds color table are used to fill in
# sst polygons. The first seven colors are used for
# colors of lines and text in the plot. Climo sst 
# = 0 deg C corresponds to color 8 while sst >= max deg C use color 48.
	colors<-as.integer(round((40/max)*sst)) + 8
# set colors greater than max deg C to top of color scale
	index.hi<-sst>max
	colors[index.hi]<-48
# set colors greater than 20 m/s to top of color scale
# Plot ssmi winds as filled polygons
	polygon(x,y,border=F,col=colors)
# Plot legend
	plot.add.image.legend(zlim=c(0,max),lines=c(-2.5,-2),axisloc=c(0.3,0.7))
	mtext('deg C', side=1, line=3.5, at=xs+(xe-xs)*0.2)
}



