plot.wind.obs <- function (wo,
	u=wo$u, v=wo$v, lat=wo$lat, lon=wo$lon, plot.wind.symbols=T,
	plot.contour=F, plot.image=F, plot.image.legend=plot.image,
	sd=uv2sd(u,v), speed=sd$vel, direction=sd$theta,
	usr=par('usr'), xs=usr[1], xe=usr[2], ys=usr[3], ye=usr[4],
	delx=(xe-xs)/180, dely=(ye-ys)/180,
	x=seq(xs,xe,delx), y=seq(ys,ye,dely),
	select=(xs<=lon & lon<=xe) &  (ys<=lat & lat<=ye),
	barbscale=sqrt((xe-xs)*(ye-ys)/n)/sqrt(2),barbcolor=1,
        barbthick=1.5, location.only=F,
	windmax=max(speed), levels=seq(0,windmax,1), zlim=c(0,windmax),
	no.low.winds=F, lowind = 0.1, 
	units = "")
{
#!#   $Id: plot.wind.obs.q,v 1.5 2005/06/13 17:12:34 leidner Exp $
#!#   $Log: plot.wind.obs.q,v $
#!#   Revision 1.5  2005/06/13 17:12:34  leidner
#!#   moved calculation of gridded wind speed outside of arg list.
#!#
#!#   Revision 1.4  1999/09/24 21:13:51  trn
#!#   Added changes from working version, fixed handling of colors
#!#
#!#   Revision 1.3  1997/04/25 21:32:49  leidner
#!#   added no.low.winds flag
#!#
#!#	Revision 1.2  1997/04/24  21:51:54  leidner
#!#	added location.only LOGICAL, changed location and size of image legend,
#!#	and wind barb attributes (barbthick and barbcolor)
#!#
#!#	Revision 1.1  1997/04/22  20:20:00  leidner
#!#	Initial revision
#!#
#
#
# Parameters:
#
#                wo - wind obs list(u=u, v=v, lat=lat, lon=lon)
#             (u,v) - u- and v-components of wind obs
#         (lat,lon) - location of wind obs
# plot.wind.symbols - LOGICAL; plot wind barbs at gridpoints?
#     plot.contours - LOGICAL; contour wind speed?
#        plot.image - LOGICAL; plot image of wind speed?
# plot.image.legend - LOGICAL; plot image.legend of wind speed?
#                sd - list(vel=speed, theta=direction(in radians))
# (speed,direction) - speed and direction of wind obs
# usr=c(xs,xe,ys,ye)- plotting boundaries
#       (delx,dely) - grid increment for interpolation
#             (x,y) - coordinates for gridded speed
#            select - LOGICAL vector of obs to be plotted
#         barbscale - length of wind symbol in lat-lon units
#     location.only - LOGICAL; plot data locations only
#           windmax - maximum wind speed for contours, greyscales
#            levels - countour levels
#              zlim - range for image levels
#      no.low.winds - LOGICAL; plot abs(wind speed differences) < lowind
#                     in image plot?
#            lowind - low threshold for plotting winds;
#                     used only if no.low.winds==T
#
# Adjust data window by 4% if axis type is r
# but first force selection based on full axis
	n <- sum(select)
	print(paste('num selected ', n))

#       gridded.speed - gridded wind speed interpolated from
#       observations to a regular grid
        gridded.speed=interp(lon[select], lat[select], speed[select], x, y)$z

	if (par('xaxs')=='r') {
		temp <- xs + (xe-xs)*(0.04/1.08) 
		xe <- xs + (xe-xs)*(1.04/1.08)
		xs <- temp }
	if (par('yaxs')=='r') {
		temp <- ys + (ye-ys)*(0.04/1.08) 
		ye <- ys + (ye-ys)*(1.04/1.08)
		ys <- temp }
# Plot greyscale image (if requested) and greyscale image (if requested)
	if (plot.image) {
	  sel.hi <- gridded.speed > windmax
	  gridded.speed[sel.hi] <- windmax
	  if (no.low.winds) {
# Don't plot image where wind speed difference is > -lowind or < lowind
	    sel <- abs(gridded.speed) < lowind
	    gridded.speed[sel] <- 0
	  }
	  image(x, y, gridded.speed, zlim = zlim, add = T,xpd=F) 
	  if(plot.image.legend) {
	    plot.add.image.legend(zlim = zlim, lines = c(-2.5, -2), axisloc
				  = c(0.3, 0.7))
	    mtext(units, side = 1, line = 3.5, at = xs + (xe - xs) * 0.2)
	  }
	}
#
# Contour speeds (if requested)
	if (plot.contour) contour(x, y, gridded.speed, levels=levels,
		add=T, plotit=T, triangles=F, labex=0, lwd=1.5)
#
# Plot wind barbs at gridpoints (if requested)
	if (plot.wind.symbols & n>0) {
#               do not clip the wind symbols
	  oldpar <- par(xpd = T,lwd=barbthick,col=barbcolor)
	  if (location.only) speed[] <- 0 
#               plot wind barbs
	  windsym(lon, lat, speed, direction, scale=barbscale, 
		  select=select, flip=(lat<0))
	  invisible(par(oldpar)) 
	} else {
	  if (n==0) warning ('No wind obs in region.')
	}
#
      }
