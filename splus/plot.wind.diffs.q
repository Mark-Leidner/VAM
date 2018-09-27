plot.wind.diffs <- function(field1, field2, skip=1, skipx=skip, skipy=skip,
	 winds1 = wf2wo(field1,skipx=skipx,skipy=skipy), 
	 winds2 = wf2wo(field2,skipx=skipx,skipy=skipy), 
	 scaling = 10, lowind = 0, xs = -180, xe = 180, ys = -85, ye = 85, 
	 delln = 10, dellt = 10, initialize = T, drawmap = T, 
	 plot.diff = T, select, 
	 main = if (plot.diff) "Wind difference field" else "Wind fields", 
         sub = sub.def, 
	 barbc = c(3, 1), barbt = c(1, 0.25), ...)
{
#!# Plot differences of 2 gridded wind fields, either:
#!#    - one on top of the other, or
#!#    - as a difference wind field
#!# Only plot every skipx, skipy points in lon,lat, and only those
#!# with vector differences > lowind
#!# Difference wind field scaled by scaling
#!# ... passed to plot.wind.obs
#!# $Id: plot.wind.diffs.q,v 1.1 1999/09/10 18:11:09 trn Exp $
#!# $Log: plot.wind.diffs.q,v $
#!# Revision 1.1  1999/09/10 18:11:09  trn
#!# Initial revision
#!#
	winds.diff <- winds1
	winds.diff$u <- scaling * (winds1$u - winds2$u)
	winds.diff$v <- scaling * (winds1$v - winds2$v)
	if(missing(select))
		select <- (sqrt(winds.diff$u^2 + winds.diff$v^2) > scaling * 
			lowind) & (xs <= winds.diff$lon & winds.diff$lon <= xe) &
			(ys <= winds.diff$lat & winds.diff$lat <= ye)
	plot.background(field1, lns = xs, lne = xe, lts = ys, lte = ye, delln
		 = delln, dellt = dellt, initialize = initialize, drawmap = 
		drawmap)
	sub.def <- ""
	if (lowind > 0) sub.def <- paste(sub.def,"lowind=",lowind)
	if(plot.diff) {
	  plot.wind.obs(winds.diff, select = select, barbc = barbc[1], 
			barbt = barbt[1], ...)
	  sub.def <- paste(sub.def,"scaling=",scaling)
	} else {
	  if(length(barbc) < 2)
	    barbc <- rep(barbc, 2)
	  if(length(barbt) < 2)
	    barbt <- rep(barbt, 2)
	  plot.wind.obs(winds1, select = select, barbc = barbc[1], barbt
			= barbt[1], ...)
	  plot.wind.obs(winds2, select = select, barbc = barbc[2], barbt
			= barbt[2], ...)
	}
	if(!is.null(main) && !is.null(sub))
		title(main = main, sub = sub)
}
