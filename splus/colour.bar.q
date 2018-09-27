colour.bar <- 
  function(xs = xs, xe = xe, ys = ys, ye = ye, 
	   divisions = c("0", "<2", "2-4", 
	     "4-8", "8-16", ">=16"), colours = c(3, 4, 5, 6, 7, 8),
	   yoff=-0.2, ythick=0.11)
{
#!# $Id: colour.bar.q,v 1.1 1999/09/22 16:42:42 trn Exp $
#!# Plot colour bar at bottom of plot - used by plot.overview
#!# $Log: colour.bar.q,v $
#!# Revision 1.1  1999/09/22 16:42:42  trn
#!# Initial revision
#!#
  pl.range <- list(x = xe - xs, y = ye - ys)
  width <- pl.range$x/length(colours)
  xbox.position <- vector("list", length(colours))
  ybox.position <- vector("list", length(colours))
  xtext.position <- vector("list", length(colours))
  ytext.position <- vector("list", length(colours))
  for(k in 1:length(colours)) {
    if(k == 1) {
      xvalue <- c(xs + (width * 1)/4, xs + (width * 3)/4, 
		  xs + (width * 3)/4, xs + (width * 1)/4)
      yvalue <- c(rep(ys + yoff * pl.range$y,2),
		  rep(ys + (yoff+ythick) * pl.range$y,2))
    }
      else {
	xvalue <- c(xvalue[1] + width, xvalue[2] + width, 
		    xvalue[3] + width, xvalue[4] + width)
      }
    xbox.position[[k]] <- xvalue
    ybox.position[[k]] <- yvalue
    if(k == 1) {
      text.xvalue <- mean(c(xbox.position[[1]][1], 
			    xbox.position[[1]][2]))
      text.yvalue <- ybox.position[[1]][1] - pl.range$y/20
    }
      else {
	text.xvalue <- text.xvalue + width
      }
    xtext.position[[k]] <- text.xvalue
    ytext.position[[k]] <- text.yvalue	
					
# divisions: vector defining limits of coloured bars
# colours: colours to be used (need one fewer colours than lenth of divisions)
# text.position: x,y coords (wrt plotted values) for placement of colour bar
    par(xpd = T)	#Plot coloured wind speed bars
    polygon(list(x = xbox.position[[k]], y = ybox.position[[k]]), 
	    col = colours[k])	#Plot wind speed bins
    text(labels = divisions[[k]], x = xtext.position[[k]], y = 
	 ytext.position[[k]], adj = 0.5)
  }
}
