plot.all.jpl.amb <- function (goes0=goes,
	    jpl=amb.JPLranked, dellt=2, delln=2,
	    matches=match.anal$mask, plot.bg=T,
	    xs=-90, xe=-30, ys=0, ye=60, sub="a", subsub=1,
	    make.ps=T, drawmap=T, file, plot.goes=F,
	    vam.data.dir="/plum/scat.p145/vam/output/nscat/" ,thin=1,
	    outdir=paste (vam.data.dir, "r", jpl$revs[1], "/ps", sep=""),
	    barbscale=0.11, vam.factor=2,
	    colours=c(1,4), plotl=c(T,T,T,T), title)
{
#!# $Id: plot.all.jpl.amb.q,v 1.5 2005/07/14 18:48:37 jhalland Exp $
#!# Plot all 4 ambiguities
#!# $Log: plot.all.jpl.amb.q,v $
#!# Revision 1.5  2005/07/14 18:48:37  jhalland
#!# Changed title handling, a few minor prefs changed (wind barb color, barb
#!# thickness)
#!#
#!# Revision 1.4  2005/06/10 19:15:34  jhalland
#!# Added title string while leaving default title set if not title is given, changed wind barb ambiguity color scheme so each barb has a different color making first ambiguity red, added optional argument 'matches' if there is not a vam.ambiguities included, changed timestamp location from line 6 to line 4.
#!#
#!# Revision 1.3  1999/10/27 13:50:08  trn
#!# Fixed names and default values of vam.factor (size of VAM ambiguity locations)
#!#
#!# Revision 1.2  1999/09/24 21:13:51  trn
#!# Improved control over size of barbs/circles
#!#
#!# Revision 1.1  1999/09/22 16:42:42  trn
#!# Initial revision
#!#
###
###   Parameters
###
###         goes0  GOES image object
###           jpl  all JPL ambiguities with JPL-selected in the first position
###       matches  logical mask where two ambiguity selection methods agree/disagree
###        xs, xe  starting and ending longitudes of the region of interest
###        ys, ye  starting and ending latitudes of the region of interest
###           sub  name of subregion of interest
###        subsub  name of sub-subregion of interest
###       make.ps  logical; create a PostScript version of the plot?
###       drawmap  logical; draw map background?
###         dellt - latitude increment for annotation
###         delln - longitude increment for annotation
###          file  name of PostScript file
###  vam.data.dir  VAM data directory
###       plot.bg  logical whether to plot just plain background map
###         title  title string for the plot
###

#  title <- paste ("rev", jpl$revs[1], " ", sub, " : all JPL ambiguities", 
#		  sep="")

  if (missing(file)) {
### Make outdir for Postscript files
    unix(paste ("mkdir -p ", outdir, sep=""))
    file <- paste (outdir, "/amb.all.subregion-", sub, subsub, ".ps", sep="")
  }

## Zero out QC'd winds                        # dual amb plotting
#  index <- jpl$wgt <0                        # dual amb plotting
#  jpl$u[index] <- NA                         # dual amb plotting
#  jpl$v[index] <- NA                         # dual amb plotting
#  jpl$u2[index] <- NA                        # dual amb plotting
#  jpl$v2[index] <- NA                        # dual amb plotting
#  plt (jpl, gridd=F, pt="jpl", color=F,      # dual amb plotting

# do not enable postscript file printing in plt
# do it here:
### If make.ps
   if (make.ps) {
#	ps.options (colors=HeatB.lines.ps)
#	ps.options (image.colors=HeatB.images.ps)
	ps.options (colors=ps.colors.rgb[c('black','green','red','blue'),])
	ps.options (image.colors=ps.colors.rgb[c('black','green','red','blue'),])
#	ps.options (lines=ps.colors.rgb[c('black','grey20','bisque','cyan'),])
	postscript(file=file,print.it=F, horizontal=F)
		print(list(ps.file=file)) 
   }

  if (plot.goes) {
    plt (goes0, gridd=F, pt="goes.tb", color=T,
	 xs=xs, xe=xe, ys=ys, ye=ye, labsiz=0.85,
	 title=title, subt=T, drawmap=drawmap, #make.ps=make.ps
	 delln=delln, dellt=dellt,    
	 file=file, barbt=1, sel=F)
  }

  if (plot.bg) {
    plot.background(back,main=title,
		    lns=xs,lne=xe,lts=ys,lte=ye,delln=delln,dellt=dellt,
		    initialize=T,drawmap=drawmap,sub=sub)
  }
 
### Plot all JPL ambiguities
  select <- (xs<=jpl$lon & jpl$lon<=xe) & (ys<=jpl$lat & jpl$lat<=ye)
  if (thin > 1) select <- select & ((seq(length(select)) %% thin) == 1)
  if (is.null(barbscale)) barbscale<-sqrt(((xe - xs) * (ye - ys))/sum(select))/sqrt(2)
  print(paste("colours=", colours))
  plot.jpl.winds (jpl, barbcolors=colours, plotl=plotl,
		  barbscale=barbscale, barbt=0.8, use.sel=F,select=select)

### Mark points where JPL != VAM ambiguity selection
  ## Select only those points within the plotting region
  matches[is.na(matches)] <- T
  plot.wind.obs (jpl, loc=T, barbthick=0.8, barbscale=vam.factor * barbscale, 
		 barbcolor=colours[2], select=(matches == F & select))

  timestamp(line=3)

  if (make.ps) {
     print('closing postscript file')
     dev.off() 
}
}
