plot.analyses <- 
  function (back0=back, anal0=anal, goes0=goes,
	    amb=amb.JPLranked, select.a.d=rep(T,length(amb$lon)),
	    matches=match.anal$mask, delln=10, dellt=10,
	    xs=-180, xe=180, ys=-80, ye=80, sub="a", plot.bg=T,
	    make.ps=T, drawmap=T, file, plot.goes=F, title=title, 
	    background.name='ECMWF', subt=T, horizontal=T,
	    vam.data.dir="/plum/scat.p145/vam/output/nscat/",
	    outdir=paste (vam.data.dir, "r", amb$revs[1], "/ps", sep=""),
	    plot.scat=T, plot.vam=T, plot.back=T, plot.anal=T, plot.image=T,
            plot.wind.symbols=T,
	    barbscale.jpl=NULL, barbthick=0.25, vam.factor=1, back.color=3, anal.color=1,
	    skip=1,skipx=skip,skipy=skip,thin=1,colours=c(1,4),labsiz=0.85,
            plot.box=F,
            box.coord=list(x1=c(-74,-74,-70,-70),y1=c(32,38,38,32),
                           x2=c(-75,-75,-71,-71),y2=c(23,29,29,23)))
#!# $Id: plot.analyses.q,v 1.12 2005/07/14 18:50:08 jhalland Exp $
#!# Plot analysis, background, scatterometer locations
#!# $Log: plot.analyses.q,v $
#!# Revision 1.12  2005/07/14 18:50:08  jhalland
#!# Added portrait/landscape arg to function for plotting to PostScript files.
#!#
#!# Revision 1.11  2005/06/13 17:03:47  leidner
#!# added plot.image and plot.wind.symbols logicals to arg list.
#!#
#!# Revision 1.10  2005/06/10 15:04:33  jhalland
#!# reintroduced timestamp, and argument 'outdir' is now used to construct output pa
#!# th to ps file, even if argument 'file' is defined.
#!#
#!# Revision 1.9  2002/07/24 14:29:50  leidner
#!# box plotting now uses polygon; wind barb thickness for analysis and
#!# background is now the same (background used to be thinner)
#!#
#!# Revision 1.8  2002/07/23 16:41:02  leidner
#!# made barb thickness an optional argument
#!#
#!# Revision 1.7  2001/05/17 22:38:11  jmh
#!# Updated to allow plot.goes to update colour table; option to plot two boxes
#!#
#!# Revision 1.6  2001/05/16 18:04:41  jmh
#!# removed default plotting of timestamp; added capability to plot box
#!#
#!# Revision 1.5  1999/11/18 20:53:20  jmh
#!# Fixed argument list to pass in main title
#!#
#!# Revision 1.4  1999/10/25 16:54:02  trn
#!# Fix default for select.a.d
#!#
#!# Revision 1.3  1999/10/20 19:56:23  jmh
#!# Updated to enable separation of data by row (e.g. ascending) when plotting
#!#
#!# Revision 1.2  1999/09/24 21:13:51  trn
#!# Improved control over size of barbs/circles, color of background/analysis vectors
#!#
#!# Revision 1.1  1999/09/22 16:42:42  trn
#!# Initial revision
#!#
###
###   Parameters
###
###         back0  background wind field object
###         anal0  VAM analysis wind field object
###         goes0  GOES image object
###           amb  all JPL ambiguities with JPL-selected in the first position
###       matches  logical mask where two ambiguity selection methods agree/disagree
###        xs, xe  starting and ending longitudes of the region of interest
###        ys, ye  starting and ending latitudes of the region of interest
###           sub  name of subregion of interest
###       make.ps  logical; create a PostScript version of the plot?
###       drawmap  logical; draw map background?
###         dellt  latitude increment for annotation
###         delln  longitude increment for annotation
###          file  name of PostScript file
###  vam.data.dir  VAM data directory
###     plot.goes  logical whether or not to plot goes image
###       plot.bg  logical whether or not to plot background map
###

{

### Make outdir for Postscript files
  unix(paste ("mkdir -p ", outdir, sep=""))

### Plot GOES data
  if(missing(title)) 
    title <- paste ("rev", amb$revs[1], " ", sub, " : VAM ",
		    "(black) and ",background.name," (red) ",
		    "analyses", sep="")
  if (missing(file))
    file <- paste (outdir, "/analyses.subregion-", sub, ".ps", sep="")
  else 
    file <- paste (outdir, file, sep ="/")   

# do not enable postscript file printing in plt
# do it here:
### If make.ps
   if (make.ps) {
	ps.options (colors=HeatB.lines.ps)
	ps.options (image.colors=mark.windspeed.image.colors)
        print('printing in colour...')
#	ps.options (colors=ps.colors.rgb[c('black','grey10','red','blue'),])
#	ps.options (image.colors=ps.colors.rgb[c('black','grey10','red','blue'),])
#	ps.options (lines=ps.colors.rgb[c('black','grey20','bisque','cyan'),])
	postscript(file=file,print.it=F, horizontal=horizontal)
		print(list(ps.file=file)) 
   }

  if (plot.goes) {

  plt (goes0, gridd=F, pt="goes.tb", color=T,
       xs=xs, xe=xe, ys=ys, ye=ye, labsiz=labsiz, make.ps=make.ps,
       title=title, subt=subt, drawmap=drawmap,
       delln=delln, dellt=dellt,
       file=file)
}

 if (plot.bg) {
       plot.background(back,main=title,lns=xs,lne=xe,lts=ys,lte=ye,delln=delln,
          dellt=dellt,initialize=T,drawmap=drawmap,sub=sub)
}

  if (plot.scat || plot.vam) {
### Plot NSCAT data locations
    select <- select.a.d & (xs<=amb$lon & amb$lon<=xe) & (ys<=amb$lat & amb$lat<=ye)
    if (thin > 1) select <- select & ((seq(length(select)) %% thin) == 1)
    if (is.null(barbscale.jpl)) 
      barbscale.jpl<-sqrt(((xe - xs) * (ye - ys))/sum(select))/sqrt(2)
    if (plot.scat) 
      plot.wind.obs (amb, loc=T, barbc=colours[1], barbthick=barbthick, select=select,
		     barbscale=barbscale.jpl)

    if (plot.vam) {
### Mark points where JPL != VAM ambiguity selection
  ## Only consider the same selection as in the plot.wind.obs call:
      matches[is.na(matches)] <- T
      plot.wind.obs (amb, loc=T, barbthick=barbthick, barbcolor=colours[2],
#set above barbcolor to colours[1] if want non-match WVCs to be same as matches
		     select=(matches == F & select), 
		     barbscale=vam.factor*barbscale.jpl)
#  points (amb$lon[matches == F & select], amb$lat[matches == F & select],
#	 col=4, pch=16, cex=0.2, lwd=0.3)
    }
  }
  
### Plot background and VAM analysis wind fields
  if (plot.back) plot.wind.obs (wf2wo(back0,skipx=skipx,skipy=skipy),
                                plot.image=plot.image, windmax=30,
				barbc=back.color, barbt=1,
                                plot.wind.symbols=plot.wind.symbols)
  if (plot.anal) plot.wind.obs (wf2wo(anal0,skipx=skipx,skipy=skipy),
                                plot.image=plot.image, windmax=30,
				barbc=anal.color, barbt=1,
                                plot.wind.symbols=plot.wind.symbols)

 if (plot.image) {
       plot.background(back,main=title,lns=xs,lne=xe,lts=ys,lte=ye,delln=delln,
          dellt=dellt,initialize=F,drawmap=drawmap,sub=sub)
}

# Plot box
  if (plot.box) {
     polygon(box.coord$x1,box.coord$y1,density=0,lwd=5)
     polygon(box.coord$x2,box.coord$y2,density=0,lwd=5)
#    label boxes
     text(-69,33,'A',cex=2)
     text(-70,24,'B',cex=2)
   }

  timestamp(line=3)
  if (make.ps) {
     print('closing postscript file')
     dev.off() 
}

}
