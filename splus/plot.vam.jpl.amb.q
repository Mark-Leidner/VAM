plot.vam.jpl.amb <- function (goes0=goes, vam=closest.anal,
	    jpl=amb.JPLranked,
	    matches=match.anal$mask, dellt=2, delln=2,
	    xs=-90, xe=-30, ys=0, ye=60, sub="a", subsub=1,
	    make.ps=T, drawmap=T, file, title, horizontal=T,
	    plot.vam=T, plot.jpl=T, plot.goes=F, plot.bg=T,thin=1,
	    vam.data.dir="/plum/scat.p145/vam/output/nscat/" ,
	    outdir=paste (vam.data.dir, "r", jpl$revs[1], "/ps", sep="") ,
	    barbscale=NULL, vam.factor=2, colours=c(3,4),
            barbthick=0.25, my.select=select)
{
#!# $Id: plot.vam.jpl.amb.q,v 1.5 2005/07/12 22:14:43 jhalland Exp $
#!# Plot the VAM and JPL selected ambiguities
#!# $Log: plot.vam.jpl.amb.q,v $
#!# Revision 1.5  2005/07/12 22:14:43  jhalland
#!# added new args: horizontal for portrait/landscape PostScript plotting, wind barb thickness, my.select logical select vector, more flexible titles, and observation plotting across the dateline.
#!#
#!# Revision 1.4  2001/05/17 22:22:12  jmh
#!# Updated to enable goes Tb plotting (ps file now opened in plt.q if goes Tb to be plotted), clarified filenames, reversed order and colour of plotting of ambiguities and fixed minor typo involving lat/lon labelling
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
###           vam  VAM-selected ambiguities
###           jpl  all JPL ambiguities with JPL-selected in the first position
###       matches  logical mask where two ambiguity selection methods agree/disagree
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
###  vam.data.dir  VAM data directory
###     plot.goes  logical wheter or not to plot GOES data
###       plot.bg  logical whether or not to plot background map
###    horizontal  logical whether or not to plot as landscape or portrait
###


### Make outdir for Postscript files
 print(paste('outdir',outdir,sep=' '))
 if (missing(outdir)) {
  outdir <- paste (vam.data.dir, "r", jpl$revs[1], "/ps", sep="")
  unix(paste ("mkdir -p ", outdir, sep=""))
}

  if (plot.vam && !plot.jpl) {
    if (missing(title)){
      title <- paste ("rev", jpl$revs[1], " ", sub,subsub,
                    " : VAM99 selected ambiguities", sep="")
    } else {
        title <- paste(title)
      }

    if (missing(file)) { 
      file <- paste (outdir, "/vam.amb.subregion-", sub, subsub,
		     ".ps", sep="") 
    } else {
      file <- paste(outdir, file, sep="/")
      }
  }

  if (plot.jpl && !plot.vam) {
    if (missing(title)) {
      title <- paste ("rev", jpl$revs[1], " ", sub,subsub,
                    " : JPL selected ambiguities", sep="")
    } else {
        title <- paste(title)
      }

    if (missing(file)) {
      file <- paste (outdir, "/jpl.amb.subregion-", sub, subsub,
		     ".ps", sep="") 
    } else {
      file <- paste(outdir, file, sep="/")
    }
  }

  if (plot.jpl && plot.vam) {
    if (missing(title)) {
      title <- paste ("rev", jpl$revs[1], " ", sub,subsub,
                    " : Selected ambiguities: VAM99 red, JPL blue", sep="")
    } else {
        title <- paste(title)
      }
    if (missing(file)) {
      file <- paste (outdir, "/vam.jpl.amb.subregion-", sub, subsub,
		     ".ps", sep="") 
    } else {
      file <- paste(outdir, file, sep="/")
    }
  }


### plot GOES; plt modified to accept make.ps option, thus enabling opening of ps file
  if (plot.goes) {
    plt (goes0, gridd=F, pt="goes.tb", color=T,
	 xs=xs, xe=xe, ys=ys, ye=ye, labsiz=0.85, make.ps=make.ps,
	 title=title, subt=T, drawmap=drawmap,
	 delln=delln, dellt=dellt,
	 file=file)
  }

   if (!plot.goes && make.ps) { #if goes image not plotted, open ps file if requested
      postscript(file=file,print.it=F, horizontal=horizontal)
      print(list(ps.file=file)) 
   }

#reset colour table that was modified in plt by above plot.goes call
	ps.options (colors=ps.colors.rgb[c('black','grey10','red','blue'),])
	ps.options (image.colors=ps.colors.rgb[c('black','grey10','red','blue'),])

  if (plot.bg) {
    plot.background(back,main=title,lns=xs,lne=xe,lts=ys,lte=ye,
		    delln=delln,dellt=dellt,initialize=T,
		    drawmap=drawmap,sub=sub)
  }

  if (xe > 180) {  # handle plotting across the date line
        lon <- ifelse(jpl$lon<0,jpl$lon+360,jpl$lon)
        jpl$lon <- lon
  }

### Plot JPL and/or VAM selected ambiguities
#  ## Set JPL color to a different color if both are to be plotted
#  jplcol <- ifelse (plot.jpl && plot.vam, 1, 2)  # 3=red, 4=blue
#  if (plot.jpl) plot.wind.obs(jpl, barbc=jplcol, barbt=2)
#  if (plot.vam) plot.wind.obs(vam, barbc=1, barbt=2)

  if (plot.jpl) {
    select <- (xs<=jpl$lon & jpl$lon<=xe) & (ys<=jpl$lat & jpl$lat<=ye)
    if (thin > 1) select <- select & ((seq(length(select)) %% thin) == 1)
    if (is.null(barbscale)) barbscale<-sqrt(((xe - xs) * (ye - ys))/sum(select))/sqrt(2)
    plot.wind.obs(jpl, barbc=colours[2], barbt=barbthick, barbscale=barbscale, select=my.select)
  }
  if (plot.vam) {
    if (xe > 180) {
	lon <- ifelse(vam$lon<0,vam$lon+360,vam$lon)
        vam$lon <- lon
    }
    select <- (xs<=vam$lon & vam$lon<=xe) & (ys<=vam$lat & vam$lat<=ye)
    if (thin > 1) select <- select & ((seq(length(select)) %% thin) == 1)
    if (is.null(barbscale)) barbscale<-sqrt(((xe - xs) * (ye - ys))/sum(select))/sqrt(2)
    plot.wind.obs(vam, barbc=colours[1], barbt=barbthick, barbscale=barbscale, select=my.select)
  }

### Mark points where JPL != VAM ambiguity selection
  ## Select only those points within the plotting region
  select <- (xs<=jpl$lon & jpl$lon<=xe) & (ys<=jpl$lat & jpl$lat<=ye)
  if (thin > 1) select <- select & ((seq(length(select)) %% thin) == 1)
  if (is.null(barbscale)) barbscale<-sqrt(((xe - xs) * (ye - ys))/sum(select))/sqrt(2)
  matches[is.na(matches)] <- T
  plot.wind.obs (jpl, loc=T, barbthick=barbthick, barbcolor=colours[2],
		 barbscale=vam.factor*barbscale, select=(matches == F & my.select))

  timestamp(line=3)

  if (make.ps) {
     print('closing postscript file')
     dev.off() 
}

}

