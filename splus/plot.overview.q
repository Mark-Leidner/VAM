plot.overview <- 
  function (goes0=goes, amb=amb.JPLranked, vdiff=match.anal$vdiff,
	    matches=match.anal$mask, plot.goes=F, plot.bg=T,
	    xs=-180, xe=180, ys=-80, ye=80, plot.matches=T, 
	    make.ps=T, horizontal=T, drawmap=T, file, plot.a.d='full',
	    vam.data.dir="/plum/scat.p145/vam/output/nscat/",
	    outdir=paste (vam.data.dir, "S",batch,"ps", sep=""),
	    plot.last=T, batch=1, range.revs=range(rev),
	    plot.first=T, plot.back=F, plot.anal=F, bg.name='NMC',
	    rev=amb$revs[1], day=1, dellt=10, delln=10,
	    all.batches=numeric(0), all.days=numeric(0),
	    all.revs=numeric(0), skip=1, skipx=skip, skipy=skip, thin=1, 
	    breaks=c(2,4,8,16), colours=c(3,4,5,6,7,8),
	    barbscale.jpl=NULL,barbthick=0.25,bin.factor=NULL,
	    yoff=-0.4, ythick=0.11, labsiz=0.85, plot.box=F,
            box.coord=list(p1=c(0,0),p2=c(10,0),p3=c(10,10),p4=c(0,10)), ... )
{
#!# $Id: plot.overview.q,v 1.7 2002/07/23 16:42:13 leidner Exp $
#!# Overview plot: scatterometer locations with color-coded locations for
#!#    vector differences, plus (optional) background and analysis fields
#!# $Log: plot.overview.q,v $
#!# Revision 1.7  2002/07/23 16:42:13  leidner
#!# made barb thickness and spacing of lat/lon map grid top-level options
#!#
#!# Revision 1.6  1999/11/18 20:53:44  jmh
#!# Added capability to draw box; added bg.name to subtitle
#!#
#!# Revision 1.5  1999/10/29 18:12:15  jmh
#!# Minor modifications to clarify domains and selection procedures for stats
#!#
#!# Revision 1.4  1999/10/28 15:59:48  jmh
#!# Rev number now plotted on swaths (only on ascending pass for 'full')
#!#
#!# Revision 1.3  1999/10/20 20:47:40  jmh
#!# Updated to allow separation of plotting by row (e.g., ascending, etc.)
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
###           amb  all JPL ambiguities with JPL-selected in the first position
###       matches  logical mask where two ambiguity selection methods agree/disagree
###        xs, xe  starting and ending longitudes of the region of interest
###        ys, ye  starting and ending latitudes of the region of interest
###       make.ps  logical; create a PostScript version of the plot?
###     plot.last  logical; last overlay in current ps file
###    plot.first  logical; first overlay in current ps file
###       drawmap  logical; draw map background?
###          file  name of PostScript file
###  vam.data.dir  VAM data directory
###     plot.goes  logical whether to plot goes data
###  plot.matches  logical whether to plot locations of NSCAT matches 
###       plot.bg  logical whether to plot just plain background map
###      plot.a.d  ascending, descending or full passes to be plotted
###         vdiff  magnitude of vector difference (m/s) between VAM and JPL ambig
###         batch  batch number
###           day  day number
###    range.revs  range of revs (from first and last in plot.day.q)

### Make outdir for Postscript files
  unix(paste ("mkdir -p ", outdir, sep=""))

  paste('Processing rev', rev)

# Create titles
  # Accumulate lists for titles
  all.batches <- c(all.batches,batch)
  all.days <- c(all.days,day)
  all.revs <- c(all.revs,rev)

  if (plot.last) {
  # Turn numerical vector into a string
    all.batches.string <- paste(unique(all.batches),collapse=' ')
    all.days.string <- paste(unique(all.days),collapse=' ')

    title1 <- paste("Magnitude (m/s) of vector difference between JPL and VAM selected ambiguities")

    if (min(all.revs)!=max(all.revs)) {
      title2 <- paste ("batch:",all.batches.string,"   day:",all.days.string,"   revs:", min(all.revs), 'to', max(all.revs), plot.a.d,"   bg:",bg.name, sep=" ")
    } else {
      title2 <- paste ("batch:",all.batches.string,"  day:",all.days.string,"  rev:", min(all.revs), plot.a.d,"   bg:",bg.name, sep=" ")
    }
  } else {
    title1 <- ''
    title2 <- ''
  }
  
  if (missing(file))
    if (range.revs[1]==range.revs[2]) {
      file <- paste (outdir, "/overview.", range.revs[1],".",plot.a.d, ".", xs,".",xe,".",ys,".",ye,".ps", sep="")
    } else {
      file <- paste (outdir, "/overview.", range.revs[1],".",range.revs[2],".",plot.a.d, ".", xs,".",xe,".",ys,".",ye, ".ps", sep="")
    }

# Overlay plots if plot.first=F
  if (!plot.first) par(new=TRUE)

# do not enable postscript file printing in plt
# do it here:
### If make.ps
  if (make.ps && plot.first) {
    ps.options (colors=ps.colors.rgb[c('black','grey10','bisque','dim grey','chartreuse1','gold','firebrick','blue'),])
    ps.options (image.colors=ps.colors.rgb[c('black','grey10','bisque','dim grey','chartreuse1','gold','firebrick','blue'),])

    postscript(file=file,horizontal=horizontal,print.it=F)
    print(list(ps.file=file)) 
  }


# Select ascending, descending of full rev
   select.a.d <- logical(length(amb$row))

# Note: plot.day99.q separates 'both' into 'ascending' then 'descending'   
   if (plot.a.d=='full') select.a.d[] <- T
   if (plot.a.d=='ascending')  {
      select.a.d[amb$row<=812] <- T
      adj <- 1
    }
   if (plot.a.d=='descending') {
      select.a.d[amb$row>812] <- T
      adj <- 0 
    }

### Plot black dots where there are scatterometer data
  plot.analyses(goes0=goes0,amb=amb,matches=matches, delln=delln, dellt=dellt,
		xs=xs, xe=xe, ys=ys, ye=ye, sub="", 
		plot.bg=plot.bg && plot.first, 
		title=title1, subt=F, select.a.d=select.a.d,
		make.ps=F, drawmap=drawmap, plot.goes=plot.goes,
		plot.scat=T, plot.vam=F, plot.back=plot.back && plot.first, 
		plot.anal=plot.anal && plot.first,
		barbscale.jpl=barbscale.jpl,barbthick=barbthick,back.color=7,anal.color=1,
		skipx=skipx,skipy=skipy,thin=thin,colours=colours,labsiz=labsiz)
  if (title2 != '') mtext(title2, line = 0.5, cex = labsiz)

  na.mask <- is.na(matches)
  print (paste("Of all ",length(matches)," WVC's, ",sum(amb$namb <= 1),
	       " have less than 2 ambiguities, and ",
	       sum(na.mask)-sum(amb$namb <= 1),
	       " are excluded for other reasons"))
 
  match <- sum(matches[!na.mask])
  nomatch <- sum(!matches[!na.mask])
  total <- match + nomatch
  per <- 100.*match/total
  print (paste("The vam has chosen the same ambiguity as JPL for ",match," of ",total," WVC's compared worldwide (",round(per,digits=1),"%).", sep=""))

  matches[is.na(matches)] <- T
  vdiff[is.na(vdiff)] <- 0
  
 select <- select.a.d & (xs<=amb$lon & amb$lon<=xe) & (ys<=amb$lat & amb$lat<=ye)

  if (thin > 1) select <- select & ((seq(length(select)) %% thin) == 1)
  if (is.null(barbscale.jpl)) 
    barbscale.jpl<-sqrt(((xe - xs) * (ye - ys))/sum(select))/sqrt(2)

  btitles <- '0'
  lower <- 0
  for (bin in 1:(length(breaks)+1)) {
    if (bin <= length(breaks)) {
      upper <- breaks[bin] 
      if (bin == 1) {
	btitles <- c(btitles,paste('<',upper)) 
      } else {
	btitles <- c(btitles,paste(lower,'-',upper)) 
      }	
    } else {
      upper <- 10^10
      btitles <- c(btitles,paste('>=',lower))
    } 
    select.bin <- select & !matches & lower <= vdiff & vdiff < upper
    cat ('Selected in plotting region, thinned obs in bin (',btitles[length(btitles)],
	 '): ',sum(select.bin),'\n',sep='')
    if (sum(select.bin) > 0) {
      if (!is.null(bin.factor)) {
#!# Use constant size for all bins, specified multiple of size for bin 0
	plot.wind.obs (amb, loc=T, barbthick=barbthick, barbcolor=colours[bin+1],
		       select=select.bin, barbscale=bin.factor*barbscale.jpl)
      } else {
#!# Use sample-dependent size for each bin (plot.wind.obs default)
	plot.wind.obs (amb, loc=T, barbthick=barbthick, barbcolor=colours[bin+1],
		       select=select.bin)
      }
    }
    lower <- upper
  }

# Plot rev number
  if (sum(select.a.d)==length(select.a.d)) { # plot label only on ascending swath 
     select.a.d[amb$row>812] <- F
     adj <- 1
   }
     label.lons <- amb$lon[(amb$lat<(-45) & amb$lat>(-48)) & select.a.d]
     if (length(label.lons)!=0 & range(label.lons)[2]-range(label.lons)[1]<180) text(mean(label.lons),-30,rev,cex=0.75,col=1,adj=adj)
 
# Plot colour bar
  colour.bar(xs=xs,ys=ys,xe=xe,ye=ye,
           divisions=btitles,colours=colours,yoff=yoff,ythick=ythick)

# Plot box
  if (plot.box) {
     lines(c(box.coord$p1[1],box.coord$p2[1]),c(box.coord$p1[2],box.coord$p2[2]),lwd=5)
     lines(c(box.coord$p2[1],box.coord$p3[1]),c(box.coord$p2[2],box.coord$p3[2]),lwd=5)
     lines(c(box.coord$p3[1],box.coord$p4[1]),c(box.coord$p3[2],box.coord$p4[2]),lwd=5)
     lines(c(box.coord$p4[1],box.coord$p1[1]),c(box.coord$p4[2],box.coord$p1[2]),lwd=5)
   }

  if (plot.last) timestamp(line=6)

  if (make.ps) {
    if (plot.last) {
      print('closing postscript file')
      dev.off() 
    } else {
      print('Warning: not closing ps file')
    }
  }
}
