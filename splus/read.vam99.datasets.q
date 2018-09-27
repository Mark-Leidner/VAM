read.vam99.datasets <- 
  function (
	    rev=848, jul=289, year=1996, time=0000,
	    vam.date, vam.time,
	    sat.data.dir="/adeos2/leidner/p725/sat.data/splus/",
	    vam.data.dir="/plum/scat.p145/vam/output/nscat/",
	    read.goes=T, sat.file, 
	    anal.file, back.file, 
	    anal.interp2sat.file, back.interp2sat.file, 
	    amb.JPLranked.file, amb.dual.file)
#!# $Id: read.vam99.datasets.q,v 1.7 2005/06/10 15:09:16 jhalland Exp $
#!# Read in one VAM analysis + associated data files
#!# $Log: read.vam99.datasets.q,v $
#!# Revision 1.7  2005/06/10 15:09:16  jhalland
#!# The rounding was changed from 1 decimal place to 2.
#!#
#!# Revision 1.6  2004/09/22 18:18:31  leidner
#!# updated path to TAP Splus data directory
#!#
#!# Revision 1.5  1999/10/25 17:08:05  trn
#!# Carry column (cell) number. Add RCS keywords. Use rounded
#!# xs,del,ys,dely in native.wind.field
#!#

###
### Purpose: Read in one VAM analysis + associated data files,
###          create corresponding S-Plus objects and assign to the
###          local database (.Data).
###
### Parameters:
###
###           rev  NSCAT orbit number
###           jul  Julian day of GOES image
###          year  Year of GOES image
###          time  Time of GOES image (hhmm UTC)
###      vam.date  Analysis date (yymmdd) (default from anal)
###      vam.time  Analysis time (hhmmss) (default from anal)
###  sat.data.dir  GOES data directory
###  vam.data.dir  VAM data directory
###     read.goes  logical; read GOES data?
###

{

### Construct file names from input parameters
  ## Convert julian day and year to month, day, year
  sat.time <- month.day.year (jul, origin=c(month=1, day=1,
				     year=year)) 
  ## Convert year to yy form
  yy <- year %% 100

  ## Build data file paths/names
  if (missing(sat.file)) sat.file <- 
    paste ("g8.", yy, jul, ".", time, ".prepro.splus", sep="")   
  if (!is.null(sat.file)) sat.file <- paste (sat.data.dir, sat.file, sep="")   
  if (missing(anal.file)) anal.file <- 
    paste ("r", rev, "/analysis2", sep="") 
  if (!is.null(anal.file)) anal.file <- paste (vam.data.dir, anal.file, sep="") 
  if (missing(back.file)) back.file <- 
    paste ("r", rev, "/background1", sep="") 
  if (!is.null(back.file)) back.file <- paste (vam.data.dir, back.file, sep="") 
  if (missing(anal.interp2sat.file)) anal.interp2sat.file <- 
    paste ("r", rev, "/analysis2@nscat.txt", sep="") 
  if (!is.null(anal.interp2sat.file)) anal.interp2sat.file <- 
    paste (vam.data.dir, anal.interp2sat.file, sep="") 
  if (missing(back.interp2sat.file)) back.interp2sat.file <- 
    paste ("r", rev, "/background@nscat.txt", sep="") 
  if (!is.null(back.interp2sat.file)) back.interp2sat.file <- 
    paste (vam.data.dir, back.interp2sat.file, sep="") 
  if (missing(amb.JPLranked.file)) amb.JPLranked.file <- 
    paste ("r", rev, "/jpl.winds.JPLranked.txt", sep="") 
  if (!is.null(amb.JPLranked.file)) amb.JPLranked.file <- 
    paste (vam.data.dir, amb.JPLranked.file, sep="") 
  if (missing(amb.dual.file)) amb.dual.file <- 
    paste ("r", rev, "/jpl.winds.dual.txt", sep="") 
  if (!is.null(amb.dual.file)) amb.dual.file <- 
    paste (vam.data.dir, amb.dual.file, sep="") 

### Read in GOES-8 data
  ## If read.goes=F, do not read in GOES data and remove existing object
  if (read.goes) {
    if (!is.null(sat.file)) {
      gv <- scan (sat.file)
      assign ("goes", make.goes.data (gv), where=1, immediate=T)}
  } else {
    rm(goes) 
  }

### Read in VAM and background analyses
### Attach TAPHOME/splus/.Data if needed:
  if (length(find('ingest.saved.grid')) == 0) {
    tap.home <- getenv('TAPHOME')
    if (tap.home == '') tap.home <- '/home/trn/burst/new-tap'
    if(substring(tap.home, first = nchar(tap.home), last = nchar(tap.home)) != "/")
      tap.home <- paste(tap.home, "/", sep = "")
    attach(paste(tap.home,'splus/.Data',sep='')) }
  
  if (!is.null(anal.file)) {
    wfv <- ingest.saved.grid (paste(anal.file,'.hdr',sep=''),
			      paste(anal.file,'.dat',sep=''),plot.param=F)
    anal <- native.wind.field (wfv) 
    if (missing(vam.date)) vam.date <- anal$date
    if (missing(vam.time)) vam.time <- anal$time
    assign ("anal", trim.vam.wind.field (anal), where=1, immediate=T)
#Above line changed from trim.wind.field to trim.vam.wind.field
#for use with VAM output 
  }
  if (!is.null(back.file)) {
    wfv <- ingest.saved.grid (paste(back.file,'.hdr',sep=''),
			      paste(back.file,'.dat',sep=''),plot.param=F)
    back <- native.wind.field (wfv)
    assign ("back", trim.vam.wind.field (back), where=1, immediate=T)
#Above line changed from trim.wind.field to trim.vam.wind.field
  }
### Read in VAM analysis and background interpolated to NSCAT grid
  if (!is.null(anal.interp2sat.file)) {
    nv <- scan (anal.interp2sat.file)
    anal.interp2sat <- make.nscat.winds (nv,date=vam.date,time=vam.time,
					 revs=rev,nByrev=nv[19],
					 lognum=NULL)
    anal.interp2sat$lon <- ifelse(anal.interp2sat$lon > 180, 
				  anal.interp2sat$lon-360, anal.interp2sat$lon)
    assign ("anal.interp2sat", anal.interp2sat, where=1, immediate=T) 
  }
  if (!is.null(back.interp2sat.file)) {
    nv <- scan (back.interp2sat.file)
    back.interp2sat <- make.nscat.winds (nv,date=vam.date,time=vam.time,
					 revs=rev,nByrev=nv[19],
					 lognum=NULL)
    back.interp2sat$lon <- ifelse(back.interp2sat$lon > 180, 
				  back.interp2sat$lon-360, back.interp2sat$lon)
    assign ("back.interp2sat", back.interp2sat, where=1, immediate=T) 
  }

  if (!is.null(amb.JPLranked.file)) {
### Read in NSCAT ambiguous winds (ambiguities ordered as in HDF files)
    nv <- scan(amb.JPLranked.file)
    amb.JPLranked <- make.jpl.winds (nv, date=vam.date,time=vam.time,
				     revs=rev,nByrev=nv[19],
				     lognum=NULL,
				     mle1=NULL, mle2=NULL, mle3=NULL, mle4=NULL,
				     selected=NULL)

    amb.JPLranked$lon <- ifelse(amb.JPLranked$lon > 180, 
				amb.JPLranked$lon-360, amb.JPLranked$lon)
    assign ("amb.JPLranked", amb.JPLranked, where=1, immediate=T)
  }

  if (!is.null(amb.dual.file)) {
    nv <- scan(amb.dual.file)
    amb.dual <- make.jpl.winds (nv, date=vam.date,time=vam.time,
				revs=rev,nByrev=nv[19],
				lognum=NULL,
				mle1=NULL, mle2=NULL, mle3=NULL, mle4=NULL,
				selected=NULL)
    amb.dual$lon <- ifelse(amb.dual$lon > 180, 
			   amb.dual$lon-360, amb.dual$lon)
    assign ("amb.dual", amb.dual, where=1, immediate=T)
  }
### Find the closest ambiguites to VAM and background analyses
### Compute three statistics for ambiguity selection
###  1.) JPL and closest-to-background disagree by X %
###  2.) JPL and closest-to-VAM-analysis disagree by Y %
###  3.) closest-to-background and closest-to-VAM-analysis disagree by Z %
  if (!is.null(amb.JPLranked.file)) {
    if (!is.null(anal.interp2sat.file)) {
      assign ("closest.anal", find.closest (amb.JPLranked, anal.interp2sat),
	      where=1, immediate=T)
      assign ("match.anal", match.amb (amb.JPLranked, closest.anal, amb.JPLranked),
	      where=1, immediate=T )
      assign ("Y", match.anal$diff, where=1, immediate=T )
    }
    if (!is.null(back.interp2sat.file)) {
      assign ("closest.back", find.closest (amb.JPLranked, back.interp2sat),
	      where=1, immediate=T)
      assign ("match.back", match.amb (amb.JPLranked, closest.back, amb.JPLranked),
	      where=1, immediate=T ) 
      assign ("X", match.back$diff, where=1, immediate=T )
    }
    if (!is.null(anal.interp2sat.file) && !is.null(back.interp2sat.file)) {
      assign ("match.back.anal", match.amb (closest.back, closest.anal, amb.JPLranked),
	      where=1, immediate=T ) 
      assign ("Z", match.back.anal$diff, where=1, immediate=T )
    }
  }

  ("VAM data read successfully.")

}

native.wind.field <- 
  function(gridded,header=gridded$header,data=gridded$data,
	   date=header$ncicod,
	   time=header$ncicot, lognum=NULL,
	   nx=header$dims[1], ny=header$dims[2],
 	   digits=2, round.factor=10^digits,
 	   delx=round(round.factor*header$dx)/round.factor, 
 	   dely=round(round.factor*header$dy)/round.factor,
 	   xs = round(round.factor*header$reflon)/round.factor
 	        - (1 - header$refi) * delx,
 	   ys = round(round.factor*header$reflat)/round.factor
 	        - (1 - header$refi) * delx,
	   xe = xs + delx * (nx-1), ye = ys + dely * (ny-1),
	   grid.names = list(seq(xs, xe, delx), seq(ys, ye, dely)),
	   index.lev=1, index.u = '33', index.v='34',
	   u = matrix(as.vector(data[,,index.lev,index.u]), 
	     nrow = nx, ncol = ny, dimnames = grid.names),
	   v = matrix(as.vector(data[,,index.lev,index.v]), 
	     nrow = nx, ncol = ny, dimnames = grid.names) )
{
#!# $Id: read.vam99.datasets.q,v 1.7 2005/06/10 15:09:16 jhalland Exp $
#!# Convert native-format gridded data structure to VAM Splus wind field object
#!# $Log: read.vam99.datasets.q,v $
#!# Revision 1.7  2005/06/10 15:09:16  jhalland
#!# The rounding was changed from 1 decimal place to 2.
#!#
#!# Revision 1.6  2004/09/22 18:18:31  leidner
#!# updated path to TAP Splus data directory
#!#
#!# Revision 1.5  1999/10/25 17:08:05  trn
#!# Carry column (cell) number. Add RCS keywords. Use rounded
#!# xs,del,ys,dely in native.wind.field
#!#
  if (xs > 180 && xe > 180) {xs <- xs-360; xe <- xe-360}
  list(date = date, time = time, lognum = lognum, xs = xs, xe = xe, delx
       = delx, ys = ys, ye = ye, dely = dely, u = u, v = v)
}
