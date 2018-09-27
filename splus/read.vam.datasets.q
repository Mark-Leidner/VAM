#$Id: read.vam.datasets.q,v 1.1 2001/05/30 21:25:17 jmh Exp $
#$Log: read.vam.datasets.q,v $
#Revision 1.1  2001/05/30 21:25:17  jmh
#Code to read in 'old' vam (pre-vam99) output for plotting for vam99 code
#

read.vam.datasets <- function (
		     rev=848, jul=289, year=1996, time=0000,
		     sat.data.dir="/adeos2/leidner/p725/sat.data/splus/",
		     vam.data.dir="/plum/p651-revisited/vam/",
		     read.goes=T)
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
  if (year < 2000) yy <- year - 1900
  else yy <- year - 2000

  ## Build data file paths/names
  sat.file <-
    paste (sat.data.dir, "g8.", yy, jul, ".", time, ".prepro.splus",
	   sep="")   
  anal.file <-
    paste (vam.data.dir, "r", rev, "/analysis2.r", rev, sep="") 
  back.file <-
    paste (vam.data.dir, "r", rev, "/background.r", "*", sep="") 
  back.file <- unix (paste("ls", back.file, sep=" "))
  anal.interp2sat.file <-
    paste (vam.data.dir, "r", rev, "/analysis2@nscat.r", rev, sep="")
  back.interp2sat.file <-
    paste (vam.data.dir, "r", rev, "/background@nscat.r", rev, sep="")
  amb.JPLranked.file <-
    paste (vam.data.dir, "r", rev, "/jpl.winds.r", rev, ".JPLranked",
	   sep="") 
  amb.dual.file <- 
    paste (vam.data.dir, "r", rev, "/jpl.winds.r", rev, ".dual",
	   sep="") 

### Read in GOES-8 data
  ## If read.goes=F, do not read in GOES data and remove existing object
  if (read.goes) {
    gv <- scan (sat.file)
    assign ("goes", make.goes.data (gv), where=1, immediate=T)}
  else {
    rm(goes) 
  }

### Read in VAM and background analyses
  wfv <- scan (anal.file)
  anal <- make.wind.field (wfv)
  assign ("anal", trim.vam.wind.field (anal), where=1, immediate=T)
#Above line changed from trim.wind.field to trim.vam.wind.field
#for use with VAM output 
  wfv <- scan (back.file)
  back <- make.wind.field(wfv)
  assign ("back", trim.vam.wind.field (back), where=1, immediate=T)
#Above line changed from trim.wind.field to trim.vam.wind.field

### Read in VAM analysis interpolated to NSCAT grid
  nv <- scan (anal.interp2sat.file)
  assign ("anal.interp2sat", make.nscat.winds (nv), where=1,
	  immediate=T) 
  nv <- scan (back.interp2sat.file)
  assign ("back.interp2sat", make.nscat.winds (nv), where=1,
	  immediate=T) 

### Read in NSCAT ambiguous winds (ambiguities ordered as in HDF files)
  ambv <- scan(amb.JPLranked.file)
  assign ("amb.JPLranked", make.jpl.winds (ambv), where=1, immediate=T)
  ambv <- scan(amb.dual.file)
  assign ("amb.dual", make.jpl.winds (ambv), where=1, immediate=T)

### Find the closest ambiguites to VAM and background analyses
  assign ("closest.anal", find.closest (amb.JPLranked, anal.interp2sat),
	  where=1, immediate=T)
  assign ("closest.back", find.closest (amb.JPLranked, back.interp2sat),
	  where=1, immediate=T)


### Compute three statistics for ambiguity selection
###  1.) JPL and closest-to-background disagree by X %
###  2.) JPL and closest-to-VAM-analysis disagree by Y %
###  3.) closest-to-background and closest-to-VAM-analysis disagree by Z %
  assign ("match.back", match.amb (amb.JPLranked, closest.back, amb.JPLranked),
	  where=1, immediate=T ) 
  assign ("X", match.back$diff, where=1, immediate=T )
  assign ("match.anal", match.amb (amb.JPLranked, closest.anal, amb.JPLranked),
	  where=1, immediate=T )
  assign ("Y", match.anal$diff, where=1, immediate=T )
  assign ("match.back.anal", match.amb (closest.back, closest.anal, amb.JPLranked),
	  where=1, immediate=T ) 
  assign ("Z", match.back.anal$diff, where=1, immediate=T )


  ("VAM data read successfully.")

}
