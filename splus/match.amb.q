match.amb <- function (amb1, amb2, jpl.winds,
		       xs=-180, xe=180, ys=-90, ye=90,
		       region='glob', wspd.range=c(0,100))

### $Id: match.amb.q,v 1.3 1999/08/26 16:30:21 jmh Exp $
### $Log: match.amb.q,v $
### Revision 1.3  1999/08/26 16:30:21  jmh
### jmh: Updated to calculate vector wind-speed diff between JPL/VAM ambig
### jmh: Heavily updated version; use with local plot.ESTEC.q in /orchid/jmh/ambig/splus
###
### Revision 1.2  1998/08/02 19:39:56  leidner
### added more region definitions, improved documentation
###
### Revision 1.1  1998/07/31 21:36:57  leidner
### Initial revision
###

###
### Purpose: This function matches ambiguities from two scatterometer
###          winds data objects.
###
### Input: See "Parameters" below.
###
### Method: By default, matching is done globally for all wind vector
###         cells which have more than one retrieved wind.  Regional
###         matching may be specified by
###           1) setting the (lon, lat) coordinates of the lower-left
###              and upper-right corners of a region, (xs, ys) and
###              (xe, ye) respectively, or
###           2) choosing one of the pre-defined regions: SH, NH or
###              tropical.  
###     
###         Also, data may be sub-setted according to JPL ambiguity
###         windspeed.  [TBD: more documentation.]
###
###
### Output: See "Output" below.
###
### Parameters:
###       amb1   scatterometer winds data object 1  
###       amb2   scatterometer winds data object 2
###  jpl.winds   scatterometer winds data with all JPL-retrieved winds
###              stored according to JPL HDF file convention
###   (xs, ys)   lon/lat coordinate of ll corner of region in which
###              ambiguities will be matched
###   (xe, ye)   lon/lat coordinate of ur corner of region in which
###              ambiguities will be matched
###  Region specifiers --
###         SH   logical; match in SH extra-tropics only? (90S-20S)
###         NH   logical; match in NH extra-tropics only? (90N-20N)
###    tropics   logical; match in tropics only? (20S-20N)
###  Wind speed bin specifiers --
### wspd.range   vector of length (2) which gives the min,max wind
###              speed range to consider when matching 
### 
{
### Define valid options
  compare.options   <- c ('JPL/back','JPL/VAM','back/VAM')
  parameter.options <- c ('wspd')
  region.options    <- c ('glob', 'NH', 'SH', 'trop', 'N.Atl')

### Dummy check for inconsistent input scatteromenter winds objects
  ## TBD

### Set region
  if (region == 'SH') ye<-(-20)
  if (region == 'NH') ys<-20
  if (region == 'trop') { ys<-(-20); ye<-20}
  if (region == 'N.Atl') {xs<-(-90); xe<-(-30); ys<-0; ye<-60}

### Make a selection mask from JPL retrieved winds which selects only those points in the
### region of interest and only those wind speeds of interest
  ## Make mask to skip WVC's with no retrieved winds or one retrieved
  ## wind.
  ## NOTICE:: The selection criteria for WVC assumes that WVC's with
  ## less than 2 retrieved winds are to be thrown out.
  jpl.na <- jpl.winds$namb == 0
  one    <- jpl.winds$namb == 1
  skip   <- jpl.na | one
  check  <- jpl.winds$namb  < 2

  ## Check for an unexpected, but possible(?) result.
  ## (If this check is true, selection algorithm above must be changed
  ## to reflect characteristics of JPL-retrieved NSCAT winds data.)
  if (sum(skip) != sum(check)) stop (mess="Stopping in match.amb.q because sum of WVC's with no retrieved winds and one retrieved wind is not equal to the sum of WVC's with less than two retrieved winds.  This result is not expected and the data selection algorithm should be reworked.")

#print(paste("length.jpl.na =", length(jpl.na), "sum.jpl.na =", sum(jpl.na), sep=" "))
#print(paste("length.one    =", length(one),    "sum.one    =", sum(one),    sep=" "))
#print(paste("length.skip   =", length(skip),   "sum.skip   =", sum(skip),   sep=" "))
#print(paste("length.check  =", length(check),  "sum.check  =", sum(check),  sep=" "))

  ## Temporarily set missing u-, and v-components to 99
  ## to avoid trouble with wspd calculation
  jpl.winds$u[jpl.na] <- 99
  jpl.winds$v[jpl.na] <- 99

  ## Calculate wind speed
  jpl.wspd <- uv2sd(jpl.winds$u, jpl.winds$v)$vel

  ## set wind speed at points where no winds retrieved to NA
  jpl.wspd[jpl.na] <- NA

  ## Select ambiguities to match by
  ## . longitude range
  ## . latitude range
  ## . wind speed range
  ## . exclude WVC's with less than 2 retrieved ambiguous winds
  sel <- (xs<amb1$lon & amb1$lon<=xe) &
         (ys<amb1$lat & amb1$lat<=ye) &
         (wspd.range[1]<=jpl.wspd & jpl.wspd<wspd.range[2]) &
         !jpl.na  &
         !one

### Make mask for matching ambiguities   zeneith
  matches <- rep(NA,length(amb1$lat))
  matches[sel] <- amb1$u[sel] == amb2$u[sel] &
                  amb1$v[sel] == amb2$v[sel]

# Calculate vector wind-speed difference between JPL and VAM-selected ambiguities:
        vdiff <- sqrt((amb1$u-amb2$u)**2 + (amb1$v-amb2$v)**2)

  ## Count number of points matched
  ## Mask NA's first
  match.na <- is.na(matches)

#print(paste("length.match.na =", length(match.na), "sum.match.not.na =", (length(match.na) - sum(match.na, na.rm=T)), sep=" "))

  ## Now count number of matching/not-matching points, and compute %
  ## difference 
  match <- sum(matches[!match.na])
  nomatch <- sum(!matches[!match.na])
  total <- match + nomatch
  per <- 100.*nomatch/total

# Output:
#   
#  A list containing :
#
###   mask   list containing a mask for matching ambiguities
###          T  - ambiguites match
###          F  - ambiguites different
###          NA - WVC did not meet selection criteria (i.e., space,
###               time, wspd, data missing, etc.) 
###
###   diff   percent of ambiguities which do not match
###
###   vdiff  magnitudes of vector differences between JPL and VAM ambig. selections

  list(mask=matches, diff=per, vdiff=vdiff)
}



