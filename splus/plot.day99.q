plot.day99 <-function(day = NA, first = NA, last = NA, batch = NA, missing = NA, xs = -180, 
	xe = 180, ys = -80, ye = 80, plot.matches = T, plot.bg = T, plot.a.d = 
	"both", pch = ".", read.goes = F, plot.goes = F, delln = 10, dellt = 10,
	sat.location = "/smoke/nscat/S8/", vam.location = "/smoke/vam99/S8vam/",
        out.location = "/storm/scratch/jmh/",bg.name="NMC",
        plot.box=F,
        box.coord=list(p1=c(0,0),p2=c(10,0),p3=c(10,10),p4=c(0,10)),
        breaks=c(2,4,8,16),colours=c(3,4,5,6,7,8)
	)
{
# $Id: plot.day99.q,v 1.6 2001/04/24 18:38:04 jmh Exp $
# $Log: plot.day99.q,v $
# Revision 1.6  2001/04/24 18:38:04  jmh
# updated to properly set title info
#
# Revision 1.5  2001/04/20 21:42:45  jmh
# updated to fix slight error that prevented a flexible bg.name from being passed to plot.overview
#
# Revision 1.4  2001/04/20 21:29:42  jmh
# updated to merge a few mods from original write of plot.day99 (1999);
# ignored some, including how exactly read.vam99.datasets is called;
# here, updated to draw a box
#
# Revision 1.3  2001/04/20 15:22:08  jmh
# updated to make domain of default call ys=-80 and ye=80; this shrinks
# vertical size of plot and lets colour.bar actually plot labels on colour bar
#
# Revision 1.2  2001/04/20 14:06:12  jmh
# Updated to fix circle size in plot (accomplished by setting bin.factor=1)
#
# Revision 1.1  2001/04/18 20:46:54  jmh
# Initial revision
#
# Usage:  e.g., plot.day99(first=2453,last=2466,plot.a.d='ascending',missing=c(2456,2458))
#               Note: missing revs must be consecutive and not include first or last rev
#               plot.day99(day=42,plot.a.d='ascending',delln=20,dellt=20)
# plot.a.d options: ascending, descending, full, both (separate plots)
	attach("/home/nwp/NWP/tap.p584/TAPHOME/splus/.Data/")
	attach("/home/nwp/NWP/scatterometer/splus/.Data/")
	options(object.size = 11000000)	# Print info
# This code searches through the five coloured disks on burst in the nscat directory to find NSCAT data (S24, e.g.) and VAM data (S24vam, e.g.) directories.
# The user need only specify the 'day' to print or the range of revs. 
#Find details about rev
# use object named 'day.rev.list' already created by read.day.rev.list.q...
# (updated through batch 28 as of 8/16/99)
# if day is given, find first, last and batch(es)
# if first and last are given, find day(s) and batch(es)
	if(!is.na(day)) {
#retrieve first and last from the one day provided
		first <- min(day.rev.list$first[day.rev.list$day == day])
		last <- max(day.rev.list$last[day.rev.list$day == day])
	}
	if(is.na(first) & is.na(last))
		print("Warning: Update day.rev.list")
	rev.list <- first:last
	if(plot.a.d == "both") plot.a.d <- c("ascending", "descending")	
	#Initialize skip
	skip <- F
        #Initialize all.batches, all.days and all.revs
        all.batches <- NULL 
        all.days <- NULL
        all.revs <- NULL
	for(x in plot.a.d) {
		for(rev in rev.list) {
#Set flag to skip reading of rev data if missing
			if(!is.na(missing) & (rev >= min(missing) & rev <= max(
				missing))) skip <- T
			if(skip == F) print(paste("Processing rev", rev, sep = 
				  " "))	#Find details about rev
# use object named 'day.rev.list' already created by read.day.rev.list.q...
			if(is.na(day)) {
#retrieve day from current rev
				day <- day.rev.list$day[day.rev.list$first <= 
				  rev & day.rev.list$last >= rev]
			}
			batch <- day.rev.list$batch[day.rev.list$first <= rev & 
				day.rev.list$last >= rev]	#Create path names

			print(paste("Data for rev", rev, "in:", sat.location, 
				vam.location, sep = " "))
			print(paste("day", day, sep = " "))
			print(paste("batch", batch, sep = " "))
			print(paste("skip", skip, sep = " "))	

			if(rev == first)
				plot.first <- T
			else plot.first <- F
			if(rev == last)
				plot.last <- T
			else plot.last <- F
			if(skip == F) {
#test
				print(paste(rev, sat.data.dir = sat.location, 
				  vam.data.dir = vam.location, read.goes = 
				  read.goes), sep = ":")
				if(amb.JPLranked$revs[1] != rev)
				  read.vam99.datasets(rev = rev, 	
	# Changed read-in routine to vam99
				  jul = 27, year = 1997, time = 1200, 
				    sat.data.dir = sat.location, vam.data.dir
				     = vam.location, read.goes = read.goes)
				else print(
				    "Skipping read.vam99.datasets...no need to read..."
				    )
        # specify bin.factor=1 to hardwire size of WVC circles in plot.wind.obs
				plot.overview(xs = xs, xe = xe, ys = ys, ye = 
				  ye, make.ps = T, plot.goes = plot.goes, 
				  plot.matches = plot.matches, plot.bg = 
				  plot.bg, plot.a.d = x, plot.last = plot.last, 
				  plot.first = plot.first, rev = rev, day = day,
				  delln = delln, dellt = dellt, pch = pch, 
				  batch = batch, range.revs = c(first, last), 
#				  all.batches = numeric(0), all.days = numeric(
#				  0), all.revs = numeric(0), 
                                  all.batches = all.batches, all.days = all.days,
				  all.revs = all.revs,
                                  skip = 1, skipx = 
				  skip, skipy = skip, thin = 1, barbscale.jpl
				   = NULL, bin.factor = 1, yoff = -0.4, 
				  ythick = 0.11, labsiz = 0.85,
                                  outdir=out.location,bg.name=bg.name,
                                  plot.box=plot.box,
                                  box.coord=box.coord,
                                  breaks=breaks,colours=colours)
			}

        all.days <- c(all.days,day)
        all.batches <- c(all.batches,batch)
        all.revs <- c(all.revs, rev)
        print(paste('all days,batches,revs:',all.days,all.batches,all.revs))
			day <- NA
			skip <- F
		}
	}
}
