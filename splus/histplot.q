histplot <- function(x, main = "SSMI Winds Histogram", plot = T,
                   probability = F, binsize = 1, file=NULL,
                   N.obs, outdir = ".", xlab="wind speed (m/s)",
                   range=c(floor(min(x)), ceiling(max(x))),
                   ylim = c(0, as.integer(length(x)/5)),
                   subtitle="subtitle")
{
#     $Id: histplot.q,v 1.2 2005/08/11 18:18:48 jhalland Exp $
#     
#     $Log: histplot.q,v $
#     Revision 1.2  2005/08/11 18:18:48  jhalland
#     Corrected initial version of program to be better suited for a wide
#     variety of plots.
#
#     Revision 1.1  2005/06/14 20:29:49  jhalland
#     Initial version
#
###   Input Parameters
###
###               x  numeric vector of data for the histogram
###            main  name of object used to view object given histograms title
###           title  the title of the histogram
###        subtitle  the subtitle of the histogram
###            plot  logical flag: if TRUE, the histogram will be plotted;
###                  if FALSE, a list giving breakpoints and counts will be
###                  returned.
###     probability  logical flag: if TRUE, the histogram will be scaled
###                  as a probability density, if FALSE, the heights of the
###                  bars will be counts.
###         binsize  width of histogram bins
###            file  Option to write the output to a postscript file.
###                  By default (file=NULL), graphics are displayed in a
###                  motif window.
###          outdir  The file will be written to the output directory
###                  only when a filename is included in the command.
###                  If a filename is given without a directory, the
###                  file will be written to the current directory.
###           range  Controls the length of the x-axis to be between the
###                  minimum value and maximum value of the data vector by
###                  default, but can be set manually.
###            yint  Controls the length of the y-axis to be no more than
###                  a quarter of the total number of data points by default,
###                  but can be set manually.
###            xlab  Designates the label on the x-axis.
###           N.obs  The number of observation points actually ingested
###                  into the VAM analysis
###

###   Output
###   Returns no values when the function is complete unless instructed
###   to create a .ps file.

   make.ps = F   # initial value
   if (!missing(file)) {
      make.ps = T
      if (!missing(outdir)) {
          unix(paste ("mkdir -p ", outdir, sep=""))
      }
      file <- paste (outdir, file, sep="/")

      print(paste('Opening postscript output device:', file, sep=' '))
      postscript(file=file)
   }

   if(ylim[2] > 8000){
     binsize <- 0.25}
   else{
     if(ylim[2] > 6000){
       binsize <- 0.50}
     else{
       if(ylim[2] > 4000){
         binsize <- 0.75}
       else{
         binsize <- 1}
     }
   }

   breaks <- seq(range[1]-binsize, range[2]+binsize, binsize)
   hist(x, main = main, subtitle = subtitle, plot = plot,
        probability = probability, breaks = breaks, xlab=xlab, ylim=ylim)

###   Add a subtitle

   mtext(paste(subtitle), side = 3, line = 1, outer = F, cex = 1.1)

###   Set all lines to be left justified at their location

   par(adj=0)

###   Make a stats box in the upper right corner of the plot
###   containing number of points, mean, std. dev, rms, min and max.

   if(ylim[2] > 8000){
     xpos <- (max(breaks) - min(breaks))*(3/10)}
   else{
     if(ylim[2] > 6000){
       xpos <- (max(breaks) - min(breaks))*(5/10)}
     else{
       if(ylim[2] > 4000){
         xpos <- (max(breaks) - min(breaks))*(6/10)}
       else{
         xpos <- (max(breaks) - min(breaks))*(9/10)}
     }
   }


   mtext("STATISTICS", line = -4, at = xpos)

   numb <- length(x)
   mtext(paste("N = ", numb), line = -6, at = xpos, cex = .9)

   if(missing(N.obs)){

     # Debug Statement
     print(paste("N.obs = numb: ", N.obs))

     stats <- basic1way(x)
     mtext(paste("min = ", round(stats$min, digits = 2)),
                  line = -7, at = xpos, cex = .9)
     mtext(paste("max = ", round(stats$max, digits = 2)),
                  line = -8, at = xpos, cex = .9)
     mtext(paste("mean = ", round(stats$mean, digits = 5)),
                  line = -9, at = xpos, cex = .9)
     mtext(paste("std. dev. = ", round(stats$sd, digits = 2)),
                  line = -10, at = xpos, cex = .9)
     mtext(paste("rms = ", round(stats$rmse, digits = 2)),
                  line = -11, at = xpos, cex = .9)
   }

   if(!missing(N.obs)){

     # Debug Statement
     print(paste("N.obs != numb: ", N.obs))

     stats <- basic1way(x)
     mtext(paste("N.obs = ", N.obs),
                  line = -7, at = xpos, cex = .9)
     mtext(paste("min = ", round(stats$min, digits = 2)),
                  line = -8, at = xpos, cex = .9)
     mtext(paste("max = ", round(stats$max, digits = 2)),
                  line = -9, at = xpos, cex = .9)
     mtext(paste("mean = ", round(stats$mean, digits = 5)),
                  line = -10, at = xpos, cex = .9)
     mtext(paste("std. dev. = ", round(stats$sd, digits = 2)),
                  line = -11, at = xpos, cex = .9)
     mtext(paste("rms = ", round(stats$rmse, digits = 2)),
                  line = -12, at = xpos, cex = .9)
   }


###   Include a timestamp

   timestamp(line=3)

  if (make.ps) {
     print('...closing postscript file.')
     dev.off()
  }

}

 
