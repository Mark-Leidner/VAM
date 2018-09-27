locations <- function (x, y, file, qual.cont=T)
{
#
# $Id: locations.q,v 1.2 2005/08/08 20:03:38 jhalland Exp $
# $Log: locations.q,v $
# Revision 1.2  2005/08/08 20:03:38  jhalland
# First working version was not fully functional.  Added ability to
# write-out quality control logical.
#
# Revision 1.1  2005/07/14 18:51:21  jhalland
# First working version.
#
#

###  Input Parameters
###
###      x / all.amb  An ascii object produced by the VAM analysis that
###                   contains all of the QSCAT ambiguities for both u,v wind
###                   components / that have passed quality control
###      y / closest  An ascii object produced by the VAM analysis that
###                   contains the QSCAT observations that are closest to
###                   the VAM Analysis / that have passed quality control    
###  vam.sel.indices  An object that contains ambiguity number selected 
###                   by the VAM analysis
###             cols  An object that contains all of the column numbers
###                   of the WVCs in the particular data swath
###             rows  An object that contains all of the row numbers of
###                   the WVCs in the particular data swath
###      qcok.select  An object that identifies all points passing QC
###             file  The filename for the output text file
###        qual.cont  A logical that determines whether the entire observation
###                   set will be selected, e.g. "qual.cont = F", or only
###                   obs. passing quality control will be selected, 
###                   e.g. "qual. cont = T"
###      thin.select  An object that identifies all of the locations remaining 
###                   after thinning during preprocessing
###         thin.par  An object that displays the thinning parameter
###                   used during preprocessing
###


#  Store the date that the data was observed on to an object

   date <- x$date

#  Store the time that the data was observed at into an object

   time <- x$time

#  Select all of the points that have not been flagged by thinning para.
#  TBD: This step could be automated to calculate and select the
#       bitmap values that need to be used for the specified task.

   thin.select <- x$qc.bm1 != 2 & x$qc.bm1 != 6 & 
                  x$qc.bm1 != 18 & x$qc.bm1 != 22 & 
                  x$qc.bm1 != 66 & x$qc.bm1 != 70 &
                  x$qc.bm1 != 82 & x$qc.bm1 != 86 &
                  x$qc.bm1 != 514 & x$qc.bm1 != 2050 &
                  x$qc.bm1 != 1538 & x$qc.bm1 != 2562 &
                  x$qc.bm1 != 3586 & x$qc.bm1 != 16386 &
                  x$qc.bm1 != 16390 & x$qc.bm1 != 16402 &
                  x$qc.bm1 != 16406 & x$qc.bm1 != 16450 &
                  x$qc.bm1 != 16466 & x$qc.bm1 != 24578 &
                  x$qc.bm1 != 24594 & x$qc.bm1 != 32770 &
                  x$qc.bm1 != 32774 & x$qc.bm1 != 32786 &
                  x$qc.bm1 != 32790 & x$qc.bm1 != 32834 &
                  x$qc.bm1 != 32850 & x$qc.bm1 != 40962 &
                  x$qc.bm1 != 40978

   print(paste("debug: thin.select = ", sum(thin.select)))


#  Store the number of columns into an object

   cols <- y$col[thin.select]

#  Store the number of rows into an object

   rows <- y$row[thin.select]


#  Calculate the thinning parameter used, e.g. every 2 or 3....

   sq.thin.par <- 1/(sum(thin.select)/length(x$lat))
   thin.par <- sqrt(sq.thin.par)

   f.thin.par <- floor(thin.par)
   tmf.thin.par <- thin.par - f.thin.par

   if(tmf.thin.par >= 0.5){
      int.thin.par <- ceiling(thin.par)}
   else{
      int.thin.par <- floor(thin.par)}
     

#  Store the values for the data that has not been thinned
#  into the appropriate names of the objects

   {if (missing(x)) warning("Missing object containing ambiguities")}
   {if (missing(y)) warning("Missing object containing closest
                                   ambiguities")}

#  For all-ambiguities object

   all.amb <- x
   all.amb$lat <- x$lat[thin.select]
   all.amb$lon <- x$lon[thin.select]
   all.amb$namb <- x$namb[thin.select]
   all.amb$u <- x$u[thin.select]
   all.amb$u2 <- x$u2[thin.select]
   all.amb$u3 <- x$u3[thin.select]
   all.amb$u4 <- x$u4[thin.select]
   all.amb$v <- x$v[thin.select]
   all.amb$v2 <- x$v2[thin.select]
   all.amb$v3 <- x$v3[thin.select]
   all.amb$v4 <- x$v4[thin.select]
   all.amb$mle <- x$mle[thin.select]
   all.amb$mle2 <- x$mle2[thin.select]
   all.amb$mle3 <- x$mle3[thin.select]
   all.amb$mle4 <- x$mle4[thin.select]
   all.amb$row <- x$row[thin.select]
   all.amb$col <- x$col[thin.select]
   all.amb$wgt <- x$wgt[thin.select]
   all.amb$sel <- x$sel[thin.select]
   all.amb$dt <- x$dt[thin.select]
   all.amb$u.fgat <- x$u.fgat[thin.select]
   all.amb$v.fgat <- x$v.fgat[thin.select]
   all.amb$alpha <- x$alpha[thin.select]
   all.amb$qc.bm1 <- x$qc.bm1[thin.select]
   all.amb$qc.bm2 <- x$qc.bm2[thin.select]
   all.amb$qc.bm3 <- x$qc.bm3[thin.select]
   all.amb$qc.bm4 <- x$qc.bm4[thin.select]

#  For closest-ambiguities object

   closest <- y
   closest$lat <- y$lat[thin.select]
   closest$lon <- y$lon[thin.select]
   closest$u <- y$u[thin.select]
   closest$v <- y$v[thin.select]
   closest$row <- y$row[thin.select]
   closest$col <- y$col[thin.select]


if(qual.cont){

   #  Create a selection that will only pass through observations
   #  that have passed quality control.
   #  TBD: This step could be automated to calculate and select the
   #       bitmap values that need to be used for the specified task.

      qcok.select <- x$qc.bm1 == 0 | x$qc.bm1 == 512 | 
                     x$qc.bm1 == 2048 | x$qc.bm1 == 1536 | 
                     x$qc.bm1 == 2560 | x$qc.bm1 == 3584

      print(paste("debug: qcok.select = ", sum(qcok.select)))

   #  Store the number of columns into an object

      cols <- y$col[qcok.select]

   #  Store the number of rows into an object

      rows <- y$row[qcok.select]

   #  Store the date that the data was observed on to an object

      date <- x$date

   #  Store the time that the data was observed at into an object

      time <- x$time

   #  Store the values for the data that has passed quality control
   #  into the appropriate names of the objects

      {if (missing(x)) warning("Missing object containing ambiguities")}
      {if (missing(y)) warning("Missing object containing closest 
                                ambiguities")}

   #  For all-ambiguities object

      all.amb <- x
      all.amb$lat <- x$lat[qcok.select]
      all.amb$lon <- x$lon[qcok.select]
      all.amb$namb <- x$namb[qcok.select]
      all.amb$u <- x$u[qcok.select]
      all.amb$u2 <- x$u2[qcok.select]
      all.amb$u3 <- x$u3[qcok.select]
      all.amb$u4 <- x$u4[qcok.select]
      all.amb$v <- x$v[qcok.select]
      all.amb$v2 <- x$v2[qcok.select]
      all.amb$v3 <- x$v3[qcok.select]
      all.amb$v4 <- x$v4[qcok.select]
      all.amb$mle <- x$mle[qcok.select]
      all.amb$mle2 <- x$mle2[qcok.select]
      all.amb$mle3 <- x$mle3[qcok.select]
      all.amb$mle4 <- x$mle4[qcok.select]
      all.amb$row <- x$row[qcok.select]
      all.amb$col <- x$col[qcok.select]
      all.amb$wgt <- x$wgt[qcok.select]
      all.amb$sel <- x$sel[qcok.select]
      all.amb$dt <- x$dt[qcok.select]
      all.amb$u.fgat <- x$u.fgat[qcok.select]
      all.amb$v.fgat <- x$v.fgat[qcok.select]
      all.amb$alpha <- x$alpha[qcok.select]
      all.amb$qc.bm1 <- x$qc.bm1[qcok.select]
      all.amb$qc.bm2 <- x$qc.bm2[qcok.select]
      all.amb$qc.bm3 <- x$qc.bm3[qcok.select]
      all.amb$qc.bm4 <- x$qc.bm4[qcok.select]

   #  For closest-ambiguities object

      closest <- y
      closest$lat <- y$lat[qcok.select]
      closest$lon <- y$lon[qcok.select]
      closest$u <- y$u[qcok.select]
      closest$v <- y$v[qcok.select]
      closest$row <- y$row[qcok.select]
      closest$col <- y$col[qcok.select]

   }


#  Select which ambiguity has been chosen by comparing the values
#  for 'closest' and 'all.amb'

   thresh <- 0.002
   match1 <- abs(all.amb$u - closest$u) < thresh &
             abs(all.amb$v - closest$v) < thresh
   match2 <- abs(all.amb$u2 - closest$u) < thresh &
             abs(all.amb$v2 - closest$v) < thresh
   match3 <- abs(all.amb$u3 - closest$u) < thresh &
             abs(all.amb$v3 - closest$v) < thresh
   match4 <- abs(all.amb$u4 - closest$u) < thresh &
             abs(all.amb$v4 - closest$v) < thresh


#  Check to make sure the total number of selections is equal to the
#  number of obs

   s.num <- sum(match1, na.rm=T) + sum(match2, na.rm=T) + 
            sum(match3, na.rm=T) + sum(match4, na.rm=T)

   print(paste("match1 = ", sum(match1, na.rm=T)))
   print(paste("match2 = ", sum(match2, na.rm=T)))
   print(paste("match3 = ", sum(match3, na.rm=T)))
   print(paste("match4 = ", sum(match4, na.rm=T)))
   print(paste("The total number of selected matches = ", s.num))

   N <- length(closest$lat)

   {if(s.num != N) warning("The number of matches does not equal the 
                           number of observations!!")}


#  Create an object that contains which ambiguity was selected at each 
#  observation

   vam.sel.indices <- rep(0, N)
   vam.sel.indices[match1] <- 1
   vam.sel.indices[match2] <- 2
   vam.sel.indices[match3] <- 3
   vam.sel.indices[match4] <- 4

   zero <- vam.sel.indices == 0
   if (sum(zero) != 0){
     print(paste("debug: vam.sel.indices that are 0 is ", sum(zero)))
   }

#  Create an array of the objects that are to be printed out to file
#  in the steps to come

   if(qual.cont){
     selected <- c('date:','time:','thin:','QCed:','','','row','',row=rows,
                   date,time,int.thin.par,'T','','','col','', col=cols,
                   '','','','','','','sel','', sel=vam.sel.indices)


   #  Create a matrix of dimensions 'rows' number of rows by 'cols' number
   #  of columns.

      N <- N + 8
      selected.m <- matrix(selected, nrow=N, byrow=F)


   #  Now export the data to a text file

      exportData(selected.m, file, type = "FASCII", quote = T,
                  format="%8,%8,%8")}
   
   else{
      selected <- c('date:','time:','thin:','QCed:','','','row','',row=rows,
                    date,time,int.thin.par,'F','','','col','', col=cols,
                    '','','','','','','sel','', sel=vam.sel.indices)


   #  Create a matrix of dimensions 'rows' number of rows by 'cols' number
   #  of columns.

      N <- N + 8
      selected.m <- matrix(selected, nrow=N, byrow=F)


   #  Now export the data to a text file

      exportData(selected.m, file, type = "FASCII", quote = T,
                 format="%8,%8,%8")}

   
}

