plot.plots <- function (vam.dat.direct, vam.ssmi.dat, wind.vectors = T,
                       sub, anal.hist.plot = F, back.hist.plot = F, 
                       aminusb.hist.plot = F, ssmi.hist.plot = F,
                       anal.plot = F, vam.jpl.plot = F, loc.plot = F,
                       ssmi.plot = F, ssmiminusa.hist.plot = F,
                       ssmiminusb.hist.plot = F, 
                       xs.tot=120, xe.tot=220, ys.tot=-4,
                       ye.tot = 76, xs.swth = 140, xe.swth = 210, 
                       xs.zm = 166, xe.zm=176, ys.zm = 30, ye.zm=40,
                       xs.zm2 = 162, xe.zm2=185, ys.zm2=21, ye.zm2=33, 
                       ylim.anal = c(0, as.integer(x.loc/4)),
                       ylim.back = c(0, as.integer(x.loc/4)), 
                       ylim.aminusb = c(0, as.integer(x.loc/2.5)),
                       ylim.ssmi = c(0, as.integer(x.loc/4)),
                       ylim.ssmiminusa = c(0, as.integer(x.loc/2.5)),
                       ylim.ssmiminusb = c(0, as.integer(x.loc/2.5)))
{
#    $Id: plot.plots.q,v 1.1 2005/08/08 20:05:17 jhalland Exp $
#
#    $Log: plot.plots.q,v $
#    Revision 1.1  2005/08/08 20:05:17  jhalland
#    First working version.
#
#
###  Input Parameters
###
###     vam.dat.direct  The location where data output from the VAM run
###                     is stored
###       vam.ssmi.dat  The location where SSMI data can be found for 
###                     velocity magnitude only VAM analysis
###       wind.vectors  A logical, if True the input data contains both 
###                     u and v wind components, if False the input data
###                     contains only the wind magnitude
###                sub  The subtitle that will be plotted on all of the 
###                     histogram plots
###     anal.hist.plot  A logical whether to make an analysis histogram
###     back.hist.plot  A logical whether to make a background histogram
###  aminusb.hist.plot  A logical whether to make an analysis minus
###                     background windspeeds plot
###     ssmi.hist.plot  A logical whether to make a histogram of the obs.
###          ylim.****  Sets the scale for the y-axis in each of the
###                     different histograms
###
###  The purpose of this program is to generate multiple histograms
###  by giving a single command, to help rid the need for redundant
###  single line commands to generate a single plot. Also added, is the
###  ability to make different plots using the 'plot.ssmi', 
###  'plot.all.jpl.winds', 'plot.vam.jpl.winds', and 'plot.analyses' functions
###


if(wind.vectors){

  #  Read the data from the VAM run into splus objects

     read.vam99.datasets(sat.data.dir = NULL, vam.data.dir = vam.dat.direct,
          read.goes = F,
          sat.file = NULL, anal.file = "analysis2", back.file = "background1",
          anal.interp2sat.file = "output-ascii/analysis2@qscat.txt",
          back.interp2sat.file = "output-ascii/background@qscat.txt",
          amb.JPLranked.file = "output-ascii/qscat-JPLranked.txt", amb.dual.file
          = "output-ascii/qscat-qc2.txt")

  #  Scan in the observation data swath locations for plotting of winds

     sv <- scan(paste(vam.ssmi.dat, "output-ascii/qced_Simulated-SSMI", sep=""))
     ssmi <- make.ssmi.data(sv)

  #  Select data that have passed quality control, even if they were thinned

     qcok.select <- amb.dual$qc.bm1 == 0 | amb.dual$qc.bm1 == 32770 | amb.dual$
          qc.bm1 == 1536 | amb.dual$qc.bm1 == 2048 | amb.dual$qc.bm1 == 512 |
          amb.dual$qc.bm1 == 3584 | amb.dual$qc.bm1 == 2560

  #  Select only the data that passed all quality control

     N.obs <- sum(amb.dual$qc.bm1 == 0 | amb.dual$qc.bm1 == 1536 |
                  amb.dual$qc.bm1 == 2048 | amb.dual$qc.bm1 == 512 |
                  amb.dual$qc.bm1 == 3584 | amb.dual$qc.bm1 == 2560)

  #  Create analysis, background and difference objects based upon
  #  the wind.vectors logical input

     
       xdiff <- (anal.interp2sat$u - back.interp2sat$u)^2
       ydiff <- (anal.interp2sat$v - back.interp2sat$v)^2
       velm <- (xdiff + ydiff)^0.5
       aminusb <- ssmi
       aminusb$velm <- velm
       print(paste("Anal minus Back Loc. Plotted = ", sum(qcok.select)))     
 
       back.velm <- ((back.interp2sat$u)^2 + (back.interp2sat$v)^2)^0.5
       back.qscat <- ssmi
       back.qscat$velm <- back.velm
       print(paste("Back Loc. Plotted = ", sum(qcok.select)))
 
       anal.velm <- ((anal.interp2sat$u)^2 + (anal.interp2sat$v)^2)^0.5
       anal.qscat <- ssmi
       anal.qscat$velm <- anal.velm
       print(paste("Anal Loc. Plotted = ", sum(qcok.select)))

       diff <- ((closest.back$u - back.interp2sat$u)^2 +
                (closest.back$v - back.interp2sat$v)^2)^0.5
       qscatminusb <- ssmi
       qscatminusb$velm <- diff

       diff <- ((closest.anal$u - anal.interp2sat$u)^2 +
                (closest.anal$v - anal.interp2sat$v)^2)^0.5
       qscatminusa <- ssmi
       qscatminusa$velm <- diff
}

else{

  #  Read the data from the VAM run into splus objects

     read.vam99.datasets(sat.data.dir = NULL, vam.data.dir = vam.dat.direct,
        read.goes = F, sat.file =
        NULL, anal.file = "analysis1", back.file = "background1",
        anal.interp2sat.file = NULL, back.interp2sat.file = NULL,
        amb.JPLranked.file = "output-ascii/qced_qscat.txt", amb.dual.file =
        NULL)

  #  Select data that have passed quality control, even if they were thinned
 
     qcok.select <- amb.JPLranked$qc.bm1 == 0 | amb.JPLranked$qc.bm1 == 16386

  #  Select only the data that have passed all quality control flags

     N.obs <- sum(amb.JPLranked$qc.bm1 == 0)

  #  Create analysis, background and difference objects based upon
  #  the wind.vectors logical input

     sv <- scan(paste(vam.dat.direct, "output-ascii/qced_Simulated-SSMI",
                      sep=""))
     ssmi <- make.ssmi.data(sv)

     sv <- scan(paste(vam.dat.direct, "output-ascii/background@SSMI", sep=""))
     back.ssmi <- make.ssmi.interp.data(sv)

     sv <- scan(paste(vam.dat.direct, "output-ascii/anal@SSMI", sep=""))
     anal.ssmi <- make.ssmi.interp.data(sv)

     diff <- anal.ssmi$velm - back.ssmi$velm
       aminusb <- anal.ssmi
       aminusb$velm <- diff
       
     diff <- ssmi$velm - anal.ssmi$velm
       ssmiminusa <- anal.ssmi
       ssmiminusa$velm <- diff

     diff <- ssmi$velm - back.ssmi$velm
       ssmiminusb <- anal.ssmi
       ssmiminusb$velm <- diff
}

#  Calculate the number of total locations

   x.loc <- sum(qcok.select)

#  Create the selection criteria that will select from all of the obs

   Numb <- length(amb.JPLranked$lat)
   my.select <- rep(T, Numb)

#  Make Selected histogram plots

   if(anal.hist.plot){

     if(wind.vectors){
       histplot(anal.qscat$velm[qcok.select], file = "hist.qc.anal.qscat.ps",
                outdir= paste(vam.dat.direct, "ps", sep=""), main =
                "QCed VAM Analysis Vector Windspeeds Histogram", subtitle = sub,
                ylim = ylim.anal, N.obs=N.obs)
     }
     else{
       
       histplot(anal.ssmi$velm[qcok.select], file = "hist.qc.anal.ssmi.ps", 
                outdir = paste(vam.dat.direct, "ps", sep=""),
                main = "QCed VAM Analysis Windspeeds Histogram", 
                subtitle = sub, ylim = ylim.anal, N.obs=N.obs) 
     }
   }

   if(back.hist.plot){

     if(wind.vectors){

       histplot(back.qscat$velm[qcok.select], file = "hist.qc.back.qscat.ps", 
                outdir = paste(vam.dat.direct, "ps", sep=""), main =
                "QCed Background Vector Windspeeds Histogram", subtitle = sub,
                ylim = ylim.back, N.obs=N.obs)
     }
     else{

       histplot(back.ssmi$velm[qcok.select], file = "hist.qc.back.ssmi.ps", 
                outdir = paste(vam.dat.direct, "ps", sep=""), main =
                "QCed Background Windspeeds Histogram",
                subtitle = sub, ylim = ylim.back, N.obs=N.obs)
     }
   }

   if(aminusb.hist.plot){

     if(wind.vectors){

       histplot(aminusb$velm[qcok.select], file = "hist.qc.aminusb.ps", 
                outdir = paste(vam.dat.direct, "ps", sep=""), main =
               "QCed VAM Analysis Minus Background Vector Windspeeds Histogram",
                subtitle = sub, ylim = ylim.aminusb, N.obs=N.obs)   
     }
     else{

       histplot(aminusb$velm[qcok.select], file = "hist.qc.aminusb.ps", 
                outdir = paste(vam.dat.direct, "ps", sep=""), main =
                "QCed VAM Analysis Minus Background Windspeeds Histogram",
                subtitle = sub, ylim = ylim.aminusb, N.obs=N.obs)
     }
   }  

   if(ssmiminusa.hist.plot){

     if(wind.vectors){

       histplot(qscatminusa$velm[qcok.select], file = "hist.qc.qscatminusa.ps",
                outdir = paste(vam.dat.direct, "ps", sep=""), main =
               "QCed QSCAT Minus VAM Analysis Vector Windspeeds Histogram",
                subtitle = sub, ylim = ylim.ssmiminusa, N.obs = N.obs)
     }
     else{

       histplot(ssmiminusa$velm[qcok.select], file = "hist.qc.ssmiminusa.ps",
                outdir = paste(vam.dat.direct, "ps", sep=""), main =
                "QCed SSMI Minus VAM Analysis Windspeeds Histogram",
                subtitle = sub, ylim = ylim.ssmiminusa, N.obs = N.obs)
     }
   }

   if(ssmiminusb.hist.plot){

     if(wind.vectors){

       histplot(qscatminusb$velm[qcok.select], file = "hist.qc.qscatminusb.ps",
                outdir = paste(vam.dat.direct, "ps", sep=""), main =
               "QCed QSCAT Minus Background Vector Windspeeds Histogram",
                subtitle = sub, ylim = ylim.ssmiminusb, N.obs = N.obs)
     }
     else{

       histplot(ssmiminusb$velm[qcok.select], file = "hist.qc.ssmiminusb.ps",
                outdir = paste(vam.dat.direct, "ps", sep=""), main =
                "QCed SSMI Minus Background Windspeeds Histogram",
                subtitle = sub, ylim = ylim.ssmiminusb, N.obs = N.obs)
     }
   }

   if(ssmi.hist.plot){

     histplot(ssmi$velm[qcok.select], file = "hist.qc.ssmi.ps",
              outdir = paste(vam.dat.direct, "ps", sep=""), main =
              "QCed SSMI Windspeeds Histogram",
              subtitle = sub, ylim = ylim.ssmi, N.obs=N.obs)
   }


#  Make Desired Observation and Analysis Plots

   if(anal.plot){

     plot.analyses(plot.vam = F, plot.anal = T, plot.back = F, plot.scat = F, 
                   xs = xs.tot, xe = xe.tot, ys = ys.tot, ye = ye.tot, 
                   dellt = 5, skip = 10, amb = NULL, file = "anal.ps",
                   title = "VAM Analysis Winds", sub = sub, delln=5,
                   outdir = paste(vam.dat.direct, "ps", sep=""), plot.image = T)

     plot.analyses(plot.vam = F, plot.anal = T, plot.back = T, plot.scat = F, 
                   xs = xs.tot, xe = xe.tot, ys =ys.tot, ye=ye.tot, delln = 5, 
                   dellt = 5, amb = NULL, file = "anal+back.ps", plot.image = F,
                   title ="VAM Analysis (black) and Background (red) Winds",
                   sub = sub, skip = 5, outdir = paste(vam.dat.direct, "ps",
                   sep = ""))

     plot.analyses(plot.vam = F, plot.anal = T, plot.back = T, plot.scat = F, 
                   xs = xs.zm, xe = xe.zm, ys = ys.zm, ye = ye.zm, delln = 2, 
                   dellt = 2, amb = NULL, file = "anal+back-zoom.ps", 
                   plot.image=F, drawmap=F, sub = sub, skip = 1, title =
                   "VAM Analysis (black) and Background (red) Winds",
                   outdir = paste(vam.dat.direct, "ps", sep=""),
                   horizontal=F)
   }


   if(vam.jpl.plot && wind.vectors){

       lons.360 <- ifelse(amb.dual$lon < 0, amb.dual$lon + 360, amb.dual$lon)

       qcok.select <- amb.dual$qc.bm1 == 0 | amb.dual$qc.bm1 == 512 | 
                      amb.dual$qc.bm1 == 2048 | amb.dual$qc.bm1 == 1536 |
                      amb.dual$qc.bm1 == 2560 | amb.dual$qc.bm1 == 3584

       qcok.zoom.select <- amb.dual$qc.bm1 == 0 | amb.dual$qc.bm1 == 512 |
                           amb.dual$qc.bm1 == 2048 | amb.dual$qc.bm1 == 1536 |
                           amb.dual$qc.bm1 == 2560 | amb.dual$qc.bm1 == 3584 & 
                           (xs.zm <= lons.360 & lons.360 <= xe.zm) & 
                           (ys.zm <=amb.dual$lat & amb.dual$lat <= ye.zm)

       qcok.zoom2.select <- amb.dual$qc.bm1 == 0 | amb.dual$qc.bm1 == 512 |
                            amb.dual$qc.bm1 == 2048 | amb.dual$qc.bm1 == 1536 |
                            amb.dual$qc.bm1 == 2560 | amb.dual$qc.bm1 == 3584 &
                            (xs.zm2 <= lons.360 & lons.360 <= xe.zm2) &
                            (ys.zm2 <=amb.dual$lat & amb.dual$lat <= ye.zm2)

       thin.select <- amb.dual$qc.bm1 != 32770 & amb.dual$qc.bm1 != 32774 &
                      amb.dual$qc.bm1 != 32834 & amb.dual$qc.bm1 != 32850 &
                      amb.dual$qc.bm1 != 32786 & amb.dual$qc.bm1 != 32790 &
                      amb.dual$qc.bm1 != 40978 & amb.dual$qc.bm1 != 40962

     print(paste("Vam.jpl.plot:"))
     print(paste("Debug: sum(qcok.select) = ", sum(qcok.select)))
     print(paste("Debug: sum(thin.select) = ", sum(thin.select)))


## TEST##

     plot.vam.jpl.amb(jpl = amb.JPLranked, vam = closest.anal, dellt = 10,
                      delln=10, xs=xs.swth, xe=xe.swth, ys=ys.tot, ye=ye.tot,
                      sub = sub, drawmap = T, file = paste(vam.dat.direct,
                      "ps/test.ps", sep=""), title = "VAM Analysis (red) and JPL (blue) Ambiguity Selections After Thinning",
                      horizontal = F, plot.vam = T, plot.jpl = T, plot.bg = T,
                      thin = 1, vam.data.dir = vam.dat.direct, outdir = "",
                      colours = c(3, 4), my.select = thin.select)


  #  Plot Number 1

     plot.vam.jpl.amb(jpl = amb.JPLranked, vam = closest.anal, dellt = 10, 
                      delln=10, xs=xs.swth, xe=xe.swth, ys=ys.tot, ye=ye.tot,
                      sub = sub, drawmap = T, file = paste(vam.dat.direct,
                      "ps/vam.jpl.sel.ps", sep=""), title = "VAM Analysis (red) and JPL (blue) Ambiguity Selections After Thinning",
                      horizontal = F, plot.vam = T, plot.jpl = T, plot.bg = T,
                      thin = 1, vam.data.dir = vam.dat.direct, outdir = "",
                      colours = c(3, 4), my.select = thin.select)


  #  Plot Number 2

     plot.vam.jpl.amb(jpl = amb.JPLranked, vam = closest.anal, dellt = 10,
                      delln=10, xs=xs.swth, xe=xe.swth, ys=ys.tot, ye=ye.tot,
                      sub = sub, drawmap = T, file = paste(vam.dat.direct,
                      "ps/qc.vam.jpl.sel.ps",sep=""), title = "VAM Analysis (red) and JPL (blue) Ambiguity Select. After Thin & QC",
                      horizontal = F, plot.vam = T, plot.jpl = T, plot.bg = T,
                      thin = 1, vam.data.dir = vam.dat.direct, outdir= "",
                      colours = c(3, 4), my.select = qcok.select)


  # Plot Number 3

     plot.vam.jpl.amb(jpl = amb.JPLranked, vam = closest.anal, dellt = 2, 
                      delln = 2, xs =xs.zm, xe =xe.zm, ys = ys.zm, ye = ye.zm,
                      sub = sub, drawmap = F, file = paste(vam.dat.direct,
                      "ps/qc.vam.jpl.sel-zoom.ps", sep=""), title = "VAM Analysis (red) and JPL (blue) Ambiguity Select. After Thin & QC",
                      horizontal = F, plot.vam = T, plot.jpl = T, plot.bg = T,
                      thin = 1, vam.data.dir = vam.dat.direct, outdir = "",
                      colours = c(3, 4), my.select = qcok.zoom.select)


  # Plot Number 4

     plot.vam.jpl.amb(jpl = amb.JPLranked, vam = closest.anal, dellt = 2,
                      delln = 2, xs=xs.zm2, xe=xe.zm2, ys=ys.zm2, ye=ye.zm2, 
                      sub = sub, drawmap = F, file = paste(vam.dat.direct,
                      "ps/qc.vam.jpl.sel-zoom2.ps", sep=""), title = "VAM Analysis (red) and JPL (blue) Ambiguity Selections After Thinning & QC",
                      horizontal = T, plot.vam = T, plot.jpl = T, plot.bg = T,
                      thin = 1, vam.data.dir = vam.dat.direct, outdir = "",
                      colours = c(3, 4), my.select = qcok.zoom2.select)


  # Plot Number 5

     plot.all.jpl.amb(jpl = amb.JPLranked, dellt = 2, delln = 2, matches =
                      match.anal$mask, plot.bg = T, xs = xs.zm, xe = xe.zm, 
                      ys =ys.zm, ye=ye.zm, sub = sub, make.ps = T, drawmap = F,
                      file = paste(vam.dat.direct, "ps/jpl.amb-zoom.ps", 
                      sep=""), vam.data.dir = vam.dat.direct, thin = 1, 
                      outdir = paste(vam.dat.direct, "ps", sep=""), colours
                      = c(1, 2, 3, 4), plotl = c(T, T, T, T), title =
                     "All JPL Ambiguities: 1st(blk) 2nd(grn) 3rd(red) 4th(blu)")
  }

   if(wind.vectors){
     if(loc.plot){

       locations(x = amb.dual, y = closest.anal, qual.cont = F, 
                 file = paste(vam.dat.direct, "sel.obs.txt", sep=""))

       locations(x = amb.dual, y = closest.anal, qual.cont = T, 
                 file = paste(vam.dat.direct, "sel.qc.obs.txt", sep=""))
     }
   }
  
   

   if(ssmi.plot){

     if(wind.vectors){

       qcok.select <- amb.dual$qc.bm1 == 0 | amb.dual$qc.bm1 == 512 |
                      amb.dual$qc.bm1 == 2048 | amb.dual$qc.bm1 == 1536 |
                      amb.dual$qc.bm1 == 2560 | amb.dual$qc.bm1 == 3584

       thin.select <- amb.dual$qc.bm1 != 32770 & amb.dual$qc.bm1 != 32774 &
                      amb.dual$qc.bm1 != 32834 & amb.dual$qc.bm1 != 32850 &
                      amb.dual$qc.bm1 != 32786 & amb.dual$qc.bm1 != 32790 &
                      amb.dual$qc.bm1 != 40978 & amb.dual$qc.bm1 != 40962
     }
     else{

       qcok.select <- amb.JPLranked$qc.bm1 == 0

       thin.select <- amb.JPLranked$qc.bm1 == 16448 |
                      amb.JPLranked$qc.bm1 == 0 |
                      amb.JPLranked$qc.bm1 == 16404 |
                      amb.JPLranked$qc.bm1 == 16388 |
                      amb.JPLranked$qc.bm1 == 16400 |
                      amb.JPLranked$qc.bm1 == 24576 |
                      amb.JPLranked$qc.bm1 == 24592
     }


     plot.ssmi(ssmi, plot.velm = T, outdir = paste(vam.dat.direct, "ps",
               sep=""), title = "SSMI Winds Selected From Thinning",
               sub = sub, xs = xs.swth, xe = xe.swth, ys=ys.tot, ye=ye.tot, 
               my.select = thin.select, file = "ssmi.ps")

     plot.ssmi(ssmi, plot.velm = T, outdir = paste(vam.dat.direct, "ps",
               sep=""), title ="SSMI Winds Selected From Thinning and QC",
               sub = sub, xs = xs.swth, xe = xe.swth, ys = ys.tot, ye = ye.tot,
               my.select = qcok.select, file = "qc.ssmi.ps")

     if(wind.vectors){

       plot.ssmi(anal.qscat, plot.velm = T, outdir = paste(vam.dat.direct,
                 "ps", sep=""), sub = sub, xs = xs.swth, xe =xe.swth, ys=ys.tot,
                 title ="VAM Analysis Vector Winds At SSMI Locations",
                 ye = ye.tot, my.select = my.select, file = "anal.ssmi.ps")

       plot.ssmi(back.qscat, plot.velm = T, outdir = paste(vam.dat.direct,
                 "ps", sep=""), sub = sub, xs =xs.swth, xe =xe.swth, ys =ys.tot,
                 ye=ye.tot, title ="Background Vector Winds At SSMI Locations",
                 my.select = my.select, file = "back.ssmi.ps")

       plot.ssmi(aminusb, plot.velm = T, outdir = paste(vam.dat.direct, "ps",
                 sep=""), sub = sub, diff = 2, title =
                 "VAM Analysis Minus Background Vector Winds At SSMI Locations",
                 xs = xs.swth, xe = xe.swth, ys = ys.tot, ye = ye.tot,
                 my.select = my.select, file = "aminusb.ps", zlim=c(0,10))

       postscript(file=paste(vam.dat.direct, "ps/aminusb.overlay.ps",
                  sep=""), horiz=F)
       plot.ssmi(aminusb, plot.velm = T, sub = sub, diff = 2, title =
                 "VAM Analysis Minus Background Vector Winds w/ Selected Obs",
                 xs = xs.swth, xe = xe.swth, ys = ys.tot, ye = ye.tot,
                 my.select = my.select, zlim=c(0,10), make.ps = F)
       plot.ssmi(ssmi, plot.velm = T, sub = sub, diff = 1, title =
                 "VAM Analysis Minus Background Vector Winds w/ Selected Obs",
                 xs = xs.swth, xe = xe.swth, ys = ys.tot, ye = ye.tot,
                 my.select = thin.select, make.ps = F,
                 plot.bg = F, plot.legend = F, nseg=0)
       dev.off()


     }

     else{

       plot.ssmi(anal.ssmi, plot.velm = T, outdir = paste(vam.dat.direct,
                 "ps", sep=""), title ="VAM Analysis Winds At SSMI Locations",
                 sub = sub, xs = xs.swth, xe =xe.swth, ys = ys.tot, ye=ye.tot,
                 my.select = my.select, file = "anal.ssmi.ps")

       plot.ssmi(back.ssmi, plot.velm = T, outdir = paste(vam.dat.direct,
                 "ps", sep=""), sub = sub, xs=xs.swth, xe=xe.swth, ys=ys.tot,
                 ye =ye.tot, title ="Background Winds At SSMI Locations",
                 my.select = my.select, file = "back.ssmi.ps")

       plot.ssmi(aminusb, plot.velm = T, outdir = paste(vam.dat.direct, "ps",
                 sep=""), sub = sub, diff = 1, title =
                 "VAM Analysis Minus Background Winds At SSMI Locations",
                 xs = xs.swth, xe = xe.swth, ys = ys.tot, ye = ye.tot,
                 my.select = my.select, file = "aminusb.ps", zlim = c(-6, 6))


       postscript(file=paste(vam.dat.direct, "ps/aminusb.overlay.ps",
                  sep=""), horiz=F)
       plot.ssmi(aminusb, plot.velm = T, sub = sub, diff = 1, title =
                 "VAM Analysis Minus Background Vector Winds w/ Selected Obs",
                 xs = xs.swth, xe = xe.swth, ys = ys.tot, ye = ye.tot,
                 my.select = my.select, zlim=c(-6,6), make.ps = F)
       plot.ssmi(ssmi, plot.velm = T, sub = sub, diff = 0, title =
                 "VAM Analysis Minus Background Vector Winds w/ Selected Obs",
                 xs = xs.swth, xe = xe.swth, ys = ys.tot, ye = ye.tot,
                 my.select = thin.select, make.ps = F,
                 plot.bg = F, plot.legend = F, nseg=0)
       dev.off()


     }
   }    

}
