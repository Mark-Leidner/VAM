#!#   $Id: make.jpl.winds.q,v 1.3 2005/06/20 17:36:27 leidner Exp $
#!#   $Log: make.jpl.winds.q,v $
#!#   Revision 1.3  2005/06/20 17:36:27  leidner
#!#   added typing statements, and added qc.bm definitions.
#!#
#!#   Revision 1.2  1998/10/30 20:31:22  leidner
#!#   added selected wind index and data weight (<0 means negative sigma0)
#!#
#!#	Revision 1.1  1997/04/24  21:24:42  leidner
#!#	Initial revision
#!#
make.jpl.winds <- function(wov,
	date    =as.integer(wov[1]),
        time    =as.integer(wov[2]),
        lognum  =as.single (wov[3]),
	revs    =as.integer(wov[4:18]),
	nByrev  =as.integer(wov[19:33]),
        n       =as.integer(wov[34]), 
	lat     =as.single (wov[(1:n)+34]),
	lon     =as.single (wov[(1:n)+(n+34)]),
	namb    =as.integer(wov[(1:n)+(2*n+34)]),
	u1      =as.single (wov[(1:n)+(3*n+34)]),
	v1      =as.single (wov[(1:n)+(4*n+34)]),
	mle1    =as.single (wov[(1:n)+(5*n+34)]),
	u2      =as.single (wov[(1:n)+(6*n+34)]),
	v2      =as.single (wov[(1:n)+(7*n+34)]),
	mle2    =as.single (wov[(1:n)+(8*n+34)]),
	u3      =as.single (wov[(1:n)+(9*n+34)]),
	v3      =as.single (wov[(1:n)+(10*n+34)]),
	mle3    =as.single (wov[(1:n)+(11*n+34)]),
	u4      =as.single (wov[(1:n)+(12*n+34)]),
	v4      =as.single (wov[(1:n)+(13*n+34)]),
	mle4    =as.single (wov[(1:n)+(14*n+34)]),
	row     =as.integer(wov[(1:n)+(15*n+34)]),
	col     =as.integer(wov[(1:n)+(16*n+34)]),
	wgt     =as.single (wov[(1:n)+(17*n+34)]),
	selected=as.integer(wov[(1:n)+(18*n+34)]),
        dt      =as.integer(wov[(1:n)+(19*n+34)]),
        u.fgat  =as.single (wov[(1:n)+(20*n+34)]),
        v.fgat  =as.single (wov[(1:n)+(21*n+34)]),
        alpha   =as.single (wov[(1:n)+(22*n+34)]),
        qc.bm1  =as.integer(wov[(1:n)+(23*n+34)]),
        qc.bm2  =as.integer(wov[(1:n)+(24*n+34)]),
        qc.bm3  =as.integer(wov[(1:n)+(25*n+34)]),
        qc.bm4  =as.integer(wov[(1:n)+(26*n+34)]) )
{
# wov is a wind observation vector usually scanned from an ascii file,
# which contains in order the elements of the wind observation object
# NOTE: See /home/nwp/NWP/scatterometer/vam2d.fortran/wja_wrwind.F

#        print(list(nsw.nwvc=n,nByrev=nByrev))
# Replace missing data flags (-999) with NA's
	sel <- u1 == -999.
#	print(list(length.u1.NA=length(sel[sel==T])))
	u1[sel==T] <- NA
	v1[sel==T] <- NA
	mle1[sel==T] <- NA
	sel <- u2 == -999.
#	print(list(length.u2.NA=length(sel[sel==T])))
	u2[sel==T] <- NA
	v2[sel==T] <- NA
	mle2[sel==T] <- NA
	sel <- u3 == -999.
#	print(list(length.u3.NA=length(sel[sel==T])))
	u3[sel==T] <- NA
	v3[sel==T] <- NA
	mle3[sel==T] <- NA
	sel <- u4 == -999.
#	print(list(length.u4.NA=length(sel[sel==T])))
	u4[sel==T] <- NA
	v4[sel==T] <- NA
	mle4[sel==T] <- NA
	sel <- selected == -999.
	selected[sel] <- NA
### Take care of 1-ambiguity cases
	sel <- selected == 0
	selected[sel] <- 1
# Form jpl wind list
 	list(date=date, time=time, revs=revs, nByrev=nByrev,
 		lat=lat, lon=lon, lognum=lognum,
                namb=namb,
                u=u1, u2=u2, u3=u3, u4=u4, v=v1, v2=v2, v3=v3, v4=v4,
	        mle=mle1, mle2=mle2, mle3=mle3, mle4=mle4,
                row=row, col=col, wgt=wgt, sel=selected,
                dt=dt, u.fgat=u.fgat, v.fgat=v.fgat, alpha=alpha,
                qc.bm1=qc.bm1, qc.bm2=qc.bm2, qc.bm3=qc.bm3, qc.bm4=qc.bm4)
      }
