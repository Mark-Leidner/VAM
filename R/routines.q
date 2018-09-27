
evalNA <- function (x, fun, ...)
{
# From Id: miscellaneous.q,v 1.2 2005/04/27 14:08:00 trn Exp
# Evaluate fun(x) only on those elements of x which are not NA.  
# Option arguments can be passed in.  x should be a vector.
	fun(x[!is.na(x)], ...)
}

aspect <- function ( a0 ) {
#!# $Id: routines.q,v 1.1 2009/10/20 20:00:39 rnh Exp $
# set the plot area aspect ratio
    wh <- par( "pin" )
    w <- wh[1]
    h <- wh[2]
    a <- w/h
# if the current aspect ratio is too small, the current height is too large
    if ( a < a0 ) wh<-c(w,w/a0)
# if the current aspect ratio is too large, the current width is too large
    if ( a > a0 ) wh<-c(a0*h,h)
    par ( pin = wh )
# return the new values
    list ( pin = wh )
}
