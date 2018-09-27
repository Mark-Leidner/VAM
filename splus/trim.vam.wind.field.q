trim.vam.wind.field <- function(wf, date = wf$date, time = wf$time, lognum = wf$lognum, xs = wf$xs, xe
	 = wf$xe, delx = wf$delx, ys = wf$ys, ye = wf$ye, dely = wf$dely, u = 
	wf$u, v = wf$v)
{
#!#   $Id: trim.vam.wind.field.q,v 1.2 2001/05/17 22:30:19 jmh Exp $
#!#   $Log: trim.vam.wind.field.q,v $
#!#   Revision 1.2  2001/05/17 22:30:19  jmh
#!#   Fixed junior error in function definition
#!#
#!#   Revision 1.1  2001/04/16 16:14:22  leidner
#!#   Initial revision
#!#
###
### Purpose: Trims the outer rows and columns from a VAM wind field
###          data object.
###
### Motivation: Make VAM and TAP analyses directly comparable.
###
###
### Change grid descriptions to agree with new grid
	xs.trimmed <- xs + delx
	xe.trimmed <- xe - delx
	ys.trimmed <- ys + dely
	ye.trimmed <- ye - dely	### Strip off outer rows and colunms
	nx <- dim(u)[1]
	ny <- dim(u)[2]
	print(paste("orig dimensions", dim(u)[1], dim(u)[2]))	## u wind field
	u.trimmed <- wf$u[2:(nx - 1), 2:(ny - 1)]	## v wind field
	v.trimmed <- wf$v[2:(nx - 1), 2:(ny - 1)]	### Output:
	list(date = date, time = time, lognum = lognum, xs = xs.trimmed, xe = 
		xe.trimmed, delx = delx, ys = ys.trimmed, ye = ye.trimmed, dely
		 = dely, u = u.trimmed, v = v.trimmed)
}
