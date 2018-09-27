rename.vam99.datasets <- function(new.suffix, old.suffix=NULL, 
				  names=
c("anal", "back", "anal.interp2sat", "back.interp2sat", "amb.JPLranked", "amb.dual", "closest.anal", "closest.back", "match.back", "X", "match.anal", "Y", "match.back.anal", "Z")
 )
{
#!# $Id: rename.vam99.datasets.q,v 1.1 1999/09/22 16:42:42 trn Exp $
#!# Rename all datasets created by read.vam99.datasets
#!# $Log: rename.vam99.datasets.q,v $
#!# Revision 1.1  1999/09/22 16:42:42  trn
#!# Initial revision
#!#
if (!is.null(old.suffix)) 
  old.names <- paste(names,old.suffix,sep='.') 
else
  old.names <- names
if (!is.null(new.suffix)) 
  new.names <- paste(names,new.suffix,sep='.') 
else
  new.names <- names

if (new.names[1] == old.names[1]) {
  warning('old and new names the same, nothing done')
  return()
} else {
  for (i in 1:length(new.names)) {
    cat ('Copying',old.names[i],'to',new.names[i],'\n')
    assign(new.names[i],get(old.names[i]),where=1,immediate=T)
  }
  return()
}
}
