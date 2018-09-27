#! /bin/csh -f

# This script prepares a temporary native analysis by interpolating two 
# gridded wind analyses to the time requested (idate,itime).

# Notes: 
#   1.  timeinterp_native output filenames default to: 
#       'temp_native.hdr' and 'temp_native.dat'. user can override
#       this convention with '-O' optional command line input.
#       e.g.  timeinterp_native .. .. .. .. -O'othername.hdr' .
#
#   2.  script will execute reformat_grid to generate ascii text
#       version 'temp_native.txt' for input to vam.
#
#   3.  if command line arguments 'navfn1' and 'navfn2' are 
#       identical, then timeinterp_native replicates the input 
#       native file header and data to output.
#
# Processing steps:
#  . check argument list
#  . set local variables
#  . goto temp working directory
#  . create new native format analysis 
#
#  $Id: native2ave_tim.csh,v 1.2 2001/01/04 12:31:48 mcc Exp $
#  $Log: native2ave_tim.csh,v $
#  Revision 1.2  2001/01/04 12:31:48  mcc
#  Changed source to csh commands.
#
#  Revision 1.1  2000/10/23 19:10:51  mcc
#  This script replaces native2qscat_tim.csh. Now more general for
#  any native format files. Initial revision.
#
#  Revision 1.1  1999/10/04 14:00:39  mcc
#  Initial revision
#

#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if ( $#argv != 4 ) then
   echo ''
   echo "native2ave_tim requires 4 arguments"
   echo "usage:"
   echo " % csh  native2ave_tim.csh  IDATE ITIME NAVFN1 NAVFN2"
   echo "IDATE and ITIME are the central date/time the observations,"
   echo "NAVFN1 and NAVFN2 are the two native format files with times"
   echo "that bound the observation central time."
   echo ''
   exit 1
endif

# Set local variables
# idate in the form yyyymmdd ; itime in the form hhmmss
set idate = $1
set itime = $2
set navfn1 = $3
set navfn2 = $4

# Create new analysis
echo ''
echo ' ** Creating new analysis using native files : '
echo $navfn1
echo $navfn2
echo ''
#
$VAM99PREPRO/timeinterp_native -D$idate -T$itime -F$navfn1 -S$navfn2
#
# Create file temp_native.namelist for generation of ascii text version 
cat > !  temp_native.namelist  << EOF
&files
in_native='temp_native.hdr'
hdr_in=T, txt_out=T, more=F/
EOF
#
$VAM99PREPRO/reformat_grid < temp_native.namelist > ! temp_native.output
echo ''
echo ' ...created new temporary analysis'
echo ''

exit 0
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#
