#! /bin/csh -f

# This script prepares a temporary native analysis by interpolating two 
# gridded wind analyses to the time requested (timqs).

# Processing steps:
#  . check argument list
#  . set local variables
#  . goto temp working directory
#  . create new native format analysis 
#
#  $Id: native2qscat_tim.csh,v 1.1 1999/10/04 14:00:39 mcc Exp $
#  $Log: native2qscat_tim.csh,v $
#  Revision 1.1  1999/10/04 14:00:39  mcc
#  Initial revision
#

#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if ( $#argv != 6 ) then
   echo ''
   echo "native2qscat_tim requires 6 arguments"
   echo "usage:"
   echo " % source  native2qscat_tim.csh  DATE  TIME  TIMQS  WRKDIR NAVFN1 NAVFN2"
   echo "DATE and TIME are the central date/time the quikscat data,"
   echo "TIMQS is the time in seconds since the start of 1999 equil. to DATE/TIME, "
   echo "WRKDIR is the path to an existing temp work directory,"
   echo "NAVFN1 and NAVFN2 are the two native format files with time stamps"
   echo "that bound the observation central time."
   echo ''
   exit 1
endif

# Set local variables
set date = $1
set time = $2
set timqs = $3
set wrkdir = $4
set navfn1 = $5
set navfn2 = $6

# Setup path to native avn files
#setenv NATIVE_DIR '/plum/avn/'
#set native_dir = $NATIVE_DIR$date'/'

# Goto temp working directory
unalias cd
cd $wrkdir

# Create new analysis
echo ''
echo ' ** Creating new analysis using native files : '
echo $navfn1
echo $navfn2
echo ''

timeinterp_native -D$date -T$time -C$timqs -F$navfn1 -S$navfn2

echo ''
echo ' ...created new temporary analysis'
echo ''

exit 0
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#
