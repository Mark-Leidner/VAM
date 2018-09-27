#!/bin/csh -f
#
# Purpose:
# This script reformats QuikScat Level 2A sigma0 observations to VAM99 
# input format. Executable 'reformat_qscat2a' creates two binary files
# of reformmated data. The QuikScat orbit rev number and JPL format 
# begin/end time stamps are found.

# The environmental variable, 'VAM99HOME' must be set to top level
# VAM directory path.

# The environmental variable, 'interval', must reflect the native 
# analyses interval time. Executable 'ave_time' determines the obs 
# central date (yyyymmdd) and time (hhmmss) as well as the bracketing 
# analysis dates and times.
 
# Script 'native2ave_tim.csh' interpolates the two bracketing analyses 
# to the central time and writes them out. The VAM is then executed twice
# and the output is written to both binary and text files.
# 
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
echo ''

#
# if the number of arguments is .ne. 3, echo error mesg and exit
if ( $#argv != 3) then
    echo "vam99_qscat2a.csh requires 3 arguments"
    echo "usage:"
    echo "    % csh vam99_qscat2a.csh QS_L2A NAVFN1 NAVFN2"
    echo " where QS_L2A = full path and QuikScat Level 2A filename"
    echo " where NAVFN1 = full path and 1st AVN native format filename"
    echo " where NAVFN2 = full path and 2nd AVN native format filename"
    exit1
endif

# e.g. QS_L2A = /cyan/quikscat/data/L2A/1999/239/QS_S2A00980.19992421832
#      NAVFN1 = /plum/avn/19990827/gblav.T06Z.PGrbF00-uv10.hdr 
#      NAVFN2 = /plum/avn/19990827/gblav.T12Z.PGrbF00-uv10.hdr

set qs_l2a = $1
set navfn1 = $2
set navfn2 = $3 

# Declare some paths; test to see if already defined
if ($?VAM99HOME == 0) then 
   setenv VAM99HOME  ..
endif
if ($?VAM99MAIN == 0) then 
   setenv VAM99MAIN  $VAM99HOME/vam99.main.f90
endif
if ($?VAM99PREPRO == 0) then 
    setenv VAM99PREPRO  $VAM99HOME/vam99.prepro.f90
endif
#
echo ''
#
# Create L2A namelist for QuikScat data reformmating
#
echo ' '
echo " ** Create file reformatqs_l2a.namelist"
cat > ! reformatqs_l2a.namelist  << EOF
 &qsfn
  l2a_file='$qs_l2a',
  hdr_fname='obs.txt', 
  bin_fname='obs.dat',
  satid_const=281.,
  modfn_const=6.,
  kpm2_const=0.0256,
  xmin=0.,
  xmax=360.,
  ymin=-85.,
  ymax=85.,
 &end
EOF
echo " ...created reformatqs_l2a.namelist"
#
echo ''
echo ' ** Execute reformat_qscat2a '
echo ''
#
# Reformat QuikScat data. 
#
$VAM99PREPRO/reformat_qscat2a reformatqs_l2a.namelist > ! reform_l2a.output
#
echo " ** Create file obstoascii1.namelist for reformat_qscat2a output"
cat > !  obstoascii1.namelist  << EOF
&files
 in_file='obs',
 out_obs_file='jpl.winds.JPL.txt',
 vprint=.T., more=.F., qcflag=.F./
EOF
#
echo " ...created obstoascii1.namelist "
#
$VAM99PREPRO/obs_to_ascii < obstoascii1.namelist > ! obstoascii1.output
echo ''
#
set rev_file = "rev_times.dat"
set rev      = `awk -F' ' '{getline; print $1}' $rev_file`
set btime    = `awk -F' ' '{getline; print $2}' $rev_file`
set etime    = `awk -F' ' '{getline; print $3}' $rev_file`
echo ''
echo -n " ** Computing ave time for rev $rev "
#
# Compute ave time for this rev; execution generates 'ave_time.dat'.
# Command line arguments btime and etime must be in JPL timestamp
# format. Set time interval (hours) of native background files.
#
setenv interval 6
#
$VAM99PREPRO/ave_time -R$rev -B$btime -E$etime -T$interval
unsetenv interval 
#
#   Create interpolated analysis for central time of this rev
set tim_file = "ave_time.dat"
set idate    = `awk -F' ' '{print $2}' $tim_file`
set itime    = `awk -F' ' '{print $3}' $tim_file`
echo ''
echo -n " ... computed ave time for rev $rev "
echo ''
#
# Use two native background files with times that bound obs central 
# time. Intepolate all native fields to central time and write them
# to 'temp_native.dat' and 'temp_native.hdr'. Execute reformat_grid
# to generate ascii text version 'temp_native.txt' for vam input.
#
echo -n "Creating new background analysis for date (yyyymmdd)  $idate "
echo ''
echo -n "                                 and time (hhmmss)    $itime "
echo ''
#
csh $VAM99PREPRO/native2ave_tim.csh $idate $itime $navfn1 $navfn2
#
echo ' ** Execute VAM99'
#
$VAM99MAIN/vam  < vam_l2a.namelist  > ! history
echo ''
echo ' ...done with VAM99 processing'
echo ''
echo ' ** Process VAM99 output'
echo ''
#
echo " ** Create file reformat_grid.namelist for VAM output"
cat > !  reformat_grid.namelist  << EOF
&files
in_native='analysis.txt'
txt_in=.T., hdr_out=.T., more=.T./
&files
in_native='background1.txt'    
txt_in=.T., hdr_out=.T., more=.F./
EOF
echo " ...created reformat_grid.namelist "
#
$VAM99PREPRO/reformat_grid < reformat_grid.namelist > ! reformat_grid.output
echo ''
echo ' ** Done with VAM99/QuikScat L2A processing *****'
echo ''
exit
#
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







