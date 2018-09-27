#!/bin/csh -f
#
# Purpose:
# This script reformats QuikScat Level 2B observations to VAM99 input 
# format. Executable 'reformat_qscat2b' creates two binary files
# of reformmated data. The QuikScat orbit rev number and JPL format 
# begin/end time stamps are found.

# The environmental variable, 'VAM99HOME' must be set to top level
# VAM directory path.

# The environmental variable, 'interval', must reflect the native 
# analyses interval time. Executable 'ave_time' determines the obs 
# central date (yyyymmdd) and time (hhmmss) and the bracketing 
# analysis dates and times.
 
# Script 'native2ave_tim.csh' interpolates the two bracketing analyses 
# to the central time and writes them out. The VAM is then executed twice
# and the output is written to both binary and text files.
# 

# This script creates/updates 5 control files used as input 
# to the VAM99 processing.
# ./reformatqs_notrank.namelist
# ./reformatqs_jplrank.namelist
# ./obs_to_ascii1.namelist
# ./vam_l2b.namelist
# ./obs_to_ascii2.namelist
#
#  $Id: vam99_qscat2b.csh,v 1.6 2001/10/24 11:52:17 mcc Exp $
#  $Log: vam99_qscat2b.csh,v $
#  Revision 1.6  2001/10/24 11:52:17  mcc
#  Added lat/lon windowing and DIRTH data request capabilities to namelist.
#
#  Revision 1.5  2001/02/23 18:16:24  mcc
#  Changed obstoascii2.namelist for in_file='anal2-obs, more logic
#  from .T. to .F.  .
#
#  Revision 1.4  2001/01/03 20:20:26  trn
#  Changed source to csh commands
#
#  Revision 1.3  2000/12/18 15:49:56  mcc
#  Added construction of reformat_grid namelist and reformat_grid
#  execution for background1 and analysis2 hdr files from txt files.
#
#  Revision 1.2  2000/11/28 13:44:25  mcc
#  Made several minor changes including path declaration.
#
#  Revision 1.1  2000/10/23 19:08:43  mcc
#  This script replaces vam99_qkscat.csh for level 2b data.
#  Initial revision.
#
#  Revision 1.1  1999/10/04 18:53:50  mcc
#  Initial revision
#
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
echo ''

# if the number of arguments is .ne. 3, echo error mesg and exit
if ( $#argv != 3) then
    echo "vam99_qscat2b.csh requires 3 arguments"
    echo "usage:"
    echo "    % csh vam99_qscat2b.csh QS_L2B NAVFN1 NAVFN2"
    echo " where QS_L2B = full path and QuikScat Level 2B filename"
    echo " where NAVFN1 = full path and 1st AVN native format filename"
    echo " where NAVFN2 = full path and 2nd AVN native format filename"
    exit1
endif

# e.g. QS_L2B = /cyan/quikscat/data/L2B/1999/222/QS_S2B00733.19992460217
#      NAVFN1 = /plum/avn/19990810/gblav.T00Z.PGrbF00-uv10.hdr
#      NAVFN2 = /plum/avn/19990810/gblav.T00Z.PGrbF03-uv10.hdr

set qs_l2b = $1
set navfn1 = $2
set navfn2 = $3 
#
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
# Create namelists for QuikScat data reformmating
#
echo ' '
echo " ** Create file reformatqs_notrank.namelist "
cat > ! reformatqs_notrank.namelist  << EOF
 &qsfn
  l2b_file='$qs_l2b',
  hdr_fname='obs.txt', 
  bin_fname='obs.dat', 
  jplrank=.false.,
  wdirth=.false.,
  dfac_const=0.25,
  xmin=0.,
  xmax=360.,
  ymin=-85.,
  ymax=85.,
 &end
EOF
echo " ...created reformat_notrank.namelist"
#
echo ' '
echo " ** Create file reformatqs_jplrank.namelist "
cat > ! reformatqs_jplrank.namelist  << EOF
 &qsfn
  l2b_file='$qs_l2b',
  hdr_fname='obs_jplranked.txt', 
  bin_fname='obs_jplranked.dat', 
  jplrank=.true.,
  wdirth=.false.,
  dfac_const=0.25,
  xmin=0.,
  xmax=360.,
  ymin=-85.,
  ymax=85.,
 &end
EOF
echo " ...created reformat_jplrank.namelist"
#
echo ''
echo ' ** Execute reformat_qscat2b '
echo ''
#
# Reformat QuikScat data. 
#
$VAM99PREPRO/reformat_qscat2b reformatqs_notrank.namelist > ! reform1.output
$VAM99PREPRO/reformat_qscat2b reformatqs_jplrank.namelist > ! reform2.output
#
echo " ** Create file obstoascii1.namelist for reformat_qscat2b output"
cat > !  obstoascii1.namelist  << EOF
&files
 in_file='obs_jplranked',
 out_obs_file='jpl.winds.JPLranked.txt',
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
$VAM99MAIN/vam < vam_l2b.namelist > ! history
echo ''
echo ' ...done with VAM99 processing'
echo ''
echo ' ** Process VAM99 output'
echo ''
#
echo " ** Create file obstoascii2.namelist for VAM output"
cat > !  obstoascii2.namelist  << EOF
&files
 in_file='bg-obs',
 out_obs_file='jpl.winds.txt',
 out_int_file='background@nscat.txt',
 vprint=.T., more=.T., qcflag=.F./
&files
 in_file='dual-bg-obs',
 out_obs_file='jpl.winds.dual.txt',
 vprint=.T., more=.T., qcflag=.F./
&files
 in_file='anal2-obs',
 out_int_file='analysis2@nscat.txt',
 vprint=.T., more=.F., qcflag=.F./
&files
 in_file='analysis2@nscat',
 out_file='analysis2@nscat.txt',
 vprint=.T., more=.F., qcflag=.F./
These not currently used, but could be if needed:
 in_file='dual-bg-obs',
 in_file='dual-anal1-obs',
 in_file='anal1-obs',
EOF
echo " ...created obstoascii2.namelist "
#
$VAM99PREPRO/obs_to_ascii < obstoascii2.namelist > ! obstoascii2.output
#
echo " ** Create file reformat_grid.namelist for VAM output"
cat > !  reformat_grid.namelist  << EOF
&files
in_native='analysis2.txt'
txt_in=.T., hdr_out=.T., more=.T./
&files
in_native='background1.txt'    
txt_in=.T., hdr_out=.T., more=.F./
EOF
echo " ...created reformat_grid.namelist "
#
$VAM99PREPRO/reformat_grid < reformat_grid.namelist > ! reformat_grid.output
#
echo ''
echo ' ** Done with VAM99/QuikScat L2B processing *****'
echo ''
exit
#
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







