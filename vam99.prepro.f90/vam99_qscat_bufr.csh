#
#!/bin/csh -f
#

echo ' ' 

# if the number of arguments is .ne. 4, echo error mesg and exit
if ( $#argv != 4) then
    echo "vam99_qscat_bufr.csh requires 4 arguments"
    echo "usage:"
    echo "    % csh vam99_qscat_bufr.csh QS_BUFR NAVFN1 NAVFN2 QS_TYPE"
    echo " where QS_BUFR = full path and QuikScat Level _BUFR filename"
    echo " where NAVFN1 = full path and 1st AVN native format filename"
    echo " where NAVFN2 = full path and 2nd AVN native format filename"
    echo " where QS_TYPE = type of processing, wnds or sig0 "
    echo " "
    exit 1
endif

# e.g. QS_BUFR = /cyan/quikscat/data/BUFR/1999/222/QS_S_BUFR00733.19992460217
#      NAVFN1 = /plum/avn/19990810/gblav.T00Z.PGrbF00-uv10.hdr
#      NAVFN2 = /plum/avn/19990810/gblav.T00Z.PGrbF03-uv10.hdr
#      QS_TYPE = wnds

set qs_bufr = $1
set navfn1 = $2
set navfn2 = $3 
set qs_type = $4
#
setenv VAM99PREPRO   /home/mcc/p651/bufr/src/decoder
setenv VAM99PREPRO2  /home/mcc/vam_cvs/vam/vam99.prepro.f90
setenv VAM99MAIN  /home/mcc/vam_cvs/vam/vam99.main.f90
#
# Create namelists for QuikScat data reformatting
#
echo ' '
echo ' ** Create buffer reformatter namelists '
#
if ($qs_type == 'wnds' || $qs_type == 'WNDS') then

cat > ! reformatqs_notrank.namelist  << EOF
 &qsfnwnds
  bufrfile='$qs_bufr',
  outftype='$qs_type',
  hdrfname='obs.txt', 
  binfname='obs.dat', 
  jplrank=.false.,
  dfacconst=0.25,
  xmin=0.,
  xmax=360.,
  ymin=-90.,
  ymax=90.,
 &end
EOF
cat > ! reformatqs_jplrank.namelist  << EOF
 &qsfnwnds
  bufrfile='$qs_bufr',
  outftype='$qs_type',
  hdrfname='obs_jplranked.txt', 
  binfname='obs_jplranked.dat', 
  jplrank=.true.,
  dfacconst=0.25,
  xmin=0.,
  xmax=360.,
  ymin=-90.,
  ymax=90.,
 &end
EOF

else if ($qs_type == 'sig0' || $qs_type == 'SIG0') then

cat > ! reformatqs_sig0.namelist  << EOF
 &qsfnsig0
  bufrfile='$qs_bufr',
  outftype='$qs_type',
  hdrfname='obs.txt', 
  binfname='obs.dat',
  satidconst=281.,
  modfnconst=6.,
  kpm2const=0.0256,
  xmin=0.,
  xmax=360.,
  ymin=-90.,
  ymax=90.,
 &end
EOF

else

    echo '...$QS_TYPE in error'
    exit 0

endif
#
echo ' ...created reformatting namelists'
#
echo ''
echo ' ** Execute reformat_qscat_bufr '
echo ''
echo ' ** Create obstoascii1.namelist for reformat_qscat_bufr output'
#
# Reformat QuikScat data. 
if ($qs_type == 'wnds' || $qs_type == 'WNDS') then

$VAM99PREPRO/reformat_qscat_bufr reformatqs_notrank.namelist > ! reform1.output
$VAM99PREPRO/reformat_qscat_bufr reformatqs_jplrank.namelist > ! reform2.output

cat > !  obstoascii1.namelist  << EOF
&files
 in_file='obs_jplranked',
 out_obs_file='jpl.winds.JPLranked.txt',
 vprint=.T., more=.F., qcflag=.F./
EOF

endif
#
if ($qs_type == 'sig0' || $qs_type == 'SIG0') then
$VAM99PREPRO/reformat_qscat_bufr reformatqs_sig0.namelist > ! reform.output
exit
cat > !  obstoascii1.namelist  << EOF
&files
 in_file='obs',
 out_obs_file='jpl.winds.JPL.txt',
 vprint=.T., more=.F., qcflag=.F./
EOF
endif
#
echo ' ...created obstoascii1.namelist '
#
$VAM99PREPRO2/obs_to_ascii < obstoascii1.namelist > ! obstoascii1.output

echo ''
#
set rev_file = 'rev_times.dat'
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
$VAM99PREPRO2/ave_time -R$rev -B$btime -E$etime -T$interval
unsetenv interval
#
#   Create interpolated analysis for central time of this rev
set tim_file = 'ave_time.dat'
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
setenv VAM99PREPRO  /home/mcc/vam_cvs/vam/vam99.prepro.f90
csh $VAM99PREPRO2/native2ave_tim.csh $idate $itime $navfn1 $navfn2
#
echo ' ** Execute VAM99'
#
if ($qs_type == 'wnds' || $qs_type == 'WNDS') then

$VAM99MAIN/vam < vam_bufr_wnds.namelist > ! history

echo ' ** Create obstoascii2.namelist for VAM output'
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
echo ' ...created obstoascii2.namelist '
#
$VAM99PREPRO2/obs_to_ascii < obstoascii2.namelist > ! obstoascii2.output

endif
#
if ($qs_type == 'sig0' || $qs_type == 'SIG0') then
    $VAM99MAIN/vam < vam_bufr_sig0.namelist > ! history
endif
#
echo ''
echo ' ...done with VAM99 processing'
echo ''
echo ' ** Process VAM99 output'
echo ''
echo ' ** Create reformat_grid.namelist for VAM output'
echo ''
echo ' ...created reformat_grid.namelist '
echo ''
#
if ($qs_type == 'wnds' || $qs_type == 'WNDS') set analysistxt = 'analysis2.txt'
if ($qs_type == 'sig0' || $qs_type == 'SIG0') set analysistxt = 'analysis.txt'
#
cat > !  reformat_grid.namelist  << EOF
&files
in_native=$analysistxt
txt_in=.T., hdr_out=.T., more=.T./
&files
in_native='background1.txt'    
txt_in=.T., hdr_out=.T., more=.F./
EOF
#
$VAM99PREPRO2/reformat_grid < reformat_grid.namelist > ! reformat_grid.output
#
echo ''
echo ' ** Done with VAM99/QuikScat BUFR processing *****'
echo ''
exit
#
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







