Information on Builds (cvs tags) for the vam.

$Id: Readme-builds.txt,v 1.8 2005/03/21 21:36:45 rnh Exp $

Tags covering all files and directories:

fca-test [branch] (2005/03/21):
   Hack of fgat05-plus to allow testing of fca_mode .eq. 1.
   (u,v) used for (du,dv).  Modified all obs functions to use (du,dv)
   to interpolate into background at source locations.

fca01 (2005/03/21):
   Added fca alignment with four modes.  See updates to namelist.txt.
   Discussion of modifications and planning in vam-fca.notes.
   fca_mode .eq. 0 should reproduce earlier version.

fgat05-plus (2005/03/18):
   a few tweaks to std outputs; added scatconv data type; this version
   used for most of the lambda weight tuning for obs weights and
   smoothness constraints for NASA ReSASON CAN project (P1250).

fgat05 (2005/01/21):
   observation counts at analysis grid points added
   to analysis output data; proximity qc bit flags for
   land, rain and ice added

fgat04 (2004/09/30):
   VAM obs io routines updated to accommodate new qc_bit_map.
   Some other minor tinkering with obs QC.  This release is
   ready for further testing and tuning for Goddard's
   ReASON CAN.

pre-fgat04 (2004/09/28):
   VAM updated to include quality control on observations.  QC
   decisions are stored in new data element, qc_bit_map, in obs data
   structure.  Also, namelist defaults updated to all .FALSE.

fgat03 (2004/07/27):
   VAM updated to include date/time checking of input data sets,
   and updated to process microwave-derived wind speed obs including
   those from SSMI/TMI/AMSR.

fgat02 (2004/06/11):
   FGAT code updated to handle multiple input obs datasets as a linked
   list, as elsewhere in the VAM.  Further development is still
   planned (e.g., add obs operators for microwave-derived wind speed,
   error checking of input obs datasets), but this discrete
   development step is complete.
 
fgat01 (2004/04/09):
   Code used for first testing of VAM FGAT (First Guess at Appropriate
   Time).  Preprocessing changed for SeaWinds L2B Science data only.

Build6 (2001/01/31):
   Bench-marked version of VAM for first internal release.  This uses
   the latest version of native (-r txthdr): ASCII headers.

Build5 (2001/01/30): 
   Snap-shot of all vam directories and files.  This version includes
   the bug fix to interp_mod.F, but still uses the old version of native 
   (pre-2002/08/08: binary headers, and put_grads_header.f w/o
   Mercator grid support).

Tags covering selected subdirectories:

   vam99.main.f90 and vam99.prepro.f90: Build1, Build2, Build3, Build4
   ==>see vam99.prepro.f90/Readme for details
      1999/02/15 through 2000/01/20
      Intermediate builds for the f90 rewrite of the vam.  Most of the
      reprocessing was done with a version close to Build2 or Build3.

   vam2d.fortran: BuildGSFC, BuildNSCAT1, BuildNSCAT2, BuildNSCATo, BuildVAM1
      1997/02/12 through 1998/05/14

Changes made to this file:
$Log: Readme-builds.txt,v $
Revision 1.8  2005/03/21 21:36:45  rnh
Added fca01 and fca-test descriptions.

Revision 1.7  2005/03/18 18:34:47  leidner
updated for fgat05-plus.

Revision 1.6  2005/01/21 16:54:00  leidner
updated for fgat05

Revision 1.5  2004/09/30 20:08:41  leidner
added definitions for pre-fgat04 and fgat04.

Revision 1.4  2004/07/27 15:55:14  leidner
added description of build fgat03

Revision 1.3  2004/06/11 20:58:51  leidner
added build fgat02

Revision 1.2  2004/04/09 20:11:20  leidner
Added description of Build fgat01, the first version of the VAM tested
using the First Guess at the Appropraite Time (FGAT).

Revision 1.1  2003/01/31 15:33:30  trn
Added file containing info on Builds

