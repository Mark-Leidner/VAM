# Generic stuff from: Makefile,v 1.3 1996/01/29 15:49:47 trn Exp
# 
#HelpFile: Gnu Makefile for handling fortran routines.  Usage: gmake target
#HelpFile: 
#HelpFile: target               function
#HelpFile: --------------       ---------------------------------------
#HelpFile: OBJECTS (default)  compiles interesting_files in directory
#HelpFile: HELP                 prints this
#HelpFile: START                set-up auxilary files
#HelpFile: all.doc              all ascii documentation
#HelpFile: a specific file      makes that file
#HelpFile: PPRINT               print *.ps
#HelpFile: FPRINT               print interesting_files in directory
#HelpFile: rcstest              list of variables used in RCSMakefile
#HelpFile: rcstell              list of locked files
#HelpFile: co                   Check out all interesting_files in rcs
#HelpFile:                         not currently writable from trunk
#HelpFile: cov                  Check out all interesting_files in rcs
#HelpFile:                         not currently writable from user's branch
#HelpFile: 			N.B.: command fails (does nothing)
#HelpFile: 			   for files without user's branch from head 
#HelpFile: ci                   Check in all interesting_files in directory
#HelpFile:                         which are writable to HEAD
#HelpFile: civ                  Check in all interesting_files in directory
#HelpFile:                         which are writable to user's BRANCH
#HelpFile: rcsclean             Remove all read-only interesting_files
#HelpFile:                         in the directory (if they are in rcs)
#HelpFile: rcscleanv 		Remove and unlock all user_locked writable
#HelpFile:			   interesting_files in the directory
#HelpFile:			   (if they do not differ from
#HelpFile:			   archived version)
#HelpFile: clean                clean up work files
#HelpFile: clean_all            clean up start-up files (created by START)
#HelpFile: clean_everything     does clean, clean_all, and rcsclean and rcscleanv

.PHONY: OBJECTS START clean errors

# Non fortran files should be checked in by hand the first time.
comment:= c

bin_dir:= ../bin.fortran

Ffiles := $(wildcard *.F)
cfiles := $(wildcard *.c)

# interesting files presently in directory
files_in_dir:=$(Ffiles) $(cfiles) $(wildcard *.h)
# all rcs files which co to interesting files
comma:= ,
files_in_rcs:= $(patsubst RCS/%$(comma)v,%,$(wildcard RCS/*.F,v)) \
  $(patsubst RCS/%$(comma)v,%,$(wildcard RCS/*.c,v)) \
  $(patsubst RCS/%$(comma)v,%,$(wildcard RCS/*.h,v))

# interesting_files is the combination of the two:
interesting_files:= $(sort $(files_in_dir) $(files_in_rcs))

# Compilation

all_object_files:=$(patsubst %.F,%.o,$(Ffiles)) \
  $(patsubst %.c,%.o,$(cfiles))

OBJECTS: $(all_object_files)

users:= $(sort rnh trn $(USER))
export interesting_files users comment bin_dir

.PHONY: rcstest rcstell co cov ci civ rcsclean rcscleanv

rcstest rcstell co cov ci civ rcsclean rcscleanv: RcsInfo
	@$(MAKE) -f /home/nwp/NWP/bin/RCSMakefile --no-print-directory $@

# auxilary files which self extract from this make file
auxilary_files = strip.empty D2latex.sed HelpFile RcsInfo

# files used as tools
tool_files:= PPRINT FPRINT ETAGS

START: $(auxilary_files) ETAGS
	@echo 'Self-extracted auxiliary_files: $(filter-out ETAGS, $?)'

errors:
	@echo "fgrep -i error *.out"
	@fgrep -i error *.out || echo 'No errors found.'

# Documentation

.PHONY: all.doc

all.doc: all.purpose all.variables all.d 

all.d: $(patsubst %.F,%.d,$(files_in_dir))
# concatenate all .d files which derive from a .F file in dir
	ifneq ($(strip $(patsubst %.F,%.d,$(files_in_dir))),)
		cat $^ > $@
	endif

# Formatting

%.latex: %.d start.doc end.doc strip.empty D2latex.sed
	cat start.doc $< end.doc | sed -f D2latex.sed | \
	sed -e '/^ *\\xxxx{/s/_/\\_/g' > temp
	sed -f strip.empty temp > $@
	"rm" temp


start.doc end.doc: %: RCS/%,v
		$(CO) $(COFLAGS) $<

# Cleaning

clean: clean_docs clean_formatting 

# Auxiliary files:

# These lines are stored in the RCS/*,v files and are seen when using rlog.
#RcsInfo: Include files and driver, I/O, utility and initialization routines
#RcsInfo: for the VAM.  The scatterometer projects (AER P145,P495,P651,P725)
#RcsInfo: are funded by NASA through JPL and OSU.

# Generic functionality ================================================

$(auxilary_files): %: Makefile
# Extract the file from the Makefile
	@sed -n 's/#$@: //p' $^ > $@

# This sed script adds latex macro lines for formatting in place of
# the lines with '!# CSU' in the original file.
#D2latex.sed: /CSU IDENTIFICATION/ s/.*CSU IDENTIFICATION[ :]*\(.*\)$/\\xxxx{ \1 } \
#D2latex.sed: \\begin{verbatim} IDENTIFICATION : \1/
#D2latex.sed: /CSU SPECIFICATION AND CONSTRAINTS/c\
#D2latex.sed: \\end{verbatim} \
#D2latex.sed: \\xxxx{ Specification and constraints } \
#D2latex.sed: \\begin{verbatim}
#D2latex.sed: /CSU DESIGN/c\
#D2latex.sed: \\end{verbatim} \
#D2latex.sed: \\xxxx{ Design } \
#D2latex.sed: \\begin{verbatim}
#D2latex.sed: /CSU END/c\
#D2latex.sed: \\end{verbatim}

#
# This sed script removes empty documentation elements from *.d files. 
#
# Requirements:
# Empty documention lines must end with ":\n" (colon NEWLINE; no 
# trailing white spaces).
# Keyword lines must begin with with "c!## " in the source files
# (*.F, *.h), and "# " in *.d files.
#
# The approach is:
#  1. If the current line contains a keyword (line starts with "# "),
#     then append the next line to the current line (N).
#  2. If a ":NEWLINE # " is found in the current
#     pattern space (i.e., the next line appended to the current line),
#     then there are no comments between the current keyword
#     and the next => delete the line containing the current keyword 
#     by using sed command D.
#  3. Ditto for a ":NEWLINE.*verbatim". (rnh)
#
# Mark Leidner 6/16/97
#
#strip.empty: /^# /N
#strip.empty: /\:\n# /D
#strip.empty: /\:\n.*verbatim/D
#strip.empty: /# /s/# / /g
#

# Documentation

all.purpose: $(files_in_dir)
# Uses key string '!# PURPOSE'
# Get list with file names
# Replace garbage with a single tab
# Expand tabs to particular columns
	fgrep '!# PURPOSE' $^ | \
		sed 's/\.[a-z]*:.*!#.*PURPOSE[	 :]*/	/' | \
		expand -28,30 > $@

all.variables: $(files_in_dir)
# Uses key string '!#~'
# Get list without file names
# Remove start of line and replace white space with a single tab
# Sort and remove dups
# Replace names and tabs on continuation lines with two tabs
# Expand tabs to particular columns
	fgrep -h '!#~' $^ | \
		sed 's/.*!#~[	 ]*\([^	 ]*\)[	 ][	 ]*/\1	/' | \
		sort | uniq | \
		sed 's/^[^	 ]*\.\.*	/		/' | \
		expand -18,20 > $@

# *.ddd files are intermediaries only.
%.ddd: %.for
	fgrep '!#' $^ > $@

%.ddd: %.F
	fgrep '!#' $^ > $@

%.ddd: %.h
	fgrep '!#' $^ | sed 's/!#~/!# /' > $@

%.d: %.ddd
	(expand $< ; echo '.!# CSU END .!#') | \
		sed '/!#~/d' | \
		sed 's/.!#~*//g' > $@

# Formatting:

%.sgml: %.latex
# *.sgml files have publisher lines activated
	sed 's/^%Publisher-line: //' $< > $@

%.dvi %.aux %.lof %.log %.lot %.toc: %.latex
# Create the dvi and associated files from the latex file
# Use up to three passes.
	-latex $^
	-@fgrep -is warning $*.log && echo "===> Second pass:" && latex $^
	-@fgrep -is warning $*.log && echo "===> Third pass:" && latex $^

%.ps: %.dvi
# Create the postscript file
	dvips -o $@ $*

# Cleaning:

.PHONY: clean_docs clean_formatting clean_all clean_everything

clean_docs:
	-$(RM) *.ps *.d all.*

clean_formatting:
	-$(RM) *.aux *.dvi *.latex *.lof *.log *.lot *.sgml *.toc

clean_all:
	-$(RM) $(auxilary_files) $(tool_files) start.doc end.doc

clean_everything: clean clean_all rcsclean rcscleanv

# Printing:

PPRINT: *.ps
# Will issue an error if no .ps files currently exist
	lp $?
	touch PPRINT

FPRINT: $(files_in_dir)
	/usr/5bin/pr -f -l60 -n $? | rpr -cr10 -dv
	touch FPRINT

# Tools:

ETAGS: $(files_in_dir)
# ETAGS can't process the acronym tables for some reason
	etags $(files_in_dir)


.PHONY: help Help HELP
help Help HELP: HelpFile
# Display hints
	@cat HelpFile

####################################################################

# Specifics for GNUmakefile for vam2d i/o, init routines on sparc sunOS
# $Id: Makefile,v 1.17 2001/04/13 16:53:48 mcc Exp $
# $Log: Makefile,v $
# Revision 1.17  2001/04/13 16:53:48  mcc
# Added specification to -xtypemap=real:64,double:128,integer:mixed .
#
# Revision 1.16  2001/04/10 10:42:30  mcc
# Replaced -r8 in FFLAGS with -xtypemap .
#
# Revision 1.15  2000/11/13 16:17:08  mcc
# Changed bin_dir to  ../bin.fortran .
#
# Revision 1.14  1998/05/14 14:43:35  leidner
# gmake depend after adding flag_rain and other minor changes
# to scatterometer and ssmi data io
#
# Revision 1.13  1998/04/30 18:19:31  stanr
# change include directory for CFLAGS to /opt/include
#
# Revision 1.12  1998/03/10 16:14:25  leidner
# gmake depend after adding io for 25km nscat data
#
# Revision 1.11  1998/01/27 16:05:30  leidner
# added targets for stripping variable defs from *.for and *.F files
#
# Revision 1.10  1997/12/18 22:28:11  leidner
# gmake depend
#
# Revision 1.9  1997/06/23 18:40:26  leidner
# added %.d targets
# added "strip.empty" auxiliary file
# added latex fix for file names with "_" in them
# gmake depend
#
#!#Revision 1.8  1997/04/09  15:11:02  rnh
#!#gmake depend
#!#
#!#Revision 1.7  1997/04/08  16:40:31  rhoffman
#!#Eliminated .for files, added .c files.
#!#
#!#Revision 1.6  1997/04/07  14:06:22  leidner
#!#updated dependiencies for merged NSCATo and VAM1 version
#!#
#!#Revision 1.5  1997/04/07  13:43:24  leidner
#!#added c compiler options
#!#
#!#Revision 1.4  1997/02/18  20:46:19  leidner
#!#dependencies updated for BuildVAM1
#!#(subroutines dump_restore_grid and wga_wrgrid added)
#!#
#!#Revision 1.3  1997/02/12  20:00:29  leidner
#!#FFLAGS now -w -r8 -g
#!#dependencies updated for new include filenames (all changed to *.h)
#!#
#!#Revision 1.2  1997/02/10  19:42:17  leidner
#!#CPPFLAGS now empty
#!#adjoint compile replaced with fortran compile
#!#lib now libvam2d.a
#!#
#!#Revision 1.1  1997/02/10  19:06:53  leidner
#!#Initial revision
#!#

#Fortran options
FFLAGS = -w -xtypemap=real:64,double:128,integer:mixed  -g

#Alternative directories for include files
CPPFLAGS =

#c compiler options for compiling rdccomp.c
CC      = gcc
CFLAGS  = -I/opt/include

#Name of the library archive:
lib = libvam2d.a

#Include rules for fortran and c libraries
make-standard = $(bin_dir)/make-standard
include $(make-standard)

####################################################################
# Rules after next line generated automatically using gmake depend
# DO NOT DELETE THIS LINE -- make depend depends on it.

ckwk.o: vam.h iosubs.h
close2.o: vam.h iosubs.h gparm.h grdta.h ptdta.h
compar.o: vam.h iosubs.h gparm.h grdta.h ptdta.h
copyers1.o: vam.h iosubs.h er_bscat.h
copynscat.o: vam.h iosubs.h ns_bscat.h
dgrid.o: vam.h iosubs.h gparm.h grdta.h
dump_restore_grid.o: vam.h iosubs.h gparm.h grdta.h
er_print.o: vam.h iosubs.h er_bscat.h
errsea.o: vam.h iosubs.h
ers1wind.o: vam.h iosubs.h
execute_event.o: vam.h iosubs.h
flag_rain.o: vam.h iosubs.h ptdta.h ns_winds.h ns_bscat.h
gen_rdskip.o: vam.h iosubs.h
ginter.o: vam.h iosubs.h
glabs.o: vam.h iosubs.h
gridx.o: vam.h iosubs.h
grparm.o: vam.h iosubs.h gparm.h grdta.h grname.h
initscat.o: vam.h iosubs.h
initvam.o: vam.h iosubs.h gparm.h grdta.h ptdta.h sldta.h er_bscat.h
initvam.o: ns_bscat.h weight.h
ioblock.o: iosubs.h iolist.h
ioclose.o: iosubs.h iolist.h
iodisable.o: iosubs.h iolist.h
iolink.o: iosubs.h
ioload.o: iosubs.h iolist.h
ioopen.o: iosubs.h iolist.h
ioparse.o: iosubs.h
iounit.o: iosubs.h
jfun.o: vam.h iosubs.h funct.h
jgrad.o: vam.h iosubs.h funct.h
jperm.o: vam.h iosubs.h
jxfer.o: vam.h iosubs.h funct.h
mgdr_read.o: l20param.h mgdr_out.h mgdr_com.h l20files.h
ns_print.o: vam.h iosubs.h ns_bscat.h
nsw_print.o: vam.h iosubs.h ns_winds.h
permut.o: vam.h iosubs.h
pgrid.o: vam.h iosubs.h
process_record.o: rdglob17.h vam.h iosubs.h gparm.h ns_bscat.h
process_record20.o: rdglob20.h vam.h iosubs.h gparm.h ns_winds.h ptdta.h
pt_dealias.o: vam.h iosubs.h gparm.h grdta.h ptdta.h
pt_print.o: vam.h iosubs.h ptdta.h
rbm_rdback.o: vam.h iosubs.h ns_bscat.h ns_winds.h ptdta.h l20param.h
rbm_rdback.o: mgdr_out.h mgdr_com.h l20files.h gparm.h grdta.h
rbm_stdata.o: vam.h iosubs.h ns_bscat.h ns_winds.h ptdta.h l20param.h
rbm_stdata.o: mgdr_out.h mgdr_com.h l20files.h gparm.h grdta.h
rbo_default.o: vam.h iosubs.h
rbo_rdback.o: vam.h iosubs.h er_bscat.h
rbo_rddata.o: vam.h iosubs.h gparm.h er_bscat.h
rco_default.o: vam.h iosubs.h grname.h
rco_rdconv.o: vam.h iosubs.h sldta.h gparm.h grdta.h
rco_rddata.o: vam.h iosubs.h sldta.h gparm.h
rco_rdukmo.o: vam.h iosubs.h gparm.h sldta.h
rdhdf17f.o: rdglob17.h vam.h iosubs.h gparm.h ns_bscat.h hdf.h dffunc.h
rdhdf17f.o: netcdf.h
rdhdf20f.o: rdglob20.h vam.h iosubs.h gparm.h ns_winds.h hdf.h dffunc.h
rdhdf20f.o: netcdf.h
read_event.o: vam.h iosubs.h
readrecinfo.o: rdglob17.h
readrecinfo20.o: rdglob20.h
readsdsinfo.o: rdglob17.h
readsdsinfo20.o: rdglob20.h
readssmi.o: vam.h iosubs.h gparm.h ssmidata.h ptdta.h
regrid.o: vam.h iosubs.h gparm.h grdta.h ptdta.h sldta.h er_bscat.h
regrid.o: ns_bscat.h ns_winds.h
reset.o: vam.h iosubs.h gparm.h ptdta.h sldta.h er_bscat.h ns_bscat.h
resid.o: vam.h iosubs.h gparm.h grdta.h ptdta.h
residc.o: vam.h iosubs.h gparm.h grdta.h sldta.h
residn.o: vam.h iosubs.h gparm.h grdta.h ns_bscat.h namelist_residn.h
resids.o: vam.h iosubs.h gparm.h grdta.h er_bscat.h
rg_conv.o: vam.h iosubs.h gparm.h sldta.h
rg_ers1.o: vam.h iosubs.h gparm.h er_bscat.h
rg_nscat.o: vam.h iosubs.h gparm.h ns_bscat.h
rg_nscatw.o: vam.h iosubs.h gparm.h ns_winds.h
rg_sass.o: vam.h iosubs.h gparm.h ptdta.h
rgg_rdgrid.o: vam.h iosubs.h gparm.h grname.h grdta.h
rgp_rddata.o: vam.h iosubs.h
rgp_rdgrid.o: vam.h iosubs.h gparm.h grname.h grdta.h
rgv_rddata.o: vam.h iosubs.h
rgv_rdgrid.o: vam.h iosubs.h gparm.h grname.h grdta.h
rnb_rdback.o: vam.h iosubs.h ns_bscat.h ns_winds.h rdglob17.h gparm.h grdta.h
rnb_rdback.o: namelist_rnb_rdback.h
rnw_rddata.o: vam.h iosubs.h gparm.h ns_winds.h hdf.h dffunc.h netcdf.h
rnw_rdwind.o: vam.h iosubs.h ns_winds.h ptdta.h rdglob20.h
rpk_rdskip.o: vam.h iosubs.h
rsl_rdlos.o: vam.h iosubs.h ssmidata.h namelist_rsl_rdlos.h
rso_default.o: vam.h iosubs.h grname.h
rso_rdback.o: vam.h iosubs.h gparm.h ptdta.h
rso_rddata.o: vam.h iosubs.h gparm.h ptdta.h
rso_rdsass.o: vam.h iosubs.h ptdta.h
rso_rdukmo.o: vam.h iosubs.h gparm.h ptdta.h
rss_default.o: vam.h iosubs.h grname.h
rss_rddata.o: vam.h iosubs.h ptdta.h gparm.h
rss_rdsass.o: vam.h iosubs.h ptdta.h
sbogus.o: vam.h iosubs.h gparm.h sldta.h
scale.o: rdglob17.h
scale20.o: rdglob20.h
sdata.o: vam.h iosubs.h er_bscat.h ns_bscat.h gparm.h ptdta.h sldta.h
sdata.o: ssmidata.h
setcd.o: vam.h iosubs.h gparm.h grdta.h
setindices.o: rdglob17.h
setindices20.o: rdglob20.h
showqc.o: vam.h iosubs.h er_bscat.h
showqcn.o: vam.h iosubs.h ns_bscat.h
sl_grosscheck.o: vam.h iosubs.h sldta.h
sl_print.o: vam.h iosubs.h sldta.h
sl_superob.o: vam.h iosubs.h gparm.h sldta.h
solve.o: vam.h iosubs.h gparm.h grdta.h ptdta.h weight.h
tcheck.o: vam.h iosubs.h
trig1.o: vam.h iosubs.h gparm.h grdta.h
updaterec.o: rdglob17.h
updaterec20.o: rdglob20.h
vam.o: vam.h iosubs.h
vordiv.o: vam.h iosubs.h gparm.h grdta.h
wbo_default.o: vam.h iosubs.h
wbo_wrback.o: vam.h iosubs.h
wbo_wrdata.o: vam.h iosubs.h er_bscat.h gparm.h grdta.h
wbo_wrnback.o: vam.h iosubs.h
wbo_wrnwind.o: vam.h iosubs.h ns_bscat.h gparm.h grdta.h
wbo_wrwind.o: vam.h iosubs.h er_bscat.h gparm.h grdta.h
wca_wrwind.o: vam.h iosubs.h sldta.h gparm.h grdta.h
wco_default.o: vam.h iosubs.h
wco_wrconv.o: vam.h iosubs.h
wco_wrdata.o: vam.h iosubs.h sldta.h gparm.h grdta.h
wga_wrgrid.o: vam.h iosubs.h gparm.h grdta.h
wgp_wrdata.o: vam.h iosubs.h gparm.h grdta.h
wgp_wrgrid.o: vam.h iosubs.h gparm.h
wja_wrwind.o: vam.h iosubs.h gparm.h grdta.h ns_winds.h ptdta.h
wna_wrback.o: vam.h iosubs.h gparm.h grdta.h ns_bscat.h namelist_wna_wrback.h
wna_wrwind.o: vam.h iosubs.h gparm.h grdta.h ns_bscat.h ns_winds.h
write_event.o: vam.h iosubs.h
wsl_wrlos.o: vam.h iosubs.h gparm.h grdta.h ssmidata.h namelist_wsl_wrlos.h
wss_default.o: vam.h iosubs.h
wss_wrdata.o: vam.h iosubs.h ptdta.h
wss_wrsass.o: vam.h iosubs.h ptdta.h
rdccomp.o: hdfshell.h hdf.h

