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

comment:= c

bin_dir:= ../bin.fortran

# interesting files presently in directory
files_in_dir:=$(Fnontargets) $(forfiles) $(wildcard *.h)
# all rcs files which co to interesting files
comma:= ,
files_in_rcs:= $(patsubst RCS/%$(comma)v,%,$(wildcard RCS/*.F,v)) \
  $(patsubst RCS/%$(comma)v,%,$(wildcard RCS/*.for,v)) \
  $(patsubst RCS/%$(comma)v,%,$(wildcard RCS/*.h,v))

# interesting_files is the combination of the two:
interesting_files:= $(sort $(files_in_dir) $(files_in_rcs))

# Compilation

all_object_files:=$(patsubst %.F,%.o,$(sort $(Ffiles) $(Ftargets)))

OBJECTS: $(all_object_files)

users:= $(sort rnh trn leidner cgrass $(USER))
export interesting_files users comment bin_dir

.PHONY: rcstest rcstell co cov ci civ rcsclean rcscleanv

rcstest rcstell co cov ci civ rcsclean rcscleanv: RcsInfo
	@$(MAKE) -f /home/nwp/NWP/bin/RCSMakefile --no-print-directory $@

# auxilary files which self extract from this make file
auxilary_files = StripEmpty D2latex.sed HelpFile RcsInfo

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

%.latex: %.d start.doc end.doc StripEmpty D2latex.sed
	cat start.doc $< end.doc | sed -f D2latex.sed | \
	sed -e '/^ *\\xxxx{/s/_/\\_/g' > temp
	sed -f StripEmpty temp > $@
	"rm" temp


start.doc end.doc: %: RCS/%,v
		$(CO) $(COFLAGS) $<

# Cleaning

clean: clean_docs clean_formatting 

# Auxiliary files:

# These lines are stored in the RCS/*,v files and are seen when using rlog.
#RcsInfo: Component of the objective function calculation in the VAM.
#RcsInfo: The scatterometer projects (AER P145,P495,P651,P725) are funded 
#RcsInfo: by NASA through JPL and OSU.

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
#StripEmpty: /^# /N
#StripEmpty: /\:\n# /D
#StripEmpty: /\:\n.*verbatim/D
#StripEmpty: /# /s/# / /g
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
	echo making intermed ddd file
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

# Specifics for GNUmakefile for VAM functionals on sparc sunOS
# $Id: Makefile,v 1.4 2001/04/13 16:52:08 mcc Exp $
# $Log: Makefile,v $
# Revision 1.4  2001/04/13 16:52:08  mcc
# Added specification to -xtypemap=real:64,double:128,integer:mixed .
#
# Revision 1.3  2001/04/10 10:39:22  mcc
# Replaced -r8 in FFLAGS with -xtypemap .
#
# Revision 1.2  2000/11/13 18:54:16  mcc
# Changed bin_dir to  ../bin.fortran .
#
# Revision 1.1  2000/11/13 13:47:07  mcc
# File added for build of libss.a. Initial revision.
#
# Revision 1.6  1998/09/29 12:25:12  leidner
# upgraded documentation processing
#
# Revision 1.5  1998/01/14 14:59:36  leidner
# replaced make-fortran with make-adjoint, then updated dependencies
#
#!#Revision 1.4  1997/04/09  15:07:40  rnh
#!#gmake depend
#!#Added 12 point bicubic interpolation,
#!#controlled by parameter interp in /gparm/.
#!#
#!#Revision 1.3  1997/02/27  19:20:29  leidner
#!#dependencies updated for new include filenames (all changed to *.h)
#!#
#!#Revision 1.3  1997/02/19  15:34:53  leidner
#!#dependencies updated for new include filenames (all changed to *.h)
#!#
#!#Revision 1.2  1997/02/10  19:23:52  leidner
#!#CPPFLAGS now ../vam2d.fortran
#!#adjoint compile replaced with fortran compile
#!#lib now libss.a
#!#
#!#Revision 1.1  1997/02/10  19:00:48  leidner
#!#Initial revision
#!#

#Fortran options
FFLAGS = -w -xtypemap=real:64,double:128,integer:mixed  -g

#Alternative directories for include files
CPPFLAGS = -I../vam2d.fortran

#Name of the library archive:
lib = libss.a

#Emacs lisp code will be loaded from file:
elisp = /home/nwp/NWP/bin/adjoint.elc
#Include rules for fortran libraries and adjoint codes:
make-adjoint = $(bin_dir)/make-adjoint
include $(make-adjoint)

####################################################################
# Rules after next line generated automatically using gmake depend
# DO NOT DELETE THIS LINE -- make depend depends on it.

dtvort.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
dtvort.o: ../vam2d.fortran/gparm.h ../vam2d.fortran/grdta.h
filfft.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
filfft.o: ../vam2d.fortran/gparm.h
map.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
map.o: ../vam2d.fortran/gparm.h
sscgr.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
sscgr.o: ../vam2d.fortran/gparm.h ../vam2d.fortran/grdta.h
sscgr.o: ../vam2d.fortran/weight.h ../vam2d.fortran/ssmidata.h
ssconv.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
ssconv.o: ../vam2d.fortran/gparm.h ../vam2d.fortran/sldta.h
ssdiv.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
ssdiv.o: ../vam2d.fortran/gparm.h
ssdtz.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
ssdtz.o: ../vam2d.fortran/gparm.h
ssers1.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
ssers1.o: ../vam2d.fortran/er_bscat.h ../vam2d.fortran/gparm.h
ssfilt.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
ssfilt.o: ../vam2d.fortran/gparm.h
ssfore.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
ssfore.o: ../vam2d.fortran/gparm.h
ssnscat.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
ssnscat.o: ../vam2d.fortran/ns_bscat.h ../vam2d.fortran/gparm.h
sssass.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
sssass.o: ../vam2d.fortran/ptdta.h ../vam2d.fortran/gparm.h
ssssmi.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
ssssmi.o: ../vam2d.fortran/ssmidata.h ../vam2d.fortran/gparm.h
ssvelm.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
ssvelm.o: ../vam2d.fortran/ptdta.h ../vam2d.fortran/gparm.h
ssvort.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
ssvort.o: ../vam2d.fortran/gparm.h
uvinterp.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
uvinterp.o: ../vam2d.fortran/gparm.h
uvinterpad.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
uvinterpad.o: ../vam2d.fortran/gparm.h
uvinterptl.o: ../vam2d.fortran/vam.h ../vam2d.fortran/iosubs.h
uvinterptl.o: ../vam2d.fortran/gparm.h
