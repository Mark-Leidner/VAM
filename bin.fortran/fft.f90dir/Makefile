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

bin_dir:= ..

# Added 2/4/97, rnh
Ffiles := $(wildcard *.F)
forfiles := $(wildcard *.for)
Ftargets := $(subst .for,.F,$(forfiles)) $(subst \
 .for,tl.F,$(forfiles)) $(subst .for,ad.F,$(forfiles)) 
Fnontargets := $(filter-out $(Ftargets),$(Ffiles))

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

users:= $(sort rnh trn $(USER))
export interesting_files users comment bin_dir

.PHONY: rcstest rcstell co cov ci civ rcsclean rcscleanv

rcstest rcstell co cov ci civ rcsclean rcscleanv: RcsInfo
	@$(MAKE) -f $(bin_dir)/RCSMakefile --no-print-directory $@

# auxilary files which self extract from this make file
auxilary_files = D2latex.sed HelpFile RcsInfo

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

all.d: $(patsubst %.f,%.d,$(files_in_dir))
# concatenate all .d files which derive from a .f file in dir
	ifneq ($(strip $(patsubst %.f,%.d,$(files_in_dir))),)
		cat $^ > $@
	endif

# Formatting

%.latex: %.d start.doc end.doc D2latex.sed
	cat start.doc $< end.doc | sed -f D2latex.sed > $@

start.doc end.doc: %: RCS/../doc/%,v
		$(CO) $(COFLAGS) $<

# Cleaning

clean: clean_docs clean_formatting 

# Auxiliary files:

# These lines are stored in the RCS/*,v files and are seen when using rlog.
#RcsInfo: Fourier transform code used by VAM.
#RcsInfo: The scatterometer projects (AER P145,P495,P651,P725) are funded 
#RcsInfo: by NASA through JPL and OSU.

# Generic functionality ================================================

$(auxilary_files): %: Makefile
# Extract the file from the Makefile
	@sed -n 's/#$@: //p' $^ > $@

# This sed script adds latex macro lines for formatting in place of
# the lines with '!# CSU' in the original file.
#D2latex.sed: /CSU IDENTIFICATION/ s/.*CSU IDENTIFICATION[ :]*\(.*\)$/\\xxxx{ \1 } \\begin{verbatim} IDENTIFICATION : \1/
#D2latex.sed: /CSU SPECIFICATION AND CONSTRAINTS/c\
#D2latex.sed: \\end{verbatim} \
#D2latex.sed: \\xxxxx{ Specification and constraints } \
#D2latex.sed: \\begin{verbatim}
#D2latex.sed: /CSU DESIGN/c\
#D2latex.sed: \\end{verbatim} \
#D2latex.sed: \\xxxxx{ Design } \
#D2latex.sed: \\begin{verbatim}
#D2latex.sed: /CSU END/c\
#D2latex.sed: \\end{verbatim}

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

%.d: %.f
	(expand $< ; echo '.!# CSU END .!#') | \
	sed -n '/!#/s/.!#~*//gp' > $@

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

# Specifics for GNUmakefile for fft decks on sparc sunOS
# $Id: Makefile,v 1.3 2001/01/04 18:50:00 mcc Exp $
# $Log: Makefile,v $
# Revision 1.3  2001/01/04 18:50:00  mcc
# Makefile was altered to compile on SGI machine. See Makefile.readme in
# this directory.
#
# Revision 1.1  2001/01/04 12:08:42  mcc
# Makefile to compile on SGI machine.
#
# Revision 1.2  2000/11/08 13:38:09  mcc
# Changed 'bin_dir' to '..' . Added target for '.o' which
# was accidentally deleted.
#
# Revision 1.1  2000/11/07 20:03:42  mcc
# Makefile added here to build fft.f90dir lib.
# Initial Revision.
#
# Revision 1.2  1997/02/10 18:51:04  leidner
# CPPFLAGS now empty
# adjoint compile replaced with fortran compile
# lib now libfft.a
#
#!#Revision 1.1  1997/02/10  18:39:39  leidner
#!#Initial revision
#!#

#Fortran options
FFLAGS = -r8 -g

#Alternative directories for include files
CPPFLAGS =

#Name of the library archive:
lib = libfft.a

#Include rules for fortran libraries and adjoint codes:
make-fortran = $(bin_dir)/make-fortran
include $(make-fortran)

# 5/14/98 - sgr - f90
%.o: %.F
	f90 $(CPPFLAGS) -fixedform -O3 -c $<

####################################################################
# Rules after next line generated automatically using gmake depend
# DO NOT DELETE THIS LINE -- make depend depends on it.
