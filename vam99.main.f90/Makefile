#
#Fortran options
F90 = f90
FFLAGS = -O3 	#NOTE: recursively expanded variables

dotdot = ../bin.fortran
Minf90dir = $(dotdot)/minimize.f90dir
Fftf90dir = $(dotdot)/fft.f90dir

LDFLAGS   = -g -L$(Minf90dir) -L$(Fftf90dir) -lmin -lfft

SYS_DEF   = -lnsl -lm

# these will load in before libraries (06/03/98):

MAINOBJ:=vam_main.o

#For adjoint setenv VAM_ADJ TRUE
bin_dir:= $(dotdot)
#Emacs lisp code will be loaded from file:
elisp = /home/rhoffman/generic/emacs/src/adjoint.elc
#Include rules for fortran libraries and adjoint codes:
# For adjoint determine list of *.F which are not targets of *.for.
#
make-adjoint = $(bin_dir)/make-adjoint
include $(make-adjoint)
#
# forfiles defined in make-adjoint instead
inclforfiles:=bidim.for uvinterp.for fcainterp.for fcagrid.for
inclforFfiles:=$(patsubst %.for,%.F,$(inclforfiles))
inclallforFfiles:=$(sort $(inclforFfiles) \
 $(patsubst %.F,%tl.F, $(inclforFfiles)) \
 $(patsubst %.F,%ad.F,$(inclforFfiles)) )
#
forFfiles = $(patsubst %.for,%.F,$(filter-out $(inclforfiles), $(forfiles)))
allforFfiles = $(sort $(forFfiles) $(patsubst %.F,%tl.F,$(forFfiles)) \
 $(patsubst %.F,%ad.F,$(forFfiles)) )
MAINFfiles:=interp_mod.F fca_grid_mod.F
ffiles=$(filter-out $(patsubst  %.F,%.f, \
 $(sort $(MAINFfiles) $(allforFfiles) $(inclallforFfiles) )), $(wildcard *.f))
#
allforFobj = $(patsubst %.F, %.o, $(allforFfiles))
MAINFobj=$(patsubst %.F,%.o,$(MAINFfiles))
fobj=$(patsubst %.f,%.o,$(ffiles))
OBJ = $(filter-out $(MAINOBJ), $(sort $(MAINFobj) $(fobj) $(allforFobj) ))
OBJ : $(OBJ)

F90LIBS : 
	cd $(Minf90dir); $(MAKE) lib ;\
	cd ..; cd $(Fftf90dir); $(MAKE) lib 

#### For development I find it best to use this.
#### vam : $(MAINOBJ) $(OBJ) 
vam : cleanadj F90LIBS $(MAINOBJ) $(OBJ) 
	$(F90) $(MAINOBJ) $(OBJ) $(LDFLAGS) $(SYS_DEF) -o $@

uvinterp.F: uvinterp.for
bidim.F: bidim.for
fcainterp.F: fcainterp.for
fcagrid.F: fcagrid.for
interp_mod.o: interp_mod.F uvinterp.F bidim.F
fca_grid_mod.o: fcainterp.F fcagrid.F

%.o: %.f
	$(F90) $(FFLAGS) -c $<

%.o: %.F
	$(F90) $(FFLAGS) -fixedform -c $<

history: vam namelist
	./vam <namelist >history

diffs: history history.old
	-diff history history.old > hdiffs
	@echo 'Differences:'
	@wc -l hdiffs 

MAINtest:
	@echo Ffiles: $(Ffiles)
	@echo MAINFfiles: $(MAINFfiles)
	@echo forfiles: $(forfiles)
	@echo inclforfiles: $(inclforfiles)
	@echo forFfiles: $(forFfiles)
	@echo allforFfiles: $(allforFfiles)
	@echo allforFobj: $(allforFobj)
	@echo ffiles: $(ffiles)
	@echo MAINFobj: $(MAINFobj)
	@echo fobj: $(fobj)
	@echo MAINOBJ: $(MAINOBJ)
	@echo OBJ: $(OBJ)
	@echo interesting_files: $(interesting_files)

checkpoint: history.old $(ffiles) $(Ffiles) $(forfiles)
	ci -l -tRcsInfo $?
	touch checkpoint

clean: cleanit

cleanit: cleanadj
	-$(RM) vam *.o *.T *.M *.mod
	-$(RM) $(patsubst %.F,%.f,$(sort $(inclallforFfiles) $(allforFfiles)) )

cleanlibs :
	cd $(Minf90dir); $(MAKE) clean ; \
	cd ..; cd $(Fftf90dir); $(MAKE) clean 

clean_all : clean cleanlibs

# Documentation

# interesting_files is the combination of these:
interesting_files = $(sort $(ffiles) $(Ffiles) $(forfiles))

ETAGS: $(interesting_files)
# ETAGS can't process the acronym tables for some reason
	etags $^

all.variables: $(interesting_files)
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

# auxilary files which self extract from this make file
auxilary_files = RcsInfo

$(auxilary_files): %: Makefile
# Extract the file from the Makefile
	@sed -n 's/#$@: //p' $^ > $@

# These lines are stored in the RCS/*,v files and are seen when using rlog.
#RcsInfo: NSCAT and SeaWinds Scatterometer f90 code for vam99.
#RcsInfo: The scatterometer projects (AER P145,P495,P651,P725) are funded 
#RcsInfo: by NASA through JPL and OSU.

# The following lines created by running 
# perl ~/generic/scripts/makef90depends.pl 
# CAUTION makef90depends gets confused if two files contain the same module.
cgr_mod.o: constants.o grid_mod.o interp_mod.o ssback_mod.o types.o \
	vam_obs_mod.o
config_mod.o: string_mod.o
fgat_add_mod.o: grid_mod.o types.o vam_grid_mod.o vam_obs_mod.o
grid_mod.o: types.o
grid_ops.o: config_mod.o constants.o types.o
jb_mod.o: constants.o types.o
jscenv.o: constants.o s0_init_mod.o types.o
obs_amb_mod.o: constants.o grid_mod.o interp_mod.o string_mod.o types.o
obs_conv_mod.o: grid_mod.o interp_mod.o string_mod.o types.o
obs_s0_mod.o: grid_mod.o interp_mod.o jscenv.o s0_init_mod.o string_mod.o \
	types.o
obs_ssmi_mod.o: grid_mod.o interp_mod.o s0_init_mod.o string_mod.o types.o
s0_init_mod.o: s0_tblio_mod.o types.o
s0_tblio_mod.o: config_mod.o constants.o types.o
solve_mod.o: cgr_mod.o constants.o grid_mod.o types.o vam_obs_mod.o
ssback_mod.o: grid_mod.o interp_mod.o jb_mod.o types.o
test_mod.o: types.o
test_ssmi_mod.o: string_mod.o types.o
vam_grid_mod.o: cgr_mod.o constants.o fca_grid_mod.o grid_mod.o grid_ops.o \
	interp_mod.o solve_mod.o ssback_mod.o types.o vam_obs_mod.o \
	wind_grid_stats_mod.o
vam_main.o: fgat_add_mod.o grid_mod.o s0_init_mod.o solve_mod.o string_mod.o \
	test_mod.o types.o vam_grid_mod.o vam_obs_mod.o
vam_obs_mod.o: constants.o grid_mod.o interp_mod.o obs_amb_mod.o \
	obs_conv_mod.o obs_s0_mod.o obs_ssmi_mod.o string_mod.o types.o
winds.o: constants.o
fca_grid_mod.o: constants.o grid_mod.o interp_mod.o types.o
interp_mod.o: constants.o
jsccalc.o: types.o
jsccalcad.o: types.o
jsccalctl.o: types.o
jsccjo.o: types.o
jsccjoad.o: types.o
jsccjotl.o: types.o
jscee.o: s0_init_mod.o
jsceead.o: s0_init_mod.o
jsceetl.o: s0_init_mod.o
jsces0.o: constants.o s0_init_mod.o types.o
jsces0ad.o: constants.o s0_init_mod.o types.o
jsces0tl.o: constants.o s0_init_mod.o types.o
jscesd.o: s0_init_mod.o
jscesdad.o: s0_init_mod.o
jscesdtl.o: s0_init_mod.o
jscevd.o: s0_init_mod.o
jscevdad.o: s0_init_mod.o
jscevdtl.o: s0_init_mod.o
