########################################################################
########################DO NOT MODIFY BELOW THIS LINE###################

MAKE = make CC="$(CC)" AS="$(AS)" LD="$(LD)" AR="$(AR)" CPP="$(CPP)" MACHINE="$(MACHINE)" SYS_DEF="$(SYS_DEF)" SHELLLIB="$(SHELLLIB) HDFDIR="$(HDFDIR) HDFINC=$(HDFINC)

LDFLAGS = 	$(SHELLLIB)/nscatshell.a \
		-L$(HDFDIR)/lib -lmfhdf -ldf -ljpeg -lz 
# c compiler
ARCHIVE = 	$(SHELLLIB)/nscatshell.a

CFLAGS	= 	-D$(MACHINE) $(SYS_DEF) -I. -I$(HDFINC)

F77	= 	f77  $(F77_OPTS) -w

SRC1	=  	fnscat.c     hdfshell.c 
SRC2	=  	nscatff.f

OBJ1 	=	fnscat.o     hdfshell.o
OBJ2 	= 	nscatff.o

SRC3	=	testc.c      testc1.c
OBJ3 	= 	testc.o      testc1.o

SRC4	=	testf.f
OBJ4	=	testf.o

# install the library in the local library directory

all:		library

library:	target1	target2 link

target1:	
	$(CC) -c $(SRC1) $(CFLAGS)

target2:	
	$(F77) -c $(SRC2) $(F77_OPTS) -I$(HDFINC) $(SYS_DEF)

link:	hdfshell.o fnscat.o nscatff.o
	ar r $(ARCHIVE) hdfshell.o fnscat.o nscatff.o 
	@echo "Library NSCATSHELL has been successfully created"

clean:
	rm -f *.o testc testc1 testf
	rm -f ./lib/nscatshell.a

