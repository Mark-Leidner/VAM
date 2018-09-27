c
c     ******************************************************************
c
      subroutine read_gdas
c
c**** read_gdas reads in one global analysis from NCEP GDAS
c
c!#   $Id: read_gdas.F,v 1.3 1997/04/14 16:07:20 leidner Exp $
c!#   $Log: read_gdas.F,v $
c!#   Revision 1.3  1997/04/14 16:07:20  leidner
c!#   updated and added documentation
c!#
c!#	Revision 1.2  1997/04/07  16:47:24  leidner
c!#	changed int to int*4 and cleaned up code a bit
c!#
c!#	Revision 1.1  1997/02/24  21:40:41  leidner
c!#	Initial revision
c!#
c     PURPOSE
c
c     Read in NCEP's GDAS data from an unpacked grib file
c
c**   INTERFACE
c
     I    ( iu,
     O    im, jnp, kpds, kgds, u, v)
c
c*    Inputs
c     iu       file unit number of input file
      integer iu
c
c*    Outputs
c     im       x dimension of grid
c     jnp      y dimension of grid
c     kgds     grid description section
c     kpds     product description section
c     u        u-component of gridded 10 m wind field
c     v        v-component of gridded 10 m wind field
      integer im, jnp
      integer*4 kgds(200),kpds(100)
      real u(im*jnp), v(im*jnp)
c
c**   METHOD
c
c     Read header from unpacked grib file, read gridded u and v fields
c     into real*4 arrays, store in real*8 arrays and pass out of routine.
c    
c**   EXTERNALS
c
c     None
c
c**   REFERENCES
c
c     None
c 
c     ------------------------------------------------------------------
c
c*    0.  Define local variables
c
c     mxsize   maximum size of an unpacked grib record
c     msize    dimension of read arrays
c     lenkgds  length (in words) of the Grid Description Section
c     lenpds   length (in words) of the Product Description Section
c     merr     io status of nfile
c     nwords   number of unpacked floating point numbers in the grib record
      parameter (mxsize=100000)
      parameter (msize=mxsize*3)
      integer*4 lenkgds,lenpds,merr,nwords
      real*4 us(msize),vs(msize)
c
c     ------------------------------------------------------------------
c
c**   1.  Loop over u and v grib records
c
      do i=1,2
c
c     ------------------------------------------------------------------
c
c**   1.1.  READ 1st record: record dimensions for records 2-4
c           3 integer words: (1) length of kpds in words, (2) number 
c           of words in kgds, (3) number of unpacked floating point
c           numbers in the grib record
c
        read (iu) lenpds,lenkgds,nwords
c
c     ------------------------------------------------------------------
c
c**   1.2.  READ 2nd record: product description section (pds)
c
        read (iu) (kpds(j),j=1,lenpds)
c
c     ------------------------------------------------------------------
c
c**   1.3.  READ 3rd record: grid description section (gds)
c
        if (lenkgds.ne.0) then
          read (iu) (kgds(j),j=1,lenkgds)
        endif
        im = kgds(2)
        jnp = kgds(3)
c
c     ------------------------------------------------------------------
c
c**   1.4.  READ 4th record: unpacked grib floating point numbers
c
        if (i.eq.1) read (iu) (us(j),j=1,nwords)
        if (i.eq.2) read (iu) (vs(j),j=1,nwords)
c
      enddo
c
c     ------------------------------------------------------------------
c
c**   2.  Store unpacked grib floating point numbers in a real*8 array
c
      do j=1,nwords
        u(j)=dble(us(j))
        v(j)=dble(vs(j))
      end do
c      
      return
      end
