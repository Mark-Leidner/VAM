c!#  $Id: obsio.f,v 1.9 2004/09/30 16:22:03 leidner Exp $
c!#
c!#  Purpose: I/O routines for obs data sets
c!#           f77 compliant routines to be called by VAM99 and preprocessors

c     -----------------------------------------------------------------

      subroutine wrtobs(iuvam, hfname, bfname,
     &     cobsid, idate, itime, nconst, nloc, nocc, nvar,
     &     namcon, vconst, namvar, recl,
     &     numocc, recid, time, latdeg, londeg,
     &     uint, vint, ufgat, vfgat, alpha,   
     &     qcflag, qc_bit_map, data,
     &     ierr)
c
c!#  Purpose: write relevant contents of obs_data_typ to file
c
      implicit none
c
      integer lenname
      parameter (lenname = 10)
c!# Input arguments:
      integer iuvam	!#I/O unit number (to be opened and closed)
      character hfname*(*), bfname*(*)	!#file names used for I/O
      character*(*) cobsid		!#obs ID
      integer idate, itime	!#obs_data_typ scalars
      integer nconst, nloc, nocc, nvar	!#obs_data_typ scalars
      character*(lenname) namcon(nconst)	!#names of constants
      real vconst(nconst)		!#names of variables
      character*(lenname) namvar(nvar)	!#names of variables
      integer recl	!#record length parameter for open statement
      integer recid(nloc), numocc(nloc)	!#record ID, number of occurences
      real time(nloc), latdeg(nloc), londeg(nloc) !#time and location
      real uint(nloc), vint(nloc) !#interpolated winds
      real ufgat(nloc), vfgat(nloc), alpha(nloc) !#fgat info
      logical qcflag(nocc, nloc)	!#QC flags
      integer qc_bit_map(nocc, nloc)	!#QC bit map
      real data(nvar, nocc, nloc)	!#obs data

c!# Output arguments:
      integer ierr	!#error code (nonzero if errors)
c
      call wrthob(iuvam, hfname, cobsid, idate, itime, 
     &   nconst, nloc, nocc, nvar, namcon, vconst, namvar, ierr)
      if (ierr .ne. 0) goto 900

      call wrtdob(iuvam, bfname, nloc, nocc, nvar, recl,
     &     numocc, recid, time, latdeg, londeg,
     &     uint, vint, ufgat, vfgat, alpha,   
     &     qcflag, qc_bit_map, data, ierr)
      if (ierr .ne. 0) goto 900
c
      ierr = 0
  900 return
      end

c     -----------------------------------------------------------------

      subroutine wrthob(iuvam, fname, cobsid, idate, itime, nconst,
     &    nloc, nocc, nvar, namcon, vconst, namvar, ierr)
c
c!#  Purpose: open file, write header of obs_data_typ to file, close file
c
      implicit none
c
      integer lenname
      parameter (lenname = 10)
c!# Input arguments:
      integer iuvam	!#I/O unit number (to be opened and closed)
      character fname*(*)	!#file name used for I/O
      character*(*) cobsid		!#obs ID
      integer idate, itime	!#obs_data_typ scalars
      integer nconst, nloc, nocc, nvar	!#obs_data_typ scalars
      character*(lenname) namcon(nconst)	!#names of constants
      real vconst(nconst)		!#names of variables
      character*(lenname) namvar(nvar)	!#names of variables
c!# Output arguments:
      integer ierr	!#error code (nonzero if errors)
c!# Local variables:
      integer i, fnlen
c
      ierr = 1
      fnlen = index(fname,' ') - 1
      open(unit=iuvam, file=fname(1:fnlen), form='formatted',
     &     access='sequential', status='unknown', err=900)
c
      ierr = 2
      write (iuvam, '(a)', err=900) cobsid
      ierr = 3
      write (iuvam, *, err=900) idate
      write (iuvam, *, err=900) itime
      write (iuvam, *, err=900) nloc
      write (iuvam, *, err=900) nocc
      write (iuvam, *, err=900) nvar
      ierr = 4
      do 10 i=1,nvar
         write (iuvam, '(a)', err=900) namvar(i)
   10 continue
      write (iuvam, *, err=900) nconst
      ierr = 5
      do 20 i=1,nconst
         write (iuvam, '(a)', err=900) namcon(i)
         write (iuvam, *, err=900) vconst(i)
   20 continue

      ierr = 6
  900 close(iuvam,err=901)
      if (ierr .eq. 6) ierr=0
  901 return
      end

c     -----------------------------------------------------------------

      subroutine wrtdob(iuvam, fname, nloc, nocc, nvar, recl,
     &     numocc, recid, time, latdeg, londeg,
     &     uint, vint, ufgat, vfgat, alpha,   
     &     qcflag, qc_bit_map, data, ierr)
c
c!#  Purpose: write relevant contents of obs_data_typ to file
c
      implicit none
c
c!# Input arguments:
      integer iuvam	!#I/O unit number (to be opened and closed)
      character fname*(*)	!#file name used for I/O
      integer nloc, nocc, nvar	!#obs_data_typ scalars
      integer recl	!#record length parameter for open statement
      integer recid(nloc), numocc(nloc)	!#record ID, number of occurences
      real time(nloc), latdeg(nloc), londeg(nloc) !#time and location
      real uint(nloc), vint(nloc) !#interpolated winds
      real ufgat(nloc), vfgat(nloc), alpha(nloc) !#fgat info
      logical qcflag(nocc, nloc)	!#QC flags
      integer qc_bit_map(nocc, nloc)	!#QC bit map
      real data(nvar, nocc, nloc)	!#obs data
c!# Output arguments:
      integer ierr	!#error code (nonzero if errors)
      integer ios	!#io status
c!# Local Data:
      integer fnlen, i, igood, j, k

      igood = -777

      ierr = -1 ! for open error
      fnlen = index(fname,' ') - 1
      open(unit=iuvam, file=fname(1:fnlen), form='unformatted',
     &     access='direct', recl=recl, status='unknown', err=900)

      ierr = 0 ! index of record to be written
      ierr = ierr + 1
      write (iuvam, rec=ierr, iostat=ios, err=900) numocc
      ierr = ierr + 1
      write (iuvam, rec=ierr, err=900) recid
      ierr = ierr + 1
      write (iuvam, rec=ierr, err=900) time
      ierr = ierr + 1
      write (iuvam, rec=ierr, err=900) latdeg
      ierr = ierr + 1
      write (iuvam, rec=ierr, err=900) londeg
      ierr = ierr + 1
      write (iuvam, rec=ierr, err=900) uint
      ierr = ierr + 1
      write (iuvam, rec=ierr, err=900) vint
      ierr = ierr + 1
      write (iuvam, rec=ierr, err=900) ufgat
      ierr = ierr + 1
      write (iuvam, rec=ierr, err=900) vfgat
      ierr = ierr + 1
      write (iuvam, rec=ierr, err=900) alpha

      do 10 i=1,nocc
         ierr = ierr + 1
         write (iuvam, rec=ierr, err=900) (qcflag(i,j),j=1,nloc)
   10 continue

      do 15 i=1,nocc
         ierr = ierr + 1
         write (iuvam, rec=ierr, err=900) (qc_bit_map(i,j),j=1,nloc)
  15   continue

      do 30 j=1,nocc
         do 20 i=1,nvar
            ierr = ierr + 1
            write (iuvam, rec=ierr, err=900) (data(i,j,k),k=1,nloc)
   20    continue
   30 continue
c
      ierr = -ierr ! for close error
      igood = ierr
  900 close (iuvam, err=901)
      if (ierr .eq. igood) ierr = 0
  901 return
      end

c     -----------------------------------------------------------------

      subroutine rdhobs(iuvam, fname, mconst, mvar,
     &     cobsid, idate, itime, nconst, nloc, nocc, nvar,
     &     namcon, vconst, namvar, ierr)
c
c!#  Purpose: read header of obs_data_typ from file
c
      implicit none
c
      integer lenname
      parameter (lenname = 10)
c!# Input arguments:
      integer iuvam	!#I/O unit number (to be opened)
      character fname*(*)	!#file name
      integer mconst, mvar !#size of const and var arrays
c!# Output arguments:
      character*(*) cobsid		!#obs ID
      integer idate, itime	!#obs_data_typ scalars
      integer nconst, nloc, nocc, nvar	!#obs_data_typ scalars
      character*(lenname) namcon(mconst)	!#names of constants
      real vconst(mconst)		!#names of variables
      character*(lenname) namvar(mvar)	!#names of variables
      integer ierr	!#error code (nonzero if errors)
c!# Local variables:
      integer i, istore, fnlen
c
      ierr = 1
      fnlen = index(fname,' ') - 1
      open(unit=iuvam, file=fname(1:fnlen), form='formatted',
     &     access='sequential', status='old', err=900)
c
      ierr = 2
      read (iuvam, '(a)', err=900, end=900) cobsid
      ierr = 3
      read (iuvam, *, err=900, end=900) idate
      read (iuvam, *, err=900, end=900) itime
      read (iuvam, *, err=900, end=900) nloc
      read (iuvam, *, err=900, end=900) nocc
      read (iuvam, *, err=900, end=900) nvar
      ierr = 4
      do 10 i=1,nvar
         istore = min(i,mvar)
         read (iuvam, '(a)', err=900, end=900) namvar(istore)
   10 continue
      read (iuvam, *, err=900, end=900) nconst
      ierr = 5
      do 20 i=1,nconst
         istore = min(i,mconst)
         read (iuvam, '(a)', err=900, end=900) namcon(istore)
         read (iuvam, *, err=900, end=900) vconst(istore)
   20 continue

      ierr = 6
  900 close(iuvam,err=901)
      if (ierr .eq. 6) ierr=0
  901 return
      end

c     -----------------------------------------------------------------

      subroutine rddobs(iuvam, fname, nloc, nocc, nvar, recl,
     &     numocc, recid, time, latdeg, londeg,
     &     uint, vint, ufgat, vfgat, alpha,   
     &     qcflag, qc_bit_map, data, ierr)
c
c!#  Purpose: read relevant data of obs_data_typ from file
c
      implicit none
c
c!# Input arguments:
      integer iuvam	!#I/O unit number (to be closed upon completion)
c!# Output arguments:
      character fname*(*)	!#file name used for I/O
      integer nloc, nocc, nvar	!#obs_data_typ scalars
      integer recl	!#record length parameter for open statement
      integer recid(nloc), numocc(nloc)	!#record ID, number of occurences
      real time(nloc), latdeg(nloc), londeg(nloc) !#time and location
      real uint(nloc), vint(nloc) !#interpolated winds
      real ufgat(nloc), vfgat(nloc), alpha(nloc) !#fgat info
      logical qcflag(nocc, nloc)	!#QC flags
      integer qc_bit_map(nocc, nloc)	!#QC bit map
      real data(nvar, nocc, nloc)	!#obs data
      integer ierr	!#error code (nonzero if errors)
c!# Local Data:
      integer fnlen, i, igood, j, k

      igood = -777

      ierr = -1 ! for open error
      fnlen = index(fname,' ') - 1
      open(unit=iuvam, file=fname(1:fnlen), form='unformatted',
     &     access='direct', recl=recl, status='old', err=900)
c
      ierr = 0 ! index of record to be read
      ierr = ierr + 1
      read (iuvam, rec=ierr, err=900) numocc
      ierr = ierr + 1
      read (iuvam, rec=ierr, err=900) recid
      ierr = ierr + 1
      read (iuvam, rec=ierr, err=900) time
      ierr = ierr + 1
      read (iuvam, rec=ierr, err=900) latdeg
      ierr = ierr + 1
      read (iuvam, rec=ierr, err=900) londeg
      ierr = ierr + 1
      read (iuvam, rec=ierr, err=900) uint
      ierr = ierr + 1
      read (iuvam, rec=ierr, err=900) vint
      ierr = ierr + 1
      read (iuvam, rec=ierr, err=900) ufgat
      ierr = ierr + 1
      read (iuvam, rec=ierr, err=900) vfgat
      ierr = ierr + 1
      read (iuvam, rec=ierr, err=900) alpha

      do 10 i=1,nocc
         ierr = ierr + 1
         read (iuvam, rec=ierr, err=900) (qcflag(i,j),j=1,nloc)
   10 continue

      do 15 i=1,nocc
         ierr = ierr + 1
         read (iuvam, rec=ierr, err=900) (qc_bit_map(i,j),j=1,nloc)
   15 continue

      do 30 j=1,nocc
         do 20 i=1,nvar
            ierr = ierr + 1
            read (iuvam, rec=ierr, err=900) (data(i,j,k),k=1,nloc)
   20    continue
   30 continue
c
      ierr = -ierr ! for close error
      igood = ierr
  900 close (iuvam, err=901)
      if (ierr .eq. igood) ierr = 0
  901 return
      end
