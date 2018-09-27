c!# CSU IDENTIFICATION : config_mod
c!#     $Id: config_mod.f,v 1.2 2000/01/20 16:35:17 trn Exp $

c!# PURPOSE : Read/Write configuration file

c!# CSU SPECIFICATION AND CONSTRAINTS:

c!# REQUIREMENTS : 
c!# Read/Write configuration file
c!# Read/write information in keyword = value format, in any order.
c!# Support default integer, real, dp, complex, and logical data types.
c!# Support multiple values per keyword.
c!# Ignore non-matching lines on input.

c!# CONSTRAINTS : 
c!# Format must be readable by list-directed I/O
c!# Only one keyword = value pair per line

c!# LANGUAGE : Fortran

c!# CSU DESIGN :

c!# INPUT/OUTPUT INTERFACE :

      MODULE config_mod   !#
c!# GLOBAL AND SHARED DATA : None
      USE string_mod   !#

      implicit none

c      INTERFACE name  !only needed for overloading
c         MODULE PROCEDURE name        !
c      END INTERFACE   !

      character (len=1), private :: types(5)=(/'i','r','d','c','l'/)
      integer, parameter, private :: typ_integer=1, typ_real=2,
     &     typ_double=3, typ_complex=4, typ_logical=5

      CONTAINS        !#
      SUBROUTINE read_config (inunit, fname, line,	!#
     &     nvar, lenvar, varnames, vartypes, varlengths,	!#
     &     ni, ivals, nr, rvals, nd, dvals, nc, cvals,	!#
     &     nl, lvals, varmatch, ierr, vprint)	!#

      integer, intent(in) :: inunit, nvar, lenvar, ni, nr, nd, nc, nl	!#
      character(len=*), intent(in) :: fname, vartypes(nvar)	!#
      character(len=lenvar), intent(in) :: varnames(nvar)	!#
      character(len=*), intent(out) :: line	!#
      integer, intent(in) :: varlengths(nvar)	!#
      logical, intent(in) :: vprint	!#

      integer, intent(inout) :: ivals(ni)	!#
      real, intent(inout) :: rvals(nr)	!#
      double precision, intent(inout) :: dvals(nr)	!#
      complex, intent(inout) :: cvals(nc)	!#
      logical, intent(inout) :: lvals(nl)	!#
      integer, intent(out) :: varmatch(nvar), ierr	!#

c!# DATA CONVERSION : None

c!# ALGORITHMS : 

c!# REFERENCES : None

c!# LIMITATIONS : 

c!# CHANGE LOG : 
c!#	$Log: config_mod.f,v $
c!#	Revision 1.2  2000/01/20 16:35:17  trn
c!#	Improved error handling
c!#	
c!#	Revision 1.1  1999/12/15 21:30:48  trn
c!#	Initial revision
c!#

c!# LOCAL DATA ELEMENTS :
      integer :: equal, start_key
      integer :: nread, nlines_match, nvar_match
      integer :: i, ierr_loc, ind, ivar
      integer :: knt_i, knt_r, knt_d, knt_c, knt_l

c!# LOCAL DATA STRUCTURES : 
      integer :: var_start(nvar)

c!# DATA FILES : None

c!# LOGIC FLOW AND DETAILED ALGORITHM: 

c!#     1. Initialize output:
      if (vprint) write (*,*) 'Reading config info from file ',
     &     trim(fname)
      nread = 0
      nlines_match = 0
      nvar_match = 0
      if (nvar .le. 0) then
         ierr=-5	!#Input nvar <= 0
         goto 900
      else
         varmatch(1:nvar) = 0
c!# ERROR HANDLING : return nonzero error codes
         if (sum(varlengths) .ne. ni+nr+nd+nc+nl) then
            ierr=-3	!#Inconcistent varlengths and ni, nr, nd, nc, nl
            goto 900
         endif
         knt_i = 0; knt_r = 0; knt_d = 0; knt_c = 0; knt_l = 0
         do i=1,nvar
            ivar = where_in(vartypes(i),types,1,1,5)
            select case (ivar)
            case (typ_integer)            !integer
               var_start(i) = knt_i + 1
               knt_i = knt_i + varlengths(i)
            case (typ_real)            !real
               var_start(i) = knt_r + 1
               knt_r = knt_r + varlengths(i)
            case (typ_double)            !dp
               var_start(i) = knt_d + 1
               knt_d = knt_d + varlengths(i)
            case (typ_complex)            !complex
               var_start(i) = knt_c + 1
               knt_c = knt_c + varlengths(i)
            case (typ_logical)            !logical
               var_start(i) = knt_l + 1
               knt_l = knt_l + varlengths(i)
            case default	!invalid type
               ierr = -6	!#invalid vartypes specified
               goto 900
            end select
         enddo
         if (knt_i .ne. ni .or. knt_r .ne. nr .or. knt_d .ne. nd .or.
     &        knt_c .ne. nc .or. knt_l .ne. nl) then
            ierr = -4	!#Inconcistent vartypes and ni, nr, nd, nc, nl
            goto 900
         endif
      endif

c!#     2. Open file
      ierr = -1	!#Error opening file
      open (unit=inunit,status='old',action='read',
     &     file=fname,err=900)

c!#     3. Read and parse input lines
      ierr = 1	!# ierr>0 : Error reading/decoding line no ierr
      do while (.TRUE.)
         read (inunit,'(a)',err=900,end=800) line
         nread = nread + 1
         equal = index(line,'=')
         start_key = verify(line,' ')
c!# Silently ignore lines not in the form keyword = value:
         if (equal .gt. start_key) then
            nlines_match = nlines_match + 1
            i = where_in(line(start_key:equal-1),varnames,
     &           equal-start_key,lenvar,nvar)
c!# or with non-matching keywords:
            if (i .gt. 0) then
               nvar_match = nvar_match + 1
               ind = var_start(i) + min(varmatch(i),varlengths(i))
               varmatch(i) = varmatch(i) + 1
               ivar = where_in(vartypes(i),types,1,1,5)
c!# Exit with error in case of read error for any of the values:
               select case (ivar)
               case (typ_integer)         !integer
                  read (line(equal+1:),*,err=900) ivals(ind)
               case (typ_real)         !real
                  read (line(equal+1:),*,err=900) rvals(ind)
               case (typ_double)         !dp
                  read (line(equal+1:),*,err=900) dvals(ind)
               case (typ_complex)         !complex
                  read (line(equal+1:),*,err=900) cvals(ind)
               case (typ_logical)         !logical
                  read (line(equal+1:),*,err=900) lvals(ind)
               end select
            endif
         endif
         ierr = ierr + 1
      enddo

  800 ierr = 0	!#error-free reading until EOF
c!# Do not flag if too many values in file for any one keyword:
c!# -do this in calling routine: if (any(varmatch .gt. varlengths)) ...

  900 if (vprint) write (*,'(a,i10)')
     &     ' Finished reading file with ierr= ',ierr

      if (ierr .ge. 0) then
         ierr_loc = 0 
         close(inunit,iostat=ierr_loc)
         if (ierr_loc .ne. 0 .and. ierr .eq. 0) ierr = -2 !#Error closing file
         if (vprint) then
            write (*,'(a,i10)') ' Total lines read= ',nread
            write (*,'(a,i10)')
     &           ' Lines matching keyword=value format = ',
     &           nlines_match
            write (*,'(a,i10)') ' Lines with keyword matching input = ',
     &           nvar_match
         endif
      endif

      return
      end subroutine read_config

      SUBROUTINE write_config (outunit, fname,	nhead, header_lines, !#
     &     nvar, lenvar, varnames, vartypes, varlengths,	!#
     &     ni, ivals, nr, rvals, nd, dvals, nc, cvals,	!#
     &     nl, lvals, fmti, fmtr, fmtd, fmtc, fmtl, ierr, vprint)	!#

      integer, intent(in) :: outunit, nhead, nvar, lenvar,	!#
     &     ni, nr, nd, nc, nl	!#
      character(len=*), intent(in) :: fname, vartypes(nvar),	!#
     &     fmti, fmtr, fmtd, fmtc, fmtl, header_lines(nhead)    !#
      character(len=lenvar), intent(in) :: varnames(nvar)	!#
      integer, intent(in) :: varlengths(nvar)	!#
      logical, intent(in) :: vprint	!#

      integer, intent(in) :: ivals(ni)	!#
      real, intent(in) :: rvals(nr)	!#
      double precision, intent(in) :: dvals(nr)	!#
      complex, intent(in) :: cvals(nc)	!#
      logical, intent(in) :: lvals(nl)	!#
      integer, intent(out) :: ierr	!#

c!# DATA CONVERSION : None

c!# ALGORITHMS : 

c!# REFERENCES : None

c!# LIMITATIONS : 

c!# CHANGE LOG : 
c!#	$Log: config_mod.f,v $
c!#	Revision 1.2  2000/01/20 16:35:17  trn
c!#	Improved error handling
c!#	
c!#	Revision 1.1  1999/12/15 21:30:48  trn
c!#	Initial revision
c!#

c!# LOCAL DATA ELEMENTS : 
      integer, parameter :: maxl=255
      character(len=4) :: fmtkey='(2a,'
      character(len=maxl) :: fmt
      integer :: i, ierr_loc, ivar, j
      integer :: knt_i, knt_r, knt_d, knt_c, knt_l

c!# LOCAL DATA STRUCTURES : None

c!# DATA FILES : None

c!# LOGIC FLOW AND DETAILED ALGORITHM: 

c!#     1. Open file
      if (vprint) write (*,*) 'Writing config info to file ',
     &     trim(fname)
      ierr = -1	!#Error opening file
      open (unit=outunit,status='replace',action='write',
     &     file=fname,err=900)
      ierr = 0
c!#     2. Write header lines, if any
      if (nhead .gt. 0) then
         do ierr=1,nhead	!# Error writing header_lines(ierr)
            write (outunit,'(a)',err=900) trim(header_lines(ierr))
         enddo
      endif

c!#     3. Write out keyword = value pairs
      knt_i = 0; knt_r = 0; knt_d = 0; knt_c = 0; knt_l = 0
      do i = 1, nvar
         ierr = 1000 + i            !#error writing varnames(i)
         ivar = where_in(vartypes(i),types,1,1,5)
c!# Exit with error in case of read error for any of the values:
         select case (ivar)
         case (typ_integer)               !integer
            write (fmt,'(3a)',err=900) fmtkey,trim(fmti),')'
            do j=1,varlengths(i)
               knt_i = knt_i + 1
               write (outunit,fmt,err=900) varnames(i),' = ',
     &              ivals(knt_i)
            enddo
         case (typ_real)               !real
            write (fmt,'(3a)',err=900) fmtkey,trim(fmtr),')'
            do j=1,varlengths(i)
               knt_r = knt_r + 1
               write (outunit,fmt,err=900) varnames(i),' = ',
     &              rvals(knt_r)
            enddo
         case (typ_double)               !dp
            write (fmt,'(3a)',err=900) fmtkey,trim(fmtd),')'
            do j=1,varlengths(i)
               knt_d = knt_d + 1
               write (outunit,fmt,err=900) varnames(i),' = ',
     &              dvals(knt_d)
            enddo
         case (typ_complex)               !complex
            write (fmt,'(3a)',err=900) fmtkey,trim(fmtc),')'
            do j=1,varlengths(i)
               knt_c = knt_c + 1
               write (outunit,fmt,err=900) varnames(i),' = ',
     &              cvals(knt_c)
            enddo
         case (typ_logical)               !logical
            write (fmt,'(3a)',err=900) fmtkey,trim(fmtl),')'
            do j=1,varlengths(i)
               knt_l = knt_l + 1
               write (outunit,fmt,err=900) varnames(i),' = ',
     &              lvals(knt_l)
            enddo
         case default		!unsupported type
            goto 900
         end select
      enddo

      ierr = 0	!#error-free writing of file

      if (knt_i .ne. ni .or. knt_r .ne. nr .or. knt_d .ne. nd .or.
     &     knt_c .ne. nc .or. knt_l .ne. nl)
     &     ierr = -4	!#Inconsistent varlengths and ni, nr, nd, nc, nl

  900 if (vprint) write (*,'(a,i10)')
     &     ' Finished writing file with ierr= ',ierr

      if (ierr .ge. 0) then
         ierr_loc = 0 
         close(outunit,iostat=ierr_loc)
         if (ierr_loc .ne. 0 .and. ierr .eq. 0) ierr = -2 !#Error closing file
      endif

c!# ERROR HANDLING : return nonzero error codes
      return
      end subroutine write_config
      end module config_mod
