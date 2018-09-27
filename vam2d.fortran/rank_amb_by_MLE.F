c!#   CSU IDENTIFICATION : rank_amb_by_MLE
c!#   $Id: rank_amb_by_MLE.F,v 1.2 1998/05/21 14:15:37 leidner Exp $

c!##  PURPOSE : Rank NSCAT wind ambiguities for one Wind Vector Cell
c!#             (WVC) by Maximum Likelihood Estimator (MLE).
c!#             Pos. 1 highest MLE; remaining positions filled in order
c!#             of decreasing MLE. Input from 50-km JPL HDF data.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      subroutine rank_amb_by_MLE !#
     I     ( nambig, mlelikelihood, !#
     O     rank ) !#

      implicit none !#

c!#   Input:
c!#    nambig    Number of ambiguities retrieved
c!#    mlelikelihood  MLE values for each ambiguity
      integer nambig !#
      real*4 mlelikelihood(4) !#

c!#   Output:
c!#    rank      Rank of each ambiguity by MLE
      integer rank(4) !#

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!##  REFERENCES :

c!##  LIMITATIONS :

c!##  CHANGE LOG :
c!#   $Log: rank_amb_by_MLE.F,v $
c!#   Revision 1.2  1998/05/21 14:15:37  leidner
c!#   Finshed documentation
c!#
c!#   Revision 1.1  1998/05/20 17:06:21  leidner
c!#   Initial revision
c!#

c     ------------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

c     ------------------------------------------------------------------

c!##  LOCAL DATA ELEMENTS :

c!#    i         Counter
c!#    irank     Holder for position index
c!#    j, jj     Counters
c!#    maxmle    Holder for maximum MLE value
c!#    ranked    Logical; has this ambiguity been ranked yet?
      integer i, irank, j, jj
      logical ranked(4)
      real maxmle

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c     ------------------------------------------------------------------

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. Initialize rank (integer) and ranked (logical)

      do i=1,4
         rank(i) = 0
         ranked(i) = .false.
      enddo

c     ------------------------------------------------------------------

c!#   2. Find ambiguity with highest MLE

      if (nambig.eq.1) then  ! first and only ambiguity!
         rank(1) = 1
         ranked(1) = .true.
         go to 999
      else
         irank = 0
         maxmle = -9999.
         do i=1,nambig
            if (mlelikelihood(i).gt.maxmle) then
               maxmle = mlelikelihood(i)
               irank=i
            endif
         enddo

c!#   ... make sure highest MLE was found
         if (irank.eq.0) goto 901

         rank(irank) = 1
         ranked(irank) = .true.
      endif

c     ------------------------------------------------------------------

c!#   3. Rank remaining ambiguities

      do 100 j=1,nambig

c!#   3.1 Choose ambiguity with highest mle NOT YET RANKED
c!#   ... to determine rank of amb j, count the number of ambiguities
c!#   ... with MLE greater than or equal to the MLE of amb j

         if (ranked(j)) goto 100
         irank = 0
         do 90 jj=1,nambig
            if (mlelikelihood(jj).ge.mlelikelihood(j)) then
               irank = irank + 1
            endif
 90      enddo
         rank(j) = irank
         ranked(j) = .true.
 100  enddo

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      go to 999

 900  continue

 901  continue
      write(*,*) 'vam(rank_amb_by_MLE): error! ',
     &     'no highest MLE found for ambiguous winds'
      write(*,*) 'mlelikelihood = ', mlelikelihood
      stop 901

 999  return
      end
