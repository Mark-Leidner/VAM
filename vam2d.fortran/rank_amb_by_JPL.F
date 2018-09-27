c!#   CSU IDENTIFICATION : rank_amb_by_JPL
c!#   $Id: rank_amb_by_JPL.F,v 1.2 1998/05/21 14:27:48 leidner Exp $

c!##  PURPOSE : Rank NSCAT wind ambiguities for one Wind Vector Cell
c!#             (WVC) according to convention used in 50-km JPL HDF data.
c!#             Pos. 1 - selected ambiguity; remaining positions filled
c!#             in order of decreasing MLE.  Input from 25-km Merged
c!#             Geophysical Data Record (MGDR).

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      subroutine rank_amb_by_JPL !#
     I     ( nambig, Ambselect, Likelihood, !#
     O     rank ) !#

      implicit none !#

c!#   Input:
c!#    nambig    Number of ambiguities retrieved
c!#    Ambselect  Position of selected ambiguity in input
c!#    Likelihood  MLE values for each ambiguity
      integer nambig, Ambselect !#
      real*4 Likelihood(4) !#

c!#   Output:
c!#    rank      Rank of each ambiguity by MLE
      integer rank(4) !#

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!##  REFERENCES :

c!##  LIMITATIONS :

c!##  CHANGE LOG :
c!#   $Log: rank_amb_by_JPL.F,v $
c!#   Revision 1.2  1998/05/21 14:27:48  leidner
c!#   Finished documentation
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

c!#   2. Set rank to 1 for the selected ambiguity

      if (Ambselect.eq.0) then  ! first and only ambiguity!
         rank(1) = 1
         ranked(1) = .true.
      else
         rank(Ambselect) = 1
         ranked(Ambselect) = .true.
      endif

c     ------------------------------------------------------------------

c!#   3. Rank remaining ambiguities

      do 100 j=1,nambig

c!#   3.1 Choose ambiguity with highest mle NOT YET RANKED
c!#   ... to determine rank of amb j, count the number of ambiguities
c!#   ... already ranked and add 1
c!#   ... This assumes the MLE ordering of the 25-km data

         if (ranked(j)) goto 100
         irank = 0
         do 90 jj=1,nambig
            if (ranked(jj)) then
               irank = irank + 1
               go to 90
            endif
 90      enddo
         rank(j) = irank + 1
         ranked(j) = .true.
 100  enddo

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :
      return
      end
