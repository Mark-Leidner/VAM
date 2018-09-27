      module string_mod
      implicit none
      private
      public where_in, lcomp
      CONTAINS
c     -----------------------------------------------------------------
      integer function where_in (s1, s2_array, len1, len2, n2)

c     Finds occurence of string s1 in array s2_array.  Match is good
c     if the all the characters in s1 (excluding trailing spaces) match.
c     Comparison is case-insensitive.
c     Returns 0 if no match, -n for multiple matches (n=first match)

      integer :: len1, len2, n2
      character (len1) :: s1
      character (len2) :: s2_array(n2)
c
      integer :: i, len_compare, lentrim_1, match

      lentrim_1 = len_trim(s1)
      len_compare = min(lentrim_1, len2)
      match = 0
      do i=1,n2
         if (lcomp(s1,s2_array(i),len_compare)) then
            if (match .eq. 0) then
               match = i
            elseif (match .gt. 0) then
               match = -match
            endif
         endif
         if (match .lt. 0) exit
      enddo
      
      where_in = match
      return
      end function where_in
      
c     -----------------------------------------------------------------
      LOGICAL FUNCTION lcomp (s1,s2,len)

c     Compares contents of strings S1 and S2 ignoring case.

      INTEGER len, i
      CHARACTER*(*) s1, s2

c     -----------------------------------------------------------------

c     Statement function to test if a character is lc.
      LOGICAL lc
      CHARACTER*1 cuc, c
      lc(c) = LGE(c,'a') .AND. LLE(c,'z')
c     Statement function to make a character uc.
      cuc(c) = CHAR(ICHAR('A') + ICHAR(c) - ICHAR('a'))

c     -----------------------------------------------------------------

c     For each character while the comparison is OK:
      lcomp=len.GT.0
      i=0
      DO WHILE (lcomp .AND. i.LT.len)
        i=i+1
        IF (s1(i:i).NE.s2(i:i)) THEN
c     If they don't agree, and s1 is lc, try the uc version of s1
          IF (lc(s1(i:i))) THEN
            lcomp=cuc(s1(i:i)) .EQ. s2(i:i)
c     Or if s2 is lc, try the uc version of s2,
          ELSE IF (lc(s2(i:i))) THEN
            lcomp=s1(i:i) .EQ. cuc(s2(i:i))
c     Otherwise they just don't agree.
          ELSE
            lcomp = .FALSE.
          END IF
        END IF
      END DO

      END FUNCTION lcomp

c     -----------------------------------------------------------------
      end module string_mod
