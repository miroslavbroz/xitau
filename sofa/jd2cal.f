      SUBROUTINE iau_JD2CAL ( DJ1, DJ2, IY, IM, ID, FD, J )
*+
*  - - - - - - - - - - -
*   i a u _ J D 2 C A L
*  - - - - - - - - - - -
*
*  Julian Date to Gregorian year, month, day, and fraction of a day.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     DJ1,DJ2     d     Julian Date (Notes 1, 2)
*
*  Returned:
*     IY          i     year
*     IM          i     month
*     ID          i     day
*     FD          d     fraction of day
*     J           i     status:
*                           0 = OK
*                          -1 = unacceptable date (Note 1)
*
*  Notes:
*
*  1) The earliest valid date is -68569.5 (-4900 March 1).  The
*     largest value accepted is 10^9.
*
*  2) The Julian Date is apportioned in any convenient way between
*     the arguments DJ1 and DJ2.  For example, JD=2450123.7 could
*     be expressed in any of these ways, among others:
*
*             DJ1            DJ2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     Separating integer and fraction uses the "compensated summation"
*     algorithm of Kahan-Neumaier to preserve as much precision as
*     possible irrespective of the JD1+JD2 apportionment.
*
*  3) In early eras the conversion is from the "Proleptic Gregorian
*     Calendar";  no account is taken of the date(s) of adoption of
*     the Gregorian Calendar, nor is the AD/BC numbering convention
*     observed.
*
*  References:
*
*     Explanatory Supplement to the Astronomical Almanac,
*     P. Kenneth Seidelmann (ed), University Science Books (1992),
*     Section 12.92 (p604).
*
*     Klein, A., A Generalized Kahan-Babuska-Summation-Algorithm.
*     Computing, 76, 279-293 (2006), Section 3.
*
*  This revision:  2020 October 21
*
*  SOFA release 2021-05-12
*
*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DJ1, DJ2
      INTEGER IY, IM, ID
      DOUBLE PRECISION FD
      INTEGER J

*  Smallest number such that 1D0+EPS .NE. 1D0
      DOUBLE PRECISION EPS
      PARAMETER ( EPS = 2.2204460492503131D-16  )

*  Minimum and maximum allowed JD
      DOUBLE PRECISION DJMIN, DJMAX
      PARAMETER ( DJMIN = -68569.5D0, DJMAX = 1D9 )

      INTEGER JD, I, L, N, K
      DOUBLE PRECISION DJ, F1, F2, D, S, CS, V(2), X, T, C, F

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Verify date is acceptable.
      DJ = DJ1 + DJ2
      IF ( DJ.LT.DJMIN .OR. DJ.GT.DJMAX ) THEN
         J = -1
      ELSE
         J = 0

*     Separate day and fraction (where -0.5 <= fraction < 0.5).
         D = ANINT(DJ1)
         F1 = DJ1 - D
         JD = INT(D)
         D = ANINT(DJ2)
         F2 = DJ2 - D
         JD = JD + INT(D)

*     Compute F1+F2+0.5 using compensated summation (Klein 2006).
         S = 0.5D0
         CS = 0D0
         V(1) = F1
         V(2) = F2
         DO 1 I=1,2
            X = V(I)
            T = S + X
            IF ( ABS(S) .GE. ABS(X) ) THEN
               C = (S-T) + X
            ELSE
               C = (X-T) + S
            END IF
            CS = CS + C
            S = T
            IF ( S .GE. 1D0 ) THEN
               JD = JD + 1
               S = S - 1D0
            END IF
 1       CONTINUE
         F = S + CS
         CS = F - S

*     Deal with negative F.
         IF ( F .LT. 0D0 ) THEN

*        Compensated summation: assume that |S| <= 1.
            F = S + 1D0
            CS = CS + ((1D0-F) + S)
            S = F
            F = S + CS
            CS = F - S
            JD = JD - 1
         END IF

*     Deal with F that is 1D0 or more (when rounded to double).
         IF ( (F-1D0) .GE. -EPS/4D0 ) THEN

*        Compensated summation: assume that |S| <= 1. */
            T = S - 1D0
            CS = CS + ((S-T) - 1D0)
            S = T
            F = S + CS
            IF ( -EPS/2D0 .LT. F ) THEN
               JD = JD + 1
               F = MAX ( F, 0D0 )
            END IF
         END IF

*     Express day in Gregorian calendar.
         L = JD + 68569
         N = ( 4*L ) / 146097
         L = L - ( 146097*N + 3 ) / 4
         I = ( 4000 * (L+1) ) / 1461001
         L = L - ( 1461*I ) / 4 + 31
         K = ( 80*L ) / 2447
         ID = L - ( 2447*K ) / 80
         L = K / 11
         IM = K + 2 - 12*L
         IY = 100 * ( N-49 ) + I + L
         FD = F
      END IF

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2021
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END
