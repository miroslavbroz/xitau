      DOUBLE PRECISION FUNCTION iau_GMST06 ( UTA, UTB, TTA, TTB )
*+
*  - - - - - - - - - - -
*   i a u _ G M S T 0 6
*  - - - - - - - - - - -
*
*  Greenwich mean sidereal time (consistent with IAU 2006 precession).
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Given:
*     UTA,UTB      d      UT1 as a 2-part Julian Date (Notes 1,2)
*     TTA,TTB      d      TT as a 2-part Julian Date (Notes 1,2)
*
*  Returned:
*     iau_GMST06   d      Greenwich mean sidereal time (radians)
*
*  Notes:
*
*  1) The UT1 and TT dates UTA+UTB and TTA+TTB respectively, are both
*     Julian Dates, apportioned in any convenient way between the
*     argument pairs.  For example, JD=2450123.7 could be expressed in
*     any of these ways, among others:
*
*            Part A         Part B
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in
*     cases where the loss of several decimal digits of resolution
*     is acceptable (in the case of UT;  the TT is not at all critical
*     in this respect).  The J2000 and MJD methods are good compromises
*     between resolution and convenience.  For UT, the date & time
*     method is best matched to the algorithm that is used by the Earth
*     rotation angle routine, called internally:  maximum accuracy (or,
*     at least, minimum noise) is delivered when the UTA argument is for
*     0hrs UT1 on the day in question and the UTB argument lies in the
*     range 0 to 1, or vice versa.
*
*  2) Both UT1 and TT are required, UT1 to predict the Earth rotation
*     and TT to predict the effects of precession.  If UT1 is used for
*     both purposes, errors of order 100 microarcseconds result.
*
*  3) This GMST is compatible with the IAU 2006 precession and must not
*     be used with other precession models.
*
*  4) The result is returned in the range 0 to 2pi.
*
*  Called:
*     iau_ERA00    Earth rotation angle, IAU 2000
*     iau_ANP      normalize angle into range 0 to 2pi
*
*  Reference:
*
*     Capitaine, N., Wallace, P.T. & Chapront, J., 2005,
*     Astron.Astrophys. 432, 355
*
*  This revision:  2020 November 17
*
*  SOFA release 2021-05-12
*
*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION UTA, UTB, TTA, TTB

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  Reference epoch (J2000.0), JD
      DOUBLE PRECISION DJ00
      PARAMETER ( DJ00 = 2451545D0 )

*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

      DOUBLE PRECISION T

      DOUBLE PRECISION iau_ANP, iau_ERA00

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  TT Julian centuries since J2000.0.
      T = ( ( TTA-DJ00 ) + TTB ) / DJC

*  Greenwich mean sidereal time, IAU 2006.
      iau_GMST06 = iau_ANP ( iau_ERA00 ( UTA, UTB ) +
     :                       (    0.014506D0    +
     :                       ( 4612.156534D0    +
     :                       (    1.3915817D0   +
     :                       (  - 0.00000044D0  +
     :                       (  - 0.000029956D0 +
     :                       (  - 0.0000000368D0 )
     :                           * T ) * T ) * T ) * T ) * T ) * DAS2R )

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
