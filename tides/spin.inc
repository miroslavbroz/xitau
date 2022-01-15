c spin.inc
c Include file for spin-related routines.
c Miroslav Broz (miroslav.broz@email.cz), Apr 3rd 2021

c  spin axes data
      real*8 s(3,NPLMAX), omega(NPLMAX), L_spin(3,NPLMAX)
      real*8 dtspin, dtspinout
      character*255 outspinfile

c  input/output in common block /spin/
      common /spin/ s, omega, L_spin, dtspin, dtspinout,
     :  outspinfile


