c output.inc
c Additional output quantities.
c Miroslav Broz (miroslav.broz@email.cz), Jun 10th 2016

      parameter (Nmax=     100)
      parameter (igsmax=  8331)

      real*8 Delta_F_lambda(2*igsmax)  ! flux towards observer [erg s^-1 cm^-2 cm^-1]
      real*8 Delta_phi(2*igsmax)       ! surface elements
      real*8 Delta_theta(2*igsmax)

      common /output/ Delta_F_lambda, Delta_phi, Delta_theta


