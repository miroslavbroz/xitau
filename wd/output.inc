c output.inc
c Additional output quantities.
c Miroslav Broz (miroslav.broz@email.cz), Jun 10th 2016

      parameter (igsmax_=33202)   ! maximum grid size

      real*8 Delta_F_lambda(2*igsmax_)  ! flux towards observer [erg s^-1 cm^-2 cm^-1]
      real*8 Delta_phi(2*igsmax_)       ! surface elements
      real*8 Delta_theta(2*igsmax_)

      common /output/ Delta_F_lambda, Delta_phi, Delta_theta


