c planck.f
c Planck function, i.e. black-body intensity in J s^-1 sr^-1 m^-2 m^-1 units.
c Miroslav Broz (miroslav.broz@email.cz), Jun 24th 2016

      real*8 function planck(T, lambda)
      implicit none
      include '../misc/const.inc'

      real*8 T, lambda

      planck = 2.d0*h_P*clight**2/lambda**5
     :  / (exp(h_P*clight/(lambda*k_B*T))-1.d0)

      return
      end

