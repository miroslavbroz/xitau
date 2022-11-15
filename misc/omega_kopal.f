c omega_kopal.f
c Kopal potential; Wilson & Devinney (1971), Eq. (1)
c Miroslav Broz (miroslav.broz@email.cz), Jun 1st 2016

      real*8 function omega_kopal(r1, r2, r3, q)
      implicit none
      real*8 r1, r2, r3, q
      omega_kopal = 1.d0/r1 + q/r2 + 0.5d0*(1.d0+q)*r3**2
      return
      end

