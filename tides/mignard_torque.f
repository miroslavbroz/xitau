c mignard_torque.f
c Compute the corresponding torque, acting on the Earth.
c Miroslav Broz (miroslav.broz@email.cz), 

      subroutine mignard_torque(Gm_dash, r_dash, a, T)

      include '../misc/const.inc'

c input
      real*8 Gm_dash
      real*8 r_dash(3)
      real*8 a(3)

c Note: If we pass Gm_dash instead of m_dash as the 1st argument
c we have to divide by G to get the torque.

c output
      real*8 T(3)

      call vproduct(r_dash, a, T)

      do i = 1, 3
        T(i) = -Gm_dash/G * T(i)
      enddo

      return
      end


