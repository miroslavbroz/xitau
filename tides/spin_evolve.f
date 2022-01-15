c spin_evolve.f
c Evolve spin axes of planets due to tidal torques.
c Miroslav Broz (miroslav.broz@email.cz), Petr Pokorny, Nov 7th 2013
c
c References:
c
c Neron de Surgy O., Laskar J.: On the long term evolution
c of the spin of the Earth. Astron. Astrophys., 318, 975-989, 1997.

c Equations:
c
c   L = I omega
c   dL/dt = T
c
c Units:
c
c   [omega] = rad/day
c   [I] = AU^3/day^2 AU^2 = AU^5/day^2
c   [L] = AU^5/day^3
c   [T] = AU^2/day^2 AU^3/day^2 = AU^5/day^4 <- OK!

      subroutine spin_evolve(t,nbod,dt)

      include '../swift.inc'
      include 'spin.inc'
      include 'tides.inc'

c input
      integer nbod
      real*8 t,dt

c input/output
c     see L_spin, omega, s in common block /spin/
c     see T_tides, MoI in common block /tides/

c temporary
      integer i,j
      real*8 L_spin_abs

c simple Euler integrator

      do i = 1, nbod  ! i.e. including the Sun!
        do j = 1, 3
          L_spin(j,i) = L_spin(j,i) + T_tides(j,i)*dt
        enddo
      enddo

c convert L_spin (i.e. the angular momentum) back to spin direction and spin rate
      do i = 1, nbod
        L_spin_abs = sqrt(L_spin(1,i)**2 + L_spin(2,i)**2
     :    + L_spin(3,i)**2)
        omega(i) = L_spin_abs/MoI(i)
        do j = 1, 3
          s(j,i) = L_spin(j,i)/L_spin_abs
        enddo
      enddo

      return
      end


