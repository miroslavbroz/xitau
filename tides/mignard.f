c mignard.f
c An expression for the tidal acceleration.
c Miroslav Broz (miroslav.broz@email.cz), Petr Pokorny, Nov 7th 2013
c
c References:
c
c Neron de Surgy O., Laskar J.: On the long term evolution
c of the spin of the Earth. Astron. Astrophys., 318, 975-989, 1997.
c
c -> namely the equation at the bottom of p. 979, which is taken from:
c
c Mignard F.: The evolution of lunar orbit revisited I.
c The Moon and the Planets, 20, 301-315, 1979.
c
c Original context is the following:
c   we have the Earth--Moon--test particle system, in which
c   the Moon exerts gravitational tide on the Earth,
c   which causes a deformation of the Earth,
c   tidal dissipation in the Earth leads to a shift of bulges (i.e. a lag),
c   this changes the gravitational potential of the Earth,
c   a test particle located somewhere then feels a corresponding acceleration.
c
c Note: A test particle may be the Moon itself! (in this case, r_dash = r).
c
c Note: If we compute a torque acting on the Moon as r_dash \times m_dash a,
c   it is the SAME torque (opposite sign) which acts on the Earth
c   and slows down its spin rate! (ref. 3rd law of Newton).
c
c Note: Phase lag delta = (omega-n) Delta_t, tan(delta) = 1/Q, see Eq. (3),
c   where Q is the tidal dissipation factor

      subroutine mignard(Gm_star, capR, k_2, Delta_t,
     :  r, v, r_dash, omega, a)

      implicit none

c input
      real*8 Gm_star   ! GM of the Moon
      real*8 capR      ! Earth radius
      real*8 k_2       ! Love number of the Earth
      real*8 Delta_t   ! time lag of the Earth
      real*8 r(3)      ! vector Earth--Moon (i.e. pertubing body)
      real*8 v(3)      ! orbital velocity of the Moon
      real*8 r_dash(3) ! vector Earth--test particle (interacting body)
      real*8 omega(3)  ! spin rate of the Earth

c output
      real*8 a(3)      ! acceleration

c temporary
      integer i
      real*8 r_sq
      real*8 r_dash_sq
      real*8 omega_times_r_dash(3)
      real*8 r_times_omega(3)
      real*8 r_dash_dot_v
      real*8 r_dash_dot_r
      real*8 r_dot_v
      real*8 r_dot_omega_times_r_dash
      real*8 k1, k2, k3, k4, k5, k6, k7  ! individual terms in the equation

      real*8 r_times_v(3), T(3)

      call vproduct(omega, r_dash, omega_times_r_dash)
      call vproduct(r, omega, r_times_omega)

      r_dash_dot_v = dot_product(r_dash, v)
      r_dash_dot_r = dot_product(r_dash, r)
      r_dot_v = dot_product(r, v)
      r_dot_omega_times_r_dash = dot_product(r, omega_times_r_dash)

      r_sq = r(1)**2 + r(2)**2 + r(3)**2
      r_dash_sq = r_dash(1)**2 + r_dash(2)**2 + r_dash(3)**2

      k1 = 3.d0*Gm_star * capR**5 * k_2 * Delta_t
     :  / ((sqrt(r_dash_sq*r_sq))**5)

      k2 = 5.d0/r_dash_sq * ( r_dash_dot_r
     :  * (r_dot_omega_times_r_dash + r_dash_dot_v)
     :  - 1.d0/(2.d0*r_sq) * r_dot_v
     :  * (5.d0 * (r_dash_dot_r)**2 - r_dash_sq*r_sq) )

      k3 = r_dot_omega_times_r_dash + r_dash_dot_v
      k4 = r_dash_dot_r
      k5 = r_dot_v / r_sq
      k6 = 5.d0 * r_dash_dot_r
      k7 = r_sq

      do i = 1, 3
        a(i) = k1 * (
     :    k2 * r_dash(i)
     :    - k3 * r(i)
     :    - k4 * (r_times_omega(i) + v(i))
     :    + k5 * (k6 * r(i) - k7 * r_dash(i))
     :    )
      enddo

      return
      end


