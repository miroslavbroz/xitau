c dissipation_factor.f
c Write (approximate) Q factors for tidal dissipation.
c Miroslav Broz (miroslav.broz@email.cz), Petr Pokorny, Nov 12th 2013

      subroutine dissipation_factor(nbod,mass,xh,yh,zh)

      include '../swift.inc'
      include '../misc/const.inc'
      include 'tides.inc'
      include 'spin.inc'

c input
      integer nbod
      real*8 mass(NPLMAX)
      real*8 xh(NPLMAX), yh(NPLMAX), zh(NPLMAX)

c output NONE
c temporary
      integer i
      real*8 r, mean_motion, delta, Q

      write(*,*) '# Q factors (i.e. E/Delta E) for tidal dissipation:'
      write(*,*) '# PL_id',
     :  ' & r [AU]',
     :  ' & mean_motion [rad/day]',
     :  ' & omega_rotation [rad/day] ',
     :  ' & k_2 [] ',
     :  ' & Delta_t [sec] ',
     :  ' & delta [deg] ',
     :  ' & approximate Q []'

      do i = 1, nbod

        if (i.eq.1) then
          r = sqrt(xh(2)**2 + yh(2)**2 + zh(2)**2)  ! 1st planet instead?
          mean_motion = sqrt((mass(1)+mass(2))/r**3)
        else
          r = sqrt(xh(i)**2 + yh(i)**2 + zh(i)**2)
          mean_motion = sqrt((mass(1)+mass(i))/r**3)
        endif

        delta = (omega(i)-mean_motion)*Delta_t(i)
c        Q = 3.d0/(2.d0*abs(omega(i)-mean_motion)*k_2(i)*Delta_t(i))  ! unknown reference?!
        Q = 1.d0/(2.d0*Delta_t(i)*abs(omega(i)-mean_motion))  ! Efroimsky & Lainey (2007)

        write(*,*) -i, r, mean_motion, omega(i), k_2(i),
     :    Delta_t(i)*day, delta*degrad, Q
      enddo

      return
      end


