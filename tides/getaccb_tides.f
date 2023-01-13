c getaccb_tides.f
c Compute accelerations caused by MUTUAL tides of all massive bodies.
c Miroslav Broz (miroslav.broz@email.cz), Petr Pokorny, Apr 3rd 2021

      subroutine getaccb_tides(nbod_,mass,xb,yb,zb,vxb,vyb,vzb,
     :  axb,ayb,azb)

      include '../swift.inc'
      include '../misc/const.inc'
      include 'tides.inc'
      include 'spin.inc'
      include '../chi2/chi2.inc'
      include '../chi2/dependent.inc'

c input
      integer nbod_
      real*8 mass(NPLMAX)
      real*8 xb(NPLMAX),yb(NPLMAX),zb(NPLMAX)
      real*8 vxb(NPLMAX),vyb(NPLMAX),vzb(NPLMAX)

c input/output
      real*8 axb(NPLMAX),ayb(NPLMAX),azb(NPLMAX)

c temporary
      integer i,j,k,l
      real*8 r(3)
      real*8 v(3)
      real*8 r_dash(3)
      real*8 omega_vec(3)
      real*8 a(3)
      real*8 T(3)
      real*8 a_tides(3,NPLMAX)
      real*8 sgn

      real*8 a_abs, a_tides_abs, T_tides_abs, L_spin_abs, dt  ! dbg

      integer i1st
      save i1st
      data i1st /0/

      if (.not.use_tides) return

      if (i1st.eq.0) then
        call dissipation_factor(nbod_,mass,xb,yb,zb)
        i1st = 1
      endif

c initialisation
      do i = 1, nbod_
        do j = 1, 3
          a_tides(j,i) = 0.d0
          T_tides(j,i) = 0.d0
        enddo
      enddo

      if (is_forward) then
        sgn = 1.d0
      else
        sgn = -1.d0
      endif

c compute tidal accelerations

      do i = 1, nbod_  ! "Earth"
        do j = 1, nbod_  ! "Moon"

          if (j.ne.i) then

            do k = 1, nbod_  ! "test particle"

              if (k.ne.i) then
            
                r(1) = xb(j)-xb(i)  ! "Earth--Moon"
                r(2) = yb(j)-yb(i)
                r(3) = zb(j)-zb(i)
                v(1) = vxb(j)-vxb(i)  ! "Moon" velocity
                v(2) = vyb(j)-vyb(i)
                v(3) = vzb(j)-vzb(i)
                r_dash(1) = xb(k)-xb(i)  ! "Earth--test particle" (we are including cross tides)
                r_dash(2) = yb(k)-yb(i)
                r_dash(3) = zb(k)-zb(i)
                omega_vec(1) = omega(i)*s(1,i)
                omega_vec(2) = omega(i)*s(2,i)
                omega_vec(3) = omega(i)*s(3,i)
              
                call mignard(mass(j), capR(i), k_2(i), sgn*Delta_t_(i),
     :            r, v, r_dash, omega_vec, a)
            
                call mignard_torque(mass(k), r_dash, a, T)

c                write(*,*) i,j,k,a
c                write(*,*) i,j,k,T

                do l = 1, 3
                  a_tides(l,k) = a_tides(l,k) + a(l)  ! the acceleration is acting on the k-th body
                  T_tides(l,i) = T_tides(l,i) + T(l)  ! while the torque is assigned to the i-th body!
                enddo

              endif

            enddo

          endif

        enddo
      enddo

      if (debug_spin) then
        write(*,*) '# TP_id & acceleration a [AU/day^2]',
     :    ' & a_tides [AU/day^2]',
     :    ' & a_tides/a'

        do i = 1, nbod_
          a_abs = sqrt(axb(i)**2+ayb(i)**2+azb(i)**2)
          a_tides_abs = sqrt(a_tides(1,i)**2 + a_tides(2,i)**2
     :      + a_tides(3,i)**2)
          write(*,*) -i, a_abs, a_tides_abs, a_tides_abs/a_abs
        enddo

c a different SI conversion
        write(*,*) '# TP_id',
     :    ' & torque T [kg m^2 s^-2 = N m]',
     :    ' & angular momentum L [kg m^2 s^-1]'

        do i = 1, nbod_
          T_tides_abs = sqrt(T_tides(1,i)**2 + T_tides(2,i)**2
     :      + T_tides(3,i)**2)
          L_spin_abs = sqrt(L_spin(1,i)**2 + L_spin(2,i)**2
     :      + L_spin(3,i)**2)
          write(*,'(i8,3(1x,e16.8))') -i, T_tides_abs, L_spin_abs, 
     :      T_tides_abs/L_spin_abs
        enddo

        stop  ! dbg
      endif

c add the result to the BARYCENTRIC accelerations
      do i = 1, nbod_
        axb(i) = axb(i) + a_tides(1,i) - a_tides(1,1)
        ayb(i) = ayb(i) + a_tides(2,i) - a_tides(2,1)
        azb(i) = azb(i) + a_tides(3,i) - a_tides(3,1)
      enddo

      return
      end


