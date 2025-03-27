c chi2_func_CLO.f
c Calculate chi^2 for interferometric closure phases.
c Mirolav Broz (miroslav.broz@email.cz), Apr 1st 2016

      subroutine chi2_func_CLO(NOUT, tout, rh, chi2, n)

      implicit none
      include '../misc/const.inc'
      include '../limcof/limcof.inc'
      include 'chi2.inc'
      include 'dependent.inc'
      include 'cb_t3amp.inc'
      include 'cb_limb.inc'

c input
      integer NOUT
      real*8 tout(OUTMAX)
      real*8 rh(OUTMAX,NBODMAX,3)

c output
      real*8 chi2
      integer n

c internal variables
      integer i, j, k, l, m, i1st, iua, iub
      real*8 chi2_
      real*8 xh_interp, yh_interp, Lumtot, arg, x, y, lambda, band, d
      real*8 phi(NBODMAX)
      real*8 Lum_lambda(NBODMAX)
      real*8 B(3),u(3),v(3), tmp
      real*8 alpha, beta, u_interp
      double complex t3
      double complex mu(3)

c functions
      real*8 interp, bessj1, bessj32

      data i1st /0/
      data iua,iub /10,20/

      save i1st

c read closure phase observations (only 1st time!)

      if (i1st.eq.0) then

        call read_CLO(file_CLO, m_OBS, t_OBS, u1_OBS, v1_OBS,
     :    u2_OBS, v2_OBS, lambda_eff_OBS, band_eff_OBS, t3amp_OBS,
     :    sigma_t3amp_OBS, t3phi_OBS, sigma_t3phi_OBS, dataset_OBS)

        if (debug) then
          write(*,*) "# m_CLO = ", m_OBS
        endif

        i1st = 1

      endif  ! i1st

c chi^2 for closure phase data

      if (debug) then
        open(unit=iua,file="chi2_CLO.dat",status="unknown")
        write(iua,*) "# t_OBS & u1 [m] & v1 [m] & u2 [m] & v2 [m]",
     :    " & lambda [m] & band [m]",
     :    " & t3amp_interp [] & sigma_t3amp_OBS []",
     :    " & t3phi_interp [deg] & sigma_t3phi_OBS [deg] & dataset",
     :    " & chi^2"
        write(iua,*) "# t_OBS & u1 [m] & v1 [m] & u2 [m] & v2 [m]",
     :    " & lambda [m] & band [m]",
     :    " & t3amp_OBS    [] & sigma_t3amp_OBS []",
     :    " & t3phi_OBS    [deg] & sigma_t3phi_OBS [deg] & dataset",
     :    " & chi^2"
      endif

      if (debug_swift) then
        open(unit=iub,file="closurephase.dat",status="unknown")
        write(iub,*) "# t & u1 [m] & v1 [m] & u2 [m] & v2 [m]",
     :    " & lambda [m] & t3amp & t3phi [deg] & dataset"
      endif

      d = d_pc*pc  ! [m]
      do k = 1, nbod
        phi(k) = 2.d0*R_star(k)*R_S / d  ! uniform-disk diameter [rad]
      enddo

      chi2 = 0.d0
      n = 0
      j = 2

      do i = 1, m_OBS

        lambda = lambda_eff_OBS(i)
        band = band_eff_OBS(i)
        call luminosities(T_eff, R_star, nbod, lambda, band,
     :    Lum_lambda, Lumtot, use_planck)

c          |^ (u3,v3)   
c          | \          
c          |  \         
c  (u2,v2) V---> (u1,v1)

        B(1) = sqrt(u1_OBS(i)**2 + v1_OBS(i)**2)  ! baseline length [m]
        B(2) = sqrt(u2_OBS(i)**2 + v2_OBS(i)**2)
        u(3) = -(u2_OBS(i) + u1_OBS(i))
        v(3) = -(v2_OBS(i) + v1_OBS(i))
        B(3) = sqrt(u(3)**2 + v(3)**2)

        u(1) = u1_OBS(i)/lambda                   ! spatial frequencies [cycles]
        v(1) = v1_OBS(i)/lambda
        u(2) = u2_OBS(i)/lambda
        v(2) = v2_OBS(i)/lambda
        u(3) = u(3)/lambda
        v(3) = v(3)/lambda

        do l = 1, 3
          mu(l) = (0.d0, 0.d0)
        enddo

        do while ((j.lt.NOUT).and.(tout(j).le.t_OBS(i)))
          j = j+1
        enddo

        if (use_limbdark) then
          m = 2
          do while ((lambda_limb_(m).lt.lambda).and.(m.lt.m_limb))
            m = m+1
          enddo
        endif

        do k = 1, nbod
          if (Lum_lambda(k).gt.0.0d0) then

c linear limb-darkening coefficient at given lambda
          if (use_limbdark) then
            u_interp = interp(lambda_limb_(m-1), lambda_limb_(m),
     :        u_limb_(m-1,k), u_limb_(m,k), lambda)
          else
            u_interp = 0.d0
          endif

c linear interpolation of integrated data to a given position in time
            xh_interp = interp(tout(j-1), tout(j), rh(j-1,k,1),
     :        rh(j,k,1), t_OBS(i))  ! k-th body, x coordinate
            yh_interp = interp(tout(j-1), tout(j), rh(j-1,k,2),
     :        rh(j,k,2), t_OBS(i))  ! y coordinate

            x = -xh_interp*AU / d
            y = yh_interp*AU / d

c limb-darkened complex visibility (Hanbury-Brown et al. 1974) 
            alpha = 1.d0-u_interp
            beta = u_interp

            do l = 1, 3
              arg = pi_*phi(k)*B(l)/lambda

              mu(l) = mu(l)
     :          + Lum_lambda(k) * 1.d0/(alpha/2.0d0 + beta/3.d0)
     :          * ( alpha*bessj1(arg)/arg
     :          + beta*sqrt(pi_/2.d0)*bessj32(arg)/arg**(3.d0/2.d0) )
     :          * exp(-2.d0*pi_*(0.d0,1.d0) * (u(l)*x + v(l)*y))
            enddo

          endif
        enddo  ! nbod

        do l = 1, 3
          mu(l) = mu(l)/Lumtot
        enddo
        t3 = mu(1)*mu(2)*mu(3)
        t3amp(i) = abs(t3)
        t3phi(i) = atan2(dimag(t3),dble(t3))

        chi2_ = ((t3phi(i)-t3phi_OBS(i))/sigma_t3phi_OBS(i))**2
        lns = lns + log(sigma_t3phi_OBS(i))
        chi2 = chi2 + chi2_
        n = n + 1

        if (debug) then
          write(iua,*) t_OBS(i), u1_OBS(i), v1_OBS(i), u2_OBS(i),
     :      v2_OBS(i), lambda, band, t3amp(i), sigma_t3amp_OBS(i),
     :      t3phi(i)*rad, sigma_t3phi_OBS(i)*rad, dataset_OBS(i),
     :      chi2_
          write(iua,*) t_OBS(i), u1_OBS(i), v1_OBS(i), u2_OBS(i),
     :      v2_OBS(i), lambda, band, t3amp_OBS(i), sigma_t3amp_OBS(i),
     :      t3phi_OBS(i)*rad, sigma_t3phi_OBS(i)*rad, dataset_OBS(i),
     :      chi2_
          write(iua,*)
        endif

        if (debug_swift) then
          write(iub,*) t_OBS(i), u1_OBS(i), v1_OBS(i), u2_OBS(i),
     :      v2_OBS(i), lambda, t3amp(i), t3phi(i)*rad, dataset_OBS(i)
        endif

      enddo

      if (debug) then
        close(iua)
      endif

      if (debug_swift) then
        close(iub)
      endif

      return
      end

