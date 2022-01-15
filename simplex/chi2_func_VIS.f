c chi2_func_VIS.f
c Calculate chi^2 for interferometric visibilities.
c Mirolav Broz (miroslav.broz@email.cz), Mar 18th 2016

      subroutine chi2_func_VIS(NOUT, m, tout, rh, chi2, n)

      implicit none
      include '../misc/const.inc'
      include '../limcof/limcof.inc'
      include 'simplex.inc'
      include 'dependent.inc'
      include 'cb_limb.inc'

c input
      integer NOUT
      real*8 m(NBODMAX)
      real*8 tout(OUTMAX)
      real*8 rh(OUTMAX,NBODMAX,3)

c output
      real*8 chi2
      integer n

c observational data
      integer m_VIS
      real*8 t_VIS(OBSMAX), u_VIS(OBSMAX), v_VIS(OBSMAX),
     :  lambda_eff_VIS(OBSMAX), band_eff_VIS(OBSMAX), Vsq_VIS(OBSMAX),
     :  sigma_Vsq_VIS(OBSMAX)
      integer dataset_VIS(OBSMAX)

c internal variables
      integer i, j, k, l, i1st, iua, iub, iuc
      real*8 chi2_
      real*8 xh_interp, yh_interp, Lumtot, B, u, v, arg, x, y, Vsq,
     :  lambda, band, d
      real*8 phi(NBODMAX)
      real*8 Lum_lambda(NBODMAX)
      real*8 alpha, beta, u_interp
      complex*8 mu

c functions
      real*8 interp, bessj1, bessj32

      data i1st /0/
      data iua, iub, iuc /10,20,30/

      save i1st,
     :  m_VIS, t_VIS, u_VIS, v_VIS, lambda_eff_VIS, band_eff_VIS,
     :  Vsq_VIS, sigma_Vsq_VIS, dataset_VIS
c
c read visibility observations (only 1st time!)
c
      if (i1st.eq.0) then

        call read_VIS(file_VIS, m_VIS, t_VIS, u_VIS, v_VIS,
     :    lambda_eff_VIS, band_eff_VIS, Vsq_VIS, sigma_Vsq_VIS,
     :    dataset_VIS)

        if (debug) then
          write(*,*) "# m_VIS = ", m_VIS
        endif

        if ((use_limbdark).and.(m_VIS.gt.0)) then
          call limcof_read("limcof.dat")
        endif

        if (debug) then
          write(*,*) "# n_limb = ", n_limb
          write(*,*) "# n_avail = ", n_avail
        endif

        i1st = 1
      endif  ! i1st
c
c get linear limb-darkening coefficients u(lambda) vs wavelength
c
      if ((use_limbdark).and.(m_VIS.gt.0)) then

        do i = 1, nbod
          call limcof_interp(T_eff(i), log_g(i), metal(i),
     :      lambda_limb_, u_limb_(1,i), m_limb)
        enddo

      endif
c
c chi^2 for visibility data
c
      if (debug) then
        open(unit=iua,file="chi2_VIS.dat",status="unknown")
        write(iua,*) "# t_VIS & u [m] & v [m] & lambda [m] & band [m]",
     :    " & Vsq_interp [] & sigma_Vsq_VIS [] & dataset & chi^2"
        write(iua,*) "# t_VIS & u [m] & v [m] & lambda [m] & band [m]",
     :    " & Vsq_VIS    [] & sigma_Vsq_VIS [] & dataset & chi^2"
      endif

      if (debug_swift) then
        open(unit=iub,file="visibility.dat",status="unknown")
        write(iub,*) "# t & u [m] & v [m] & lambda [m]",
     :    " & V^2 [] & dataset"

        open(unit=iuc,file="limbdark.dat",status="unknown")
        write(iuc,*) "# component & lambda [m] & u_limb []"
      endif

      d = d_pc*pc  ! [m]
      do k = 1, nbod
        phi(k) = 2.d0*R_star(k)*R_S / d  ! uniform-disk diameter [rad]
      enddo

      chi2 = 0.d0
      n = 0
      j = 2

      do i = 1, m_VIS

        lambda = lambda_eff_VIS(i)
        band = band_eff_VIS(i)
        call luminosities(T_eff, R_star, nbod, lambda, band,
     :    Lum_lambda, Lumtot, use_planck)

        B = sqrt(u_VIS(i)**2 + v_VIS(i)**2)  ! baseline length [m]
        u = u_VIS(i)/lambda                  ! spatial frequencies [cycles]
        v = v_VIS(i)/lambda
        mu = (0.d0, 0.d0)

        do while ((j.lt.NOUT).and.(tout(j).le.t_VIS(i)))
          j = j+1
        enddo

        if (use_limbdark) then
          l = 2
          do while ((lambda_limb_(l).lt.lambda).and.(l.lt.m_limb))
            l = l+1
          enddo
        endif

        do k = 1, nbod
          if (Lum_lambda(k).gt.0.0d0) then

c linear limb-darkening coefficient at given lambda
          if (use_limbdark) then
            u_interp = interp(lambda_limb_(l-1), lambda_limb_(l),
     :        u_limb_(l-1,k), u_limb_(l,k), lambda)
          else
            u_interp = 0.d0
          endif

          if (debug_swift) then
            write(iuc,*) k, lambda, u_interp
          endif

c linear interpolation of integrated data to a given position in time
            xh_interp = interp(tout(j-1), tout(j), rh(j-1,k,1),
     :        rh(j,k,1), t_VIS(i))  ! k-th body, x coordinate
            yh_interp = interp(tout(j-1), tout(j), rh(j-1,k,2),
     :        rh(j,k,2), t_VIS(i))  ! y coordinate

            x = -xh_interp*AU / d  ! radians <- THIS WORKS!
            y = yh_interp*AU / d

c limb-darkened complex visibility (Hanbury-Brown et al. 1974) 
            arg = pi_*phi(k)*B/lambda
            alpha = 1.d0-u_interp
            beta = u_interp
            
            mu = mu + Lum_lambda(k) * 1.d0/(alpha/2.0d0 + beta/3.d0)
     :        * ( alpha*bessj1(arg)/arg
     :        + beta*sqrt(pi_/2.d0)*bessj32(arg)/arg**(3.d0/2.d0) )
     :        * exp(-2.d0*pi_*(0.d0,1.d0) * (u*x + v*y))

          endif
        enddo  ! nbod

        Vsq = (abs(mu)/Lumtot)**2

        chi2_ = ((Vsq-Vsq_VIS(i))/sigma_Vsq_VIS(i))**2
        chi2 = chi2 + chi2_
        n = n+1

        if (debug) then
          write(iua,*) t_VIS(i), u_VIS(i), v_VIS(i), lambda, band,
     :      Vsq, sigma_Vsq_VIS(i), dataset_VIS(i), chi2_
          write(iua,*) t_VIS(i), u_VIS(i), v_VIS(i), lambda, band,
     :      Vsq_VIS(i), sigma_Vsq_VIS(i), dataset_VIS(i), chi2_
          write(iua,*)
        endif

        if (debug_swift) then
          write(iub,*) t_VIS(i), u_VIS(i), v_VIS(i), lambda,
     :      Vsq, dataset_VIS(i)
        endif

      enddo

      if (debug) then
        close(iua)
      endif

      if (debug_swift) then
        close(iub)
        close(iuc)
      endif

      return
      end

