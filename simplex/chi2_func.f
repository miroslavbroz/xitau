c chi2_func.f
c Run integrator and calculate chi^2 for a given starting vector x().
c Miroslav Broz (miroslav.broz@email.cz), Jun 7th 2017

      real*8 function chi2_func(x)

      include '../swift.inc'
      include '../misc/const.inc'
      include '../tides/spin.inc'
      include '../tides/tides.inc'
      include 'simplex.inc'
      include 'dependent.inc'
      real*8 x(ndim)

c variables for numerical integration
      integer NOUT1, NOUT2, NOUT
      real*8 m(NBODMAX), r(NBODMAX,3), v(NBODMAX,3)
      real*8 omega0(NBODMAX), s0(NBODMAX,3)
      real*8 elmts(NBODMAX,6)
      real*8 tout1(OUTMAX1), tout2(OUTMAX2), tout(OUTMAX)
      real*8 rout1(OUTMAX1,NBODMAX,3), vout1(OUTMAX1,NBODMAX,3),
     :       rout2(OUTMAX2,NBODMAX,3), vout2(OUTMAX2,NBODMAX,3)
      real*8 rb(OUTMAX,NBODMAX,3), vb(OUTMAX,NBODMAX,3),
     :       rh(OUTMAX,NBODMAX,3), vh(OUTMAX,NBODMAX,3),
     :       rp(OUTMAX,NBODMAX,3), vp(OUTMAX,NBODMAX,3),
     :       rp3(OUTMAX,NBODMAX,3), vp3(OUTMAX,NBODMAX,3)
      character*80 inparfile

c internal variables
      integer i, j, k, l, i1st, iu, ialpha, nmin
      real*8 chi2
      real*8 r_photocentre(3), v_photocentre(3), Lumtot
      real*8 r_barycentre(3), v_barycentre(3), msum
      real*8 lambda_eff, band_eff
      real*8 tmin(MINMAX), duration(MINMAX)  ! from chi2_func_TTV.f
      real*8 gamma, gamma_auday
      integer n_of_interest
      real*8 t_of_interest(TIMEMAX)
      real*8 t_of_interest1(TIMEMAX), t_of_interest2(TIMEMAX)
      real*8 dummy

c functions
      real*8 merit_func, kms_auday

      data iu /10/
      data i1st /0/
      save i1st, n_of_interest, t_of_interest
      save omega0, s0

c-----------------------------------------------------------------------
c
c get both free and fixed parameters
c
      j = 0
      do i = 1, nparam
        if (variable(i)) then
          j = j+1
          x_param(i) = x(j)
        endif
      enddo

      write(*,30) "# x() array:"
30    format(a,$)
      write(*,*) (x(i), i=1,ndim)

c create m(), elmts() arrays for easy manipulation

      j = 0
      do i = 1, nbod
        j = j+1
        m(i) = x_param(j)*GM_S
      enddo

      do i = 2, nbod
        do k = 1, 6
          j = j+1
          elmts(i,k) = x_param(j)
        enddo
      enddo

      do i = 1, nbod
        j = j+1
        T_eff(i) = x_param(j)
      enddo
      do i = 1, nbod
        j = j+1
c        R_star(i) = x_param(j)
        log_g(i) = x_param(j)
      enddo
      do i = 1, nbod
        j = j+1
        v_rot(i) = x_param(j)
      enddo
      do i = 1, nbod
        j = j+1
        metal(i) = x_param(j)
      enddo

      do i = 1, nbod
        j = j+1
        Delta_t_(i) = x_param(j)/day
      enddo

      do i = 1, nband
        j = j+1
        zero(i) = x_param(j)
      enddo

      j = j+1
      gamma = x_param(j)
      j = j+1
      d_pc = x_param(j)

      j = j+1
      pole_l_ = x_param(j)*deg
      j = j+1
      pole_b_ = x_param(j)*deg

      if (j.ne.nparam) then
        write(*,*) "chi2_func.f: Error number of parameters is ", j,
     :    " .ne. nparam = ", nparam
        stop
      endif
c
c constrain orbital inclinations and nodes by pole (equator) of 1
c
      if (use_varpole) then
        do i = 2, nbod
          elmts(i,3) = 90.d0-pole_b_/deg
          elmts(i,4) = 180.d0+pole_l_/deg
        enddo
      endif
c
c compute log g [cgs] for synthetic spectra and limb darkening
c
c      do i = 1, nbod
c        log_g(i) = log10(m(i)*AU**3/day**2/(R_star(i)*R_S)**2*100.d0)
c      enddo
c
c      if (debug_swift) then
c        do i = 1, nbod
c          write(*,*) '# log_g(', i, ') = ', log_g(i)
c        enddo
c      endif
c
c compute R_star (for eclipses and visibilities)
c
      do i = 1, nbod
        R_star(i) = sqrt(m(i)*AU**3/day**2/(10.d0**log_g(i)/100.d0))/R_S
      enddo

      if (debug_swift) then
        do i = 1, nbod
          write(*,*) '# R_star(', i, ') = ', R_star(i), ' R_S'
        enddo
      endif
c
c constrain some of the components by Harmanec (1988) relations
c
      do i = 1, nbod
        if (use_hec88(i)) then
          call hec88(T_eff(i), R_star(i), m(i), dummy, log_g(i))

          m(i) = m(i)*GM_S

          if (debug) then
            write(*,*) '# Using Harmanec (1988) for component no. ', i
            write(*,*) '# T_eff(', i, ') = ', T_eff(i), ' K'
            write(*,*) '# R_star(', i, ') = ', R_star(i), ' R_S'
            write(*,*) '# m(', i, ') = ', m(i)/GM_S, ' M_S'
            write(*,*) '# log_g(', i, ') = ', log_g(i), ' [cgs]'
          endif
        endif
      enddo
c
c compute luminosities (for photocentre computations)
c
      lambda_eff = 550.d-9
      band_eff = 88.d-9

      call luminosities(T_eff, R_star, nbod, lambda_eff, band_eff,
     :  Lum, Lumtot, use_planck)

      if (debug_swift) then
        do i = 1, nbod
          write(*,*) '# Lum(', i, ') = ', Lum(i)/Lumtot
        enddo
      endif
c
c convert elements to coordinates (wrt. the geometry of the system)
c
      call geometries(nbod, m, elmts, r, v, geometry)

      if (debug_swift) then
        write(*,*) "# barycentric coordinates:"
        do i = 1, nbod
          write(*,*) (r(i,k), k=1,3), (v(i,k), k=1,3)
        enddo

        write(*,*) "# 1-centric coordinates:"
        do i = 1, nbod
          write(*,*) (r(i,k)-r(1,k), k=1,3), (v(i,k)-v(1,k), k=1,3)
        enddo
      endif

c-----------------------------------------------------------------------
c
c init tidal code
c
      if (i1st.eq.0) then
        call io_init_spin("spin.in", nbod)
        call io_init_tides("tides.in", nbod)
        call io_init_tides2("tides2.in", nbod)

c save values from spin.in
        do i = 1, nbod
          omega0(i) = omega(i)
        enddo
        i1st = 1
      endif

c modify values from spin.in, tides.in
      s0(1,1) = cos(pole_l_)*cos(pole_b_)
      s0(1,2) = sin(pole_l_)*cos(pole_b_)
      s0(1,3) = sin(pole_b_)
      do i = 1, nbod
        do j = 1, 3
          s0(i,j) = s0(1,j)
        enddo
      enddo

      do i = 1, nbod
        Delta_t(i) = Delta_t_(i)
      enddo

c-----------------------------------------------------------------------
c
c read all times of interest
c

      call read_time_all(n_of_interest, t_of_interest)

      do i = 1, n_of_interest
        t_of_interest1(i) = t_of_interest(i)-T0
        j = n_of_interest-i+1
        t_of_interest2(j) = T0-t_of_interest(i)
      enddo

c-----------------------------------------------------------------------
c
c forward integration of orbits
c
      inparfile = "param.in"
      is_forward = .true.
      call swift_bs_xyzb(nbod,m,r,v,omega0,s0,NOUT1,OUTMAX1,
     :  tout1,rout1,vout1,inparfile,eps_BS,debug,
     :  n_of_interest,t_of_interest1)

c backward integration (i.e., with reversed velocities)

      do i = 1, nbod
        do j = 1, 3
          v(i,j) = -v(i,j)
          s0(i,j) = -s0(i,j)
        enddo
      enddo

      inparfile = "param_BACK.in"
      is_forward = .false.
      call swift_bs_xyzb(nbod,m,r,v,omega0,s0,NOUT2,OUTMAX2,
     :  tout2,rout2,vout2,inparfile,eps_BS,debug,
     :  n_of_interest,t_of_interest2)

c merge the integration output and add Julian date

      NOUT = NOUT1+NOUT2
      if (NOUT.gt.OUTMAX) then
        write(*,*) "chi2_func: Error number of output data ",
     :    "> OUTMAX = ", OUTMAX
        stop
      endif

      if (debug) then
        write(*,*) "# NOUT = ", NOUT
      endif

      do i = 1, NOUT2
        l = NOUT2-i+1
        tout(l) = T0 - tout2(i)
        do j = 1, nbod
          do k = 1,3
            rb(l,j,k) = rout2(i,j,k)
            vb(l,j,k) = -vout2(i,j,k)
          enddo
        enddo
      enddo

      do i = 1, NOUT1
        l = NOUT2+i
        tout(l) = T0 + tout1(i)
        do j = 1, nbod
          do k = 1,3
            rb(l,j,k) = rout1(i,j,k)
            vb(l,j,k) = vout1(i,j,k)
          enddo
        enddo
      enddo

c add systemic velocity

      gamma_auday = kms_auday(gamma)
      do i = 1, NOUT
        do j = 1, nbod
          vb(i,j,3) = vb(i,j,3) + gamma_auday
        enddo
      enddo

      if (debug_swift) then
        open(unit=iu,file="out_JDATE_barycentric.dat",status="unknown")
        do i = 1, NOUT
          do j = 1, nbod
            write(iu,*) tout(i),-j,rb(i,j,1),rb(i,j,2),rb(i,j,3),
     :        vb(i,j,1),vb(i,j,2),vb(i,j,3)
          enddo
        enddo
        close(iu)
      endif

c convert to heliocentric (1-centric) coordinates

      do i = 1, NOUT
        do j = 1, nbod
          do k = 1, 3
            rh(i,j,k) = rb(i,j,k) - rb(i,1,k)
            vh(i,j,k) = vb(i,j,k) - vb(i,1,k)
          enddo
        enddo
      enddo

      if (debug_swift) then
        open(unit=iu,file="out_JDATE_heliocentric.dat",status="unknown")
        do i = 1, NOUT
          do j = 1, nbod
            write(iu,*) tout(i),-j,rh(i,j,1),rh(i,j,2),rh(i,j,3),
     :        vh(i,j,1),vh(i,j,2),vh(i,j,3)
          enddo
        enddo
        close(iu)
      endif

c convert to photocentric (1+2-centric) coordinates

      do i = 1, NOUT
        do k = 1, 3
          Lumtot = 0.d0
          r_photocentre(k) = 0.d0
          v_photocentre(k) = 0.d0
          do j = 1, min(2, nbod)
             Lumtot = Lumtot + Lum(j)
             r_photocentre(k) = r_photocentre(k) + rb(i,j,k)*Lum(j)
             v_photocentre(k) = v_photocentre(k) + vb(i,j,k)*Lum(j)
          enddo
          r_photocentre(k) = r_photocentre(k)/Lumtot
          v_photocentre(k) = v_photocentre(k)/Lumtot
        enddo
        do j = 1, nbod
          do k = 1, 3
            rp(i,j,k) = rb(i,j,k) - r_photocentre(k)
            vp(i,j,k) = vb(i,j,k) - v_photocentre(k)
          enddo
        enddo
      enddo

      if (debug_swift) then
        open(unit=iu,file="out_JDATE_photocentric.dat",status="unknown")
        do i = 1, NOUT
          do j = 1, nbod
            write(iu,*) tout(i),-j,rp(i,j,1),rp(i,j,2),rp(i,j,3),
     :        vp(i,j,1),vp(i,j,2),vp(i,j,3)
          enddo
        enddo
        close(iu)
      endif

c convert to yet another photocentric (1+2+3-centric) coordinates

      do i = 1, NOUT
        do k = 1, 3
          Lumtot = 0.d0
          r_photocentre(k) = 0.d0
          v_photocentre(k) = 0.d0
          do j = 1, min(3, nbod)
             Lumtot = Lumtot + Lum(j)
             r_photocentre(k) = r_photocentre(k) + rb(i,j,k)*Lum(j)
             v_photocentre(k) = v_photocentre(k) + vb(i,j,k)*Lum(j)
          enddo
          r_photocentre(k) = r_photocentre(k)/Lumtot
          v_photocentre(k) = v_photocentre(k)/Lumtot
        enddo
        do j = 1, nbod
          do k = 1, 3
            rp3(i,j,k) = rb(i,j,k) - r_photocentre(k)
            vp3(i,j,k) = vb(i,j,k) - v_photocentre(k)
          enddo
        enddo
      enddo

      if (debug_swift) then
        open(unit=iu,file="out_JDATE_photocentric3.dat",
     :    status="unknown")
        do i = 1, NOUT
          do j = 1, nbod
            write(iu,*) tout(i),-j,rp3(i,j,1),rp3(i,j,2),rp3(i,j,3),
     :        vp3(i,j,1),vp3(i,j,2),vp3(i,j,3)
          enddo
        enddo
        close(iu)
      endif

c optionally, write (u, v, w) coordinates

      if (debug_swift.and.use_vardist) then
        call write_uvw(NOUT, tout, rh, rp, rp3)
      endif

c-----------------------------------------------------------------------
c
c calculate the chi^2 values
c
c   1. speckle-interferometry data
c   2. radial-velocity data
c   3. transit-timing variations
c   4. eclipse durations
c   5. interferometric visibilities
c   6. interferometric closure phases
c   7. interferometric triple product amplitude
c   8. light curve
c   9. synthetic spectra
c  10. spectral-energy distribution
c  11. adaptive optics data
c  12. differential astrometry
c  13. angular velocity
c
      call chi2_func_SKY(NOUT, tout, rh, rp, rp3, chi2_SKY, n_SKY)

      call chi2_func_RV(NOUT, tout, vb, chi2_RV, n_RV)

      call chi2_func_TTV(NOUT, NOUT2, m, tout, rh, rb, nmin, tmin,
     :  duration, chi2_TTV, n_TTV)

      call chi2_func_ECL(nmin, tmin, duration, chi2_ECL, n_ECL)

      call chi2_func_VIS(NOUT, m, tout, rh, chi2_VIS, n_VIS)

      call chi2_func_CLO(NOUT, tout, rh, chi2_CLO, n_CLO)

      call chi2_func_T3(chi2_T3, n_T3)

      call chi2_func_LC(NOUT, NOUT2, m, tout, rh, vh, rb, chi2_LC, n_LC)

      call chi2_func_SYN(NOUT, tout, vb, chi2_SYN, n_SYN)

      call chi2_func_SED(chi2_SED, n_SED)

      call chi2_func_AO(chi2_AO, n_AO)

      call chi2_func_SKY2(NOUT, tout, rh, chi2_SKY2, n_SKY2)

      call chi2_func_SKY3(NOUT, tout, rh, vh, chi2_SKY3, n_SKY3)

c-----------------------------------------------------------------------
c
c add an artificial term to constrain the masses!
c
      chi2_MASS = 0.d0
      do i = 1, nbod
        j = i  ! in x_param()
        chi2_MASS = chi2_MASS + merit_func(x_param(j),m_min(i),m_max(i))
      enddo

c-----------------------------------------------------------------------

c sum the results

      n_fit = n_SKY + n_RV + n_TTV + n_ECL + n_VIS + n_CLO + n_T3 + n_LC
     :  + n_SYN + n_SED + n_AO + n_SKY2 + n_SKY3
      chi2 = w_SKY*chi2_SKY + w_RV*chi2_RV + w_TTV*chi2_TTV
     :  + w_ECL*chi2_ECL + w_VIS*chi2_VIS + w_CLO*chi2_CLO
     :  + w_T3*chi2_T3 + w_LC*chi2_LC + w_SYN*chi2_SYN
     :  + w_SED*chi2_SED + w_AO*chi2_AO + w_SKY2*chi2_SKY2
     :  + w_SKY3*chi2_SKY3 + chi2_MASS

      write(*,30) "# chi^2 value: "
      write(*,*)
     :  n_SKY, n_RV, n_TTV, n_ECL, n_VIS, n_CLO, n_T3, n_LC, n_SYN,
     :  n_SED, n_AO, n_SKY2, n_SKY3, n_fit, chi2_SKY, chi2_RV, chi2_TTV,
     :  chi2_ECL, chi2_VIS, chi2_CLO, chi2_T3, chi2_LC, chi2_SYN,
     :  chi2_SED, chi2_AO, chi2_SKY2, chi2_SKY3, chi2_MASS, chi2

c write hi-precision output

      open(unit=iu, file="chi2_func.tmp", access="append")
      write(iu,*) (x_param(j), j=1,nparam),
     :  n_SKY, n_RV, n_TTV, n_ECL, n_VIS, n_CLO, n_T3, n_LC, n_SYN,
     :  n_SED, n_AO, n_SKY2, n_SKY3, n_fit, chi2_SKY, chi2_RV, chi2_TTV,
     :  chi2_ECL, chi2_VIS, chi2_CLO, chi2_T3, chi2_LC, chi2_SYN,
     :  chi2_SED, chi2_AO, chi2_SKY2, chi2_SKY3, chi2_MASS, chi2
      close(iu)

      chi2_func = chi2

      return
      end


