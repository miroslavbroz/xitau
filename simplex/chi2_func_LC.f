c chi2_func_LC.f
c Compute synthetic lightcurve and corresponding chi^2.
c Miroslav Broz (miroslav.broz@email.cz), Apr 20th 2016

      subroutine chi2_func_LC(NOUT, NOUT2, m, tout, rh, vh, rb, chi2, n)

      implicit none
      include '../misc/const.inc'
      include 'simplex.inc'
      include 'dependent.inc'
      include 'filters.inc'
c input
      integer NOUT, NOUT2
      real*8 m(NBODMAX)
      real*8 tout(OUTMAX)
      real*8 rh(OUTMAX,NBODMAX,3), vh(OUTMAX,NBODMAX,3)
      real*8 rb(OUTMAX,NBODMAX,3)

c output
      real*8 chi2
      integer n

c observational data
      integer m_OBS(BANDMAX)
      real*8 t_OBS(OBSMAX,BANDMAX), mag_OBS(OBSMAX,BANDMAX),
     :  sigma_mag_OBS(OBSMAX,BANDMAX)

c internal variables
      integer i, j, k, i1st, i2nd, i3rd, iu
      integer m_BIN(BANDMAX)
      real*8 t_BIN(OBSMAX,BANDMAX), mag(OBSMAX,BANDMAX), eps
      real*8 gpha, xincl
      integer iband
      real*8 vkm1(OBSMAX,BANDMAX), vkm2(OBSMAX,BANDMAX)
      real*8 xh_interp, yh_interp, zh_interp
      real*8 vxh_interp, vyh_interp, vzh_interp
      real*8 mag_interp, chi2_
      real*8 X(3), Y(3), Z(3), V(3), O(3), O_EB(3)
      real*8 mu, dt, LITE, LITE12
      real*8 zb1, zb2, zb_interp, zb_interp0
      real*8 x1, y1, z1, vx1, vy1, vz1
      real*8 x2, y2, z2, vx2, vy2, vz2
      integer iflg
      real*8 Lumtot, Lum3
      real*8 dist, r1, r2, r3
      real*8 tavh, tavc, poth, potc, rm, hlum, clum, el3

c functions
      real*8 interp, dotprod, au_day, omega_kopal, omega_kopal_approx

      data iu /15/  ! not 10 (which is used by the WD code)
      data i1st /0/
      data i3rd /0/

      save i1st, i2nd, i3rd,
     :  m_OBS, t_OBS, mag_OBS, sigma_mag_OBS, m_BIN, t_BIN
c
c read lightcurve data
c
      if (i1st.eq.0) then

        do k = 1, nband

          call read_LC(file_LC(k), m_OBS(k), t_OBS(1,k), mag_OBS(1,k),
     :      sigma_mag_OBS(1,k))

          if (debug) then
            write(*,*) "# m_LC(", k, ") = ", m_OBS(k)
          endif
c
c Select kind of important times of observations
c and use binning if the cadence is uselessly high.
c This is only for the computation of synthetic LC;
c the complete observed LC will be used for chi^2.
c
          eps = 1.d-8
          if (m_OBS(k).ge.1) then
            j = 1
            t_BIN(j,k) = t_OBS(1,k)-eps

            do i = 2, m_OBS(k)
              if (t_OBS(i,k)-t_BIN(j,k) > lightcurve_timestep) then
                j = j + 1
                t_BIN(j,k) = t_OBS(i,k)
              endif
            enddo

            j = j + 1
            t_BIN(j,k) = t_OBS(m_OBS(k),k)+eps
            m_BIN(k) = j
          else
            m_BIN(k) = 0
          endif

          if (debug) then
            write(*,*) "# m_BIN(", k, ") = ", m_BIN(k)
          endif
c
c read parameters from lc.in file
c
          if ((m_BIN(k).gt.0).and.(i3rd.eq.0)) then
            call lc_read_in("lc.in")
            i3rd = 1
          endif

        enddo

        i1st = 1

      endif  ! i1st

c
c adjust the parameters affected by simplex
c
      if (debug) then
        open(unit=iu, file="lightcurve.dat", status="unknown")
        write(iu,*) "# JD & magnitude & iband & true phase [0-1]",
     :    " & inclination [deg] & vkm1 & vkm2 [km/s]"
      endif

      do k = 1, nband

        tavh = T_eff(1)/1.d4
        tavc = T_eff(2)/1.d4
 
        rm = m(2)/m(1)
        iband = iband_LC(k)
 
        call luminosities(T_eff, R_star, nbod, lambda_eff(iband),
     :    band_eff(iband), Lum, Lumtot, use_planck)
 
        Lumtot = 0.d0
        do j = 1, nbod
          Lumtot = Lumtot + Lum(j)
        enddo
        hlum = Lum(1)/Lumtot
        clum = Lum(2)/Lumtot
 
        Lum3 = 0.d0
        do j = 3, min(4,nbod)
          Lum3 = Lum3 + Lum(j)
        enddo
        el3 = (Lum3/Lumtot)/(4.d0*pi_)
 
        i2nd = 0  ! enforce initialisation of WD
c
c Interpolate trajectory to the (binned) times of observations;
c use an average of keplerian drift from two neighbouring points.
c 

c barycenter at T0
        zb_interp0 = (rb(NOUT2,1,3)*m(1)+rb(NOUT2,2,3)*m(2))/(m(1)+m(2))
 
        j = 2
        do i = 1, m_BIN(k)
          
          do while ((j.lt.NOUT).and.(tout(j).le.t_BIN(i,k)))
            j = j + 1
          enddo

c account for light-time effect due to external bodies

          zb1 = interp(tout(j-1), tout(j), rb(j-1,1,3), rb(j,1,3),
     :      t_BIN(i,k))
          zb2 = interp(tout(j-1), tout(j), rb(j-1,2,3), rb(j,2,3),
     :      t_BIN(i,k))
          zb_interp = (zb1*m(1)+zb2*m(2)) / (m(1)+m(2))  ! barycenter of 1+2 body, z coordinate
 
          LITE = au_day(zb_interp - zb_interp0)
 
          mu = m(1)+m(2)
          x1 = rh(j-1,2,1)
          y1 = rh(j-1,2,2)
          z1 = rh(j-1,2,3)
          vx1 = vh(j-1,2,1)
          vy1 = vh(j-1,2,2)
          vz1 = vh(j-1,2,3)
          LITE12 = -au_day(z1)
          dt = t_BIN(i,k) - (tout(j-1) + LITE + LITE12)
          call drift_one(mu, x1, y1, z1, vx1, vy1, vz1, dt, iflg)

c positive timespan -> negative velocities
          x2 = rh(j,2,1)
          y2 = rh(j,2,2)
          z2 = rh(j,2,3)
          vx2 = -vh(j,2,1)
          vy2 = -vh(j,2,2)
          vz2 = -vh(j,2,3)
          LITE12 = -au_day(z2)
          dt = tout(j) + LITE + LITE12 - t_BIN(i,k)
          call drift_one(mu, x2, y2, z2, vx2, vy2, vz2, dt, iflg)
c
c compute true phase and inclination (i.e. NOT usual mean phase)
c
c vectors expressed in 1-centric frame:
c
c   X ... along the eclipsing binary O--o
c   V ... velocity of secondary wrt. primary
c   Z ... X x V, i.e. perpendicular to the orbital plane
c   Y ... Z x X
c   O ... observer's direction
c
c vectors projected to the EB system:
c
c   O_EB ... observer's direction

          X(1) = (x1+x2)/2.d0
          X(2) = (y1+y2)/2.d0
          X(3) = (z1+z2)/2.d0
          dist = sqrt(X(1)**2 + X(2)**2 + X(3)**2)
          V(1) = (vx1-vx2)/2.d0
          V(2) = (vy1-vy2)/2.d0
          V(3) = (vz1-vz2)/2.d0
 
          call vproduct(X, V, Z)
          call vproduct(Z, X, Y)
          call normalize(X)
          call normalize(Y)
          call normalize(Z)
 
          O(1) = 0.d0
          O(2) = 0.d0
          O(3) = -1.d0
 
          O_EB(1) = dotprod(O, X)
          O_EB(2) = dotprod(O, Y)
          O_EB(3) = dotprod(O, Z)
 
          gpha = atan2(O_EB(2), O_EB(1))/(2.d0*pi_)
          xincl = acos(-O_EB(3))*rad 
 
          if (gpha.lt.0.d0) then
            gpha = gpha + 1.d0
          endif
c
c approximate values of Kopal potential
c
          poth = omega_kopal_approx(R_star(1)*R_S/(dist*AU), rm, 0.d0)
          potc = omega_kopal_approx(R_star(2)*R_S/(dist*AU), rm, 1.d0)
c
c compute magnitude with the Wilson & Devinney (1971) code
c
          call lc_call_nbody(gpha, xincl, tavh, tavc,
     :      poth, potc, rm, iband, hlum, clum, el3, debug, i2nd,
     :      mag(i,k), vkm1(i,k), vkm2(i,k))

          mag(i,k) = mag(i,k) + zero(k)

          if (debug) then
            write(iu,*) t_BIN(i,k), mag(i,k), iband, gpha, xincl,
     :        vkm1(i,k), vkm2(i,k)
          endif
        enddo
        if (debug) then
          write(iu,*)
        endif
      enddo  ! nband

      if (debug) then
        close(iu)
      endif

c
c interpolate synthetic lightcurve to the exact times of observations
c
      if (debug) then
        open(unit=iu,file="chi2_LC.dat",status="unknown")
        write(iu,*) "# t_OBS & mag_interp & sigma_mag_OBS & iband &",
     :    " chi^2"
        write(iu,*) "# t_OBS & mag_OBS    & sigma_mag_OBS & iband &",
     :    " chi^2"
      endif

      n = 0
      chi2 = 0.d0
      do k = 1, nband

        j = 2
        do i = 1, m_OBS(k)

          do while ((j.lt.m_BIN(k)).and.(t_BIN(j,k).le.t_OBS(i,k)))
            j = j + 1
          enddo

          if (m_BIN(k).gt.1) then
            mag_interp = interp(t_BIN(j-1,k), t_BIN(j,k), mag(j-1,k),
     :        mag(j,k), t_OBS(i,k))
          else
            mag_interp = mag(1,k)
          endif

          chi2_ = ((mag_interp-mag_OBS(i,k))/sigma_mag_OBS(i,k))**2
          n = n + 1
          chi2 = chi2 + chi2_
         
          if (debug) then
            write(iu,*) t_OBS(i,k), mag_OBS(i,k), sigma_mag_OBS(i,k),
     :        iband_LC(k), chi2_
            write(iu,*) t_OBS(i,k), mag_interp, sigma_mag_OBS(i,k),
     :        iband_LC(k), chi2_
            write(iu,*)
          endif

        enddo

      enddo  ! nband

      if (debug) then
        close(iu)
      endif

      return
      end

