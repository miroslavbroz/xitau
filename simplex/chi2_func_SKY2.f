c chi2_func_SKY2.f
c Calculate chi^2 for differential astrometry data.
c Miroslav Broz (miroslav.broz@email.cz), Sep 9th 2020

      subroutine chi2_func_SKY2(NOUT, tout, rh, chi2, n)

      implicit none
      include '../misc/const.inc'
      include 'simplex.inc'
      include 'dependent.inc'

c input
      integer NOUT
      real*8 tout(OUTMAX)
      real*8 rh(OUTMAX,NBODMAX,3)

c output
      real*8 chi2
      integer n

c observational data
      integer m_SKY, one(OBSMAX), two(OBSMAX)
      real*8 t_SKY(OBSMAX), x12_arcsec(OBSMAX), y12_arcsec(OBSMAX),
     :  major_arcsec(OBSMAX), minor_arcsec(OBSMAX), PA_ellipse(OBSMAX),
     :  vardist(OBSMAX), ecl(OBSMAX), ecb(OBSMAX)

c internal variables
      real*8 x12_SKY(OBSMAX), y12_SKY(OBSMAX), major(OBSMAX),
     :  minor(OBSMAX)
      integer i, j, k, l, i1st, iu, nmin, ialpha
      real*8 chi2_
      real*8 x12_interp, y12_interp, z12_interp
      real*8 dx, dy, dz, dx_, dy_, dz_, phi
      real*8 tmp, u, v, w, u_arcsec, v_arcsec
      real*8 t_interp, lite

c functions
      real*8 arcsec_au, au_arcsec, interp

      data i1st /0/
      data m_SKY /0/
      data iu /10/

      save i1st, m_SKY, t_SKY, one, two, x12_SKY, y12_SKY, major,
     :  minor, PA_ellipse, vardist, ecl, ecb

c-----------------------------------------------------------------------
c
c read differential astrometry observations (only 1st time!)
c
      if (i1st.eq.0) then

        call read_SKY2(file_SKY2, m_SKY, t_SKY, one, two,
     :    x12_arcsec, y12_arcsec, major_arcsec, minor_arcsec,
     :    PA_ellipse, vardist, ecl, ecb)

        if (debug) then
          write(*,*) "# m_SKY2 = ", m_SKY
        endif

        i1st = 1

      endif  ! i1st

c convert arcsec -> AU (always)

      if (debug) then
        iu=20
        open(unit=iu,file="arcsec_AU2.dat",status="unknown")
        write(iu,*) "# t_SKY & x12_SKY [AU] & y12_SKY & major & ",
     :    "minor & PA_ellipse [rad] & vardist [au] & one & two"
      endif

      do i = 1, m_SKY

        if (use_vardist) then
          tmp = vardist(i)*AU/pc
        else
          tmp = d_pc
        endif
        x12_SKY(i) = arcsec_au(x12_arcsec(i), tmp)
        y12_SKY(i) = arcsec_au(y12_arcsec(i), tmp)
        major(i) = arcsec_au(major_arcsec(i), tmp)
        minor(i) = arcsec_au(minor_arcsec(i), tmp)

        if (debug) then
          write(iu,*) t_SKY(i), x12_SKY(i), y12_SKY(i), major(i),
     :      minor(i), PA_ellipse(i), vardist(i), one(i), two(i)
        endif

      enddo

      if (debug) then
        close(iu)
      endif

c-----------------------------------------------------------------------
c
c calculate the chi^2 value (differential astrometry data)
c
      chi2 = 0.d0
      n = 0

      if (debug) then
        open(unit=iu,file="chi2_SKY2.dat",status="unknown")
        write(iu,*) "# t_SKY+lite & x12_interp [arcsec]",
     :    " & y12_interp [arcsec] & major & minor & PA_ellipse [deg]",
     :    " & vardist [au] & one & two & chi^2"
        write(iu,*) "# t_SKY      & x12_SKY    [arcsec]",
     :    " & y12_SKY    [arcsec] & major & minor & PA_ellipse [deg]",
     :    " & vardist [au] & one & two & chi^2"
      endif

      j = 2
      do i = 1, m_SKY

c light-time effect

        if (use_vardist) then
          lite = -vardist(i)/c * AU/day 
        else
          lite = 0.d0
        endif
        t_interp = t_SKY(i) + lite

        do while ((j.lt.NOUT).and.(tout(j).le.t_interp))
          j = j+1
        enddo

c linear interpolation of integrated data to a given position in time

c use relative coordinates

        dx = rh(j,two(i),1) - rh(j,one(i),1)
        dy = rh(j,two(i),2) - rh(j,one(i),2)
        dz = rh(j,two(i),3) - rh(j,one(i),3)

        dx_ = rh(j-1,two(i),1) - rh(j-1,one(i),1)
        dy_ = rh(j-1,two(i),2) - rh(j-1,one(i),2)
        dz_ = rh(j-1,two(i),3) - rh(j-1,one(i),3)

        x12_interp = interp(tout(j-1), tout(j), dx_, dx, t_interp)
        y12_interp = interp(tout(j-1), tout(j), dy_, dy, t_interp)
        z12_interp = interp(tout(j-1), tout(j), dz_, dz, t_interp)

        if (use_vardist) then
          call uvw(ecl(i), ecb(i), x12_interp, y12_interp, z12_interp,
     :      u, v, w)
        else
          u = x12_interp
          v = y12_interp
        endif

        dx = u - x12_SKY(i)
        dy = v - y12_SKY(i)

        phi = +(PA_ellipse(i)+pi_/2.d0)
        dx_ =  dx*cos(phi) + dy*sin(phi)
        dy_ = -dx*sin(phi) + dy*cos(phi)

        chi2_ = (dx_/major(i))**2 + (dy_/minor(i))**2
        chi2 = chi2 + chi2_
        n = n + 2

        if (debug) then

          if (use_vardist) then
            tmp = vardist(i)*AU/pc
          else
            tmp = d_pc
          endif

! use constant d_pc instead of vardist for debugging
!          tmp = d_pc

          u_arcsec = au_arcsec(u, tmp)
          v_arcsec = au_arcsec(v, tmp)
          write(iu,*) t_interp, u_arcsec, v_arcsec,
     :      major_arcsec(i), minor_arcsec(i), PA_ellipse(i)*rad,
     :      vardist(i), one(i), two(i), chi2_

          u_arcsec = au_arcsec(x12_SKY(i), tmp)
          v_arcsec = au_arcsec(y12_SKY(i), tmp)
          write(iu,*) t_SKY(i), u_arcsec, v_arcsec,
     :      major_arcsec(i), minor_arcsec(i), PA_ellipse(i)*rad,
     :      vardist(i), one(i), two(i), chi2_
          write(iu,*)
        endif

      enddo

      if (debug) then
        close(iu)
      endif

      return
      end

