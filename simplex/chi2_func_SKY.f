c chi2_func_SKY.f
c Calculate chi^2 for a speckle-interferometry data.
c Miroslav Broz (miroslav.broz@email.cz), Mar 1st 2016

      subroutine chi2_func_SKY(NOUT, tout, rp, rp3, chi2, n)

      implicit none
      include '../misc/const.inc'
      include 'simplex.inc'
      include 'dependent.inc'

c input
      integer NOUT
      real*8 tout(OUTMAX)
      real*8 rp(OUTMAX,NBODMAX,3)
      real*8 rp3(OUTMAX,NBODMAX,3)

c output
      real*8 chi2
      integer n

c observational data
      integer m_SKY(NBODMAX)
      real*8 t_SKY(OBSMAX,NBODMAX),
     :  xh_arcsec(OBSMAX,NBODMAX), yh_arcsec(OBSMAX,NBODMAX),
     :  major_arcsec(OBSMAX,NBODMAX), minor_arcsec(OBSMAX,NBODMAX),
     :  PA_ellipse(OBSMAX,NBODMAX)

c internal variables
      real*8 xh_SKY(OBSMAX,NBODMAX), yh_SKY(OBSMAX,NBODMAX),
     :  major(OBSMAX,NBODMAX), minor(OBSMAX,NBODMAX)
      integer i, j, k, l, i1st, iu, nmin, ialpha
      real*8 chi2_
      real*8 xh_interp, yh_interp, dx, dy, dx_, dy_, phi

c functions
      real*8 arcsec_au,interp

      data i1st /0/
      data m_SKY /NBODMAX*0/
      data iu /10/

      save i1st,
     :  m_SKY, t_SKY, xh_SKY, yh_SKY, major, minor, PA_ellipse

c-----------------------------------------------------------------------
c
c read speckle-interferometry observations (only 1st time!)
c
      if (i1st.eq.0) then

        do j = 1, nbod

          call read_SKY(file_SKY(j), m_SKY(j), t_SKY(1,j),
     :      xh_arcsec(1,j), yh_arcsec(1,j), major_arcsec(1,j),
     :      minor_arcsec(1,j), PA_ellipse(1,j))

c convert angular coordinates to AU according to given distance

          if (debug) then
            write(*,*) "# m_SKY(", j, ") = ", m_SKY(j)
          endif

        enddo

        i1st = 1

      endif  ! i1st

c convert arcsec -> AU (always)

      if (debug) then
        iu=20
        open(unit=iu,file="arcsec_AU.dat",status="unknown")
        write(iu,*) "# t_SKY & xh_SKY [AU] & yh_SKY & major & ",
     :    "minor & PA_ellipse [rad] & ibod"
      endif

      do j = 1, nbod
        do i = 1, m_SKY(j)

          xh_SKY(i,j) = arcsec_au(xh_arcsec(i,j), d_pc)
          yh_SKY(i,j) = arcsec_au(yh_arcsec(i,j), d_pc)
          major(i,j) = arcsec_au(major_arcsec(i,j), d_pc)
          minor(i,j) = arcsec_au(minor_arcsec(i,j), d_pc)

          if (debug) then
            write(iu,*) t_SKY(i,j), xh_SKY(i,j), yh_SKY(i,j),
     :        major(i,j), minor(i,j), PA_ellipse(i,j), j
          endif

        enddo
      enddo

      if (debug) then
        close(iu)
      endif

c-----------------------------------------------------------------------
c
c calculate the chi^2 value (speckle-interferometry data)
c

      chi2 = 0.d0
      n = 0

      if (debug) then
        open(unit=iu,file="chi2_SKY.dat",status="unknown")
        write(iu,*) "# t_SKY & xh_interp [AU] & yh_interp & ",
     :    "major & minor & PA_ellipse [rad] & ibod & chi^2"
        write(iu,*) "# t_SKY & xh_SKY         & yh_SKY    & ",
     :    "major & minor & PA_ellipse [rad] & ibod & chi^2"
      endif

      do k = 1, nbod
        j = 2
        do i = 1, m_SKY(k)

          do while ((j.lt.NOUT).and.(tout(j).le.t_SKY(i,k)))
            j = j+1
          enddo

c linear interpolation of integrated data to a given position in time

          if (k.lt.4) then
            xh_interp = interp(tout(j-1), tout(j), rp(j-1,k,1),
     :        rp(j,k,1), t_SKY(i,k))  ! k-th body, x coordinate
            yh_interp = interp(tout(j-1), tout(j), rp(j-1,k,2),
     :        rp(j,k,2), t_SKY(i,k))  ! y coordinate

c use the 1+2+3 photocentre for low-resolution interferometry

          else
            xh_interp = interp(tout(j-1), tout(j), rp3(j-1,k,1),
     :        rp3(j,k,1), t_SKY(i,k))
            yh_interp = interp(tout(j-1), tout(j), rp3(j-1,k,2),
     :        rp3(j,k,2), t_SKY(i,k))

          endif

          dx = xh_interp - xh_SKY(i,k)
          dy = yh_interp - yh_SKY(i,k)

          phi = +(PA_ellipse(i,k)+pi_/2.d0)
          dx_ =  dx*cos(phi) + dy*sin(phi)
          dy_ = -dx*sin(phi) + dy*cos(phi)

          chi2_ = (dx_/major(i,k))**2 + (dy_/minor(i,k))**2
          chi2 = chi2 + chi2_
          n = n + 2

          if (debug) then
            write(iu,*) t_SKY(i,k), xh_interp, yh_interp,
     :        major(i,k), minor(i,k), PA_ellipse(i,k), k, chi2_
            write(iu,*) t_SKY(i,k), xh_SKY(i,k), yh_SKY(i,k),
     :        major(i,k), minor(i,k), PA_ellipse(i,k), k, chi2_
            write(iu,*)
          endif

        enddo
      enddo  ! nbod

      if (debug) then
        close(iu)
      endif

      return
      end


