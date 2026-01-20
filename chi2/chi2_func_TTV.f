c chi2_func_TTV.f
c Calculate chi^2 for transit-timing variations.
c Miroslav Broz (miroslav.broz@email.cz), Jul 24th 2015

      subroutine chi2_func_TTV(NOUT, NOUT2, m, tout, rh,
     :  nmin, tmin, duration, chi2, n)

      implicit none
      include '../misc/const.inc'
      include 'chi2.inc'
      include 'dependent.inc'

c input
      integer NOUT,NOUT2
      real*8 m(NBODMAX)
      real*8 tout(OUTMAX)
      real*8 rh(OUTMAX,NBODMAX,3)

c output
      integer nmin
      integer eclipsed(MINMAX)
      real*8 tmin(MINMAX), duration(MINMAX)
      real*8 chi2
      integer n

c observational data
      integer m_TTV
      real*8 t_TTV(OBSMAX), sigmat_TTV(OBSMAX)

c internal variables
      integer i, j, k, l, i1st, iu
      real*8 chi2_
      real*8 xh_interp, yh_interp, dx, dy
      real*8 A(2), B(2), C_(2), t_, dmax, v, d
      logical extra
      real*8 t_of_closest,OMC
      real*8 vx, vy

c functions
      real*8 interp, au_day, distance_AB_C

      data i1st /0/
      data m_TTV /0/
      data iu /10/

      save i1st,
     :  m_TTV, t_TTV, sigmat_TTV

c-----------------------------------------------------------------------
c
c read transit-timing variations (only 1st time!)
c
      if (i1st.eq.0) then

        call read_TTV(file_TTV, m_TTV, t_TTV, sigmat_TTV)

        if (debug) then
          write(*,*) "# m_TTV = ", m_TTV
        endif

        i1st = 1

      endif  ! i1st

c-----------------------------------------------------------------------
c
c  chi^2 for the transit-timing variations
c

c find ALL synthetic minima of the eclipsing binary

      nmin = 0
      dmax = (R_star(1)+R_star(2))*R_S/AU

      if (debug_swift) then
        open(unit=iu,file="eclipses.dat",status='unknown')
        write(iu,*) "# A(1) A(2) B(1) B(2) d  dmax vx   vy   duration",
     :    " eclipsed"
        write(iu,*) "# au   au   au   au   au au   au/d au/d day     ",
     :    " -"
      endif

      if ((m_TTV.gt.0).or.(debug)) then
        do i = 1, NOUT-1
          A(1) = rh(i,2,1)
          A(2) = rh(i,2,2)
          B(1) = rh(i+1,2,1)
          B(2) = rh(i+1,2,2)
          C_(1) = 0.d0
          C_(2) = 0.d0

          d = distance_AB_C(A, B, C_, t_, extra)

          if (.not.extra) then
            if (d.le.dmax) then
              nmin = nmin+1
              if (nmin.lt.MINMAX) then
                tmin(nmin) = tout(i) + t_ * (tout(i+1)-tout(i))
                v = sqrt((B(1)-A(1))**2 + (B(2)-A(2))**2)
     :            / (tout(i+1)-tout(i))
                duration(nmin) = 2.d0*sqrt(dmax**2-d**2) / v
                if (rh(i,2,3).gt.0.d0) then
                  eclipsed(nmin) = 2
                else
                  eclipsed(nmin) = 1
                endif
                if (debug_swift) then
                  vx = (B(1)-A(1))/(tout(i+1)-tout(i))
                  vy = (B(2)-A(2))/(tout(i+1)-tout(i))
                  write(iu,*) A(1),A(2),B(1),B(2),d,dmax,vx,vy,
     :              duration(nmin),eclipsed(nmin),i,tout(i),t_,extra
                endif
              else
                write(*,*) "chi2_func_TTV.f: Error number of minima ",
     :            "exceeds MINMAX = ", MINMAX
                stop
              endif
            else
c              write(*,*) "Warning: missing primary or secondary ",
c     :          "minimum. d = ", d, " AU, dmax = ", dmax, " AU"
            endif
          endif

        enddo
      endif

      if (debug_swift) then
        close(iu)
      endif

      if (debug_swift) then
        write(*,*) "# nmin = ", nmin

        open(unit=iu,file="minima.dat",status="unknown")
        write(iu,*) "# Min JD (with LITE) & eclipsed [1|2]"
        do i = 1, nmin
          write(iu,*) tmin(i), eclipsed(i)
        enddo
        close(iu)
      endif

      if (debug) then
        open(unit=iu,file="chi2_TTV.dat",status="unknown")
        write(iu,*) "# t_TTV & t_of_closest [JD] & ",
     :    "O-C (with LITE) [day] & sigmat_TTV & chi^2"
      endif

      chi2 = 0.d0
      n = 0

      if (nmin.ge.2) then

        k = 2
        do i = 1, m_TTV

c find the closest synthetic minimum to the observed one

          do while ((k.lt.nmin).and.(tmin(k).le.t_TTV(i)))
            k = k+1
          enddo

          if ((tmin(k)-t_TTV(i)).lt.(t_TTV(i)-tmin(k-1))) then
            t_of_closest = tmin(k)
          else
            t_of_closest = tmin(k-1)
          endif

          OMC = t_TTV(i)-t_of_closest
          chi2_ = (OMC/sigmat_TTV(i))**2
          lns = lns + log(sigmat_TTV(i))
          chi2 = chi2 + chi2_
          n = n + 1

          if (debug) then
            write(iu,*) t_TTV(i), t_of_closest, OMC, sigmat_TTV(i),
     :        chi2_
          endif

        enddo

      endif  ! nmin

      if (debug) then
        close(iu)
      endif

      return
      end


