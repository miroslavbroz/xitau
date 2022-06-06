c chi2_func_ECL.f
c Calculate chi^2 for eclipse durations.
c Miroslav Broz (miroslav.broz@email.cz), Jul 24th 2015

      subroutine chi2_func_ECL(nmin, tmin, duration, chi2, n)

      implicit none
      include '../misc/const.inc'
      include 'simplex.inc'
      include 'dependent.inc'

c input
      integer nmin
      real*8 tmin(MINMAX), duration(MINMAX)  ! from chi2_func_TTV.f

c output
      real*8 chi2
      integer n

c observational data
      integer m_ECL
      real*8 t_ECL(OBSMAX), d_ECL(OBSMAX), sigmad_ECL(OBSMAX)

c internal variables
      integer i,j,k,l,i1st,iu
      real*8 chi2_
      real*8 d_of_closest

      data i1st /0/
      data m_ECL /0/
      data iu /10/

      save i1st,
     :  m_ECL,t_ECL,d_ECL,sigmad_ECL

c-----------------------------------------------------------------------
c
c read eclipse durations (only 1st time!)
c
      if (i1st.eq.0) then

        call read_ECL(file_ECL, m_ECL, t_ECL, d_ECL, sigmad_ECL)

        if (debug) then
          write(*,*) "# m_ECL = ", m_ECL
        endif

        i1st = 1

      endif  ! i1st

c-----------------------------------------------------------------------
c
c  chi^2 for the eclipse durations
c
      if (debug_swift) then
        open(unit=iu,file="duration.dat",status="unknown")
        write(iu,*) "# Min JD & eclipse durations [day]"
        do i = 1, nmin
          write(iu,*) tmin(i), duration(i)
        enddo
        close(iu)
      endif

      if (debug) then
        open(unit=iu,file="chi2_ECL.dat",status="unknown")
        write(iu,*) "# t_ECL [JD] & duration     [day] & sigmad_ECL",
     :    " & chi^2"
        write(iu,*) "# t_ECL [JD] & duration_ECL [day] & sigmad_ECL",
     :    " & chi^2"
      endif

      chi2 = 0.d0
      n = 0

      if (nmin.ge.2) then

        k = 2
        do i = 1, m_ECL

c find the closest synthetic minimum to the observed one

          do while ((k.lt.nmin).and.(tmin(k).le.t_ECL(i)))
            k = k+1
          enddo

          if ((tmin(k)-t_ECL(i)).lt.(t_ECL(i)-tmin(k-1))) then
            d_of_closest = duration(k)
          else
            d_of_closest = duration(k-1)
          endif

c account for light-time effect too? <- probably not necessary

          chi2_ = ((d_ECL(i)-d_of_closest)/sigmad_ECL(i))**2
          lns = lns + log(sigmad_ECL(i))
          chi2 = chi2 + chi2_
          n = n + 1

          if (debug) then
            write(iu,*) t_ECL(i), d_ECL(i), sigmad_ECL(i), chi2_
            write(iu,*) t_ECL(i), d_of_closest, sigmad_ECL(i), chi2_
            write(iu,*)
          endif

        enddo

      endif  ! nmin

      if (debug) then
        close(iu)
      endif

      return
      end


